#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)

datadir <- "/data/DataFest2018/subset2.csv"
my_data <- read_csv(file = datadir)
my_data %>% group_by(stateProvince) %>% summarize(count = n()) -> stateList # For list of unique states

# First, set 0's to None's
my_data %>% mutate(educationRequirement = replace(educationRequirement, 
                                                  educationRequirement == "0", "None")) -> my_data

# Second, set 1's to High school's
my_data %>% mutate(educationRequirement = replace(educationRequirement, 
                                                  educationRequirement == "1", "High school")) -> my_data

# Third, set 2's to High education's
my_data %>% mutate(educationRequirement = replace(educationRequirement, 
                                                  educationRequirement == "2", "Higher education")) -> my_data

# Fourth, set avgOverallRatings of 0 to NA since they're missing anyways
my_data %>% mutate(avgOverallRating = replace(avgOverallRating, avgOverallRating == 0, NA)) -> 
  my_data

my_data %>% mutate(estimatedSalary = replace(estimatedSalary, estimatedSalary == 0, NA)) ->
  my_data 
##################################################################################################

library(shiny)

ui <- fluidPage(
   
   # Application title
  titlePanel("Exploratory Data Analysis"),
   
   sidebarLayout(
      sidebarPanel(
         
         checkboxGroupInput(inputId = "education", label = h3("Education Level"), choices = 
                       c("None", "High school", "Higher education"), 
                       selected = c("None", "High school", "Higher education")),
         
         checkboxGroupInput(inputId = "supervisor", label = h3("Supervising Role"), choices =
                        list("No" = 0, "Yes" = 1), selected = list("No" = 0)),
         
         selectInput(inputId = "state", label = h3("State/Province"), choices = stateList$stateProvince, 
                     multiple = TRUE),
         
         sliderInput(inputId = "salaryRange", label = h3("Salary Range"), min = 0, 
                     max = max(my_data$estimatedSalary, na.rm = TRUE), value = c(40000, 70000), 
                     round = 3, pre = "$"),
         
         selectInput(inputId = "response", label = h3("Response"), choices = 
                       c("estimatedSalary", "jobAgeDays", "clicks", "localClicks", "avgOverallRating", 
                         "experienceRequired"), 
                     multiple = FALSE),
         
         selectInput(inputId = "predictor", label = h3("Predictor"), choices = 
                       c("estimatedSalary", "jobAgeDays", "clicks", "localClicks", "avgOverallRating",
                         "experienceRequired"), 
                     selected = "avgOverallRating", multiple = FALSE)
         
      ),
      
      mainPanel(
        fluidRow(
            plotOutput(outputId = "benjiPlot"),
            tableOutput("table")
        )
      )
   )
)

server <- function(input, output, session) {
  
   selectedData <- reactive({
     req(input$education, cancelOutput = TRUE)
     req(input$state, cancelOutput = TRUE)
     req(input$supervisor, cancelOutput = TRUE)
     
     tmp <- my_data %>%
       filter(educationRequirement %in% input$education) %>%
       filter(estimatedSalary >= input$salaryRange[1] & estimatedSalary <= input$salaryRange[2]) %>%
       filter(stateProvince %in% input$state) %>%
       filter(supervisingJob %in% input$supervisor)
     tmp <- data.frame(xs = unlist(tmp[, input$predictor]), ys = unlist(tmp[, input$response]))
   })
   
   selectedData2 <- reactive({
     req(input$education, cancelOutput = TRUE)
     req(input$state, cancelOutput = TRUE)
     #req(input$supervior, cancelOutput = TRUE)
     
     my_data %>%
       filter(educationRequirement %in% input$education) %>%
       filter(stateProvince %in% input$state) %>%
       filter(supervisingJob %in% input$supervisor) %>%
       filter(estimatedSalary >= input$salaryRange[1] & estimatedSalary <= input$salaryRange[2]) %>%
       group_by(stateProvince, normTitle) %>%
       summarize("Average_Salary" = mean(estimatedSalary, na.rm = TRUE)) %>%
       top_n(n = 5) %>%
       rename("State/Province" = stateProvince, "Job" = normTitle) %>%
       arrange(desc(Average_Salary)) -> tmp2
   })
   
   output$benjiPlot <- renderPlot({
     ggplot(selectedData(), aes(x = selectedData()$xs, y = selectedData()$ys)) + 
       geom_point() + 
       #geom_smooth(method = "lm") +
       xlab(input$predictor) +
       ylab(input$response) + 
       ylim(0.9*min(selectedData()$ys, na.rm = TRUE), 1.1*max(selectedData()$ys, na.rm = TRUE)) +
       theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 12, face = "bold"),
             axis.text = element_text(size = 12))
   })
   
   output$table <- renderTable(selectedData2(), hover = TRUE)
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

