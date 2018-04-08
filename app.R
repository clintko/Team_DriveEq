#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)

datadir <- "/data/DataFest2018/subset1.csv"
my_data <- read_csv(file = datadir)
my_data %>% group_by(stateProvince) %>% summarize(count = n()) -> stateList # For list of unique states

# First, set 0's to None's
my_data[!is.na(my_data$educationRequirement) & my_data$educationRequirement == "0", ]$educationRequirement <- "None"
# Second, set 1's to High school's
my_data[!is.na(my_data$educationRequirement) & my_data$educationRequirement == "1", ]$educationRequirement <- "High school"
# Third, set 2's to High education's
my_data[!is.na(my_data$educationRequirement) & my_data$educationRequirement == "2", ]$educationRequirement <- "Higher education"

##################################################################################################

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  titlePanel("Exploratory Data Analysis"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         
         selectInput(inputId = "education", label = h3("Education Level"), choices = 
                       c("None", "High school", "Higher education"), multiple = FALSE),
         
         selectInput(inputId = "state", label = h3("State"), choices = stateList$stateProvince, 
                     multiple = FALSE),
         
         sliderInput(inputId = "salaryRange", label = h3("Salary Range"), min = 0, 
                     max = max(my_data$estimatedSalary), value = c(40000, 70000), 
                     round = 3, pre = "$"),
         
         selectInput(inputId = "response", label = h3("Response"), choices = 
                       c("estimatedSalary", "jobAgeDays", "clicks", "localClicks"), 
                     multiple = FALSE),
         
         selectInput(inputId = "predictor", label = h3("Predictor"), choices = 
                       c("estimatedSalary", "jobAgeDays", "clicks", "localClicks"), 
                     selected = "jobAgeDays", multiple = FALSE)
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
          plotOutput("benjiPlot"),
          textOutput("selected_var"),
          tableOutput("table")
      )
   )
)

server <- function(input, output, session) {
  
   selectedData <- reactive({
     tmp <- my_data %>%
       filter(educationRequirement == input$education) %>%
       filter(estimatedSalary >= input$salaryRange[1] & estimatedSalary <= input$salaryRange[2]) %>%
       filter(stateProvince == input$state) 
     tmp <- data.frame(x = unlist(tmp[, input$predictor]), y = unlist(tmp[, input$response]))
   })
   
   selectedData2 <- reactive({
     my_data %>% 
       na.omit %>% 
       group_by(stateProvince, normTitle, input$response) %>% 
       summarize() %>%
       arrange(stateProvince, desc(input$response)) %>%
       filter(!duplicated(normTitle))
   })
   
   selectedData2 <- reactive({
     my_data %>%
       filter(educationRequirement == input$education) %>%
       filter(stateProvince == input$state) %>%
       filter(estimatedSalary >= input$salaryRange[1] & estimatedSalary <= input$salaryRange[2]) %>%
       group_by(stateProvince, normTitle) %>%
       summarize("Average_Salary" = mean(estimatedSalary)) %>%
       top_n(n = 5) %>%
       rename("State/Province" = stateProvince, "Job" = normTitle) %>%
       arrange(desc(Average_Salary))
   })
   
   output$benjiPlot <- renderPlot({
     ggplot(selectedData(), aes(x = selectedData()$x, y = selectedData()$y)) + 
       geom_point() + 
       geom_smooth(method = "lm") +
       xlab(input$predictor) +
       ylab(input$response) + 
       theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 12, face = "bold"),
             axis.text = element_text(size = 12))
   })
   
   output$selected_var <- renderText({ 
     paste(str(input$response), str(input$predictor))
   })
   
   output$table <- renderTable(selectedData2(), spacing = "l", hover = TRUE)
   
}

# Run the application 
shinyApp(ui = ui, server = server)

