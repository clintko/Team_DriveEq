# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#### Load necessary packages and data ####
library(tidyverse)
library(shiny)
library(networkD3)

datadir <- "/data/DataFest2018"
MAX <- 0.99
CURRENCY_CAD2USD = 0.78
CURRENCY_EUR2USD = 1.23

##########################
dat_job <- read_csv(file.path(datadir, "data_job_subset2.csv"))
adj     <- read_csv(file.path(datadir, "adj_matrix_subset2.csv"))

# ##########################
# # READ In data
# 
# ## sub dataset
# data_input <- read_csv(file.path(datadir, "subset2.csv"))
# #data_input <- read_csv(file.path(datadir, "subset1.csv"))
# data_input <- data_input[,-1]
# 
# 
# ## CAUTION: full dataset
# #data_input <- read_csv(file.path(datadir, "datafest2018.csv"))
# 
# ##########################
# subdata <- data_input %>% 
#     #filter(salaryCurrency == "USD") %>%
#     select(normTitle, #normTitleCategory, 
#          educationRequirement, 
#          experienceRequired, 
#          supervisingJob, 
#          estimatedSalary, 
#          licenseRequiredJob,
#          salaryCurrency) %>%
#     na.omit
# 
# ##########################
# tmp <- subdata
# 
# # deal with Currency
# tmp <- tmp %>% 
#     mutate(
#         estimatedSalary = ifelse(
#             salaryCurrency == "EUR", 
#                 estimatedSalary * CURRENCY_EUR2USD, estimatedSalary)) %>% 
#     mutate(
#         estimatedSalary = ifelse(
#             salaryCurrency == "CAD", 
#                 estimatedSalary * CURRENCY_CAD2USD, estimatedSalary))
# 
# # deal with education
# x <- c(0, 1, 2, 0, 1, 2)
# y <- c(0, 1, 2, "None", "High school", "Higher education")
# tmp$educationRequirement <- x[match(tmp$educationRequirement, y)]
# 
# # summarizing the job
# tmp <- tmp %>% 
#     group_by(normTitle) %>% 
#     summarise(
#         educationRequirement = mean(educationRequirement), 
#         experienceRequired   = mean(experienceRequired), 
#         supervisingJob       = mean(supervisingJob), 
#         licenseRequiredJob   = mean(licenseRequiredJob),
#         estimatedSalary      = mean(estimatedSalary))
# dat_job <- tmp %>%  as.data.frame
# 
# ###############
# 
# tmp <- dat_job
# rownames(tmp) <- tmp$normTitle
# 
# tmp <- tmp %>%
#     select(-normTitle) %>% #, -normTitleCategory) %>% 
#     as.matrix
# 
# dst <- as.matrix(dist(scale(tmp)))
# sim <- 1 / (1 + dst)
# adj <- sim - diag(1, dim(sim))


#### Server ####
server <- function(input, output) {
    output$simple <- renderSimpleNetwork({
      
        adj2  <- ifelse(adj > input$cutoff, 1, 0)
        links <- melt(adj2) %>% filter(value == 1) %>% select(-value)
    
        jobs1 <- input$job
        tmp   <- links %>% filter(Var1 %in% jobs1)
        jobs2 <- c(jobs1, tmp$Var2)
        net   <- links %>% filter(Var1 %in% jobs2)
    
        simpleNetwork(
            net, 
            fontSize     = input$fontSize,
            charge       = input$charge,
            linkDistance = input$linkDistance,
            opacity      = input$opacity)
    })
} # end server

#### UI ####

ui <- shinyUI(fluidPage(
  
  titlePanel("Shiny networkD3 "),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("job", "Select Your Job", rownames(dst), 
                  selected = "data scientist", multiple = TRUE,
                  selectize = TRUE, width = NULL, size = NULL),
      
      sliderInput("cutoff", "Cut Off Value", 0.6, 
                  min = 0.5, max = MAX, step = 0.01),
      
      sliderInput("fontSize", "Font Size", 15, 
                  min = 5, max = 20, step = 1),
      
      sliderInput("linkDistance", "Scale", 50, 
                  min = 50, max = 1000, step = 50),
      
      sliderInput("charge", "Force", -500, 
                  min = -1000, max = 0, step = 100),
      
      sliderInput("opacity", "Opacity", 0.7, 
                  min = 0.1, max = 1, step = .1)
      
    ), # end siderbarPanel
    
    
      
    mainPanel(
      tabsetPanel(
        tabPanel("Simple Network", simpleNetworkOutput("simple"))
      ) # end tabsetPanel
    ) # end MainPanel
  ) # end sidebarLayout
)) # end shinyUI

#### Run ####
shinyApp(ui = ui, server = server)