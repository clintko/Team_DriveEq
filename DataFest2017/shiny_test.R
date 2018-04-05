library(shiny)
library(ggplot2)

#ui <- fluidPage()
#server <- function(input, output) {}
#shinyApp(ui, server)

###############################################

server <- function(input, output) {
  
  # render in server.R goes with output in ui.R
  output$hist <- renderPlot({
    n <- input $n
    df <- data.frame(x=rnorm(n))
    g <- ggplot(df, aes(x=x)) +
      geom_histogram(bins=n/5) +
      geom_vline(xintercept = mean(df$x), color="red")
    g
  })
  
  # render in server.R goes with output in ui.R
  output$table <- renderTable({
    n <- input $n
    df <- data.frame(x=rnorm(n))
    df %>% summarise(mean=mean(x), median=median(x), sd=sd(x))
  })
}

##################################################

ui <- fluidPage(
  h1("Look at the mean"),
  sliderInput("n", "Number of data points:", 10, 100, 50), # variable , label, arguments
  
  # plotOutput in ui.R goes with renderPlot in server.ui
  plotOutput("hist"),
  
  # plotTable in ui.R goes with renderTable in server.ui
  tableOutput("table")
)

shinyApp(ui, server)
