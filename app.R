#  info Democracy Shiny app

# Packages ----------------------------------------------------------------
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
load('info_democracy.Rdata')

# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("infoDemocracy"),
   h1('Test'),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        dateRangeInput("dates", label = "Date range"),
        checkboxInput("checkbox", label = "Include public funding?", value = FALSE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

