#  info Democracy Shiny app

# Packages ----------------------------------------------------------------
library(shiny)
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
        dateRangeInput("date_range",
                       label = "Date range",
                       start = min(donations$x_donation_date, na.rm = T),
                       end = Sys.Date()),
        checkboxInput("public_funding", label = "Include public funding?", value = FALSE),
        checkboxInput("not_yet_coded", label = "Include not yet coded?", value = FALSE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("main_plot")
      )
   )
)

# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$main_plot <- renderPlot({
      # filter data
      data <- donations %>% 
        filter(x_donation_date >= input$date_range[1],
               x_donation_date <= input$date_range[2]) %>% 
        group_by(level_1_short) %>% 
        summarise(value = sum(dntn_value))
      
      # create graph
      ggplot(data, aes(reorder(level_1_short, value), value)) +
        geom_bar(stat = 'identity') +
        coord_flip() +
        labs(title = 'Donations by interest group of donor',
             x = 'Interest Group',
             y = 'Total value of donations')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
