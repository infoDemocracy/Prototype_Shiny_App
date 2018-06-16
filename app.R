#  info Democracy Shiny app

# Packages ----------------------------------------------------------------
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# Data --------------------------------------------------------------------
load('info_democracy.Rdata')

# UI ----------------------------------------------------------------------
ui <- fluidPage(
   
   # Application title
   titlePanel("infoDemocracy"),
   h1('All donations within specified period'),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(width = 3, 
        dateRangeInput("date_range",
                       label = "Date range",
                       start = min(donations$x_donation_date, na.rm = T),
                       end = Sys.Date()),
        checkboxInput("public_funding", label = "Include public funding?", value = FALSE),
        checkboxInput("not_yet_coded", label = "Include not yet coded?", value = FALSE)
      ),
      
      mainPanel(width = 9,
        tabsetPanel(
          tabPanel("Plot", 
                  plotOutput("main_plot"),
                  plotOutput("main_plot2")),
          tabPanel("Donors", DT::dataTableOutput("donor_table"))
        )
      )
   )
)

# Server ------------------------------------------------------------------
server <- function(input, output) {
   
  plot_data <- reactive({
    donations %>% 
      filter(x_donation_date >= input$date_range[1],
             x_donation_date <= input$date_range[2])
  })
  
  plot_data_public <- reactive({
    if(input$public_funding) return(plot_data())
      filter(plot_data(), level_1 != 'P1')
  })
  
  plot_data_not_yet_coded <- reactive({
    if(input$not_yet_coded) return(plot_data_public())
    filter(plot_data_public(), level_1 != 'Z')
  })
  
   output$main_plot <- renderPlot({
     plot_data_not_yet_coded() %>%
       group_by(level_1_short) %>% 
       summarise(value = sum(dntn_value)) %>% 
       ggplot(aes(reorder(level_1_short, value), value)) +
       geom_bar(stat = 'identity', fill = 'navyblue') +
       coord_flip() +
       labs(title = 'Donations by interest group of donor',
             x = 'Interest Group',
             y = 'Total value of donations (£)')
   })
   
   output$main_plot2 <- renderPlot({
     plot_data_not_yet_coded() %>%
       filter(dntn_regulated_entity_type == 'Political Party') %>% 
       group_by(dntn_regulated_entity_name) %>% 
       summarise(value = sum(dntn_value)) %>% 
       ggplot(aes(reorder(dntn_regulated_entity_name, value), value)) +
       geom_bar(stat = 'identity', fill = 'navyblue') +
       coord_flip() +
       labs(title = 'Donations by interest party',
            x = 'Political Party',
            y = 'Total value of donations (£)')
   })
   
   output$donor_table <- DT::renderDataTable({
     plot_data_not_yet_coded() %>% 
       group_by(Donor = x_donor_name,
                `Interest Group` = level_1_short,
                Wikipedia = wikipedia,
                Powerbase = powerbase) %>% 
       summarise(Donations = n(),
                 Value = round(sum(dntn_value))) %>% 
       ungroup() %>% 
       arrange(desc(Value)) %>% 
       mutate(Wikipedia = ifelse(!is.na(Wikipedia), paste0('<a href="', Wikipedia, '" target="_blank">Here</a>'), NA),
              Powerbase = ifelse(!is.na(Powerbase), paste0('<a href="', Powerbase, '" target="_blank">Here</a>'), NA))
   }, escape = FALSE)
}

# Run application ---------------------------------------------------------
shinyApp(ui = ui, server = server)
