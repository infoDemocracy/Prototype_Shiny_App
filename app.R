# info Democracy Shiny app

# Packages ----------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)

# Data --------------------------------------------------------------------
load('info_democracy.Rdata')

# Remove pre-poll duplicates
donations <- donations %>% 
  filter(dntn_is_reported_pre_poll %in% c('False', 'Normal'),
         level_1 != 'P1')

# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = 'infoDemocracy'),
  dashboardSidebar(
    sidebarMenu(menuItem("Overview", tabName = "overview", icon = icon("signal")),
                menuItem("By party", tabName = "by_party", icon = icon("th")),
                menuItem("By sector", tabName = "by_sector", icon = icon("th")),
                menuItem("Notes", tabName = "notes", icon = icon("th")))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'overview',
              fluidRow(column(width = 8,
                              box(width = 12,
                                  title = 'Introduction',
                                  p('Welcome to infoDemocracy!')),
                              box(width = 12,
                                  plotOutput('overview_time'))
                              ),
                       column(width = 4,
                              infoBox(width = 12,
                                      title = 'Value',
                                      paste0('£', format(sum(donations$dntn_value), big.mark = ',', nsmall = 2))),
                              infoBox(width = 12,
                                      title = 'Donations',
                                      format(nrow(donations), big.mark = ',')),
                              infoBox(width = 12,
                                      title = 'Donations Coded',
                                      paste0(round(mean(donations$x_coded)*100, 1), '%')),
                              infoBox(width = 12,
                                      title = 'Value Coded',
                                      paste0(round((sum(donations[donations$x_coded == TRUE, ]$dntn_value/sum(donations$dntn_value)))*100, 1), '%'))
                              )
                       )
      ),
      
      tabItem(tabName = 'by_party',
              fluidRow(
                column(width = 8,
                       box(width = 12,
                           title = 'Donations by sector',
                           plotOutput('by_party_sector'))),
                column(width = 4,
                       box(width = 12,
                           title = 'Inputs',
                           selectInput('by_party_party',
                                       label = 'Party',
                                       choices = list(
                                         'Conservative and Unionist Party',
                                         'Labour Party',
                                         'Liberal Democrats',
                                         'UK Independence Party (UKIP)',
                                         'Green Party',
                                         'Scottish National Party (SNP)',
                                         'Plaid Cymru - The Party of Wales'
                                       )
                           ),
                           dateRangeInput("by_party_date_range",
                                          label = "Date range",
                                          start = min(donations$x_donation_date, na.rm = T),
                                          end = Sys.Date()),
                           checkboxInput("by_party_not_yet_coded",
                                         label = "Include not yet coded?",
                                         value = FALSE)
                           )
                       )
                )
              ),
      
      tabItem(tabName = 'notes',
              box(title = 'Notes', p('Notes coming soon.'))
              ),
      
      tabItem(tabName = 'template',
              fluidRow(
                column(width = 9,
                       box(width = 12,
                           title = 'Test')),
                column(width = 3,
                       box(width = 12,
                           title = 'Inputs',
                           dateRangeInput("date_range",
                                          label = "Date range",
                                          start = min(donations$x_donation_date, na.rm = T),
                                          end = Sys.Date()))
                       )
                )
              )
    )
  )
)

# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  # Overview
  
  output$overview_time <- renderPlot({
    donations %>% 
      filter(!is.na(x_donation_year)) %>% 
      group_by(x_donation_year) %>% 
      summarise(total = sum(dntn_value)) %>% 
      ggplot(aes(x_donation_year, total)) + 
      geom_bar(stat = 'identity') +
      labs(title = 'Total value of donations by year',
           x = 'Year',
           y = 'Total value (£)')
  })
  
  # By party
  # By sector
  
  # Data
  
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
  
  # Plots
  
  output$main_plot <- renderPlot({
    plot_data_not_yet_coded() %>%
      group_by(level_1_short) %>% 
      summarise(value = sum(dntn_value)) %>% 
      ggplot(aes(reorder(level_1_short, value), value)) +
      geom_bar(stat = 'identity', fill = 'navyblue') +
      coord_flip() +
      labs(x = 'Interest Group',
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
      labs(x = 'Political Party',
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
