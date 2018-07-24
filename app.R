# infoDemocracy Shiny app

# Packages ----------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(dplyr)
library(forcats)
library(ggplot2)
library(DT)

# Data --------------------------------------------------------------------
load('info_democracy.Rdata')

# Remove pre-poll duplicates
donations <- donations %>% 
  filter(dntn_is_reported_pre_poll %in% c('False', 'Normal'),
         level_1 != 'P1')

parties <- c('Conservative and Unionist Party',
             'Labour Party',
             'Liberal Democrats',
             'UK Independence Party (UKIP)',
             'Green Party',
             'Scottish National Party (SNP)',
             'Plaid Cymru - The Party of Wales') 

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
                                       choices = as.list(parties)
                           ),
                           dateRangeInput("by_party_date_range",
                                          label = "Date range",
                                          start = min(donations$x_donation_date, na.rm = T),
                                          end = Sys.Date()),
                           checkboxInput("by_party_not_yet_coded",
                                         label = "Include not yet coded?",
                                         value = FALSE)
                           ),
                       infoBoxOutput(width = 12,
                                     "by_party_infobox")
                       )
                ),
              fluidRow(
                box(width = 12,
                    title = 'Donors',
                    DT::dataTableOutput("by_party_donor_table"))
              )
              ),
      tabItem(tabName = 'by_sector',
              fluidRow(
                column(width = 8,
                       box(width = 12,
                           title = 'Donations by sector',
                           plotOutput('by_sector_party'))),
                column(width = 4,
                       box(width = 12,
                           title = 'Inputs',
                           selectInput('by_sector_sector',
                                       label = 'Sector',
                                       choices = donations %>%
                                         pull(level_1_short) %>%
                                         unique() %>%
                                         sort() %>% 
                                         as.list()
                           ),
                           dateRangeInput("by_sector_date_range",
                                          label = "Date range",
                                          start = min(donations$x_donation_date, na.rm = T),
                                          end = Sys.Date())
                       ),
                       infoBoxOutput(width = 12,
                                     "by_sector_infobox")
                       )
              ),
              fluidRow(
                box(width = 12,
                    title = 'Donors',
                    DT::dataTableOutput("by_sector_donor_table"))
                )
              ),
      
      tabItem(tabName = 'notes',
              box(title = 'Notes', p('Notes coming soon.'))
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
      geom_bar(stat = 'identity', fill = 'navyblue') +
      labs(title = 'Total value of donations by year',
           x = 'Year',
           y = 'Total value (£)')
  })
  
  # By party
  by_party_party <- reactive({
    donations %>% 
      filter(dntn_regulated_entity_name == input$by_party_party)
  })
  
  by_party_date_range <- reactive({
    by_party_party() %>% 
      filter(x_donation_date >= input$by_party_date_range[1],
             x_donation_date <= input$by_party_date_range[2])
  })
  
  by_party_not_yet_coded <- reactive({
    if(input$by_party_not_yet_coded) return(by_party_date_range())
    filter(by_party_date_range(), level_1 != 'Z')
  })
  
  output$by_party_sector <- renderPlot({
    by_party_not_yet_coded() %>%
      group_by(level_1_short) %>% 
      summarise(value = sum(dntn_value)) %>% 
      ggplot(aes(level_1_short, value)) +
      geom_bar(stat = 'identity', fill = 'navyblue') +
      coord_flip() +
      labs(x = 'Sector',
           y = 'Total value of donations (£)')
  })
  
  output$by_party_donor_table <- DT::renderDataTable({
    by_party_not_yet_coded() %>% 
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
  
  output$by_party_infobox <- renderInfoBox({
    infoBox(
      "Value selected", paste0('£', format(sum(by_party_not_yet_coded()$dntn_value), nsmall = 2, big.mark = ','))
    )
  })
  
  # By sector
  by_sector_sector <- reactive({
    donations %>% 
      filter(level_1_short == input$by_sector_sector,
             dntn_regulated_entity_name %in% parties)
  })
  
  by_sector_date_range <- reactive({
    by_sector_sector() %>% 
      filter(x_donation_date >= input$by_sector_date_range[1],
             x_donation_date <= input$by_sector_date_range[2])
  })
  
  output$by_sector_infobox <- renderInfoBox({
    infoBox(
      "Value selected", paste0('£', format(sum(by_sector_date_range()$dntn_value), nsmall = 2, big.mark = ','))
    )
  })
  
  output$by_sector_party <- renderPlot({
    by_sector_date_range() %>%
      group_by(dntn_regulated_entity_name) %>% 
      summarise(value = sum(dntn_value)) %>% 
      ggplot(aes(fct_reorder(dntn_regulated_entity_name, value), value)) +
      geom_bar(stat = 'identity', fill = 'navyblue') +
      coord_flip() +
      labs(x = 'Sector',
           y = 'Total value of donations (£)')
  })
  
  output$by_sector_donor_table <- DT::renderDataTable({
    by_sector_date_range() %>% 
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
