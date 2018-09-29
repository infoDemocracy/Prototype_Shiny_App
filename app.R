# infoDemocracy Shiny app

# Packages ----------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(dplyr)
library(forcats)
library(ggplot2)
library(DT)
library(readr)
library(lubridate)

# Data --------------------------------------------------------------------
load('info_democracy.Rdata')
brexit <- read_csv('brexit.csv')
evidence <- read_csv("evidence.csv")

# Remove pre-poll duplicates
donations <- donations %>% 
  filter(dntn_is_reported_pre_poll == FALSE,
         level_1 != 'P1') %>% 
  left_join(brexit, by = 'dntn_regulated_entity_name')

parties <- c('Conservative and Unionist Party',
             'Labour Party',
             'Liberal Democrats',
             'UK Independence Party (UKIP)',
             'Green Party',
             'Scottish National Party (SNP)',
             'Plaid Cymru - The Party of Wales') 

sectors <- donations %>%
  pull(level_1_short) %>%
  unique() %>%
  sort()

donors <- donations %>% 
  pull(x_donor_name) %>% 
  unique() %>% 
  sort()

# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = 'infoDemocracy'),
  dashboardSidebar(
    sidebarMenu(menuItem("Overview", tabName = "overview", icon = icon("th")),
                menuItem("By party", tabName = "by_party", icon = icon("th")),
                menuItem("By sector", tabName = "by_sector", icon = icon("th")),
                menuItem("Brexit", tabName = "brexit", icon = icon("th")),
                menuItem("Donors", tabName = "donors", icon = icon("th")),
                menuItem("Notes", tabName = "notes", icon = icon("th")))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'overview',
              fluidRow(column(width = 8,
                              box(width = 12,
                                  title = 'Introduction',
                                  p('Welcome to infoDemocracy! The idea of this project is to try and understand political funding in the UK in a more systematic way.',
                                    'We do this by researching individual donors and assigning \'interest codes\' based on their primary source of income.',
                                    'In this way the project takes inspiration from', a('Thomas Ferguson', href = 'https://en.wikipedia.org/wiki/Thomas_Ferguson_(academic)'), 'and', a('Open Secrets', href = 'https://www.opensecrets.org/'), ', and applies their approach to political finance to the UK.'),
                                  p('More information about the methodology behind the data can be found on the notes tab.')),
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
                           title = 'By party',
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
                           title = 'By sector',
                           plotOutput('by_sector_party'))),
                column(width = 4,
                       box(width = 12,
                           title = 'Inputs',
                           selectInput('by_sector_sector',
                                       label = 'Sector',
                                       choices = as.list(sectors)
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
      
      tabItem(tabName = 'brexit',
              fluidRow(
                column(width = 8,
                       box(width = 12,
                           title = 'Brexit',
                           plotOutput('brexit_by_sector'))),
                column(width = 4,
                       box(width = 12,
                           title = 'Inputs',
                           selectInput('brexit_position',
                                       label = 'Position',
                                       choices = as.list(c('Leave', 'Remain'))),
                           checkboxInput("brexit_not_yet_coded",
                                         label = "Include not yet coded?",
                                         value = FALSE)
                           ),
                       infoBox(width = 12,
                               title = 'Value - Leave',
                               paste0('£', format(sum(filter(donations, brexit_position == 'Leave')$dntn_value), big.mark = ',', nsmall = 2))),
                       infoBox(width = 12,
                               title = 'Value - Remain',
                               paste0('£', format(sum(filter(donations, brexit_position == 'Remain')$dntn_value), big.mark = ',', nsmall = 2)))
                       )
              ),
              fluidRow(
                box(width = 12,
                    title = 'Donors',
                    DT::dataTableOutput("brexit_donor_table"))
              )),
      
      tabItem(tabName = 'donors',
              fluidRow(
                column(width = 8,
                       box(width = 12,
                           title = 'Donors',
                           plotOutput('donor_by_year'))),
                column(width = 4,
                       box(width = 12,
                           title = 'Inputs',
                           selectizeInput(inputId = 'donors',
                                          label = 'Select donor',
                                          choices = c(Choose = '', as.list(donors))),
                           strong('Interest code:'),
                           p(textOutput(outputId = 'donor_interest_code')),
                           strong('Profiles:'),
                           uiOutput(outputId = 'donor_wikipedia'),
                           uiOutput(outputId = 'donor_powerbase')),
                       infoBoxOutput(width = 12,
                                     "donor_infobox")
                       )
              ),
              fluidRow(
                tabBox(width = 12,
                       tabPanel('Data', DT::dataTableOutput("donor_info_table")),
                       tabPanel('Evidence', DT::dataTableOutput("donor_evidence"))
                       )
                )),
      
      tabItem(tabName = 'notes',
              box(title = 'Notes',
                  width = 12,
                  p('Notes coming soon.'))
              )
    )
  )
)

# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  # Overview ----
  
  output$overview_time <- renderPlot({
    donations %>% 
      filter(!is.na(x_donation_year)) %>% 
      group_by(x_donation_year) %>% 
      summarise(total = sum(dntn_value)) %>% 
      ggplot(aes(x_donation_year, total)) + 
      geom_bar(stat = 'identity', fill = 'navyblue') +
      scale_x_continuous(limits = c(2000, year(today())+1), breaks = 2000:(year(today())+1)) +
      labs(title = 'Total value of donations by year',
           x = 'Year',
           y = 'Total value (£)')
  })
  
  # By party ----
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
  
  # By sector ----
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
      labs(x = 'Party',
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
  
  # Brexit ---- 
  
  brexit <- reactive({
    donations %>% 
      filter(dntn_reporting_period_name == 'Referendum on the UK’s membership of the EU')
  })
  
  brexit_not_yet_coded <- reactive({
    if(input$brexit_not_yet_coded) return(brexit())
    filter(brexit(), level_1 != 'Z')
  })
  
  brexit_position <- reactive({
    brexit_not_yet_coded() %>% 
      filter(brexit_position == input$brexit_position)
  })
  
  output$brexit_by_sector <- renderPlot({
    brexit_not_yet_coded() %>% 
        group_by(level_1_short, Position = brexit_position) %>% 
        summarise(value = sum(dntn_value)) %>% 
        ggplot(aes(level_1_short, value, fill = Position)) +
        geom_bar(stat = 'identity', position = 'dodge') +
        coord_flip() +
        labs(x = 'Sector',
             y = 'Total value of donations (£)')
    })
    
    output$brexit_donor_table <- DT::renderDataTable({
      brexit_position() %>% 
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
    
    # Donors ----
    
    donor_info <- reactive({
      
      req(input$donors)
      
      donations %>% 
        filter(x_donor_name == input$donors)
    })
    
    output$donor_info_table <- DT::renderDataTable({
      donor_info() %>%
        select(Reference = dntn_ec_ref,
               Date = x_donation_date,
               Recipient = dntn_regulated_entity_name,
               `Donated as` = dntn_donor_name,
               Value = dntn_value) %>% 
        arrange(Date)
    }, escape = FALSE)
    
    output$donor_evidence <- DT::renderDataTable({
      id <- donations$donor_id[match(input$donors, donations$x_donor_name)]
      
      filter(evidence, donor_id == id) %>% 
        select(Evidence = evidence) %>% 
        mutate(Evidence = paste0('<a href="', Evidence, '">', Evidence, '</a>'))
    }, escape = FALSE)
    
    output$donor_interest_code <- renderText({
      donor_info() %>% 
        pull(level_1_short) %>%
        unique()
    })
    
    output$donor_wikipedia <- renderUI({
      wikipedia <- donor_info() %>% 
        pull(wikipedia) %>%
        unique()
      
      if(is.na(wikipedia)) return(NULL)
      
      a('Wikipedia', href = wikipedia)
      
    })
    
    output$donor_powerbase <- renderUI({
      powerbase <- donor_info() %>% 
        pull(powerbase) %>%
        unique()
      
      if(is.na(powerbase)) return(NULL)
      
      a('Powerbase', href = powerbase)
      
    })
    
    output$donor_infobox <- renderInfoBox({
      infoBox(
        "Value selected", paste0('£', format(sum(donor_info()$dntn_value), nsmall = 2, big.mark = ','))
      )
    })
    
    output$donor_by_year <- renderPlot({
      donor_info() %>% 
        group_by(x_donation_year) %>% 
        summarise(total = sum(dntn_value)) %>% 
        ggplot(aes(x_donation_year, total)) + 
        geom_bar(stat = 'identity', fill = 'navyblue') +
        scale_x_continuous(limits = c(2000, year(today())+1), breaks = 2000:(year(today())+1)) +
        labs(title = 'Total value of donations by year',
             x = 'Year',
             y = 'Total value (£)')
    })

}

# Run application ---------------------------------------------------------
shinyApp(ui = ui, server = server)
