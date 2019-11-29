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
library(scales)

# Data --------------------------------------------------------------------
load('Data/info_democracy.Rdata')
brexit <- read_csv('Data/brexit.csv')
evidence <- read_csv("Data/evidence.csv")
ge2019 <- info_democracy %>% filter(dntn_reporting_period_name == "Pre-Poll 1 - Party(06/11/19 - 12/11/19) UKPGE 2019")

# Remove pre-poll duplicates
info_democracy <- info_democracy %>% 
  filter(x_is_reported_pre_poll == FALSE,
         level_1 != 'P1') %>% 
  left_join(brexit, by = 'dntn_regulated_entity_name')

parties <- c('Conservative and Unionist Party',
             'Labour Party',
             'Liberal Democrats',
             'UK Independence Party (UKIP)',
             'Green Party',
             'Scottish National Party (SNP)',
             'Plaid Cymru - The Party of Wales') 

sectors <- info_democracy %>%
  pull(level_1_short) %>%
  unique() %>%
  sort()

donors <- info_democracy %>% 
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
                menuItem("Donors", tabName = "donors", icon = icon("th")),
                menuItem("Brexit", tabName = "brexit", icon = icon("th")),
                menuItem("General Election 2019", tabName = "general_election", icon = icon("th")),
                menuItem("Download data", tabName = "data", icon = icon("th")),
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
                                      paste0('£', format(sum(info_democracy$dntn_value), big.mark = ',', nsmall = 2))),
                              infoBox(width = 12,
                                      title = 'Donations',
                                      format(nrow(info_democracy), big.mark = ',')),
                              infoBox(width = 12,
                                      title = 'Donations Coded',
                                      paste0(round(mean(info_democracy$x_coded)*100, 1), '%')),
                              infoBox(width = 12,
                                      title = 'Value Coded',
                                      paste0(round((sum(info_democracy[info_democracy$x_coded == TRUE, ]$dntn_value/sum(info_democracy$dntn_value)))*100, 1), '%'))
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
                                          start = min(info_democracy$x_donation_date, na.rm = T),
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
                                          start = min(info_democracy$x_donation_date, na.rm = T),
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
                                          choices = as.list(donors)),
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
                               paste0('£', format(sum(filter(info_democracy, brexit_position == 'Leave')$dntn_value), big.mark = ',', nsmall = 2))),
                       infoBox(width = 12,
                               title = 'Value - Remain',
                               paste0('£', format(sum(filter(info_democracy, brexit_position == 'Remain')$dntn_value), big.mark = ',', nsmall = 2)))
                       )
              ),
              fluidRow(
                box(width = 12,
                    title = 'Donors',
                    DT::dataTableOutput("brexit_donor_table"))
              )),
      
      tabItem(tabName = 'general_election',
              fluidRow(
                width = 12,
                box(width = 4,
                    title = 'Inputs',
                    selectizeInput(inputId = 'ge2019',
                                   label = 'Select party',
                                   choices = as.list(parties))),
                infoBoxOutput(width = 4,
                              "general_election_infobox"),
                infoBoxOutput(width = 4,
                              "general_election_total")
              ),
              fluidRow(
                column(
                  width = 12,
                  box(width = 6,
                      title = 'Summary',
                      plotOutput('general_election_summary')),
                  box(width = 6,
                      title = 'Party',
                      plotOutput('general_election_party'))
                )
              ),
              fluidRow(
                box(width = 12,
                    title = 'Donors',
                    DT::dataTableOutput("general_election_donor_table"))
              )
      ),
      
      tabItem(tabName = 'data',
              box(title = 'Download data',
                  width = 12,
                  p('Data coming soon.'))
              ),
      
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
    info_democracy %>% 
      filter(!is.na(x_donation_year)) %>% 
      group_by(x_donation_year) %>% 
      summarise(total = sum(dntn_value)) %>% 
      ggplot(aes(x_donation_year, total)) + 
      geom_bar(stat = 'identity', fill = 'navyblue') +
      scale_x_continuous(limits = c(2000, year(today())+1), breaks = 2000:(year(today())+1)) +
      scale_y_continuous(labels = dollar_format(prefix = '£')) +
      labs(title = 'Total value of donations by year',
           x = 'Year',
           y = 'Total value (£)')
  })
  
  # By party ----
  by_party <- reactive({
    by_party <- info_democracy %>% 
      filter(dntn_regulated_entity_name == input$by_party_party,
             x_donation_date >= input$by_party_date_range[1],
             x_donation_date <= input$by_party_date_range[2])
    
    if(input$by_party_not_yet_coded) return(by_party)
    filter(by_party, level_1 != 'Z')
    
  })
  
  output$by_party_sector <- renderPlot({
    by_party() %>%
      group_by(level_1_short) %>% 
      summarise(value = sum(dntn_value)) %>% 
      ggplot(aes(level_1_short, value)) +
      geom_bar(stat = 'identity', fill = 'navyblue') +
      scale_y_continuous(labels = dollar_format(prefix = '£')) +
      coord_flip() +
      labs(x = 'Sector',
           y = 'Total value of donations (£)')
  })
  
  output$by_party_donor_table <- DT::renderDataTable({
    by_party() %>% 
      group_by(Donor = x_donor_name,
               `Interest Group` = level_1_short,
               Wikipedia = wikipedia,
               Powerbase = powerbase) %>% 
      summarise(Donations = n(),
                Value = round(sum(dntn_value))) %>% 
      ungroup() %>% 
      arrange(desc(Value)) %>% 
      mutate(Wikipedia = ifelse(!is.na(Wikipedia), paste0('<a href="', Wikipedia, '" target="_blank">Here</a>'), NA),
             Powerbase = ifelse(!is.na(Powerbase), paste0('<a href="', Powerbase, '" target="_blank">Here</a>'), NA)) %>% 
      DT::datatable(escape = FALSE) %>% 
      formatCurrency('Value', currency = '£')
  })
  
  output$by_party_infobox <- renderInfoBox({
    infoBox(
      "Value selected", paste0('£', format(sum(by_party()$dntn_value), nsmall = 2, big.mark = ','))
    )
  })
  
  # Donors ----
  
  donor_info <- reactive({
    
    req(input$donors)
    
    info_democracy %>% 
      filter(x_donor_name == input$donors)
  })
  
  output$donor_info_table <- DT::renderDataTable({
    donor_info() %>%
      select(Reference = dntn_ec_ref,
             Date = x_donation_date,
             Recipient = dntn_regulated_entity_name,
             `Donated as` = dntn_donor_name,
             Value = dntn_value) %>% 
      arrange(Date) %>% 
      DT::datatable(escape = FALSE) %>% 
      formatCurrency('Value', currency = '£')
  })
  
  output$donor_evidence <- DT::renderDataTable({
    id <- info_democracy$donor_id[match(input$donors, info_democracy$x_donor_name)]
    
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
      scale_y_continuous(labels = dollar_format(prefix = '£')) +
      labs(title = 'Total value of donations by year',
           x = 'Year',
           y = 'Total value (£)')
  })
  
  # By sector ----
  by_sector <- reactive({
    info_democracy %>% 
      filter(level_1_short == input$by_sector_sector,
             dntn_regulated_entity_name %in% parties,
             x_donation_date >= input$by_sector_date_range[1],
             x_donation_date <= input$by_sector_date_range[2])
  })
  
  output$by_sector_infobox <- renderInfoBox({
    infoBox(
      "Value selected", paste0('£', format(sum(by_sector()$dntn_value), nsmall = 2, big.mark = ','))
    )
  })
  
  output$by_sector_party <- renderPlot({
    by_sector() %>%
      group_by(dntn_regulated_entity_name) %>% 
      summarise(value = sum(dntn_value)) %>% 
      ggplot(aes(fct_reorder(dntn_regulated_entity_name, value), value)) +
      geom_bar(stat = 'identity', fill = 'navyblue') +
      scale_y_continuous(labels = dollar_format(prefix = '£')) +
      coord_flip() +
      labs(x = 'Party',
           y = 'Total value of donations (£)')
  })
  
  output$by_sector_donor_table <- DT::renderDataTable({
    by_sector() %>% 
      group_by(Donor = x_donor_name,
               `Interest Group` = level_1_short,
               Wikipedia = wikipedia,
               Powerbase = powerbase) %>% 
      summarise(Donations = n(),
                Value = round(sum(dntn_value))) %>% 
      ungroup() %>% 
      arrange(desc(Value)) %>% 
      mutate(Wikipedia = ifelse(!is.na(Wikipedia), paste0('<a href="', Wikipedia, '" target="_blank">Here</a>'), NA),
             Powerbase = ifelse(!is.na(Powerbase), paste0('<a href="', Powerbase, '" target="_blank">Here</a>'), NA)) %>% 
      DT::datatable(escape = FALSE) %>% 
      formatCurrency('Value', currency = '£')
  })
  
  # Brexit ---- 
  
  brexit <- reactive({
    info_democracy %>% 
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
      scale_y_continuous(labels = dollar_format(prefix = '£')) +
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
               Powerbase = ifelse(!is.na(Powerbase), paste0('<a href="', Powerbase, '" target="_blank">Here</a>'), NA)) %>% 
        DT::datatable(escape = FALSE) %>% 
        formatCurrency('Value', currency = '£')
    })
    
    # General Election 2019 ----
    
    ge2019_party <- reactive({
      ge2019 %>%
        filter(dntn_regulated_entity_name == input$ge2019)
    })
    
    output$general_election_summary <- renderPlot({
      ge2019 %>% 
        filter(dntn_regulated_entity_name %in% parties) %>% 
        group_by(dntn_regulated_entity_name) %>% 
        summarise(total = sum(dntn_value)) %>% 
        ggplot(aes(fct_reorder(dntn_regulated_entity_name, total), total)) +
        geom_col(fill = 'navyblue') +
        scale_y_continuous(labels = dollar_format(prefix = '£')) +
        labs(x = 'Party') +
        coord_flip()
    })
    
    output$general_election_party <- renderPlot({
      ge2019_party() %>% 
        group_by(level_1_short) %>% 
        summarise(Total = sum(dntn_value)) %>% 
        ggplot(aes(fct_reorder(level_1_short, Total), Total)) +
        geom_col(fill = 'navyblue') +
        scale_y_continuous(labels = dollar_format(prefix = '£')) +
        labs(x = 'Party') +
        coord_flip()
    })
    
    output$general_election_donor_table <- DT::renderDataTable({
      ge2019_party() %>% 
        group_by(Donor = x_donor_name,
                 `Interest Group` = level_1_short,
                 Wikipedia = wikipedia,
                 Powerbase = powerbase) %>% 
        summarise(Donations = n(),
                  Value = round(sum(dntn_value))) %>% 
        ungroup() %>% 
        arrange(desc(Value)) %>% 
        mutate(Wikipedia = ifelse(!is.na(Wikipedia), paste0('<a href="', Wikipedia, '" target="_blank">Here</a>'), NA),
               Powerbase = ifelse(!is.na(Powerbase), paste0('<a href="', Powerbase, '" target="_blank">Here</a>'), NA)) %>% 
        DT::datatable(escape = FALSE) %>% 
        formatCurrency('Value', currency = '£')
    })
    
    output$general_election_infobox <- renderInfoBox({
      infoBox(
        "Value selected", paste0('£', format(sum(ge2019_party()$dntn_value), nsmall = 2, big.mark = ','))
      )
    })
    
    output$general_election_total <- renderInfoBox({
      infoBox(
        "General Election Total", paste0('£', format(sum(ge2019$dntn_value), nsmall = 2, big.mark = ','))
      )
    })

}

# Run application ---------------------------------------------------------
shinyApp(ui = ui, server = server)