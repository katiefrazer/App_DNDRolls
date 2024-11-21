# Author: Katie Frazer

library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(bslib)

# UI - user interface
ui <- page_sidebar(
  title = "D&D Dice Roll Analysis",
  
  # sidebar on the left where users can select info
  sidebar = sidebar(
    
    # include description of purpose somewhere
    # include template Excel sheet for people to download from
    
    # accept Excel sheet from 
    fileInput("upload", "Upload Excel Sheet", accept = c(".xlsx", ".xls")),
    
    # initial dropdown for variable selection
    selectInput("variable", "Select Variable Type to Analyze:",
                choices = c("Rolls by Die Type" = "die_type",
                            "Rolls by Die Set" = "die_set",
                            "Rolls by Campaign" = "campaign",
                            "Rolls by Session" = "sess",
                            "Rolls Over Time" = "date")),
    
    # create a second drop down for each choice
    # die_type conditional panel for second drop down 
    conditionalPanel(
      condition = "input.variable == 'die_type'",
      selectInput("selected_type", "Select Die Type:", 
                  choices = c("d4", "d6", "d8", "d10", "d%%", "d12", "d20"))
    ),
    
    # die_set conditional panel for second drop down
    conditionalPanel(
      condition = "input.variable == 'die_set'",
      selectInput("selected_set", "Select Die Set:",
                  choices = NULL)
      # need to make it either text 
      # or read the input dataset and give dependent options
    ),
    
    # campaign conditional panel for second drop down
    conditionalPanel(
      condition = "input.variable == 'campaign'",
      selectInput("selected_campaign", "Select Campaign:",
                  choices = NULL)
    ),
    
    # session conditional panel for second drop down
    conditionalPanel(
      condition = "input.variable == 'sess'",
      selectInput("session_campaign", "Select Campaign:",
                  choices = NULL),
      selectInput("selected_session", "Select Session",
                  choices = NULL)
    ),
    
    # date conditional panel for second drop down
    conditionalPanel(
      condition = "input.variable == 'date'",
      dateRangeInput("date_range", "Select Date Range:",
                     start = NULL, end = NULL)
    )
    
    # filter ID_Number by campaign and session for conditional payments
  
  ),
  
  # main panel with plot output
  card(
    card_header("Dice Roll Distribution"),
    plotOutput("dice_plot")
  )
  
)
  

# server - backend
server <- function(input, output, session){
  
  # Read and process uploaded data
  dice_data <- reactive({
    req(input$upload)
    df <- read_excel(input$upload$datapath)
    df$Date <- as.Date(df$Date, format = "%Y %m %d")
    return(df)
    
  })
  
  # update choices based on uploaded data for DieType, DieSet, Campaign, Date
  observe({
    req(dice_data())
    updateSelectInput(session, "selected_type",
                      choices = unique(dice_data()$DieType))
    updateSelectInput(session, "selected_set",
                      choices = unique(dice_data()$DieSet))
    updateSelectInput(session, "selected_campaign",
                      choices = unique(dice_data()$Campaign))
    updateSelectInput(session, "session_campaign",
                      choices = unique(dice_data()$Campaign))
    updateDateRangeInput(session, "date_range",
                         start = min(dice_data()$Date),
                         end = max(dice_data()$Date))
  })
  
  observe({
    req(dice_data(), input$session_campaign)
    sessions <- dice_data() %>%
      filter(Campaign == input$session_campaign) %>%
      pull(Session) %>%
      unique() %>%
      sort()
    updateSelectInput(session, "selected_session",
                      choices = sessions)
  })
  
  # Generate the plot based on user input
  output$dice_plot <- renderPlot({
    req(dice_data())
    
  if (input$variable == "die_type"){
    req(input$selected_type)
    
    filtered_data <- dice_data() %>%
      filter(DieType == input$selected_type)
    
    ggplot(filtered_data, aes(x = DieRoll)) +
      geom_bar() + 
      labs(title = paste("Distribution of", input$selected_type, "Rolls"),
           x = "Roll Value",
           y = "Number of Times Rolled")
    # add scale somehow for each graph, hardset scale for each die type
    # dropdown dependent on order produced by excel sheet, order it by number
    # make scale free
  } else if (input$variable == "die_set"){
    req(input$selected_set)
    
    # adding this variable so the facet_wrap is in my specified order
    neworder <- c("d4", "d6", "d8", "d10", "d%%", "d12", "d20")
    
    filtered_data <- dice_data() %>%
      filter(DieSet == input$selected_set) %>%
      # mutating so facet_wrap is in my specified order
      mutate(across(DieType, ~factor(., levels = neworder)))
    
    # plotting DieRoll by DieSet, faceted by DieType
    ggplot(filtered_data, aes(x = DieRoll)) +
      geom_bar() +
      # offer option to only do one DieType?
      facet_wrap(~DieType, scales = "free") +
      labs(title = paste("Distribution of", input$selected_set, "Die Set"),
           x = "Roll Value",
           y = "Number of Time Rolled")
    
    # renders plots based on user-specified campaign
  } else if (input$variable == "campaign"){
    req(input$selected_campaign)
    
    # adding this variable so the facet_wrap is in my specified order
    neworder <- c("d4", "d6", "d8", "d10", "d%%", "d12", "d20")
    
    filtered_data <- dice_data() %>%
      filter(Campaign == input$selected_campaign) %>%
      # mutating so facet_wrap is in my specified order
      mutate(across(DieType, ~factor(., levels = neworder)))
    
    # plotting DieRoll by Campaign, facet by DieType
    ggplot(filtered_data, aes(x = DieRoll)) +
      geom_bar() +
      facet_wrap(~DieType, scales = "free") +
      labs(title = paste("Distribution of Die Rolls in the", input$selected_campaign, "Campaign"))
    
    # renders plots based on user-specified campaign & session
  } else if (input$variable == "sess"){
    req(input$selected_campaign, input$selected_session)
    
    # adding this variable so the facet_wrap is in my specified order
    neworder <- c("d4", "d6", "d8", "d10", "d%%", "d12", "d20")
    
    filtered_data <- dice_data() %>%
      filter(Campaign == input$session_campaign,
             Session == input$selected_session) %>%
      # mutating so facet_wrap is in my specified order
      mutate(across(DieType, ~factor(., levels = neworder)))
    
    # plotting DieRoll by a specific Campaign-Session combo, facet by DieType
    ggplot(filtered_data, aes(x = DieRoll)) +
      geom_bar() +
      facet_wrap(~DieType, scales = "free") +
      labs(title = paste("Distribution of Die Rolls for Campaign", input$session_campaign, 
                         "Session", input$selected_session),
           x = "Roll Value",
           y = "Number of Times Rolled")
    
    # renders plots based on user-specified date
  } else if (input$variable == "date"){
    req(input$date_range)
    
    # adding this variable so the facet_wrap is in my specified order
    neworder <- c("d4", "d6", "d8", "d10", "d%%", "d12", "d20")
    
    filtered_data <- dice_data() %>%
      filter(Date >= input$date_range[1],
             Date <= input$date_range[2]) %>%
      # mutating so facet_wrap is in my specified order
      mutate(across(DieType, ~factor(., levels = neworder)))
    
    # plotting DieRoll by Date, faceted by DieType
    ggplot(filtered_data, aes(x = DieRoll)) +
      geom_bar() +
      facet_wrap(~DieType, scales = "free") +
      labs(title = paste("Distribution of Die Rolls From", input$date_range[1], "to", input$date_range[2]),
           x = "Roll Value",
           y = "Number of Times Rolled")
  }
      
  })
}

shinyApp(ui, server)

# do I add another dropdown to give option for color 
# or just give the user every combo?

# add captions to say "generated by app made by MYNAME"

# make persons able to download plot?

# make plots interactive

# be able to share link

# tabsets on the right (tabs: plots, tables, summary)

# conditional formatting for DieType, DieSet, Campaign, Session, Date