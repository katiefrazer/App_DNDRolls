library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(bslib)
library(viridis)

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
  
  
  # creating tabs for plot, summary statistics, and analysis
  navset_card_tab(
    # plot output, visual representation, where the graphs will be
    nav_panel(
      title = "Plot",
      #card_header("Visual Representation"),
      plotOutput("dice_plot")
    ),
    
    # summary statistics, numerical representation, summary of input data relevance
    nav_panel(
      title = "Summary",
      #card_header("Summary Statistics"),
      verbatimTextOutput("summary_text"),
      tableOutput("summary_stats")
    ),
    
    # data analysis, some analysis of the data to present useful conclusions to user
    nav_panel(
      title = "Analysis",
      #card_header("Statistical Analysis"),
      tableOutput("analysis_table")
    )
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
  
  # create filtered_data reactive to use across all outputs
  # filtered_data is to make all the facet_wrap graphs appear in specified order
  filtered_data <- reactive({
    req(dice_data())
    
    if (input$variable == "die_type"){
      dice_data() %>%
        filter(DieType == input$selected_type)
    } else if (input$variable == "die_set"){
      dice_data() %>%
        filter(DieSet == input$selected_set)
    } else if (input$variable == "campaign"){
      dice_data() %>%
        filter(Campaign == input$selected_campaign)
    } else if (input$variable == "sess"){
      dice_data() %>%
        filter(Campaign == input$session_campaign,
               Session == input$selected_session)
    } else if (input$variable == "date"){
      dice_data() %>%
        filter(Date >= input$date_range[1],
               Date <= input$date_range[2])
    }
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
  
  # Visual Representation
  # Generate the user-specified plot 
  output$dice_plot <- renderPlot({
    req(filtered_data())
    
  if (input$variable == "die_type"){
    ggplot(filtered_data(), aes(x = DieRoll)) +
      geom_bar(aes(fill = DieSet)) + 
      labs(title = paste("Distribution of", input$selected_type, "Rolls"),
           x = "Roll Value",
           y = "Number of Times Rolled")
    
  } else {
    neworder <- c("d4", "d6", "d8", "d10", "d%%", "d12", "d20")
    
    filtered_data() %>%
      mutate(across(DieType, ~factor(., levels = neworder))) %>%
      ggplot(aes(x = DieRoll)) +
      geom_bar(aes(fill = DieSet)) +
      facet_wrap(~DieType, scales = "free") +
      labs(title = case_when(
        input$variable == "die_set" ~ paste("Distribution of the", input$selected_set, "Die Set"),
        input$variable == "campaign" ~ paste("Distribution of the", input$selected_campaign, "Campaign"),
        input$variable == "sess" ~ paste("Distribution of the", input$session_campaign, "Session", input$selected_session),
        input$variable == "date" ~ paste("Distribution from", input$date_range[1], "to", input$date_range[2])
        ),
        x = "Roll Value",
        y = "Number of Times Rolled"
      )
  }
    
  })
  
  # Summary Statistics
  # Generate summary statistics about user-selected input
  output$summary_text <- renderText({
    req(filtered_data())
    
    total_rolls <- nrow(filtered_data())
    unique_die_sets <- length(unique(filtered_data()$DieSet))
    unique_campaigns <- length(unique(filtered_data()$Campaign))
    date_range <- if("Date" %in% names(filtered_data())){
      paste(min(filtered_data()$Date), "to", max(filtered_data()$Date))
    } else {
      "N/A"
    }
    
    paste0(
      "Overall Summary:\n\n",
      "Total number of rolls: ", total_rolls, "\n",
      "Number of unique die sets: ", unique_die_sets, "\n",
      "Number of unique campaigns: ", unique_campaigns, "\n",
      "Date Range: ", date_range, "\n"
    )
  
  })
  
  output$summary_stats <- renderTable({
    req(filtered_data())
    
    neworder <- c("d4", "d6", "d8", "d10", "d%%", "d12", "d20")
    
    filtered_data() %>%
      group_by(DieType) %>%
      summarise(
        Count = n(),
        Mean = round(mean(DieRoll, na.rm = TRUE), 2),
        Median = median(DieRoll, na.rm = TRUE),
        SD = round(sd(DieRoll, na.rm = TRUE), 2),
        Min = min(DieRoll, na.rm = TRUE),
        Max = max(DieRoll, na.rm = TRUE)
      ) %>%
      arrange(factor(DieType, levels = neworder))
  })
  
  # Statistical Analysis
  # Generate some conclusions about the data
  

}

shinyApp(ui, server)

# add captions to say "generated by app made by MYNAME"

# make persons able to download plot?

# make plots interactive

# be able to share link

# do not have conditonal panels set already

# give an error if they select drop downs but no excel sheet

# color code session and campaign

# black with viridis color palette

# do i need to add another card to the main info?

# add template and directions for excel sheet

# add custom theme

# check to see what happens if sort other options than just session

# add scale somehow for each graph, hardset scale for each die type

# dropdown dependent on order produced by excel sheet, order it by number

# die_set does not need to be in the else portion, would prefer it as it's own if