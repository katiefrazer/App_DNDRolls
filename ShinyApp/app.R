library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(bslib)
library(viridis)
library(DT)

# UI - user interface
ui <- page_navbar(
  
  title = "DND Dice Roll Analysis",
  nav_spacer(),
  
  # main dashboard
  nav_panel(
    title = "Overview",
    
    # sidebar on the left where users can select info
    page_sidebar(
      
      #class = "bslib-page-dashboard",
      
      sidebar = sidebar(
        
        # accept excel sheet inputs
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
        
      ),
      
      layout_column_wrap(
        width = 1,
        fill = FALSE,
        style = css(grid_template_rows = "0.18fr 0.67fr 0.27fr"),
        
        # Top row with metrics plots
        layout_column_wrap(
          width = 1/3,
          
          value_box(
            title = "Total Rolls",
            value = textOutput("total_rolls"),
            #showcase = bsicons::bs_icon("dice-6")
            #theme = "bg-gradient-purple-indigo"
            #theme = value_box_theme(bg = "#453781FF", fg = "#000000")
          ),
          
          value_box(
            title = "Unique Die Sets",
            value = textOutput("unique_sets"),
            #theme = "bg-gradient-blue-teal"
            #theme = value_box_theme(bg = "#287D8EFF", fg = "#000000")
          ),
          value_box(
            title = "Unique Campaigns",
            value = textOutput("unique_campaigns"),
            #theme = "bg-gradient-teal-green"
            # theme = value_box_theme(bg = "#3CBB75FF", fg = "#000000")
          ),
          
        ),
        
        # Main dice distribution plot
        layout_column_wrap(
          card(
            full_screen = TRUE,
            card_header("Visual Representation"),
            plotOutput("dice_plot")
          )
        ),
        
        # Summary Statistics Table
        layout_column_wrap(
          card(
            full_screen = TRUE,
            card_header("Summary Statistics"),
            tableOutput("summary_stats")
          )
        )
      )
    )
  ),
    
    # data dashboard
    nav_panel(
      title = "Data Explorer",
      layout_column_wrap(
        width = 1,
        card(
          card_header("Raw Data"),
          DT::dataTableOutput("raw_data")
        )
      )
    ),
    
    # about section
    nav_panel(
      title = "About",
      card(
        card_header("About this App")
      )
    ),
  
  # add option to switch between dark and light mode
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
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
    theme <- current_theme()
    
  if (input$variable == "die_type"){
    ggplot(filtered_data(), aes(x = DieRoll)) +
      geom_bar(aes(fill = DieSet)) + 
      scale_fill_viridis(discrete = TRUE) +
      labs(title = paste("Distribution of", input$selected_type, "Rolls"),
           x = "Roll Value",
           y = "Frequency Rolled") 
    
  } else if (input$variable == "die_set"){
    neworder <- c("d4", "d6", "d8", "d10", "d%%", "d12", "d20")
    
    filtered_data() %>%
      mutate(across(DieType, ~factor(., levels = neworder))) %>%
      ggplot(aes(x = DieRoll)) +
      geom_bar(aes(fill = DieSet)) +
      scale_fill_viridis(discrete = TRUE) +
      facet_wrap(~DieType, scales = "free") +
      theme(legend.position = "none") +
      labs(title = paste("Distribution of the", input$selected_set, "Die Set"),
           x = "Roll Value",
           y = "Frequency Rolled") 
    
  } else {
    neworder <- c("d4", "d6", "d8", "d10", "d%%", "d12", "d20")
    
    filtered_data() %>%
      mutate(across(DieType, ~factor(., levels = neworder))) %>%
      ggplot(aes(x = DieRoll)) +
      geom_bar(aes(fill = DieSet)) +
      scale_fill_viridis(discrete = TRUE) +
      facet_wrap(~DieType, scales = "free") +
      labs(title = case_when(
        #input$variable == "die_set" ~ paste("Distribution of the", input$selected_set, "Die Set"),
        input$variable == "campaign" ~ paste("Distribution of the", input$selected_campaign, "Campaign"),
        input$variable == "sess" ~ paste("Distribution of the", input$session_campaign, "Session", input$selected_session),
        input$variable == "date" ~ paste("Distribution from", input$date_range[1], "to", input$date_range[2])
        ),
        x = "Roll Value",
        y = "Frequency Rolled") 
  }
    
  })

  # summary statistics output
  output$summary_stats <- renderTable({
    req(filtered_data())
    theme <- current_theme()
    
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

  
  # total rolls output
  output$total_rolls <- renderText({
    req(filtered_data())
    nrow(filtered_data())
  })
  
  # total unique sets output
  output$unique_sets <- renderText({
    req(filtered_data())
    length(unique(filtered_data()$DieSet))
  })
  
  # total unique campaigns
  output$unique_campaigns <- renderText({
    req(filtered_data())
    length(unique(filtered_data()$Campaign))
  })
  
  
  # dates played output
  
  # raw data navset option
  output$raw_data <- DT::renderDataTable({
    req(filtered_data())
    theme <- current_theme()
    
    DT::datatable(
      filtered_data(),
      options = list(
        pageLength = 5,
        lengthMenu = c(5, 10, 15, 20, 25, 30),
        dom = "Brtipsl",
        buttons = c("copy", "csv", "excel")
      ),
      extensions = "Buttons"
    ) 
  })
}

shinyApp(ui, server)

# add captions to say "generated by app made by MYNAME"

# make persons able to download plot?

# make plots interactive

# be able to share link

# do not have conditonal panels set already

# give an error if they select drop downs but no excel sheet

# black with viridis color palette

# add template and directions for excel sheet

# add custom theme

# check to see what happens if sort other options than just session

# add scale somehow for each graph, hardset scale for each die type

# dropdown dependent on order produced by excel sheet, order it by number

# include description of purpose somewhere
# include template Excel sheet for people to download from

# make margins between cards smaller

# add light mode and dark mode changes to plots