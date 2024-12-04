library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(bslib)
library(viridis)
library(ggdark)
library(rsconnect)

# UI - user interface
ui <- page_navbar(
  
  title = "DND Dice Roll Analysis",
  nav_spacer(), # moves navigation bar to the right
  
  # Overview Tab
  nav_panel(
    title = "Overview",
    
    # sidebar where users can select info
    page_sidebar(
      
      sidebar = sidebar(
        
        # accept excel sheet inputs
        fileInput("upload", "Upload Excel Sheet", accept = c(".xlsx", ".xls")),
        
        # initial dropdown
        selectInput("variable", "Select Variable to Analyze:",
                    choices = c("Rolls by Die Type" = "die_type",
                                "Rolls by Die Set" = "die_set",
                                "Rolls by Campaign" = "campaign",
                                "Rolls by Session" = "sess",
                                "Rolls Over Time" = "date")),
        
        # second drop down - dietype
        conditionalPanel(
          condition = "input.variable == 'die_type'",
          selectInput("selected_type", "Select Die Type:", 
                      choices = c("d4", "d6", "d8", "d10", "d%%", "d12", "d20"))
        ),
        
        # second drop down - dieset
        conditionalPanel(
          condition = "input.variable == 'die_set'",
          selectInput("selected_set", "Select Die Set:",
                      choices = NULL)
        ),
        
        # second drop down - campaign
        conditionalPanel(
          condition = "input.variable == 'campaign'",
          selectInput("selected_campaign", "Select Campaign:",
                      choices = NULL)
        ),
        
        # second drop down - session
        conditionalPanel(
          condition = "input.variable == 'sess'",
          selectInput("session_campaign", "Select Campaign:",
                      choices = NULL),
          selectInput("selected_session", "Select Session",
                      choices = NULL)
        ),
        
        # second drop down - date range
        conditionalPanel(
          condition = "input.variable == 'date'",
          dateRangeInput("date_range", "Select Date Range:",
                         start = NULL, end = NULL)
        )
      ),
      
      class = "bslib-page-dashboard", # gray background behind cards
      
      layout_column_wrap(
        width = 1,
        style = css(grid_template_rows = "0.2fr 1fr"), # first row 1/5 that of second
        
        # Top row with metrics plots
        layout_column_wrap(
          width = 1/3,
          
          # Total Rolls
          value_box(
            title = "Total Rolls",
            value = textOutput("total_rolls")
          ),
          
          # Unique Die Sets
          value_box(
            title = "Unique Die Sets",
            value = textOutput("unique_sets")
          ),
          
          # Unique Campaigns
          value_box(
            title = "Unique Campaigns",
            value = textOutput("unique_campaigns")
          ),
        ),
        
        # Main distribution plot
        layout_column_wrap(
          card(
            full_screen = TRUE,
            plotOutput("dice_plot")
          )
        )
      )
    )
  ),
    
  # Data Explorer Tab
  nav_panel(
    title = "Data Explorer",
    class = "bslib-page-dashboard",
      
    layout_column_wrap(
      width = 1,
        
      # summary statistics
      card(
        full_screen = TRUE,
        card_header("Summary Statistics"),
        tableOutput("summary_stats")
      ),
        
      # raw data
      card(
        full_screen = TRUE,
        card_header("Raw Data"),
        tableOutput("raw_data")
      )
    )
  ),
    
  # How To Tab
  nav_panel(
    title = "How to Use",
    class = "bslib-page-dashboard",
      
      card(
        card_header("Instructions for Using this Website"),
        # using html-shiny convertability 
        card_body(
          tags$ol(
            tags$li("Download the Excel file template and catalogue your die rolls!",
                    br(),
                    downloadButton("download_template", "Download Template"),
                    br()),
            tags$li("Make sure to be consistent with your naming scheme, and the preferred format:",
                    tags$ul(
                      tags$li("DieRoll: The die value rolled"),
                      tags$li("DieType: The type of die (d4, d6, d8, d10, d%%, d12, d20)"),
                      tags$li("DieSet: The dice set with which you rolled (Hi, fellow dice goblins!)"),
                      tags$li("Campaign: The name of the campaign"),
                      tags$li("Session: The session number or name"),
                      tags$li("Date: The date of the roll (YYYY-MM-DD format)")
                      )
                    ),
            tags$li("Navigate to the Overview tab, and upload your Excel file."),
            tags$li("Use the dropdown menus to select what aspect of your dice rolls you want to analyze."),
            tags$li("Explore the visualization and statistics in the Overview and Data Explorer tabs."),
            tags$li("Have fun, nerds!")
          )
        )
      ),
   ),
  
  # About Tab 
  nav_panel(
    title = "About",
    class = "bslib-page-dashboard",
      
    card(
      card_header("Purpose"),
      card_body(
        p("This Dungeons & Dragons Dice Roll Analysis App helps any player or Dungeon Master analyze their dice rolling patterns and statistics. Whether you're curious if you really did roll poorly in your last session or if one of your dice sets is luckier than the others, this tool can help you visualize and understand your dice rolling data.")
        )
      ),
      
    card(
      card_header("About Me"),
      card_body(          
        p("My name is Katie Frazer, and I made this app! This app actually started as a passion project, originally intended to be for personal use. One session, shortly after I started playing Dungeons and Dragons, I was convinced that one particular die set had screwed me over big time. Out of frustration (and lots of curiosity), I started tracking my die rolls and developed code to analyze my results. It was working great, but I had encountered a small issue of some kind, and brought it up to one of my data science professors (Katie Willi). She suggested I create an R Shiny app, and here I am! I am incredibly proud of the time and effort I put into this, and I am loving how it turned out. I hope you all like it too!"),
        tags$span(
          "Here's my ",
          tags$a(
            href = "https://www.linkedin.com/in/katie-m-frazer",
            "LinkedIn",
            target = "_blank", # open link in new tab
            style = "color = #0077B5"
          ),
          "and ",
          tags$a(
            href = "https://github.com/katiefrazer",
            "GitHub",
            target = "_blank",
            style = "color = #0077B5"
          ),
          "!"
        )
      )
    )
  ),
    
  # add option to switch between dark and light mode
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  )
)

# server - backend
server <- function(input, output, session){
  
  # read and process uploaded data
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
  
  # update choices based on uploaded data for DieType, DieSet, Campaign, Session, Date
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
  
  # pull unique session numbers and update selection
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
  
  # card - total rolls
  output$total_rolls <- renderText({
    req(filtered_data())
    nrow(filtered_data())
  })
  
  # card - total unique sets
  output$unique_sets <- renderText({
    req(filtered_data())
    length(unique(filtered_data()$DieSet))
  })
  
  # card - total unique campaigns
  output$unique_campaigns <- renderText({
    req(filtered_data())
    length(unique(filtered_data()$Campaign))
  })
  
  # plot - data visualization 
  output$dice_plot <- renderPlot({
    req(filtered_data())
    
  p <- if (input$variable == "die_type"){
    ggplot(filtered_data(), aes(x = DieRoll)) +
      geom_bar(aes(fill = DieSet)) + 
      scale_fill_viridis(discrete = TRUE) +
      labs(title = paste("Distribution of", input$selected_type, "Rolls"),
           x = "Roll Value",
           y = "Frequency Rolled",
           caption = "Generated by Katie Frazer's DND Dice Roll Analysis Website") 
  } else {
    neworder <- c("d4", "d6", "d8", "d10", "d%%", "d12", "d20")
    
    filtered_data() %>%
      mutate(across(DieType, ~factor(., levels = neworder))) %>%
      ggplot(aes(x = DieRoll)) +
      geom_bar(aes(fill = DieSet)) +
      scale_fill_viridis(discrete = TRUE) +
      facet_wrap(~DieType, scales = "free") +
      labs(title = case_when(
        input$variable == "die_set" ~ paste("Distribution of the", input$selected_set, "Die Set"),
        input$variable == "campaign" ~ paste("Distribution of the", input$selected_campaign, "Campaign"),
        input$variable == "sess" ~ paste("Distribution of the", input$session_campaign, "Session", input$selected_session),
        input$variable == "date" ~ paste("Distribution from", input$date_range[1], "to", input$date_range[2])
        ),
        x = "Roll Value",
        y = "Frequency Rolled",
        caption = "Generated by Katie Frazer's DND Dice Roll Analysis Website") 
  }
  
  if (input$dark_mode == "dark") {
    p + dark_theme_bw() +
      theme(plot.background = element_rect(fill = "#1d1f21"),
            legend.background = element_rect(fill = "#1d1f21"),
            panel.background = element_rect(fill = "#1d1f21"),
            plot.title = element_text(size = "20"),
            axis.title = element_text(size = "12"))
  } else {
    p + theme_bw() +
      theme(plot.title = element_text(size = "20"),
            axis.title = element_text(size = "12"))
  }
  })

  # table - summary statistics
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
  
  # table - raw data
  output$raw_data <- renderTable({
    req(filtered_data())
    
    filtered_data() %>%
      mutate(Date = format(Date, "%Y-%m-%d"))
  })
  
  #template download button
  output$download_template <- downloadHandler(
    filename = function() {
      "DND_Roll_Tracker_Template.xlsx"
    },
    content = function(file) {
      file.copy("DND_Roll_Tracker_Template.xlsx", file)
    }
    
  )
}

shinyApp(ui, server)

# GENERAL
# host the link on POSIT
# make the website bookmarkable
# add comments for understanding and explanation
# recruit Luke and Zach to actually make it an app?
# make the GitHub repository private if end up charging like friends suggest

# UI
# make the website prettier
# recruit Valen for ui stuff

# SIDEBAR
# sort options after uploading excel sheet (ex. session)
# give notification if they didn't upload an excel sheet

# PLOTS
# make people able to download plots
# make plots interactable (plotly??)
# hardset each scale for each die type (supposedly difficult)
# get rid of the line around the plots when switch to dark mode
# fix legend.position = "none" for dieset

# TABLES
# make tables interactable (package: DT)
# make people able to download tables

# HOW TO USE
# make template button in less weird location

# ABOUT
# add acknowledgements