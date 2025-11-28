library(shiny)
library(Potterful)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)

# UI Definition (Stays the same)
ui <- navbarPage(
  title = "Potterful Analytics",
  theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),

  # Tab 1: Larval Growth
  tabPanel("Larval Growth",
           sidebarLayout(
             sidebarPanel(
               h4("Data Source"),
               fileInput("growth_file", "Upload Growth CSV (Optional)",
                         accept = c(".csv"),
                         placeholder = "Using Historical Project Data"),
               hr(),
               h4("Controls"),
               uiOutput("protocol_selector"),
               selectInput("y_metric", "Y-Axis Metric:",
                           choices = c("Length (mm)" = "mean_length_mm",
                                       "Body Depth" = "depth")),
               hr(),
               helpText("Default displays historical Potter's Angelfish data.")
             ),
             mainPanel(
               plotOutput("growthPlot"),
               br(),
               h4("Growth Statistics"),
               tableOutput("growthStats")
             )
           )
  ),

  # Tab 2: Spawning Trends
  tabPanel("Spawning Trends",
           sidebarLayout(
             sidebarPanel(
               h4("Data Source"),
               fileInput("spawn_file", "Upload Spawning CSV (Optional)",
                         accept = c(".csv"),
                         placeholder = "Using Historical Project Data"),
               hr(),
               h4("Filters"),
               uiOutput("date_range_ui"),
               p("Analyze daily egg production and viability.")
             ),
             mainPanel(
               plotOutput("spawnPlot"),
               hr(),
               h4("Viability Summary"),
               tableOutput("spawnTable")
             )
           )
  )
)

# Server Logic
server <- function(input, output, session) {

  # --- REACTIVE: Growth Data ---
  growth_dataset <- reactive({
    if (is.null(input$growth_file)) {
      # FIXED: Removed 'Potterful::' prefix
      # Since we loaded library(Potterful), 'growth_data' is already available
      return(growth_data)
    } else {
      # Process uploaded file
      raw <- read.csv(input$growth_file$datapath)

      # Quick normalization
      if("Protocol" %in% names(raw)) raw <- rename(raw, protocol = Protocol)
      if("length" %in% names(raw)) raw <- rename(raw, mean_length_mm = length)
      if("Length" %in% names(raw)) raw <- rename(raw, mean_length_mm = Length)

      return(raw)
    }
  })

  # --- REACTIVE: Spawning Data ---
  spawn_dataset <- reactive({
    if (is.null(input$spawn_file)) {
      # FIXED: Removed 'Potterful::' prefix
      return(spawn_data)
    } else {
      # Process uploaded file
      raw <- read.csv(input$spawn_file$datapath)

      # Cleaning logic
      raw %>%
        mutate(
          Date = tryCatch(lubridate::mdy(Date), error = function(e) as.Date(Date)),
          Viable = as.numeric(gsub(",", "", Viable)),
          Unviable = as.numeric(gsub(",", "", Unviable))
        )
    }
  })

  # --- DYNAMIC UI INPUTS ---

  output$protocol_selector <- renderUI({
    req(growth_dataset())
    df <- growth_dataset()
    if("protocol" %in% names(df)) {
      selectInput("protocol", "Select Protocol:",
                  choices = c("All", unique(as.character(df$protocol))))
    } else {
      NULL
    }
  })

  output$date_range_ui <- renderUI({
    req(spawn_dataset())
    df <- spawn_dataset()
    # Check if 'Date' exists to prevent crash on bad upload
    if("Date" %in% names(df)) {
      dateRangeInput("dates", "Select Date Range:",
                     start = min(df$Date, na.rm=TRUE),
                     end = max(df$Date, na.rm=TRUE))
    } else {
      NULL
    }
  })

  # --- OUTPUTS: Tab 1 (Growth) ---

  output$growthPlot <- renderPlot({
    req(growth_dataset())
    df <- growth_dataset()

    # Filter if protocol is selected and exists
    if (!is.null(input$protocol) && input$protocol != "All" && "protocol" %in% names(df)) {
      df <- df %>% filter(protocol == input$protocol)
    }

    p <- ggplot(df, aes(x = dph, y = .data[[input$y_metric]]))

    if("protocol" %in% names(df)) {
      p <- p + aes(color = protocol)
    }

    p + geom_point(alpha = 0.6, size = 2) +
      geom_smooth(se = FALSE, method = "loess") +
      Potterful::theme_potter() +
      labs(title = paste("Larval", input$y_metric, "over Time"),
           x = "Days Post Hatch (dph)")
  })

  output$growthStats <- renderTable({
    req(growth_dataset())
    df <- growth_dataset()

    if("protocol" %in% names(df)) {
      df %>%
        group_by(protocol) %>%
        summarise(
          Mean_Size = mean(mean_length_mm, na.rm=TRUE),
          Max_Size = max(mean_length_mm, na.rm=TRUE),
          N_Obs = n()
        )
    } else {
      df %>%
        summarise(
          Mean_Size = mean(mean_length_mm, na.rm=TRUE),
          Max_Size = max(mean_length_mm, na.rm=TRUE),
          N_Obs = n()
        )
    }
  })

  # --- OUTPUTS: Tab 2 (Spawning) ---

  output$spawnPlot <- renderPlot({
    req(spawn_dataset(), input$dates)

    # Filter and Pivot Long for Plotting
    plot_data <- spawn_dataset() %>%
      filter(Date >= input$dates[1] & Date <= input$dates[2]) %>%
      pivot_longer(cols = c(Viable, Unviable),
                   names_to = "Egg_Type",
                   values_to = "Count")

    ggplot(plot_data, aes(x = Date, y = Count, fill = Egg_Type)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Viable" = "#4e79a7", "Unviable" = "#f28e2b")) +
      scale_y_continuous(labels = comma) +
      Potterful::theme_potter() +
      labs(title = "Daily Egg Production & Viability", y = "Egg Count")
  })

  output$spawnTable <- renderTable({
    req(spawn_dataset(), input$dates)

    spawn_dataset() %>%
      filter(Date >= input$dates[1] & Date <= input$dates[2]) %>%
      summarise(
        `Total Eggs` = sum(Viable + Unviable, na.rm=TRUE),
        `Total Viable` = sum(Viable, na.rm=TRUE),
        `Avg Viability %` = mean(Viable/(Viable+Unviable)*100, na.rm=TRUE)
      ) %>%
      mutate(
        `Total Eggs` = format(`Total Eggs`, big.mark=","),
        `Total Viable` = format(`Total Viable`, big.mark=",")
      )
  })
}

shinyApp(ui, server)
