library(shiny)
library(tidyverse)
library(lubridate)
library(bslib)
library(janitor)

# --- 1. DEFINE THEME LOCALLY ---
theme_potter <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(
      strip.background = element_rect(fill = "#2c3e50"),
      strip.text = element_text(color = "white", face = "bold"),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}

# --- 2. DATA LOADING FUNCTIONS ---
get_default_growth <- function() {
  if (file.exists("pott_growth_data.csv")) {
    raw <- read.csv("pott_growth_data.csv", stringsAsFactors = FALSE) %>%
      janitor::clean_names()

    # Standardize column names
    if("length" %in% names(raw)) raw <- rename(raw, mean_length_mm = length)
    if("days" %in% names(raw)) raw <- rename(raw, dph = days)

    # Cleaning: Remove empty rows causing issues
    raw <- raw %>%
      filter(!is.na(dph), !is.na(mean_length_mm))

    if("protocol" %in% names(raw)) raw <- raw %>% filter(protocol != "")

    return(raw)
  }
  return(NULL)
}

get_default_spawn <- function() {
  if (file.exists("pott_spawn_data.csv")) {
    raw <- read.csv("pott_spawn_data.csv", stringsAsFactors = FALSE) %>%
      mutate(
        # Handle Date formats robustly
        Date = tryCatch(lubridate::mdy(Date), error = function(e) lubridate::ymd(Date)),
        Viable = as.numeric(gsub(",", "", Viable)),
        Unviable = as.numeric(gsub(",", "", Unviable))
      ) %>%
      filter(!is.na(Date))
    return(raw)
  }
  return(NULL)
}

# --- 3. UI DEFINITION ---
ui <- navbarPage(
  title = "Potterful Analytics",
  theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),

  tabPanel("Larval Growth",
           sidebarLayout(
             sidebarPanel(
               h4("Data Source"),
               fileInput("growth_file", "Upload Growth CSV", accept = ".csv"),
               # Dynamic status message
               uiOutput("growth_file_status"),
               hr(),
               uiOutput("protocol_selector"),
               selectInput("y_metric", "Y-Axis Metric:",
                           choices = c("Length (mm)" = "mean_length_mm", "Body Depth" = "depth"),
                           selected = "mean_length_mm")
             ),
             mainPanel(
               plotOutput("growthPlot"),
               br(),
               tableOutput("growthStats")
             )
           )
  ),

  tabPanel("Spawning Trends",
           sidebarLayout(
             sidebarPanel(
               h4("Data Source"),
               fileInput("spawn_file", "Upload Spawning CSV", accept = ".csv"),
               uiOutput("spawn_file_status"),
               hr(),
               uiOutput("date_range_ui")
             ),
             mainPanel(
               plotOutput("spawnPlot"),
               hr(),
               tableOutput("spawnTable")
             )
           )
  )
)

# --- 4. SERVER LOGIC ---
server <- function(input, output, session) {

  # --- Growth Data ---
  growth_dataset <- reactive({
    if (is.null(input$growth_file)) {
      return(get_default_growth())
    } else {
      req(input$growth_file)
      raw <- read.csv(input$growth_file$datapath) %>% janitor::clean_names()
      if("length" %in% names(raw)) raw <- rename(raw, mean_length_mm = length)
      if("days" %in% names(raw)) raw <- rename(raw, dph = days)

      raw <- raw %>% filter(!is.na(dph), !is.na(mean_length_mm))
      if("protocol" %in% names(raw)) raw <- raw %>% filter(protocol != "")
      return(raw)
    }
  })

  output$growth_file_status <- renderUI({
    if (is.null(growth_dataset())) {
      helpText("⚠️ Data file not found. Please upload a CSV.", style = "color: red;")
    } else if (is.null(input$growth_file)) {
      helpText("Using: Default Project Data", style = "color: green;")
    } else {
      helpText(paste("Using:", input$growth_file$name))
    }
  })

  # --- Spawn Data ---
  spawn_dataset <- reactive({
    if (is.null(input$spawn_file)) {
      return(get_default_spawn())
    } else {
      req(input$spawn_file)
      raw <- read.csv(input$spawn_file$datapath)
      raw %>%
        mutate(
          Date = tryCatch(lubridate::mdy(Date), error = function(e) lubridate::ymd(Date)),
          Viable = as.numeric(gsub(",", "", Viable)),
          Unviable = as.numeric(gsub(",", "", Unviable))
        ) %>%
        filter(!is.na(Date))
    }
  })

  output$spawn_file_status <- renderUI({
    if (is.null(spawn_dataset())) {
      helpText("⚠️ Data file not found. Please upload a CSV.", style = "color: red;")
    } else if (is.null(input$spawn_file)) {
      helpText("Using: Default Project Data", style = "color: green;")
    } else {
      helpText(paste("Using:", input$spawn_file$name))
    }
  })

  # --- UI Inputs ---
  output$protocol_selector <- renderUI({
    req(growth_dataset())
    df <- growth_dataset()
    if("protocol" %in% names(df)) {
      selectInput("protocol", "Select Protocol:", choices = c("All", unique(as.character(df$protocol))))
    }
  })

  output$date_range_ui <- renderUI({
    req(spawn_dataset())
    df <- spawn_dataset()
    if("Date" %in% names(df) && nrow(df) > 0) {
      dateRangeInput("dates", "Select Date Range:", start = min(df$Date, na.rm=TRUE), end = max(df$Date, na.rm=TRUE))
    }
  })

  # --- Plots (FIXED: No more LOESS warnings) ---
  output$growthPlot <- renderPlot({
    req(growth_dataset())
    df <- growth_dataset()

    validate(
      need("dph" %in% names(df), "Missing 'dph' column."),
      need(input$y_metric %in% names(df), paste("Missing", input$y_metric, "column."))
    )

    if (!is.null(input$protocol) && input$protocol != "All" && "protocol" %in% names(df)) {
      df <- df %>% filter(protocol == input$protocol)
    }

    p <- ggplot(df, aes(x = dph, y = .data[[input$y_metric]]))
    if("protocol" %in% names(df)) p <- p + aes(color = protocol)

    p <- p + geom_point(alpha = 0.6, size = 3)

    # FIX: Use 'lm' (Linear Model) instead of 'loess' to stop the warnings
    # This draws a straight trend line, which is robust and won't crash
    if (nrow(df) >= 3) {
      p <- p + geom_smooth(se = FALSE, method = "lm", formula = y ~ x)
    }

    p + theme_potter() + labs(title = paste("Larval", input$y_metric), x = "Days Post Hatch (dph)")
  })

  output$growthStats <- renderTable({
    req(growth_dataset())
    df <- growth_dataset()

    # Check if protocol exists to decide how to group
    if("protocol" %in% names(df)) {
      df %>% group_by(protocol) %>%
        summarise(Mean = mean(.data[[input$y_metric]], na.rm=TRUE),
                  Max = max(.data[[input$y_metric]], na.rm=TRUE),
                  N = n())
    } else {
      df %>% summarise(Mean = mean(.data[[input$y_metric]], na.rm=TRUE),
                       Max = max(.data[[input$y_metric]], na.rm=TRUE),
                       N = n())
    }
  })

  output$spawnPlot <- renderPlot({
    req(spawn_dataset())
    df <- spawn_dataset()

    # Suppress warnings about removed rows (common in bar charts with NAs)
    suppressWarnings({
      if (!is.null(input$dates)) df <- df %>% filter(Date >= input$dates[1] & Date <= input$dates[2])

      plot_data <- df %>% pivot_longer(cols = c(Viable, Unviable), names_to = "Egg_Type", values_to = "Count")

      ggplot(plot_data, aes(x = Date, y = Count, fill = Egg_Type)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = c("Viable" = "#4e79a7", "Unviable" = "#f28e2b")) +
        scale_y_continuous(labels = scales::comma) +
        theme_potter() +
        labs(title = "Daily Egg Production", y = "Egg Count")
    })
  })

  output$spawnTable <- renderTable({
    req(spawn_dataset())
    df <- spawn_dataset()
    if (!is.null(input$dates)) df <- df %>% filter(Date >= input$dates[1] & Date <= input$dates[2])

    if(nrow(df) == 0) return(NULL)

    df %>% summarise(
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
