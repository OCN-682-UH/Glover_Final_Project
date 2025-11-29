library(shiny)
library(tidyverse)
library(lubridate)
library(bslib)
library(janitor) # Explicitly load janitor

# --- 1. DEFINE THEME LOCALLY (Bypasses Package Issue) ---
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
# Reads files from the current deployment directory
get_default_growth <- function() {
  # Look for file in the same folder as app.R
  if (file.exists("pott_growth_data.csv")) {
    raw <- read.csv("pott_growth_data.csv", stringsAsFactors = FALSE) %>%
      janitor::clean_names()

    # Rename columns if needed to match what the app expects
    if("length" %in% names(raw)) raw <- rename(raw, mean_length_mm = length)
    if("days" %in% names(raw)) raw <- rename(raw, dph = days)

    # Cleaning: Filter out empty rows causing plot errors
    raw <- raw %>%
      filter(!is.na(dph), !is.na(mean_length_mm))

    if("protocol" %in% names(raw)) raw <- raw %>% filter(protocol != "")

    return(raw)
  }
  return(NULL) # Return NULL if file missing
}

get_default_spawn <- function() {
  if (file.exists("pott_spawn_data.csv")) {
    raw <- read.csv("pott_spawn_data.csv", stringsAsFactors = FALSE) %>%
      mutate(
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

  # Tab 1: Larval Growth
  tabPanel("Larval Growth",
           sidebarLayout(
             sidebarPanel(
               h4("Data Source"),
               fileInput("growth_file", "Upload Growth CSV",
                         accept = c(".csv"),
                         placeholder = "Using pott_growth_data.csv"),
               helpText("Required columns: 'dph', 'protocol', 'length'"),
               hr(),
               h4("Controls"),
               uiOutput("protocol_selector"),
               selectInput("y_metric", "Y-Axis Metric:",
                           choices = c("Length (mm)" = "mean_length_mm",
                                       "Body Depth" = "depth"),
                           selected = "mean_length_mm"),
               hr()
             ),
             mainPanel(
               textOutput("growth_status"),
               tags$hr(),
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
               fileInput("spawn_file", "Upload Spawning CSV",
                         accept = c(".csv"),
                         placeholder = "Using pott_spawn_data.csv"),
               helpText("Required columns: 'Date', 'Viable', 'Unviable'"),
               hr(),
               h4("Filters"),
               uiOutput("date_range_ui")
             ),
             mainPanel(
               textOutput("spawn_status"),
               tags$hr(),
               plotOutput("spawnPlot"),
               hr(),
               h4("Viability Summary"),
               tableOutput("spawnTable")
             )
           )
  )
)

# --- 4. SERVER LOGIC ---
server <- function(input, output, session) {

  # Growth Data
  growth_dataset <- reactive({
    if (is.null(input$growth_file)) {
      return(get_default_growth())
    } else {
      req(input$growth_file)
      raw <- read.csv(input$growth_file$datapath) %>% janitor::clean_names()
      if("length" %in% names(raw)) raw <- rename(raw, mean_length_mm = length)
      if("days" %in% names(raw)) raw <- rename(raw, dph = days)

      # Clean uploaded data same as default
      raw <- raw %>% filter(!is.na(dph), !is.na(mean_length_mm))
      if("protocol" %in% names(raw)) raw <- raw %>% filter(protocol != "")
      return(raw)
    }
  })

  output$growth_status <- renderText({
    if (is.null(input$growth_file)) "Viewing: Original Project Data" else paste("Viewing:", input$growth_file$name)
  })

  # Spawning Data
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

  output$spawn_status <- renderText({
    if (is.null(input$spawn_file)) "Viewing: Original Project Data" else paste("Viewing:", input$spawn_file$name)
  })

  # Dynamic UI
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

  # Plots
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

    # Smart Smoothing
    if (nrow(df) >= 10 && length(unique(df$dph)) >= 5) {
      p <- p + geom_smooth(se = FALSE, method = "loess", span = 1.0)
    } else if (nrow(df) >= 3) {
      p <- p + geom_smooth(se = FALSE, method = "lm")
    }

    suppressWarnings(print(p + theme_potter() + labs(title = paste("Larval", input$y_metric), x = "Days Post Hatch (dph)")))
  })

  output$growthStats <- renderTable({
    req(growth_dataset())
    df <- growth_dataset()
    if("protocol" %in% names(df)) {
      df %>% group_by(protocol) %>% summarise(Mean = mean(.data[[input$y_metric]], na.rm=TRUE), Max = max(.data[[input$y_metric]], na.rm=TRUE), N = n())
    } else {
      df %>% summarise(Mean = mean(.data[[input$y_metric]], na.rm=TRUE), Max = max(.data[[input$y_metric]], na.rm=TRUE), N = n())
    }
  })

  output$spawnPlot <- renderPlot({
    req(spawn_dataset())
    df <- spawn_dataset()
    validate(need("Viable" %in% names(df), "Missing columns"), need(nrow(df) > 0, "No data"))

    if (!is.null(input$dates)) df <- df %>% filter(Date >= input$dates[1] & Date <= input$dates[2])

    plot_data <- df %>% pivot_longer(cols = c(Viable, Unviable), names_to = "Egg_Type", values_to = "Count")

    ggplot(plot_data, aes(x = Date, y = Count, fill = Egg_Type)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Viable" = "#4e79a7", "Unviable" = "#f28e2b")) +
      scale_y_continuous(labels = scales::comma) +
      theme_potter() +
      labs(title = "Daily Egg Production", y = "Egg Count")
  })

  output$spawnTable <- renderTable({
    req(spawn_dataset(), input$dates)
    df <- spawn_dataset() %>% filter(Date >= input$dates[1] & Date <= input$dates[2])
    if(nrow(df) == 0) return(NULL)
    df %>% summarise(`Total Eggs` = sum(Viable + Unviable, na.rm=TRUE), `Total Viable` = sum(Viable, na.rm=TRUE), `Avg Viability %` = mean(Viable/(Viable+Unviable)*100, na.rm=TRUE)) %>%
      mutate(`Total Eggs` = format(`Total Eggs`, big.mark=","), `Total Viable` = format(`Total Viable`, big.mark=","))
  })
}

shinyApp(ui, server)
