library(shiny)
library(tidyverse)
library(lubridate)
library(bslib)
# Try to load the package, but don't crash if it's not installed
try(library(Potterful), silent = TRUE)

# --- HELPER FUNCTIONS: LOAD LOCAL DATA ---

read_local_csv <- function(filename) {
  possible_paths <- c(
    file.path("data-raw", filename),
    file.path("..", "..", "..", "data-raw", filename),
    file.path(".", filename)
  )
  valid_path <- possible_paths[file.exists(possible_paths)][1]
  if (is.na(valid_path)) return(NULL)
  read.csv(valid_path, stringsAsFactors = FALSE)
}

get_default_growth <- function() {
  raw_data <- read_local_csv("pott_growth_data.csv")

  if (!is.null(raw_data)) {
    # CLEANING: Match names AND remove the empty rows causing warnings
    clean_data <- raw_data %>%
      janitor::clean_names() %>%
      rename(
        mean_length_mm = length,
        protocol = protocol
      ) %>%
      # FIX: Filter out rows where crucial data is missing
      filter(!is.na(dph), !is.na(mean_length_mm), protocol != "")

    return(clean_data)
  }

  # Fallback
  tryCatch({
    return(Potterful::potteri_larvae)
  }, error = function(e) return(NULL))
}

get_default_spawn <- function() {
  raw_data <- read_local_csv("pott_spawn_data.csv")

  if (!is.null(raw_data)) {
    clean_data <- raw_data %>%
      mutate(
        Date = lubridate::mdy(Date),
        Viable = as.numeric(gsub(",", "", Viable)),
        Unviable = as.numeric(gsub(",", "", Unviable))
      ) %>%
      filter(!is.na(Date)) # Remove empty rows

    return(clean_data)
  }

  tryCatch({
    return(Potterful::spawn_data)
  }, error = function(e) return(NULL))
}

get_potter_theme <- function() {
  tryCatch({
    Potterful::theme_potter()
  }, error = function(e) {
    theme_minimal()
  })
}

# --- UI DEFINITION ---
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
                         placeholder = "Using data-raw/pott_growth_data.csv"),
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
                         placeholder = "Using data-raw/pott_spawn_data.csv"),
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

# --- SERVER LOGIC ---
server <- function(input, output, session) {

  # --- REACTIVE: Growth Data ---
  growth_dataset <- reactive({
    if (is.null(input$growth_file)) {
      return(get_default_growth())
    } else {
      req(input$growth_file)
      raw <- read.csv(input$growth_file$datapath) %>%
        janitor::clean_names()

      if("length" %in% names(raw)) raw <- rename(raw, mean_length_mm = length)
      if("days" %in% names(raw)) raw <- rename(raw, dph = days)

      # FIX: Apply the same cleaning to user uploads
      raw <- raw %>%
        filter(!is.na(dph), !is.na(mean_length_mm))

      if("protocol" %in% names(raw)) {
        raw <- raw %>% filter(protocol != "")
      }

      return(raw)
    }
  })

  output$growth_status <- renderText({
    if (is.null(input$growth_file)) {
      "Viewing: Original Project Data (from data-raw/pott_growth_data.csv)"
    } else {
      paste("Viewing: User Uploaded Data -", input$growth_file$name)
    }
  })

  # --- REACTIVE: Spawning Data ---
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
        filter(!is.na(Date)) # Remove empty rows
    }
  })

  output$spawn_status <- renderText({
    if (is.null(input$spawn_file)) {
      "Viewing: Original Project Data (from data-raw/pott_spawn_data.csv)"
    } else {
      paste("Viewing: User Uploaded Data -", input$spawn_file$name)
    }
  })

  # --- DYNAMIC UI INPUTS ---
  output$protocol_selector <- renderUI({
    req(growth_dataset())
    df <- growth_dataset()
    if("protocol" %in% names(df)) {
      selectInput("protocol", "Select Protocol:",
                  choices = c("All", unique(as.character(df$protocol))))
    }
  })

  output$date_range_ui <- renderUI({
    req(spawn_dataset())
    df <- spawn_dataset()
    if("Date" %in% names(df) && nrow(df) > 0) {
      dateRangeInput("dates", "Select Date Range:",
                     start = min(df$Date, na.rm=TRUE),
                     end = max(df$Date, na.rm=TRUE))
    }
  })

  # --- OUTPUTS: Tab 1 (Growth) ---
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

    # Start the plot
    p <- ggplot(df, aes(x = dph, y = .data[[input$y_metric]]))

    if("protocol" %in% names(df)) {
      p <- p + aes(color = protocol)
    }

    p <- p + geom_point(alpha = 0.6, size = 3)

    # --- SMART SMOOTHING ---
    # Only try LOESS if we have enough unique days (>4) and enough points (>9)
    # This prevents the "Singularity" and "Reciprocal condition" errors
    unique_days <- length(unique(df$dph))
    total_points <- nrow(df)

    if (unique_days >= 5 && total_points >= 10) {
      # Use span=1 to be less sensitive to small local variations
      p <- p + geom_smooth(se = FALSE, method = "loess", span = 1.0)
    } else if (total_points >= 3) {
      # Fallback to linear model if data is sparse
      p <- p + geom_smooth(se = FALSE, method = "lm")
    }

    p <- p + get_potter_theme() +
      labs(title = paste("Larval", input$y_metric, "over Time"),
           x = "Days Post Hatch (dph)")

    # Suppress remaining warnings so they don't clutter the console
    print(p)
  })

  output$growthStats <- renderTable({
    req(growth_dataset())
    df <- growth_dataset()

    if("protocol" %in% names(df)) {
      df %>%
        group_by(protocol) %>%
        summarise(
          Mean = mean(.data[[input$y_metric]], na.rm=TRUE),
          Max = max(.data[[input$y_metric]], na.rm=TRUE),
          N = n()
        )
    } else {
      df %>%
        summarise(
          Mean = mean(.data[[input$y_metric]], na.rm=TRUE),
          Max = max(.data[[input$y_metric]], na.rm=TRUE),
          N = n()
        )
    }
  })

  # --- OUTPUTS: Tab 2 (Spawning) ---
  output$spawnPlot <- renderPlot({
    req(spawn_dataset())
    df <- spawn_dataset()

    validate(
      need("Viable" %in% names(df), "Data not available or missing columns"),
      need(nrow(df) > 0, "Not enough data to plot")
    )

    if (!is.null(input$dates)) {
      df <- df %>% filter(Date >= input$dates[1] & Date <= input$dates[2])
    }

    plot_data <- df %>%
      pivot_longer(cols = c(Viable, Unviable),
                   names_to = "Egg_Type",
                   values_to = "Count")

    p <- ggplot(plot_data, aes(x = Date, y = Count, fill = Egg_Type)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Viable" = "#4e79a7", "Unviable" = "#f28e2b")) +
      scale_y_continuous(labels = scales::comma) +
      get_potter_theme() +
      labs(title = "Daily Egg Production", y = "Egg Count")

    print(p)
  })

  output$spawnTable <- renderTable({
    req(spawn_dataset(), input$dates)

    df <- spawn_dataset() %>%
      filter(Date >= input$dates[1] & Date <= input$dates[2])

    if(nrow(df) == 0) return(NULL)

    df %>%
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
