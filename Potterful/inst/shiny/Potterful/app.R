library(shiny)
library(tidyverse)
library(lubridate)
library(bslib)
# Try to load the package, but don't crash if it's not installed
try(library(Potterful), silent = TRUE)

# --- HELPER FUNCTIONS: LOAD LOCAL DATA ---

# Function to find and read the CSV from data-raw
read_local_csv <- function(filename) {
  # List of possible paths to data-raw depending on where app is running
  possible_paths <- c(
    file.path("data-raw", filename),           # If running from package root
    file.path("..", "..", "..", "data-raw", filename), # If running from inst/shiny/Potterful
    file.path(".", filename)                   # If files are in same folder
  )

  # Find the first path that actually exists
  valid_path <- possible_paths[file.exists(possible_paths)][1]

  if (is.na(valid_path)) {
    return(NULL) # File not found
  }

  read.csv(valid_path, stringsAsFactors = FALSE)
}

get_default_growth <- function() {
  # 1. Try to read the actual CSV from data-raw
  raw_data <- read_local_csv("pott_growth_data.csv")

  if (!is.null(raw_data)) {
    # CLEANING: Match the column names your app expects
    clean_data <- raw_data %>%
      janitor::clean_names() %>%
      rename(
        mean_length_mm = length,   # Rename 'length' to 'mean_length_mm'
        protocol = protocol        # Ensure protocol is lowercase
      )
    return(clean_data)
  }

  # 2. Fallback: Try package data if CSV isn't found
  tryCatch({
    return(Potterful::potteri_larvae)
  }, error = function(e) return(NULL))
}

get_default_spawn <- function() {
  # 1. Try to read the actual CSV from data-raw
  raw_data <- read_local_csv("pott_spawn_data.csv")

  if (!is.null(raw_data)) {
    # CLEANING: Fix dates and remove commas from numbers
    clean_data <- raw_data %>%
      mutate(
        # Parse Date (Handle M/D/YY format)
        Date = lubridate::mdy(Date),
        # Remove commas and convert to numeric (e.g., "1,000" -> 1000)
        Viable = as.numeric(gsub(",", "", Viable)),
        Unviable = as.numeric(gsub(",", "", Unviable))
      ) %>%
      filter(!is.na(Date)) # Remove empty rows if any

    return(clean_data)
  }

  # 2. Fallback: Try package data
  tryCatch({
    return(Potterful::spawn_data)
  }, error = function(e) return(NULL))
}

get_potter_theme <- function() {
  # Helper to safely get the custom theme
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
      # Use our new helper function to get data-raw
      return(get_default_growth())
    } else {
      # Process uploaded file
      req(input$growth_file)
      raw <- read.csv(input$growth_file$datapath) %>%
        janitor::clean_names()

      # Standardize names
      if("length" %in% names(raw)) raw <- rename(raw, mean_length_mm = length)
      if("days" %in% names(raw)) raw <- rename(raw, dph = days)

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
      # Use our new helper function to get data-raw
      return(get_default_spawn())
    } else {
      req(input$spawn_file)
      raw <- read.csv(input$spawn_file$datapath)

      # Cleaning logic
      raw %>%
        mutate(
          Date = tryCatch(lubridate::mdy(Date), error = function(e) lubridate::ymd(Date)),
          Viable = as.numeric(gsub(",", "", Viable)),
          Unviable = as.numeric(gsub(",", "", Unviable))
        )
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

    p <- ggplot(df, aes(x = dph, y = .data[[input$y_metric]]))

    if("protocol" %in% names(df)) {
      p <- p + aes(color = protocol)
    }

    p + geom_point(alpha = 0.6, size = 3) +
      geom_smooth(se = FALSE, method = "loess") +
      get_potter_theme() +
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

    ggplot(plot_data, aes(x = Date, y = Count, fill = Egg_Type)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Viable" = "#4e79a7", "Unviable" = "#f28e2b")) +
      scale_y_continuous(labels = scales::comma) +
      get_potter_theme() +
      labs(title = "Daily Egg Production", y = "Egg Count")
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
