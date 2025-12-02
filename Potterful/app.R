options(shiny.autoload.r = FALSE) # Fix: Prevent Shiny from autoloading R/ scripts which conflicts with package loading
library(shiny)
library(tidyverse)
library(lubridate)
library(bslib)
library(janitor)
library(Potterful)

# --- 1. DATA LOADING HELPER FUNCTIONS ---
get_default_growth <- function() {
  if (file.exists("pott_growth_data.csv")) {
    raw <- read.csv("pott_growth_data.csv", stringsAsFactors = FALSE) %>%
      janitor::clean_names()

    # Cleaning
    if("length" %in% names(raw)) raw <- rename(raw, mean_length_mm = length)
    if("days" %in% names(raw)) raw <- rename(raw, dph = days)

    raw <- raw %>% filter(!is.na(dph), !is.na(mean_length_mm))
    return(raw)
  } else {
    # FALLBACK: Use the dataset included in the package
    return(Potterful::growth_data)
  }
}

get_default_spawn <- function() {
  if (file.exists("pott_spawn_data.csv")) {
    raw <- read.csv("pott_spawn_data.csv", stringsAsFactors = FALSE) %>%
      mutate(
        # FIX: "parse_date_time" checks MDY, YMD, and DMY automatically.
        # No tryCatch needed. This prevents the "No Data" bug.
        Date = as.Date(lubridate::parse_date_time(Date, orders = c("mdy", "ymd", "dmy"))),

        # Ensure numbers are clean (removes commas)
        Viable = as.numeric(gsub(",", "", Viable)),
        Unviable = as.numeric(gsub(",", "", Unviable))
      ) %>%
      filter(!is.na(Date))
    return(raw)
  } else {
    return(Potterful::spawn_data)
  }
}

# --- 2. UI DEFINITION ---
ui <- navbarPage(
  title = div(
    # Fish Icon
    img(src = "potters_angel.png", height = "30px", style = "margin-right: 10px;"),
    "Potterful Analytics"
  ),
  theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),

  # --- TAB 1: LARVAL GROWTH ---
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

               # FIX 1: Ensure values match janitor cleaned names (lowercase 'depth')
               # and the 'selected' argument uses the value, not the label.
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

  # --- TAB 2: SPAWNING TRENDS ---
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
  ),

  # --- TAB 3: FEEDING SCHEDULE ---
  tabPanel("Feeding Schedule",
           sidebarLayout(
             sidebarPanel(
               h4("Feeding Schedule Analysis"),
               p("Upload a raw hatchery daily log (.xlsx or .csv) to visualize the feeding timeline."),

               # Accepts Excel files now
               fileInput("feed_file", "Upload Daily Log",
                         accept = c(".csv", ".xlsx", ".xls")),

               hr(),
               helpText("Tip: Ensure the file has a 'Culture Age' header row.")
             ),
             mainPanel(
               plotOutput("feedingPlot", height = "600px")
             )
           )
  )
)

# --- 3. SERVER LOGIC ---
server <- function(input, output, session) {

  # ==========================
  # LOGIC: Larval Growth
  # ==========================
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

  output$growth_status <- renderText({
    if (is.null(input$growth_file)) "Viewing: Original Project Data" else paste("Viewing:", input$growth_file$name)
  })

  output$protocol_selector <- renderUI({
    req(growth_dataset())
    df <- growth_dataset()
    if("protocol" %in% names(df)) {
      selectInput("protocol", "Select Protocol:", choices = c("All", unique(as.character(df$protocol))))
    }
  })

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

    if (nrow(df) >= 3) {
      p <- p + geom_smooth(se = FALSE, method = "lm")
    }

    # FIX 2: Create a human-readable label based on the input selection
    pretty_label <- switch(input$y_metric,
                           "mean_length_mm" = "Length (mm)",
                           "depth" = "Body Depth",
                           input$y_metric) # Fallback to raw name if needed

    suppressWarnings(print(
      p + Potterful::theme_potter() +
        labs(title = paste("Larval", pretty_label),
             y = pretty_label, # Use the pretty label on the axis too
             x = "Days Post Hatch (dph)")
    ))
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

  # ==========================
  # LOGIC: Spawning Trends
  # ==========================
  spawn_dataset <- reactive({
    if (is.null(input$spawn_file)) {
      return(get_default_spawn())
    } else {
      req(input$spawn_file)
      raw <- read.csv(input$spawn_file$datapath, stringsAsFactors = FALSE)

      # FIX: Apply the same robust cleaning to uploaded files
      raw %>%
        mutate(
          Date = as.Date(lubridate::parse_date_time(Date, orders = c("mdy", "ymd", "dmy"))),
          Viable = as.numeric(gsub(",", "", Viable)),
          Unviable = as.numeric(gsub(",", "", Unviable))
        ) %>%
        filter(!is.na(Date))
    }
  })

  output$spawn_status <- renderText({
    if (is.null(input$spawn_file)) "Viewing: Original Project Data" else paste("Viewing:", input$spawn_file$name)
  })

  output$date_range_ui <- renderUI({
    req(spawn_dataset())
    df <- spawn_dataset()
    if("Date" %in% names(df) && nrow(df) > 0) {
      dateRangeInput("dates", "Select Date Range:", start = min(df$Date, na.rm=TRUE), end = max(df$Date, na.rm=TRUE))
    }
  })

  output$spawnPlot <- renderPlot({
    req(spawn_dataset())
    df <- spawn_dataset()
    validate(need("Viable" %in% names(df), "Missing columns"), need(nrow(df) > 0, "No data found after cleaning."))

    if (!is.null(input$dates)) df <- df %>% filter(Date >= input$dates[1] & Date <= input$dates[2])

    plot_data <- df %>% pivot_longer(cols = c(Viable, Unviable), names_to = "Egg_Type", values_to = "Count")

    ggplot(plot_data, aes(x = Date, y = Count, fill = Egg_Type)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Viable" = "#4e79a7", "Unviable" = "#f28e2b")) +
      scale_y_continuous(labels = scales::comma) +
      Potterful::theme_potter() +
      labs(title = "Daily Egg Production", y = "Egg Count")
  })

  output$spawnTable <- renderTable({
    req(spawn_dataset(), input$dates)
    df <- spawn_dataset() %>% filter(Date >= input$dates[1] & Date <= input$dates[2])
    if(nrow(df) == 0) return(NULL)
    df %>% summarise(`Total Eggs` = sum(Viable + Unviable, na.rm=TRUE), `Total Viable` = sum(Viable, na.rm=TRUE), `Avg Viability %` = mean(Viable/(Viable+Unviable)*100, na.rm=TRUE)) %>%
      mutate(`Total Eggs` = format(`Total Eggs`, big.mark=","), `Total Viable` = format(`Total Viable`, big.mark=","))
  })

  # ==========================
  # LOGIC: Feeding Schedule
  # ==========================
  output$feedingPlot <- renderPlot({
    req(input$feed_file)
    clean_feed <- Potterful::read_hatchery_data(input$feed_file$datapath)

    validate(
      need(!is.null(clean_feed), "Could not read data. Check file format."),
      need(nrow(clean_feed) > 0, "No feeding data found in file.")
    )

    Potterful::plot_feeding_schedule(clean_feed)
  })
}

shinyApp(ui, server)
