library(shiny)
library(tidyverse)
library(lubridate)
library(bslib)
library(janitor)
library(readxl)

#Theme
theme_potter <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(
      strip.background = element_rect(fill = "#2c3e50"),
      strip.text = element_text(color = "white", face = "bold"),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}

# Feeding Schedule Plot
plot_feeding_schedule <- function(tidy_data) {
  ggplot(tidy_data, aes(x = age, y = feed_category, fill = feed_type)) +
    geom_tile(color = "white", height = 0.8) +
    labs(
      title = "Hatchery Feeding Schedule",
      x = "Culture Age (Days Post Hatch)",
      y = "Feed Category",
      fill = "Feed Type"
    ) +
    scale_x_continuous(breaks = seq(0, max(tidy_data$age, na.rm=TRUE), by = 2)) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# Data Reader
read_hatchery_data <- function(file_path) {

  # Determine extension
  ext <- tolower(tools::file_ext(file_path))

  # 1.Dynamic Skip: find where "Culture Age" is
  if (ext %in% c("xlsx", "xls")) {
    # Read first 30 rows
    header_check <- suppressMessages(readxl::read_excel(file_path, n_max = 30, col_names = FALSE))
    #Search for "Culture Age" in any column
    match_row <- which(apply(header_check, 1, function(x) any(grepl("Culture Age", x))), arr.ind = TRUE)
    skip_n <- if (length(match_row) > 0) match_row[1] - 1 else 0
  } else {
    # Text search for CSV
    lines <- readLines(file_path, n = 30)
    match_row <- which(grepl("Culture Age", lines))
    skip_n <- if (length(match_row) > 0) match_row[1] - 1 else 0
  }

  if (length(match_row) == 0) {
    warning("Could not find 'Culture Age' header.")
    return(NULL)
  }

  # 2. Read Data
  if (ext %in% c("xlsx", "xls")) {
    raw_data <- readxl::read_excel(file_path, skip = skip_n, col_names = FALSE)
  } else {
    raw_data <- read.csv(file_path, skip = skip_n, header = FALSE, stringsAsFactors = FALSE)
  }

  # 3. Clean
  tryCatch({
    # Select columns by index (A, B, M, Q, Y, AM, AP)
    tidy_data <- raw_data %>%
      dplyr::select(
        age = 1, date = 2, Algae = 13, Live_Feed = 17,
        Rotifers = 25, Dry_Feed = 39, Frozen_Feed = 42
      ) %>%
      dplyr::mutate(age = suppressWarnings(as.numeric(age))) %>%
      dplyr::filter(!is.na(age)) %>%
      tidyr::pivot_longer(
        cols = c(Algae, Live_Feed, Rotifers, Dry_Feed, Frozen_Feed),
        names_to = "feed_category", values_to = "feed_type"
      ) %>%
      dplyr::filter(!is.na(feed_type), feed_type != "0") %>%
      dplyr::mutate(feed_type = trimws(feed_type))

    return(tidy_data)
  }, error = function(e) return(NULL))
}


#Data Loading Helpers
get_default_growth <- function() {
  if (file.exists("pott_growth_data.csv")) {
    raw <- read.csv("pott_growth_data.csv", stringsAsFactors = FALSE) %>%
      janitor::clean_names()
    if("length" %in% names(raw)) raw <- rename(raw, mean_length_mm = length)
    if("days" %in% names(raw)) raw <- rename(raw, dph = days)
    raw <- raw %>% filter(!is.na(dph), !is.na(mean_length_mm))
    return(raw)
  }
  return(NULL) # No fallback package data
}

get_default_spawn <- function() {
  if (file.exists("pott_spawn_data.csv")) {
    raw <- read.csv("pott_spawn_data.csv", stringsAsFactors = FALSE) %>%
      mutate(
        Date = as.Date(lubridate::parse_date_time(Date, orders = c("mdy", "ymd", "dmy"))),
        Viable = as.numeric(gsub(",", "", Viable)),
        Unviable = as.numeric(gsub(",", "", Unviable))
      ) %>%
      filter(!is.na(Date))
    return(raw)
  }
  return(NULL)
}

#UI
ui <- navbarPage(
  title = "Potterful Analytics"
  ,
  theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),

  tabPanel("Larval Growth",
           sidebarLayout(
             sidebarPanel(
               h4("Data Source"),
               fileInput("growth_file", "Upload Growth CSV", accept = c(".csv"), placeholder = "Using pott_growth_data.csv"),
               helpText("Required columns: 'dph', 'protocol', 'length'"),
               hr(),
               h4("Controls"),
               uiOutput("protocol_selector"),
               selectInput("y_metric", "Y-Axis Metric:",
                           choices = c("Length (mm)" = "mean_length_mm", "Body Depth" = "depth"),
                           selected = "mean_length_mm"),
               hr()
             ),
             mainPanel(
               textOutput("growth_status"),
               hr(),
               plotOutput("growthPlot"),
               br(),
               h4("Growth Statistics"),
               tableOutput("growthStats")
             )
           )
  ),

  tabPanel("Spawning Trends",
           sidebarLayout(
             sidebarPanel(
               h4("Data Source"),
               fileInput("spawn_file", "Upload Spawning CSV", accept = c(".csv"), placeholder = "Using pott_spawn_data.csv"),
               hr(),
               h4("Filters"),
               uiOutput("date_range_ui")
             ),
             mainPanel(
               textOutput("spawn_status"),
               hr(),
               plotOutput("spawnPlot"),
               hr(),
               h4("Viability Summary"),
               tableOutput("spawnTable")
             )
           )
  ),

  tabPanel("Feeding Schedule",
           sidebarLayout(
             sidebarPanel(
               h4("Feeding Schedule Analysis"),
               p("Upload a raw hatchery daily log (.xlsx or .csv)."),
               fileInput("feed_file", "Upload Daily Log", accept = c(".csv", ".xlsx", ".xls")),
               hr(),
               helpText("Tip: Ensure file has 'Culture Age' header.")
             ),
             mainPanel(
               plotOutput("feedingPlot", height = "600px")
             )
           )
  ),

  #Footer with Big Image
  footer = div(
    style = "text-align: center; padding: 40px; background-color: #f9f9f9; border-top: 1px solid #e3e3e3;",
    h4("Centropyge potteri (Potter's Angelfish) - Kent Glover"),
    img(src = "https://raw.githubusercontent.com/OCN-682-UH/Glover_Final_Project/main/Potterful/inst/shiny/Potterful/www/potters_angel.jpg", height = "400px", style = "max-width: 100%; border-radius: 10px; margin-top: 10px;")
  )
)

#SERVER
server <- function(input, output, session) {

  # Growth
  growth_dataset <- reactive({
    if (is.null(input$growth_file)) return(get_default_growth())
    req(input$growth_file)
    raw <- read.csv(input$growth_file$datapath) %>% janitor::clean_names()
    if("length" %in% names(raw)) raw <- rename(raw, mean_length_mm = length)
    if("days" %in% names(raw)) raw <- rename(raw, dph = days)
    raw %>% filter(!is.na(dph), !is.na(mean_length_mm))
  })

  output$growth_status <- renderText({
    if (is.null(input$growth_file)) "Viewing: Default Data" else paste("Viewing:", input$growth_file$name)
  })

  output$protocol_selector <- renderUI({
    req(growth_dataset())
    df <- growth_dataset()
    if("protocol" %in% names(df)) selectInput("protocol", "Select Protocol:", choices = c("All", unique(as.character(df$protocol))))
  })

  output$growthPlot <- renderPlot({
    req(growth_dataset())
    df <- growth_dataset()
    validate(need("dph" %in% names(df), "Missing 'dph'"), need(input$y_metric %in% names(df), "Missing Metric"))

    if (!is.null(input$protocol) && input$protocol != "All" && "protocol" %in% names(df)) {
      df <- df %>% filter(protocol == input$protocol)
    }

    p <- ggplot(df, aes(x = dph, y = .data[[input$y_metric]]))
    if("protocol" %in% names(df)) p <- p + aes(color = protocol)
    p <- p + geom_point(alpha = 0.6, size = 3)
    if(nrow(df) >= 3) p <- p + geom_smooth(se = FALSE, method = "loess")

    label <- switch(input$y_metric, "mean_length_mm" = "Length (mm)", "depth" = "Body Depth", input$y_metric)

    print(p + theme_potter() + labs(title = paste("Larval", label), y = label, x = "Days Post Hatch (dph)"))
  })

  output$growthStats <- renderTable({
    req(growth_dataset())
    df <- growth_dataset()
    if("protocol" %in% names(df)) {
      df %>% group_by(protocol) %>% summarise(Mean = mean(.data[[input$y_metric]], na.rm=TRUE), N = n())
    } else {
      df %>% summarise(Mean = mean(.data[[input$y_metric]], na.rm=TRUE), N = n())
    }
  })

  # Spawn
  spawn_dataset <- reactive({
    if (is.null(input$spawn_file)) return(get_default_spawn())
    req(input$spawn_file)
    read.csv(input$spawn_file$datapath, stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(lubridate::parse_date_time(Date, orders = c("mdy", "ymd", "dmy"))),
             Viable = as.numeric(gsub(",", "", Viable)), Unviable = as.numeric(gsub(",", "", Unviable))) %>%
      filter(!is.na(Date))
  })

  output$spawn_status <- renderText({
    if (is.null(input$spawn_file)) "Viewing: Default Data" else paste("Viewing:", input$spawn_file$name)
  })

  output$date_range_ui <- renderUI({
    req(spawn_dataset())
    df <- spawn_dataset()
    dateRangeInput("dates", "Select Date Range:", start = min(df$Date, na.rm=TRUE), end = max(df$Date, na.rm=TRUE))
  })

  output$spawnPlot <- renderPlot({
    req(spawn_dataset())
    df <- spawn_dataset()
    validate(need("Viable" %in% names(df), "Missing data"))
    if(!is.null(input$dates)) df <- df %>% filter(Date >= input$dates[1] & Date <= input$dates[2])

    df %>% pivot_longer(cols = c(Viable, Unviable), names_to = "Egg_Type", values_to = "Count") %>%
      ggplot(aes(x = Date, y = Count, fill = Egg_Type)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Viable" = "#4e79a7", "Unviable" = "#f28e2b")) +
      scale_y_continuous(labels = scales::comma) +
      theme_potter() + labs(title = "Daily Egg Production", y = "Egg Count")
  })

  output$spawnTable <- renderTable({
    req(spawn_dataset(), input$dates)
    df <- spawn_dataset() %>% filter(Date >= input$dates[1] & Date <= input$dates[2])
    if(nrow(df)==0) return(NULL)
    df %>% summarise(`Total Eggs` = sum(Viable + Unviable, na.rm=TRUE), `Total Viable` = sum(Viable, na.rm=TRUE), `Avg Viability %` = mean(Viable/(Viable+Unviable)*100, na.rm=TRUE)) %>%
      mutate(`Total Eggs` = format(`Total Eggs`, big.mark=","), `Total Viable` = format(`Total Viable`, big.mark=","))
  })

  # Feeding
  output$feedingPlot <- renderPlot({
    req(input$feed_file)
    clean_feed <- read_hatchery_data(input$feed_file$datapath)
    validate(need(!is.null(clean_feed), "Could not read data. Check file format."),
             need(nrow(clean_feed) > 0, "No feeding data found in file."))
    plot_feeding_schedule(clean_feed)
  })
}

shinyApp(ui, server)
