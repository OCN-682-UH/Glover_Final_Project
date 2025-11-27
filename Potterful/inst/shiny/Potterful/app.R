library(shiny)
library(Potterful)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Potterful: Centropyge potteri Analytics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("protocol", "Select Protocol:", choices = c("All", "Protocol A", "Protocol C")),
      hr(),
      helpText("Data loaded from Potterful package.")
    ),
    mainPanel(
      plotOutput("growthPlot"),
      tableOutput("statsTable")
    )
  )
)

server <- function(input, output) {
  output$growthPlot <- renderPlot({
    data <- Potterful::potteri_larvae
    if(input$protocol != "All") {
      data <- data %>% filter(protocol == input$protocol)
    }

    ggplot(data, aes(x = dph, y = mean_length_mm, color = protocol)) +
      geom_point() +
      geom_smooth() +
      theme_potter() +
      labs(title = "Larval Growth Trajectory", y = "Length (mm)", x = "Days Post Hatch")
  })

  output$statsTable <- renderTable({
    Potterful::potteri_larvae %>%
      group_by(protocol) %>%
      summarise(
        Max_Length = max(mean_length_mm),
        Survival_Count_Day60 = min(survival_count)
      )
  })
}

shinyApp(ui = ui, server = server)
