# app.R

library(shiny)
library(leaflet)
library(dplyr)
library(readr)

flu_data <- read.csv("ICL_NREVSS_Summary.csv")
state_centers <- data.frame(state.center, State = state.name)

ui <- fluidPage(
  titlePanel("Flu Case Heatmap by Year"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year", choices = sort(unique(flu_data$Year)), selected = 2023)
    ),
    mainPanel(
      leafletOutput("fluMap", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  output$fluMap <- renderLeaflet({
    yearly_flu <- flu_data %>%
      filter(Year == input$year) %>%
      group_by(State) %>%
      summarise(Total_Cases = sum(Total_Cases, na.rm = TRUE)) %>%
      left_join(state_centers, by = "State")
    
    pal <- colorNumeric("YlOrRd", domain = yearly_flu$Total_Cases)
    
    leaflet(yearly_flu) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.583, lat = 39.833, zoom = 4) %>%
      addCircleMarkers(
        lng = ~x, lat = ~y,
        radius = ~sqrt(Total_Cases) / 2,
        color = ~pal(Total_Cases),
        stroke = FALSE, fillOpacity = 0.8,
        popup = ~paste0("<b>", State, "</b><br>Total Cases: ", Total_Cases)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~Total_Cases,
                title = paste("Total Cases in", input$year))
  })
}

shinyApp(ui, server)

