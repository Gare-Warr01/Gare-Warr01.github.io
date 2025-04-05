# app.R

library(shiny)
library(prophet)
library(dygraphs)
library(dplyr)
library(readr)
library(xts)
library(leaflet)
library(lubridate)  # For year handling

# Load the real flu dataset
flu_data <- read_csv("ICL_NREVSS_Summary.csv")

# Get subtype columns (everything but State, Year, Total_Cases)
subtype_columns <- colnames(flu_data)[!(colnames(flu_data) %in% c("State", "Year", "Total_Cases"))]

# Get state center coordinates for map markers
state_centers <- data.frame(state.center, State = state.name)

# UI layout
ui <- fluidPage(
  titlePanel("Flu Subtype Trends & Forecast by State"),
  sidebarLayout(
    sidebarPanel(
      selectInput("subtype", "Select Flu Subtype:", choices = subtype_columns),
      selectInput("year", "Select Year for Map:", choices = sort(unique(flu_data$Year))),
      textOutput("clicked_state"),
      numericInput("years", "Years to Forecast Ahead:", value = 2, min = 1, max = 5),
      helpText("Click a state on the map to view forecasts by flu subtype.")
    ),
    mainPanel(
      leafletOutput("heatmap", height = 450),
      br(),
      dygraphOutput("forecast_plot")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Selected state (reactive from map click)
  selected_state <- reactiveVal("California")
  
  # Text display of clicked state
  output$clicked_state <- renderText({
    paste("Currently Selected State:", selected_state())
  })
  
  # Render the leaflet heatmap
  output$heatmap <- renderLeaflet({
    subtype_data <- flu_data %>%
      filter(Year == input$year) %>%
      group_by(State) %>%
      summarise(Subtype_Cases = sum(get(input$subtype), na.rm = TRUE)) %>%
      left_join(state_centers, by = "State")
    
    pal <- colorNumeric("YlOrRd", domain = subtype_data$Subtype_Cases)
    
    leaflet(subtype_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.583, lat = 39.833, zoom = 4) %>%
      addCircleMarkers(
        lng = ~x, lat = ~y,
        radius = ~sqrt(Subtype_Cases) / 2,
        color = ~pal(Subtype_Cases),
        stroke = FALSE, fillOpacity = 0.8,
        label = ~paste0(State, ": ", Subtype_Cases, " cases"),
        layerId = ~State
      ) %>%
      addLegend("bottomright", pal = pal, values = ~Subtype_Cases,
                title = paste(input$subtype, "cases in", input$year))
  })
  
  # Listen for marker click
  observeEvent(input$heatmap_marker_click, {
    clicked <- input$heatmap_marker_click
    selected_state(clicked$id)
  })
  
  # Render the forecast dygraph
  output$forecast_plot <- renderDygraph({
    df <- flu_data %>%
      filter(State == selected_state()) %>%
      group_by(Year) %>%
      summarise(y = sum(get(input$subtype), na.rm = TRUE)) %>%
      mutate(ds = as.Date(paste0(Year, "-01-01"))) %>%
      select(ds, y)
    
    if (nrow(df) < 3 || sum(df$y, na.rm = TRUE) == 0) {
      return(dygraph(xts(), main = paste("Insufficient data for", selected_state(), "-", input$subtype)))
    }
    
    # Fit Prophet model
    m <- prophet(df, yearly.seasonality = TRUE)
    
    # Create future yearly dates (Jan 1st)
    future_years <- max(df$ds) + years(1:input$years)
    future <- data.frame(ds = c(df$ds, future_years))
    
    # Forecast
    forecast <- predict(m, future)
    
    # Clamp negative predictions
    forecast$yhat[forecast$yhat < 0] <- 0
    forecast$yhat_lower[forecast$yhat_lower < 0] <- 0
    forecast$yhat_upper[forecast$yhat_upper < 0] <- 0
    
    # Build xts objects for dygraph
    hist_xts <- xts(df$y, order.by = df$ds)
    pred_xts <- xts(forecast$yhat, order.by = forecast$ds)
    lower_xts <- xts(forecast$yhat_lower, order.by = forecast$ds)
    upper_xts <- xts(forecast$yhat_upper, order.by = forecast$ds)
    
    combined_xts <- cbind(
      Historical = hist_xts,
      Forecast = pred_xts[index(pred_xts) > max(index(hist_xts))],
      Lower = lower_xts[index(pred_xts) > max(index(hist_xts))],
      Upper = upper_xts[index(pred_xts) > max(index(hist_xts))]
    )
    
    # Final plot
    dygraph(combined_xts, main = paste("Forecast for", input$subtype, "in", selected_state())) %>%
      dySeries("Historical", color = "navy") %>%
      dySeries(c("Lower", "Forecast", "Upper"), label = "Forecast ± CI", color = "firebrick") %>%
      dyRangeSelector() %>%
      dyOptions(axisLabelFontSize = 14)
  })
}

# Run app
shinyApp(ui = ui, server = server)

