# app.R - Flu Forecast Dashboard (Final Clean Version)

# ---- Libraries ----
library(shiny)
library(prophet)
library(dygraphs)
library(dplyr)
library(readr)
library(xts)
library(leaflet)
library(lubridate)
library(bslib)
library(shinycssloaders)
library(ggplot2)

# ---- Load Datasets ----
flu_data <- read_csv("cleaned_influenza_state_data.csv")
national_trend <- read_csv("national_trend_by_year.csv")

# ---- Flu Subtype Labels ----
subtype_map <- c(
  "Total_A" = "Influenza A",
  "Total_B" = "Influenza B",
  "Total_A_H1" = "A (H1N1)",
  "Total_A_H3" = "A (H3N2)"
)
subtype_columns <- names(subtype_map)

# ---- State Coordinates ----
state_centers <- data.frame(state.center, State = state.name)

# ---- UI ----
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly", version = 5),
  
  titlePanel("U.S. Flu Subtype Trends and Forecast Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("subtype", "Select Flu Subtype:",
                  choices = setNames(subtype_columns, subtype_map)),
      selectInput("year", "Select Year for Map:",
                  choices = sort(unique(flu_data$Year))),
      numericInput("years", "Years to Forecast Ahead:", value = 2, min = 1, max = 5),
      checkboxInput("show_ci", "Show Confidence Interval", TRUE),
      checkboxInput("show_us_avg", "Overlay National Average Trend", TRUE),
      helpText(tags$span("Click a state on the map to view flu subtype forecast.",
                         title = "Forecasts are based on the selected subtype and state.",
                         style = "text-decoration: underline; cursor: help;")),
      verbatimTextOutput("clicked_state")
    ),
    
    mainPanel(
      withSpinner(leafletOutput("heatmap", height = 450)),
      br(),
      withSpinner(dygraphOutput("forecast_plot")),
      br(),
      verbatimTextOutput("forecast_metrics"),
      br(),
      plotOutput("subtype_compare")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  selected_state <- reactiveVal("California")
  
  output$clicked_state <- renderText({
    paste("Selected State:", selected_state())
  })
  
  output$heatmap <- renderLeaflet({
    subtype_data <- flu_data %>%
      filter(Year == input$year) %>%
      group_by(State) %>%
      summarise(Cases = sum(get(input$subtype), na.rm = TRUE)) %>%
      left_join(state_centers, by = "State")
    
    pal <- colorNumeric("YlOrRd", domain = subtype_data$Cases)
    
    leaflet(subtype_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.583, lat = 39.833, zoom = 4) %>%
      addCircleMarkers(
        lng = ~x, lat = ~y,
        radius = ~pmin(sqrt(Cases), 20),
        color = ~pal(Cases),
        stroke = FALSE, fillOpacity = 0.8,
        label = ~paste0(State, ": ", round(Cases), " cases"),
        layerId = ~State
      )
    # Legend removed to avoid map corner box
    # %>% addLegend("bottomright", pal = pal, values = ~Cases,
    #              title = paste(subtype_map[input$subtype], "Cases in", input$year))
  })
  
  observeEvent(input$heatmap_marker_click, {
    clicked <- input$heatmap_marker_click
    selected_state(clicked$id)
  })
  
  forecast_data <- reactive({
    df <- flu_data %>%
      filter(State == selected_state()) %>%
      group_by(Year) %>%
      summarise(y = sum(get(input$subtype), na.rm = TRUE)) %>%
      mutate(ds = as.Date(paste0(Year, "-01-01"))) %>%
      select(ds, y)
    
    if (nrow(df) < 3 || sum(df$y, na.rm = TRUE) == 0) return(NULL)
    
    m <- prophet(df, yearly.seasonality = TRUE)
    future_years <- max(df$ds) + years(1:input$years)
    future <- data.frame(ds = c(df$ds, future_years))
    forecast <- predict(m, future)
    
    forecast <- forecast %>%
      mutate(across(c(yhat, yhat_lower, yhat_upper), ~ ifelse(. < 0, 0, .)))
    
    list(df = df, forecast = forecast)
  })
  
  output$forecast_plot <- renderDygraph({
    data <- forecast_data()
    if (is.null(data)) return(dygraph(xts(), main = "Insufficient data."))
    
    df <- data$df
    forecast <- data$forecast
    
    hist_xts <- xts(df$y, order.by = df$ds)
    pred_xts <- xts(forecast$yhat, order.by = forecast$ds)
    lower_xts <- xts(forecast$yhat_lower, order.by = forecast$ds)
    upper_xts <- xts(forecast$yhat_upper, order.by = forecast$ds)
    
    forecast_range <- index(pred_xts) > max(index(hist_xts))
    
    combined_xts <- cbind(
      Historical = hist_xts,
      Forecast = pred_xts[forecast_range]
    )
    
    if (input$show_ci) {
      combined_xts <- cbind(
        combined_xts,
        Lower = lower_xts[forecast_range],
        Upper = upper_xts[forecast_range]
      )
    }
    
    if (input$show_us_avg) {
      us_df <- national_trend %>%
        select(Year, value = all_of(input$subtype)) %>%
        mutate(ds = as.Date(paste0(Year, "-01-01")))
      
      us_xts <- xts(us_df$value, order.by = us_df$ds)
      combined_xts <- cbind(combined_xts, NationalAvg = us_xts[index(us_xts) %in% index(hist_xts)])
    }
    
    dygraph(combined_xts, main = paste("Forecast for", subtype_map[input$subtype], "in", selected_state())) %>%
      dySeries("Historical", color = "navy") %>%
      {
        if (input$show_ci) {
          dySeries(., c("Lower", "Forecast", "Upper"), label = "Forecast ± CI", color = "firebrick")
        } else {
          dySeries(., "Forecast", label = "Forecast", color = "firebrick")
        }
      } %>%
      {
        if (input$show_us_avg) {
          dySeries(., "NationalAvg", label = "National Avg", color = "gray")
        } else .
      } %>%
      dyRangeSelector() %>%
      dyOptions(
        axisLabelFontSize = 14,
        drawGrid = TRUE,
        drawPoints = TRUE
      ) %>%
      dyLegend(show = "follow", hideOnMouseOut = TRUE)
  })
  
  output$forecast_metrics <- renderPrint({
    data <- forecast_data()
    if (is.null(data)) return("Not enough data to evaluate model.")
    
    df <- data$df
    forecast <- data$forecast
    
    actual <- tail(df$y, 2)
    predicted <- tail(forecast$yhat, length(actual))
    
    mae <- round(mean(abs(actual - predicted), na.rm = TRUE), 2)
    rmse <- round(sqrt(mean((actual - predicted)^2, na.rm = TRUE)), 2)
    
    peak_idx <- which.max(forecast$yhat)
    peak_year <- year(forecast$ds[peak_idx])
    direction <- ifelse(tail(forecast$yhat, 1) > tail(df$y, 1), "Increasing", "Decreasing")
    
    cat("Forecast Summary\n")
    cat("Peak Year:", peak_year, "\n")
    cat("Trend Direction:", direction, "\n")
    cat("MAE:", mae, "\n")
    cat("RMSE:", rmse, "\n")
  })
  
  output$subtype_compare <- renderPlot({
    subtypes <- c("Total_A", "Total_B", "Total_A_H1", "Total_A_H3")
    labels <- c("Influenza A", "Influenza B", "A (H1N1)", "A (H3N2)")
    
    df <- flu_data %>%
      filter(State == selected_state(), Year == input$year) %>%
      summarise(across(all_of(subtypes), sum, na.rm = TRUE))
    
    if (nrow(df) == 0) return(NULL)
    
    df_long <- data.frame(
      Subtype = labels,
      Cases = as.numeric(df[1, ])
    )
    
    ggplot(df_long, aes(x = Subtype, y = Cases, fill = Subtype)) +
      geom_col(show.legend = FALSE) +
      labs(title = paste("Flu Subtype Breakdown in", selected_state(), "-", input$year),
           x = "Subtype", y = "Cases") +
      theme_minimal(base_size = 14)
  })
}

# ---- Run App ----
shinyApp(ui = ui, server = server)

