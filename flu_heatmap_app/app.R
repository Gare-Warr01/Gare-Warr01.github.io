
# app.R

library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(tidyr)
library(bslib)
library(shinycssloaders)
library(ggplot2)
library(plotly)
library(DT)

# Load long-format data
long_flu_data <- read.csv("ICL_NREVSS_LongFormat.csv")
state_centers <- data.frame(state.center, State = state.name)

# Create friendly strain labels
strain_labels <- c(
  "A_H1N1_2009" = "H1N1",
  "A_H3" = "H3",
  "A_Subtype_Not_Performed" = "A (Unsubtyped)",
  "B" = "Influenza B (Total)",
  "BVic" = "B/Victoria",
  "BYam" = "B/Yamagata",
  "H3N2v" = "H3N2v (Variant)",
  "A_H5" = "H5 (Avian)"
)

# Add friendly label column
long_flu_data$Strain_Label <- strain_labels[long_flu_data$Strain]

# UI
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  tags$div(style = "padding: 15px;",
           titlePanel("Flu Case Heatmap by Strain & Year"),
           
           sidebarLayout(
             sidebarPanel(
               helpText("Visualize flu case totals by strain and state for each year."),
               
               selectInput(
                 "strain", "Select Flu Strain",
                 choices = unique(long_flu_data$Strain_Label),
                 selected = unique(long_flu_data$Strain_Label)[1]
               ),
               
               sliderInput(
                 "year", "Select Year", 
                 min = min(long_flu_data$Year), 
                 max = max(long_flu_data$Year), 
                 value = max(long_flu_data$Year), 
                 step = 1, 
                 animate = animationOptions(interval = 1500, loop = TRUE)
               ),
               
               textOutput("totalCasesText"),
               br(),
               h5("Top 5 States"),
               DTOutput("topStates")
             ),
             
             mainPanel(
               withSpinner(leafletOutput("fluMap", height = "600px")),
               tags$div(
                 style = "text-align: right; font-size: 12px; color: gray; margin-top: 10px;",
                 "Data source: ICL NREVSS | Map powered by Leaflet & Shiny"
               )
             )
           )
  )
)

# Server
server <- function(input, output, session) {
  
  selected_strain <- reactive({
    names(strain_labels)[which(strain_labels == input$strain)]
  })
  
  filtered_data <- reactive({
    long_flu_data %>%
      filter(Year == input$year, Strain == selected_strain())
  })
  
  output$fluMap <- renderLeaflet({
    yearly_flu <- filtered_data() %>%
      group_by(State) %>%
      summarise(Total_Cases = sum(Cases, na.rm = TRUE)) %>%
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
        popup = ~paste(
          "<strong>State:</strong>", State, "<br/>",
          "<strong>Total Cases:</strong>", format(Total_Cases, big.mark = ",")
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = ~Total_Cases,
                title = paste("Total Cases in", input$year))
  })
  
  output$totalCasesText <- renderText({
    total <- sum(filtered_data()$Cases, na.rm = TRUE)
    paste("Total", input$strain, "Cases:", format(total, big.mark = ","))
  })
  
  output$topStates <- renderDT({
    filtered_data() %>%
      group_by(State) %>%
      summarise(Cases = sum(Cases, na.rm = TRUE)) %>%
      arrange(desc(Cases)) %>%
      slice_head(n = 5) %>%
      mutate(Rank = row_number()) %>%
      dplyr::select(Rank, State, Cases)
  }, options = list(dom = 't', pageLength = 5), rownames = FALSE)
}

# Run app
shinyApp(ui, server)

