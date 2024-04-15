library(readxl)
library(tidyverse)

#set up CRAN Mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

acled_sexual_violence <- read_csv("acled_sexual_violence.csv", name_repair = "universal")



#######################                          #######################
#######################          Part 1:         #######################
#######################          Shiny R         #######################
#######################                          #######################

#cleaning for Shiny
acled_sexual_violence$event_date <- as.Date(acled_sexual_violence$event_date, format = "%d %B %Y")

acled_sexual_violence <- 
  acled_sexual_violence %>%
  mutate(Actor_Type = case_when(
    inter1 == 1 ~ "State Actor",
    inter1 == 2 ~ "Rebel Group",
    inter1 == 3 ~ "Political Militias",
    inter1 == 4 ~ "Identity Militias",
    inter1 == 5 ~ "Rioters",
    inter1 == 6 ~ "Protesters",
    inter1 == 7 ~ "Civilians",
    inter1 == 8 ~ "External/Other Forces",
    TRUE ~ NA_character_  
  ))


acled_sexual_violence %>% 
  group_by(Actor_Type) %>% 
  summarize(n = n())
# too many individual civilian actions, remove them

acled_sexual_violence <- 
  acled_sexual_violence %>% 
  filter(Actor_Type != "Civilians")

#make the Shiny
#install.packages("shiny")

# Things to add 26FEB 
    # search by country
    # search for an actor (like keyword: Islamic State (North Africa) contained in ISIS)
    # add ability to search for "all" in type of actor
    # add definitions of each type of actor

library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(leaflet.extras)
library(maps)


# UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
        .tab-content {
          margin-top: 20px;
        }
        
        .selectors {
          margin-bottom: 20px;
        }
        "
      )
    )
  ),
  title = "World Map of Conflict Related Sexual Violence",
  titlePanel(
    HTML("<h1 style='font-family:Georgia; font-size:46px; text-align:center;'>World Map of Conflict Related Sexual Violence</h1>")
  ),
  tabsetPanel(
    tabPanel("Instance Search", fluid = TRUE,
             fluidRow(
               column(3,
                      dateRangeInput("date_select", "Date Range", start = "2022-01-01", end = "2024-12-31"),
                      checkboxInput("all_dates", "See all available dates", value = FALSE)
               ),
               column(3,
                      selectInput("country_select", "Select Country", choices = NULL, multiple = TRUE)
               ),
               column(3,
                      selectInput("region_select", "Select Region", choices = NULL, multiple = TRUE)
               ),
               column(3,
                      selectInput("type", "Type of Actor", choices = NULL)
               ),
               column(3,
                      textInput("search_input", "Search", placeholder = "Search key-word")
               )
             ),
             leafletOutput("map"),
             fluidRow(
               column(12, style = "margin-top: 20px;",
                      checkboxGroupInput("graph_type", "Graph Type", choices = c("Frequency", "Fatalities"), selected = "Frequency"),
                      plotOutput("frequency_plot")
               )
             ),
             column(12,
                    h4("Actor Type Definitions"),
                    HTML("<ul>"),
                    HTML("<li><b>State Forces:</b> Collective actors, including military and police, that are recognized to perform government functions over a given territory.</li>"),
                    HTML("<li><b>Rebel Groups:</b> Political organizations with the goal of countering an established national governing regime through violence.</li>"),
                    HTML("<li><b>Political Militias:</b> A diverse set of violent actors that are often created for a specific purpose or during a specific time period and for the furthering of a political goal by violence.</li>"),
                    HTML("<li><b>Identity Militias:</b> A broad category of identity militias for armed and violent groups organized around a collective, common feature, including: community, ethnicity, region, religion, or – in exceptional cases – livelihood.</li>"),
                    HTML("<li><b>External/Other Forces:</b> International organizations, state forces active outside of their main country of operation, private security firms and their armed employees, and hired mercenaries acting independently.</li>"),
                    HTML("</ul>")
             ),
             column(12,
                    h4("About"),
                    HTML("<ul>"),
                    HTML("<li>Select from the dropdown menus to sort through the data.</li>"),
                    HTML("<li>The data begins in January 1997.</li>"),
                    HTML("<li>To search for a keyword, type into the search menu on the right. For example, searching 'Wagner' will populate every recorded sexual violence act where the Wagner Group was an actor. Or, searching 'Twitter' will populate every recorded instance where Twitter is the source.</li>"),
                    HTML("<li>Source: <a href='https://acleddata.com/'>ACLED Data</a></li>"),
                    HTML("</ul>")
             )
    ),
    tabPanel("Heat Map", fluid = TRUE,
             fluidRow(
               column(3,
                      dateRangeInput("date_select_heatmap", "Date Range", start = "2022-01-01", end = "2024-12-31"),
                      checkboxInput("all_dates_heatmap", "See all available dates", value = FALSE)
               ),
               column(3,
                      selectInput("country_select_heatmap", "Select Country", choices = NULL, multiple = TRUE)
               ),
               column(3,
                      selectInput("region_select_heatmap", "Select Region", choices = NULL, multiple = TRUE)
               ),
               column(3,
                      selectInput("type_heatmap", "Type of Actor", choices = NULL)
               )
             ),
             leafletOutput("heatmap")
    ),
    tabPanel("Case Study: Ukraine", fluid = TRUE,
             leafletOutput("ukraine_map"),
             br(),
             sliderInput("date_slider_ukraine", "Date Range", min = as.Date(min(acled_sexual_violence$event_date)),
                         max = as.Date(max(acled_sexual_violence$event_date)), value = as.Date(min(acled_sexual_violence$event_date))),
             actionButton("play_button_ukraine", "Play")
    ),
    tabPanel("Case Study: Myanmar", fluid = TRUE,
             leafletOutput("myanmar_map"),
             br(),
             sliderInput("date_slider_myanmar", "Date Range", min = as.Date(min(acled_sexual_violence$event_date)),
                         max = as.Date(max(acled_sexual_violence$event_date)), value = as.Date(min(acled_sexual_violence$event_date))),
             actionButton("play_button_myanmar", "Play")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Create reactive values to hold filtered data and other reactive values
  rv <- reactiveValues(
    filtered_data = acled_sexual_violence,
    country_choices = NULL,
    type_choices = NULL
  )
  
  # Define reactive expressions for updating select inputs based on data
  observe({
    rv$country_choices <- c("Select All", unique(rv$filtered_data$country))
    rv$type_choices <- c("Select All", unique(rv$filtered_data$Actor_Type))
  })
  
  observe({
    updateSelectInput(session, "country_select", choices = rv$country_choices)
    updateSelectInput(session, "type", choices = rv$type_choices)
    
    updateSelectizeInput(session, "region_select", choices = c(
      "Select All", c(
        "Eastern Africa", "Middle Africa", "Western Africa", "Southern Africa",
        "Northern Africa", "South Asia", "Southeast Asia", "East Asia", "Central Asia",
        "Caucasus and Central Asia", "Oceania", "South America", "Caribbean",
        "Central America", "North America", "Europe", "Middle East"
      )
    ))
    
    # Update date range input based on checkbox
    if (input$all_dates) {
      updateDateRangeInput(session, "date_select", start = min(rv$filtered_data$event_date), end = max(rv$filtered_data$event_date))
    }
  })
  
  # Define reactive expression for filtered data based on user inputs
  filtered_data <- reactive({
    filtered_data <- acled_sexual_violence
    
    # Filter by country
    if (!is.null(input$country_select) && input$country_select != "Select All") {
      filtered_data <- filtered_data %>% filter(country %in% input$country_select)
    }
    
    # Filter by region
    if (!is.null(input$region_select) && !identical(input$region_select, "Select All")) {
      filtered_data <- filtered_data %>% filter(region %in% input$region_select)
    }
    
    # Filter by actor type
    if (!is.null(input$type) && input$type != "Select All") {
      filtered_data <- filtered_data %>% filter(Actor_Type %in% input$type)
    }
    
    # Filter by date range
    filtered_data <- filtered_data %>%
      filter(event_date >= input$date_select[1] & event_date <= input$date_select[2])
    
    # Filter by search input
    if (!is.null(input$search_input) && input$search_input != "") {
      search_term <- input$search_input
      filtered_data <- filtered_data %>%
        filter(str_detect(notes, search_term) |
                 str_detect(actor1, search_term) |
                 str_detect(source, search_term) |
                 str_detect(assoc_actor_1, search_term)
        )
    }
    
    return(filtered_data)
  })
  
  # Render map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      addTiles() %>%
      setView(lng = 0, lat = 30, zoom = 2) %>%
      addMarkers(data = filtered_data(),
                 lng = ~longitude, lat = ~latitude,
                 popup = ~paste0(
                   "<b>Event Date:</b> ", event_date, "<br>",
                   "<b>Country:</b> ", country, "<br>",
                   "<b>Actor Name:</b> ", actor1, "<br>",
                   ifelse(!is.na(assoc_actor_1), paste0("<b>Secondary Actor:</b> ", assoc_actor_1, "<br>"), ""),
                   "<b>Actor Type:</b> ", Actor_Type, "<br>",
                   "<b>Description:</b> ", notes, "<br>",
                   "<b>Source:</b> ", source
                 ))
  })
  
  # Render heatmap
  output$heatmap <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addHeatmap(data = filtered_data(), lng = ~longitude, lat = ~latitude)
  })
  
  # Render Ukraine map
  output$ukraine_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 5)) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", attribution = NULL) %>%
      setView(lng = 31, lat = 49, zoom = 5) %>%
      addMarkers(data = filtered_data() %>% filter(country == "Ukraine"),
                 lng = ~longitude, lat = ~latitude)
  })
  
  # Render Myanmar map
  output$myanmar_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 5)) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", attribution = NULL) %>%
      setView(lng = 95, lat = 21, zoom = 5) %>%
      addMarkers(data = filtered_data() %>% filter(country == "Myanmar"),
                 lng = ~longitude, lat = ~latitude)
  })
  
  # Reactive expressions for date slider
  observe({
    if (!is.null(input$date_select_ukraine)) {
      updateSliderTextInput(session, "date_slider_ukraine", 
                            choices = as.character(seq(input$date_select_ukraine[1], input$date_select_ukraine[2], by = "day")))
    }
  })
  
  observe({
    if (!is.null(input$date_select_myanmar)) {
      updateSliderTextInput(session, "date_slider_myanmar", 
                            choices = as.character(seq(input$date_select_myanmar[1], input$date_select_myanmar[2], by = "day")))
    }
  })
  
  # Timer for play button
  observeEvent(input$play_button_ukraine, {
    # Placeholder for timer action
  })
  
  observeEvent(input$play_button_myanmar, {
    # Placeholder for timer action
  })
  
  # Render frequency plot
  output$frequency_plot <- renderPlot({
    frequency_data <- filtered_data() %>%
      mutate(year = lubridate::year(event_date)) %>%
      group_by(year) %>%
      summarise(frequency = n())
    
    frequency_data_fatal <- filtered_data() %>%
      mutate(year = lubridate::year(event_date)) %>%
      group_by(year) %>%
      summarise(fatalities = sum(fatalities))
    
    if ("Frequency" %in% input$graph_type && "Fatalities" %in% input$graph_type) {
      ggplot() +
        geom_line(data = frequency_data, aes(x = year, y = frequency, color = "Frequency"), size = 1.5) +
        geom_line(data = frequency_data_fatal, aes(x = year, y = fatalities, color = "Fatalities"), size = 1.5) +
        labs(title = "Frequency and Fatalities of Sexual Violence Events by Year and Search Criteria",
             x = "Year", y = "Count") +
        scale_color_manual(values = c(Frequency = "lightblue", Fatalities = "salmon")) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size = 15, family = "Georgia"),
              axis.text.x = element_text(angle = 45, hjust = 1))
    } else if ("Frequency" %in% input$graph_type) {
      ggplot(data = frequency_data, aes(x = year, y = frequency, color = "Frequency")) +
        geom_line(size = 1.5) +
        labs(title = "Frequency of Sexual Violence Events by Year and Search Criteria",
             x = "Year", y = "Count") +
        scale_color_manual(values = c(Frequency = "lightblue")) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size = 15, family = "Georgia"),
              axis.text.x = element_text(angle = 45, hjust = 1))
    } else if ("Fatalities" %in% input$graph_type) {
      ggplot(data = frequency_data_fatal, aes(x = year, y = fatalities, color = "Fatalities")) +
        geom_line(size = 1.5) +
        labs(title = "Fatalities of Sexual Violence Events by Year and Search Criteria",
             x = "Year", y = "Count") +
        scale_color_manual(values = c(Fatalities = "salmon")) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size = 15, family = "Georgia"),
              axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      ggplot() +  # Add a default plot in case none of the checkboxes are selected
        labs(title = "No data selected",
             x = "Year", y = "Count") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size = 15, family = "Georgia"))
    }
  })
}

shinyApp(ui = ui, server = server)
