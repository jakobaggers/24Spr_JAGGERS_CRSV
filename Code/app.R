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


# Define UI
ui <- fluidPage(
  title = "World Map of Conflict Related Sexual Violence",
  titlePanel(
    HTML("<h1 style='font-family:Georgia; font-size:46px; text-align:center;'>World Map of Conflict Related Sexual Violence</h1>")
  ),
  fluidRow(
    column(3,
           dateRangeInput("date_select", "Date Range", start = NULL, end = NULL),
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
           plotOutput("frequency_plot")
    )
  ),
  fluidRow(
    column(12,
           h4("Actor Type Definitions"),
           HTML("<ul>"),
           HTML("<li><b>State Forces:</b> Collective actors, including military and police, that are recognized to perform government functions over a given territory.</li>"),
           HTML("<li><b>Rebel Groups:</b> Political organizations with the goal of countering an established national governing regime through violence.</li>"),
           HTML("<li><b>Political Militias:</b> A diverse set of violent actors that are often created for a specific purpose or during a specific time period and for the furthering of a political goal by violence.</li>"),
           HTML("<li><b>Identity Militias:</b> A broad category of identity militias for armed and violent groups organized around a collective, common feature, including: community, ethnicity, region, religion, or – in exceptional cases – livelihood.</li>"),
           HTML("<li><b>External/Other Forces:</b> International organizations, state forces active outside of their main country of operation, private security firms and their armed employees, and hired mercenaries acting independently.</li>"),
           HTML("</ul>")
    )
  ),
  fluidRow(
    column(12,
           h4("About"),
           HTML("<ul>"),
           HTML("<li>Select from the dropdown menus to sort through the data.</li>"),
           HTML("<li>The data begins in January 1997.</li>"),
           HTML("<li>To search for a keyword, type into the search menu on the right. For example, searching 'Wagner' will populate every recorded sexual violence act where the Wagner Group was an actor. Or, searching 'Twitter' will populate every recorded instance where Twitter is the source.</li>"),
           HTML("<li>Source: <a href='https://acleddata.com/'>ACLED Data</a></li>"),
           HTML("</ul>")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  observe({
    # Update select inputs based on data
    updateSelectInput(session, "country_select", choices = c("Select All", unique(acled_sexual_violence$country)))
    updateSelectInput(session, "type", choices = c("Select All", unique(acled_sexual_violence$Actor_Type)))
    
    updateSelectizeInput(session, "region_select", choices = c("Select All", c(
      "Eastern Africa", "Middle Africa", "Western Africa", "Southern Africa",
      "Northern Africa", "South Asia", "Southeast Asia", "East Asia", "Central Asia",
      "Caucasus and Central Asia", "Oceania", "South America", "Caribbean",
      "Central America", "North America", "Europe", "Middle East"
    )))
    
    # Update date range input based on checkbox
    if (input$all_dates) {
      updateDateRangeInput(session, "date_select", start = min(acled_sexual_violence$event_date), end = max(acled_sexual_violence$event_date))
    }
  })
  
  # Define filtered data based on user inputs
  observe({
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
    
    output$frequency_plot <- renderPlot({
      frequency_data <- filtered_data %>%
        mutate(year = lubridate::year(event_date)) %>%
        group_by(year) %>%
        summarise(frequency = n())
      
      ggplot(frequency_data, aes(x = factor(year), y = frequency)) +
        geom_bar(stat = "identity", fill = "skyblue", color = "black") +
        labs(title = "Frequency of Sexual Violence Events by Year and Search Criteria",
             x = "Year", y = "Frequency") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5,  size = 15, family = "Georgia"),
              axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 2)) %>%
        addTiles() %>%
        setView(lng = 0, lat = 30, zoom = 2) %>%
        addMarkers(data = filtered_data,
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
  })
}


shinyApp(ui, server)



