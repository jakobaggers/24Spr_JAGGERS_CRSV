library(readxl)
library(tidyverse)

#set up CRAN Mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

acled_sexual_violence <- read_csv("Data/acled_sexual_violence.csv", name_repair = "universal")


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

library(forcats)
acled_sexual_violence$Actor_Type <- as.factor(acled_sexual_violence$Actor_Type)

acled_sexual_violence %>% 
  group_by(Actor_Type) %>% 
  summarize(n = n())
# to many individual civilian actions, remove them

acled_sexual_violence <- 
  acled_sexual_violence %>% 
  filter(Actor_Type != "Civilians")

#make the Shiny
#install.packages("shiny")
library(shiny)
library(leaflet)


ui <- fluidPage(
  titlePanel("World Map of Sexual Violence in War"),
  leafletOutput("map"),
  dateRangeInput("date_select", "What date range do you want to view?"),
  selectizeInput("type", "Type of Actor", choices = NULL)
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    filtered_data <- acled_sexual_violence[as.Date(acled_sexual_violence$event_date) >= input$date_select[1] &
                                             as.Date(acled_sexual_violence$event_date) <= input$date_select[2] &
                                             acled_sexual_violence$Actor_Type == input$type, ]
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 30, zoom = 2) %>%
      addMarkers(data = filtered_data, lng = ~longitude, lat = ~latitude, 
                 popup = ~paste0("<b>Event Date:</b> ", event_date, "<br>",
                                 "<b>Actor Name:</b> ", actor1, "<br>",
                                 "<b>Actor Type:</b> ", Actor_Type, "<br>",
                                 "<b>Description:</b> ", notes, "<br>",
                                 "<b>Source:</b> ", source))
  })
  observe({
    updateSelectizeInput(session, "type", choices = unique(acled_sexual_violence$Actor_Type))
  })
}

shinyApp(ui, server)

#https://ufy5du-jakobaggers.shinyapps.io/24Spr_JAGGERS_projectName/

# Things to add 26FEB 
    # search by country
    # search for an actor (like keyword: Islamic State (North Africa) contained in ISIS)
    # add ability to search for "all" in type of actor
    # add definitions of each type of actor



