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
    ),
    tags$style(HTML("
    .slider-width-custom {
      width: 100%;  /* or any other width */
    }
  "))
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
    tabPanel("Case Study: Ukraine",
             sliderInput("date_slider_ukraine", "Date Range",
                         min = as.Date("2018-01-01"), 
                         max = Sys.Date(), 
                         value = as.Date("2018-01-01"), step = 7,
                         animate = animationOptions(interval = 50, loop = FALSE)),
             leafletOutput("ukraine_map")
    ),
    tabPanel("Case Study: Myanmar",
             sliderInput("date_slider_myanmar", "Date Range",
                         min = as.Date("2010-01-01"), 
                         max = Sys.Date(), 
                         value = as.Date("2010-01-01"), step = 7,
                         animate = animationOptions(interval = 50, loop = FALSE)),
             leafletOutput("myanmar_map")
    ),
    tabPanel("SVAC Information",
             fluidRow(
               column(12,
                      h1("The Sexual Violence in Armed Conflict Dataset", style="text-align: center; font-family: Georgia, serif; font-size: 30px;") # Big title
               )
             ),
             fluidRow(
               column(12,
                      p("The Armed Conflict Location & Event Data Project (ACLED) is very good at providing disaggregated violence data and individual locations at which conflicts have occurred, but it doesn't tell us much else.
                        To look deeper into trends with more reliable sexual violence data, we now examine the", a(href='http://www.sexualviolencedata.org/dataset/', 'Sexual Violence in Armed Conflict (SVAC) Dataset.', target="_blank"), 
                        ". This will give us a better perspective on larger trends within the topic of conflict related sexual violence."
                        )
                      )
               ),
             fluidRow(
               column(6,
                      img(src="ACLED Frequency by Actor Type.png", style="width:100%;") 
               ),
               column(6, style = "margin-top: 40px;",
                      p("Part of the reason that trends cannot be derived from the ACLED dataset is that its time periods between countries vary. ACLED began collecting data on some countries as early as 1997, and some countries as late as 2021. As such, the frequencies (see the graph on the left) rise at a level that is more a factor of when the data was collected rather than a rise in number of events. See this ", 
                        a(href='https://acleddata.com/acleddatanew/wp-content/uploads/dlm_uploads/2019/01/ACLED_Country-and-Time-Period-coverage_updatedFeb2022.pdf', 'link', target="_blank"), " for more information."
                        )
               )
             ),
             fluidRow(
               column(6, style = "margin-top: 100px;",
                      p("The SVAC datset, however, extends from 1989-2021. As we can see in the figure on the right, SVAC has a relatively even distribution throughout its time frame. Thus, trends pulled from this dataset will likely be more accurate than those pulled from the ACLED.
                        Also, rather than deaggregated data where each point represents one instance of conflict related sexual violence, the SVAC dataset's points each represent one year within a certain conflict.")
               ),
               column(6,
                      img(src="SVAC by overall_prev (filter prevalent).png", style="width:100%;") 
               )
             ),
             fluidRow(
               column(12, style = "margin-top: 10px;",
                      tags$p(HTML("The main way that the SVAC dataset is organized is through levels of prevalence of sexual violence. According to the SVAC Codebook, the following are definitions of each level of prevalence of sexual violence:")),
                      tags$ul(
                        tags$li(HTML("<b> Prevalence = 0 (None):</b> Report issued, but no mention of rape or other sexual violence related to the conflict.")),
                        tags$li(HTML("<b> Prevalence = 1 (Isolated):</b> Sexual violence is likely related to the conflict, but did not meet the requirements for a 2 or 3 coding, and: there were “reports,” “isolated reports,” or “there continued to be reports” of occurrences of sexual violence.
                                     <b> Note: </b> Absent these or similar terms, a count of less than 25 reports of sexual violence indicates a prevalence code of 1.")),
                        tags$li(HTML("<b> Prevalence = 2 (Numerous):</b> Sexual violence is likely related to the conflict, but did not meet the requirements for a 3 coding, and: Sexual violence was described as “widespread,” “common,” “commonplace,” “extensive,” “frequent,” “often,” “persistent,” “recurring,” a “pattern,” a “common pattern,” or a “spree”. AND Sexual violence occurred “commonly,” “frequently,” “in large numbers,” “periodically,” “regularly,” “routinely,” “widely,” or on a “number of occasions;” there were “many” or “numerous instances”
                                     <b> Note: </b> Absent these or similar terms, a count of 25-999 reports of sexual violence indicates a prevalence code of 2.")),
                        tags$li(HTML("<b> Prevalence = 3 (Massive):</b> Sexual violence is likely related to the conflict, and: Sexual violence was described as “systematic” or “massive” or “innumerable” AND Actor used sexual violence as a “means of intimidation,” “instrument of control and punishment,” “weapon,” “tactic to terrorize the population,” “terror tactic,” “tool of war,” on a “massive scale” Note: Absent these or similar terms, a count of 1000 or more reports of sexual violence indicates a prevalence code of 3.
                                     <b> Note: </b> Absent these or similar terms, a count of 1000 or more reports of sexual violence indicates a prevalence code of 3. "))
                      ),
                      tags$p(HTML("We see on below that conflicts coded as 3 are present most often in the Middle East and Africa, when compared proportionally to the conflicts of other prevalence in that specific region.")),
               )
             ), 
             fluidRow(
               column(12, style = "text-align: center;",
                    img(src="Region bar by prevalence.png", style="width: 50%; height: auto; display: block; margin: 0 auto;") 
               )),
             fluidRow(
               column(6, style = "margin-top: 100px;",
                      p("Within conflicts that have a prevalence of at least 1, we can also examine region and type of actor, as seen in the figure on the right.")
               ),
               column(6, style = "margin-top: 10px;",
                      img(src="Region bar by actor.png", style="width:100%;") 
               )
             ),
             fluidRow(
               column(6, style = "margin-top: 10px;",
                      img(src="SVAC by overall_prev including not present.png", style="width:100%;") 
               ),
               column(6, style = "margin-top: 100px;",
                      HTML("<b> The Issue With Zeros: </b> At the moment, the quality of sexual violence data is poor, mainly due to a large inflation of reported “zeroes” in the data. This made it impossible to accomplish to original goal of this project: creating a model that would predict if a new armed conflict would eventually contain sexual violence based on the presence of risk factors involved. Unfortunately, there is a large inflation of zeroes in the CRSV data, largely due to stigmas around reporting incidents of sexual violence in war. To view (from what I can tell) the only academic resource on the topic, see Changwook Ju’s study: “Determinants of Conflict-Related Sexual Violence: A Meta-Reanalysis Distinguishing Two Classes of Zero Observations.” Further study on the topic is necessary as
well as using such study to improve the collection of data on sexual violence in armed conflict worldwide.")
               )
             )
    ),
    tabPanel("Data Journalism Example",
             fluidRow(
               column(12, style = "margin-top: 10px;",
                      h1("An Example of This Dashboard Being Used for Data Journalism: Ukraine", 
                         style="text-align: center; font-family: Georgia, serif; font-size: 30px;"), # Big title
                      tags$iframe(style="height:600px; width:100%", src="CRSV Journalism Example.pdf")
               ) 
             )
    )
))

server <- function(input, output, session) {
  # Reactive expressions for the main Instances tab
  instances_data <- reactive({
    data <- acled_sexual_violence
    if (!input$all_dates) {
      data <- data %>% filter(event_date >= input$date_select[1] & event_date <= input$date_select[2])
    }
    if (!is.null(input$country_select) && input$country_select != "Select All") {
      data <- data %>% filter(country %in% input$country_select)
    }
    if (!is.null(input$region_select) && input$region_select != "Select All") {
      data <- data %>% filter(region %in% input$region_select)
    }
    if (!is.null(input$type) && input$type != "Select All") {
      data <- data %>% filter(Actor_Type %in% input$type)
    }
    if (!is.null(input$search_input) && input$search_input != "") {
      search_term <- input$search_input
      data <- data %>%
        filter(str_detect(notes, search_term) |
                 str_detect(actor1, search_term) |
                 str_detect(source, search_term) |
                 str_detect(assoc_actor_1, search_term))
    }
    return(data)
  })
  
  # Reactive expressions for the Heatmap
  heatmap_data <- reactive({
    data <- acled_sexual_violence
    if (!input$all_dates_heatmap) {
      data <- data %>% filter(event_date >= input$date_select_heatmap[1] & event_date <= input$date_select_heatmap[2])
    }
    if (!is.null(input$country_select_heatmap) && input$country_select_heatmap != "Select All") {
      data <- data %>% filter(country %in% input$country_select_heatmap)
    }
    if (!is.null(input$region_select_heatmap) && input$region_select_heatmap != "Select All") {
      data <- data %>% filter(region %in% input$region_select_heatmap)
    }
    if (!is.null(input$type_heatmap) && input$type_heatmap != "Select All") {
      data <- data %>% filter(Actor_Type %in% input$type_heatmap)
    }
    return(data)
  })
  
  # Reactive expressions for Ukraine case study
  ukraine_data <- reactive({
    data <- acled_sexual_violence %>% filter(country == "Ukraine")
    if (!is.null(input$date_slider_ukraine)) {
      data <- data %>% filter(event_date >= input$date_slider_ukraine[1] & event_date <= input$date_slider_ukraine[2])
    }
    return(data)
  })
  
  # Reactive expressions for Myanmar case study
  myanmar_data <- reactive({
    data <- acled_sexual_violence %>% filter(country == "Myanmar")
    if (!is.null(input$date_slider_myanmar)) {
      data <- data %>% filter(event_date >= input$date_slider_myanmar[1] & event_date <= input$date_slider_myanmar[2])
    }
    return(data)
  })
  
  # Observers for updating input selections
  observe({
    choices <- c("Select All", unique(acled_sexual_violence$country))
    updateSelectInput(session, "country_select", choices = choices)
    updateSelectInput(session, "country_select_heatmap", choices = choices)
    
    type_choices <- c("Select All", unique(acled_sexual_violence$Actor_Type))
    updateSelectInput(session, "type", choices = type_choices)
    updateSelectInput(session, "type_heatmap", choices = type_choices)
    
    region_choices <- c("Select All", 
                        "Eastern Africa", "Middle Africa", "Western Africa", "Southern Africa",
                        "Northern Africa", "South Asia", "Southeast Asia", "East Asia", "Central Asia",
                        "Caucasus and Central Asia", "Oceania", "South America", "Caribbean",
                        "Central America", "North America", "Europe", "Middle East")
    updateSelectizeInput(session, "region_select", choices = region_choices)
    updateSelectizeInput(session, "region_select_heatmap", choices = region_choices)
  })
  
  # Maps and plots renderings based on the filtered data for each tab
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      addTiles() %>%
      setView(lng = 0, lat = 30, zoom = 2) %>%
      addMarkers(data = instances_data(),
                 lng = ~longitude, lat = ~latitude,
                 popup = ~paste0("<b>Event Date:</b> ", event_date, "<br>",
                                 "<b>Country:</b> ", country, "<br>",
                                 "<b>Actor Name:</b> ", actor1, "<br>",
                                 "<b>Secondary Actor:</b> ", assoc_actor_1, "<br>",
                                 "<b>Actor Type:</b> ", Actor_Type, "<br>",
                                 "<b>Description:</b> ", notes, "<br>",
                                 "<b>Source:</b> ", source))
  })
  
  output$heatmap <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addHeatmap(data = heatmap_data(), lng = ~longitude, lat = ~latitude)
  })
  
  render_leaflet_map <- function(data, output_id) {
    output[[output_id]] <- renderLeaflet({
      validate(
        need(data, "Waiting for data...")
      )
      leaflet(data) %>%
        addTiles() %>%
        addMarkers(~longitude, ~latitude, popup = ~paste("<b>Event Date:</b>", event_date))
    })
  }

  # Reactive expressions to filter data based on the slider input
  ukraine_data <- reactive({
    acled_sexual_violence %>%
      filter(country == "Ukraine", event_date <= input$date_slider_ukraine)
  })

  myanmar_data <- reactive({
    acled_sexual_violence %>%
      filter(country == "Myanmar", event_date <= input$date_slider_myanmar)
  })

  # Render maps
  render_leaflet_map(ukraine_data(), "ukraine_map")
  render_leaflet_map(myanmar_data(), "myanmar_map")

  
  current_date <- reactiveVal()
  
  render_leaflet_map <- function(data_func, output_id, lng, lat) {
    output[[output_id]] <- renderLeaflet({
      validate(need(data_func(), "Waiting for data..."))
      data <- data_func()
      leaflet(data) %>%
        addTiles() %>%
        setView(lng = lng, lat = lat, zoom = 5) %>%
        addMarkers(~longitude, ~latitude, popup = ~paste0("<b>Event Date:</b> ", event_date, "<br>",
                                                          "<b>Country:</b> ", country, "<br>",
                                                          "<b>Actor Name:</b> ", actor1, "<br>",
                                                          "<b>Secondary Actor:</b> ", assoc_actor_1, "<br>",
                                                          "<b>Actor Type:</b> ", Actor_Type, "<br>",
                                                          "<b>Description:</b> ", notes, "<br>",
                                                          "<b>Source:</b> ", source))
    })
  }
  
  # Setup animations for Ukraine and Myanmar using the above function
  render_leaflet_map(ukraine_data, "ukraine_map", 31, 49)
  render_leaflet_map(myanmar_data, "myanmar_map", 95, 21)
  
  
  # Render frequency plot based on filtered data for the main instances
  output$frequency_plot <- renderPlot({
    frequency_data <- instances_data() %>%
      mutate(year = lubridate::year(event_date)) %>%
      group_by(year) %>%
      summarise(frequency = n())
    
    frequency_data_fatal <- instances_data() %>%
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