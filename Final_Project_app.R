library(shiny)
library(tidyverse)     # for data cleaning and plotting
library(lubridate)     # for date manipulation
library(openintro)     # for the abbr2state() function
library(maps)          # for map data
library(ggmap)         # for mapping points on maps
library(gplots)        # for col2hex() function
library(RColorBrewer)  # for color palettes
library(sf)            # for working with spatial data
library(leaflet)       # for highly customizable mapping
library(ggthemes)      # for more themes (including theme_map())
library(plotly)        # for the ggplotly() - basic interactivity
library(gganimate)     # for adding animation layers to ggplots
library(gifski)        # for creating the gif (don't need to load this library every time,but need it installed)
library(transformr)    # for "tweening" (gganimate)
library(shiny)         # for creating interactive apps
library(patchwork)     # for nicely combining ggplot2 graphs  
library(gt)            # for creating nice tables
library(rvest)         # for scraping data
library(robotstxt)     # for checking if you can scrape data
library(readxl)
library(scales)
library(babynames)
library(shinythemes)
theme_set(theme_minimal())

evictions_state <- read_csv("evictions_state.csv")
states_midpoint <- read_csv("state-midpoints.csv")

states_map <- map_data("state")

ui <- fluidPage(
  navbarPage("Eviction rates of US", theme = shinytheme("lumen"),
             tabPanel("Country wide by year", fluid = TRUE,
                      textInput(inputId = "year",
                          label = "Year:",
                          value = "",
                          placeholder = "ex:2005"),
                          plotlyOutput(outputId = "nameplot"),
                          submitButton(text = "Create my plot!")),
             tabPanel("By state", fluid = TRUE,
                      selectInput(inputId = "name",
                                  label = "State:",
                                  choices = "Alabama",
                                  multiple = FALSE)))
)

server <- function(input, output) {
  output$nameplot <- renderPlotly({
    evictions_state %>% 
      filter(year == input$year) %>% 
      mutate(name = str_to_lower(name)) %>% 
      ggplot() +
      geom_map(map = states_map,
               aes(map_id = name,
                   fill = `eviction-rate`)) +
      expand_limits(x = states_map$long, y = states_map$lat) + 
      theme_map() +
      labs(title = "",
           fill = "") +
      scale_fill_viridis_c() +
      theme(legend.background = element_blank())

  })
}

#runs app
shinyApp(ui = ui, server = server)