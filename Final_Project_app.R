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
                      sidebarLayout(
                        sidebarPanel(
                              sliderInput(inputId = "year",
                                  label = "Year:",
                                  min = 2000,
                                  max = 2016,
                                  value = 2000,
                                  sep = ""),
                              selectInput(inputId = "colName",
                                          label = "Chloropleth:",
                                          choices = list("Population" = "population", 
                                                         "Poverty Rate" = "poverty-rate",
                                                         "Median Rent" = "median-gross-rent",
                                                         "Eviction Filings" = "eviction-filings",
                                                         "% African American" = "pct-af-am",
                                                         "% Hispanic" = "pct-hispanic",
                                                         "% White" = "pct-white",
                                                         "% American Indian" = "pct-am-ind",
                                                         "% Pacific Islander" = "pct-nh-pi"),
                                          multiple = FALSE),
                              checkboxInput(inputId = "dots",
                                            label = "Click for stacked eviction rates data",
                                            value = TRUE),
                              selectInput(inputId = "dotCol",
                                          label = "Dots:",
                                          choices = list("Eviction Rate" = "eviction-rate",
                                                         "Eviction Filing Rate" = "eviction-filing-rate"),
                                          multiple = FALSE),
                              submitButton(text = "Create my plot!")
                        ),
                        mainPanel(
                          plotlyOutput(outputId = "nameplot")
                        )
                      )
                    ),
             tabPanel("By state", fluid = TRUE,
                      selectInput(inputId = "name",
                                  label = "State:",
                                  choices = "Alabama",
                                  multiple = FALSE)
             )
    )
)

server <- function(input, output) {
  output$nameplot <- renderPlotly({
    plot <- evictions_state %>% 
      left_join(states_midpoint,
                by = c("name" = "location")) %>% 
      filter(year == input$year,
             name != "Alaska",      
             name != "Hawaii") %>% 
      mutate(lwr_name = str_to_lower(name)) %>% 
      ggplot() +
      geom_map(map = states_map,
               aes(map_id = lwr_name,
                   fill = get(input$colName),
                   text = paste0(name, paste0(":<br>", format(get(input$colName) , big.mark=","))))) +
      {if(input$dots)geom_point(aes(x = lon, y = lat, size = get(input$dotCol),
                                    text = paste0(input$dotCol, paste0(": ", get(input$dotCol)))),
                 color = "red")} + 
      expand_limits(x = states_map$long, y = states_map$lat) + 
      theme_map() +
      labs(title = "",
           fill = "",
           size = input$dotCol) +
      scale_fill_viridis_c(labels = comma) +
      theme(legend.background = element_blank(),
            legend.position = "right")
  
  ggplotly(plot, tooltip = "text")
  })
}

#runs app
shinyApp(ui = ui, server = server)