library(shiny)
library(tidyverse)     # for data cleaning and plotting
library(ggthemes)      # for more themes (including theme_map())
library(plotly)        # for the ggplotly() - basic interactivity
library(shiny)         # for creating interactive apps
library(shinythemes)
library(scales)
theme_set(theme_minimal())

evictions_state <- read_csv("evictions_state.csv")
evictions_county <- read_csv("evictions_county.csv") %>% 
  separate(col = name, sep = " ", into = c("name", "drop")) %>% 
  select(-drop)
states_midpoint <- read_csv("state-midpoints.csv")

states_map <- map_data("state")
county_map <- map_data("county")

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
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "stateName",
                                      label = "State:",
                                      choices = evictions_state$name,
                                      selected = "Alabama", #this is not selected
                                      multiple = FALSE),
                          sliderInput(inputId = "year",
                                      label = "Year:",
                                      min = 2000,
                                      max = 2016,
                                      value = 2000,
                                      sep = ""),
                          selectInput(inputId = "countyColName",
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
                          # checkboxInput(inputId = "dots",
                          #               label = "Click for stacked eviction rates data",
                          #               value = TRUE),
                          # selectInput(inputId = "dotCol",
                          #             label = "Dots:",
                          #             choices = list("Eviction Rate" = "eviction-rate",
                          #                            "Eviction Filing Rate" = "eviction-filing-rate"),
                          #            multiple = FALSE),
                          submitButton(text = "Create my plot!")
                        ),
                        mainPanel(
                          plotlyOutput(outputId = "countyplot")
                        )
                      )
                      
             )
    )
)

server <- function(input, output) {
  output$nameplot <- renderPlotly({
    country_plot <- evictions_state %>% 
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
  
  ggplotly(country_plot, tooltip = "text")
  })
  
  output$countyplot <- renderPlotly({
    
    select_county_map <- county_map %>% 
      filter(region == str_to_lower(input$stateName))
    
    county_plot <- evictions_county %>% 
      filter(year == input$year,
             `parent-location` == input$stateName) %>% 
      mutate(lwr_name = str_to_lower(name)) %>% 
      ggplot() +
      geom_map(map = select_county_map,
               aes(map_id = lwr_name,
                   fill = get(input$countyColName), # fill also doesn't work
                   group = lwr_name,
                   text = paste0(name, paste0(": ", format(get(input$countyColName) , big.mark=","))))) +
      expand_limits(x = select_county_map$long, y = select_county_map$lat) + # These limits don't seem to be taken as an arguement
      theme_map() +
      labs(title = "",
           fill = "",
           size = "") +
      scale_fill_viridis_c(labels = comma) +
      theme(legend.background = element_blank(),
            legend.position = "right")
    
    ggplotly(county_plot, tooltip = "text")
  })
}

#runs app
shinyApp(ui = ui, server = server)