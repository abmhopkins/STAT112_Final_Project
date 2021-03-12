library(shiny)
library(tidyverse)     # for data cleaning and plotting
library(ggthemes)      # for more themes (including theme_map())
library(plotly)        # for the ggplotly() - basic interactivity
library(shiny)         # for creating interactive apps
library(shinythemes)
library(scales)
library(rvest)
theme_set(theme_minimal())

# Eviction Data by State
evictions_state <- read_csv("evictions_state.csv")

# Scrape the County Centroids
url <- "https://en.wikipedia.org/wiki/User:Michael_J/County_table"
webpage <- read_html(url)
table <- html_nodes(webpage, "table")[[1]] %>% 
  html_table()
county_centroids <- table %>% 
  select(State:`County [2]`, Latitude:Longitude)

# Eviction Data by County
evictions_county <- read_csv("evictions_county.csv") %>% 
  separate(col = name, sep = " County", into = c("name", "drop")) %>% 
  select(-drop) %>% 
  mutate(name = str_replace_all(name, "St. ", "St "),
         name = str_replace_all(name, " Parish", "")) %>% 
  filter(`parent-location` != "Alaska",      
         `parent-location` != "Hawaii") %>% 
  left_join(county_centroids,
            by = c("GEOID" = "FIPS")) %>% 
  separate(Latitude, into = c("latitude", "drop1"), 
           sep = "°") %>% 
  separate(Longitude, into = c("longitude", "drop2"), 
           sep = "°") %>% 
  separate(longitude, into = c("drop3", "longitude"), 
           sep = "–") %>% 
  select(-drop1, -drop2, -drop3, -State, -"County [2]") %>% 
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude)*-1)
evictions_county$name[evictions_county$name == "LaMoure"] <- "La Moure"
evictions_county$name[evictions_county$name == "DuPage"] <- "Du Page"
evictions_county$name[evictions_county$name == "DeKalb"] <- "De Kalb"
evictions_county$name[evictions_county$name == "LaSalle"] <- "La Salle"
evictions_county$name[evictions_county$name == "DeWitt"] <- "De Witt"
evictions_county$name[evictions_county$name == "O'Brien"] <- "OBrien"
evictions_county$name[evictions_county$name == "Ste. Genevieve"] <- "Ste Genevieve"

# The Centroid of Each State
states_midpoint <- read_csv("state-midpoints.csv")

# Map Data
states_map <- map_data("state")
county_map <- map_data("county")

ui <- fluidPage(
    navbarPage("Eviction rates of US", theme = shinytheme("lumen"),
            # Country Tab User Interface
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
             # State Tab User Interface
             tabPanel("By state", fluid = TRUE,
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "stateName",
                                      label = "State:",
                                      choices = unique(evictions_county$`parent-location`),
                                      selected = "Alabama", #this is not selected
                                      multiple = FALSE),
                          sliderInput(inputId = "yearCounty",
                                      label = "Year:",
                                      min = 2000,
                                      max = 2016,
                                      value = 2000,
                                      sep = ""),
                          selectInput(inputId = "countyColName",
                                      label = "Chloropleth:",
                                      choices = list("Population" = "population", 
                                                     "Poverty Rate" = "`poverty-rate`",
                                                     "Median Rent" = "`median-gross-rent`",
                                                     "Eviction Filings" = "`eviction-filings`",
                                                     "% African American" = "`pct-af-am`",
                                                     "% Hispanic" = "`pct-hispanic`",
                                                     "% White" = "`pct-white`",
                                                     "% American Indian" = "`pct-am-ind`",
                                                     "% Pacific Islander" = "`pct-nh-pi`"),
                                      multiple = FALSE),
                          checkboxInput(inputId = "county_dots",
                                        label = "Click for stacked eviction rates data",
                                        value = TRUE),
                          selectInput(inputId = "countyDotCol",
                                      label = "Dots:",
                                      choices = list("Eviction Rate" = "`eviction-rate`",
                                                     "Eviction Filing Rate" = "`eviction-filing-rate`"),
                                     multiple = FALSE),
                          submitButton(text = "Create my plot!")
                        ),
                        mainPanel(
                          plotOutput(outputId = "countyplot")
                        )
                      )
                      
             )
    )
)

server <- function(input, output) {
  output$nameplot <- renderPlotly({
    
    # Plot for Country Wide-Data
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
                 color = "tomato")} + 
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
  
  output$countyplot <- renderPlot({
    
    # Cleaning the county map data
    map <- county_map %>% filter(region == str_to_lower(input$stateName)) %>% 
      select(-region) %>% 
      dplyr::rename(region = subregion)
    
    # Map for county-wide data
    evictions_county %>% 
      filter(year == input$yearCounty,
             `parent-location` == input$stateName) %>% 
      select (-c("low-flag", "imputed", "subbed")) %>% 
      mutate(lwr_name = str_to_lower(name)) %>% 
      ggplot(aes_string(fill = input$countyColName)) +
      geom_map(map = map,
               aes(map_id = lwr_name))  +
      {if(input$county_dots)geom_point(aes_string(x = "longitude", y = "latitude", size = input$countyDotCol),
                                color = "tomato")} +
      scale_fill_viridis_c(labels = comma) +
      expand_limits(x = map$long, y = map$lat) + 
      theme_map() +
      theme(legend.background = element_blank(),
            legend.position = "right")  +
      labs(title = "",
           fill = "",
           size = "",
           caption = "Some county dots are not aligned")

  })
}

#runs app
shinyApp(ui = ui, server = server)