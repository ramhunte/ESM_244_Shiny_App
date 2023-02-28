library(here)
library(httr)
library(jsonlite)
library(janitor)
library(tidyverse)

### generate data
# path <- fromJSON("https://api.eia.gov/v2/co2-emissions/co2-emissions-aggregates/data/?frequency=annual&data[0]=value&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000&api_key=mdOP5PhYCWMt6IrK7c2qcDCslb7MrPpyNahheLlF")
# api_rows <- seq(from=0, to = 65000, by= 5000)
# 
# emissions_data <- NULL
# for(i in api_rows){
#   path <- fromJSON(paste0("https://api.eia.gov/v2/co2-emissions/co2-emissions-aggregates/data/?frequency=annual&data[0]=value&sort[0][column]=period&sort[0][direction]=desc&offset=",
#                           as.character(i),
#                           "&length=5000&api_key=mdOP5PhYCWMt6IrK7c2qcDCslb7MrPpyNahheLlF"))
#   emissions_data <- rbind(emissions_data, path$response$data)
# }
# 
# write.csv(x=emissions_data , file="/Users/zoerennie/co2_emissions_aggregates.csv")

co2_emissions <- read_csv(here("data", "co2_emissions_aggregates.csv"))
energy_res_com_ind <- read_csv(here("data", "MER_T02_01A.csv"))
energy_transport_elec <- read_csv(here("data", "MER_T02_01B.csv"))
power_generation_states <- read_csv(here("data", "annual_generation_states.csv"))

#View(co2_emissions)
#total CO2 from all sectors for map, year and state


emissions_total <- co2_emissions %>% 
                   clean_names() %>% 
                    filter(sector_name %in% "Total carbon dioxide emissions from all sectors") %>%
                    group_by(state_name, fuel_name, period) %>%
                    summarize(value)
emissions_all_fuels <- emissions_total %>% filter(fuel_name %in% "All Fuels")

library(shiny)
library(shinydashboard)
library(sf)
library(dplyr)
library(janitor)
library(here)
library(leaflet)
library(tigris)

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title  = "Energy Usage and Greenhouse Gas Emissions", titleWidth=450),
  dashboardSidebar(width = 500,
                   sidebarMenu(id = "sidebarid",
                               style = "position:fixed; width:auto; overflow-x: clip;",
                               menuItem("Dashboard", tabName="dashboard"),
                               selectInput("years", label="Select year", choices = 1970:2020, selected = 2020),
                               # selectizeInput("states", "State Selection:",
                               #                choices = state.name, multiple=T),
                               menuItem("Energy Usage", tabName="energy",
                                        sliderInput("number", "Number:",
                                                    min = 0, max = 1000,
                                                    value = 500),
                                        sliderInput("integer", "Integer:",
                                                    min = 0, max = 1,
                                                    value = 0.5, step = 0.1),
                                        
                                        sliderInput("range", "Range:",
                                                    min = 1, max = 1000,
                                                    value = c(200,500))),
                               menuItem("Greenhouse Gas Emissions", tabname="emissions",
                                        menuSubItem("Sub-item 1", tabName="subitem1"),
                                        menuSubItem("Sub-item 2", tabName = "subitem2"))
                   )),
  dashboardBody(
    fluidRow(box(width = 12,
                 leafletOutput("USMAP")
    ))
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  st <- states() %>% st_transform('+proj=longlat +datum=WGS84')
  US <-  st %>%
    clean_names() %>%
    mutate(state_name=name)
  # states_3857 <- st_transform(states, 3857)
  states_emissions <- inner_join(US, emissions_all_fuels, by="state_name")
  #read_sf(here("cb_2021_us_all_500k", "cb_2021_us_state_500k", "cb_2021_us_state_500k.shp"))
  
  
  output$USMAP <- renderLeaflet({
    date_emissions <-  states_emissions %>% subset(period == input$years)
    pal <- colorNumeric("YlOrRd", date_emissions$value)
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = date_emissions,
                  color = ~pal(value),
                  weight = 0.5,
                  fillOpacity = 0.5,
                  smoothFactor = 0.2,
                  label = ~value) %>%
      
      setView(lng = -96.25, lat = 39.50, zoom = 4)
  })
  
  # observeEvent(input$years, {
  
  # leafletProxy() %>%
  #   clearShapes() %>%
  #   clearControls() %>%
  #   
  #   addLegend(
  #     position = "bottomright",
  #     pal=pal,
  #     values = date_emissions()$value,
  #     title = "Carbon Emissions (mmt)"
  #   )
  # })
  
}

# Run the application 
shinyApp(ui = ui, server = server)





