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
   fluidRow(
     tabBox(
       title = "Emissions Maps",
      tabPanel("Total Emissions", leafletOutput("total_emissions")),
      tabPanel("Emissions Per Capita", "content")
      ))
   )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  #read_sf(here("cb_2021_us_all_500k", "cb_2021_us_state_500k", "cb_2021_us_state_500k.shp"))
  st <- states() %>% st_transform('+proj=longlat +datum=WGS84')
  US <-  st %>%
    clean_names() %>%
    mutate(state_name=name)
 # states_3857 <- st_transform(states, 3857)
  states_emissions <- inner_join(US, emissions_all_fuels, by="state_name")
  
  
  output$total_emissions <- renderLeaflet({
    date_emissions <-  states_emissions %>% subset(period == input$years)
    pal <- colorNumeric("YlOrRd", date_emissions$value)
   leaflet() %>%
      addTiles() %>%
     
      addPolygons(data = date_emissions,
                  fillColor = ~pal(value),
                  weight = 0.5,
                  fillOpacity = 0.5,
                  smoothFactor = 0.2,
                  label = ~value,
                  stroke = T, color = "black") %>%
     addLegend("bottomright", pal = pal, values = date_emissions$value,
               title = "Carbon emissions", labFormat = labelFormat(suffix = "mmt")) %>%
      setView(lng = -96.25, lat = 39.50, zoom = 4)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

