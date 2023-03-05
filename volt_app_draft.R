library(shiny)
library(shinydashboard)
library(sf)
library(dplyr)
library(janitor)
library(here)
library(leaflet)
library(ggplot2)
library(plotly)
library(htmltools)

# Define UI for application
ui <- dashboardPage(
  
  dashboardHeader(title  = "Energy Usage and Greenhouse Gas Emissions", titleWidth=450),
  dashboardSidebar(width = 500,
                   sidebarMenu(id = "sidebarid",
                               #style = "position:fixed; width:auto; overflow-x: clip; white-space: normal;",
                               menuItem("Dashboard", tabName="dashboard"),
                               menuItem("About", tabName = "about", icon = icon("question")),
                               selectInput("years", label="Select year", choices = 1970:2020, selected = 2020),
                               menuItem("Emissions By State Over Time",
                                        menuSubItem("Total Emissions", tabName="totalemissions_map_plot"),
                                        menuSubItem("Emission per Capita", tabName="percapemissions_map_plot")),
                               menuItem("Fuel Emissions", tab_name="emissions_by_fuel"))),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about", 
              #tags$iframe(src="about_pg.html", width = '100%',  height = 1000,  style = "border:none;")), 
              p("This ShinyApp explores energy usage by both sector and state throughout the United States. 
                All energy usage data was recorded from 1970 to 2020 across a variety of different sectors.
                Emissions are presented in million metric tons of CO2. Specific fuels explored here are petroleum, 
                natural gas, coal, wind, wood, nuclear, and hydroelectric. Furthermore, here we explore how much 
                electricity was generated each year by these types of fuels across each state."), 
              p("Citations"),
              p("All data was collected from... ")),
      
      tabItem(tabName = "totalemissions_map_plot",
              box(width=NULL, status="primary", solidHeader=T, title = "Emissions Maps", leafletOutput("totalemissions"),
                  br(),
                  plotOutput("plot_emissions_state")),
              # tabPanel("Emissions Per Capita for All Fuels", "content")
              tabItem(tabName = "emissions_by_fuel"),
      )
      
      
      # tabItem(
      #   title = "Total Emissions",
      #   tabPanel("Total Emissions for All Fuels", plotOutput("plot_emissions_state")),
      #   tabPanel("Emissions Per Capita for All Fuels", "content")
      # ),
      # tabItem(
      #   title = "Emissions by Sector",
      #   tabPanel("Total Emissions by Sector", plotOutput("plot_emissions_sector")),
      #   tabPanel("Emissions Per Capita for All Fuels", "content")
      # ))
      
    ))
)



#server call
server <- function(input, output) {
  st <- read_sf(here( "cb_2021_us_state_500k", "cb_2021_us_state_500k.shp")) %>%
    st_transform('+proj=longlat +datum=WGS84')
  # st <- states() %>% st_transform('+proj=longlat +datum=WGS84')
  US <-  st %>%
    clean_names() %>%
    mutate(state_name=name)
  states_emissions <- inner_join(US, emissions_all_fuels, by="state_name")

  
  output$totalemissions <- renderLeaflet({
    date_emissions <-  states_emissions %>% subset(period == input$years)
    pal <- colorNumeric("YlOrRd", date_emissions$value)
    leaflet(date_emissions, options=leafletOptions(doubleClickZoom=F)) %>%
      addTiles() %>%
      addPolygons(layerId = ~unique(geoid),
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
  
  ggplot_state_data <- reactive({
    states_emissions %>% filter(geoid %in% input$totalemissions_shape_click$id)
  })
  
  ggplot_sector_data <- reactive({
    emissions_sector %>% filter(geoid %in% input$totalemissions_shape_click$id)
  })
  
  output$plot_emissions_state <- renderPlot({
    ggplot(data=ggplot_state_data(), aes(period, value)) + geom_line() + theme_minimal()
    # emissions_allfuels_plot <- 
    # emissions_allfuels_plot %>% ggplotly()
  })
  
  output$plot_emissions_sector <- renderPlot({
    ggplot(data=ggplot_sector_data(), aes(period, value), color=sector) + geom_line() + theme_minimal()
    # emissions_sector_plot <- 
    # emissions_sector_plot %>% ggplotly()
  })
}


# Run the application 
shinyApp(ui = ui, server = server)