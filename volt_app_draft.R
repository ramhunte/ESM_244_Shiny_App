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
library(tictoc)
library(magrittr)
library(tidyverse)

# source('data_cleaning.R')

########## UI ################
ui <- dashboardPage(
  

  dashboardHeader(title  = "Energy Usage and Greenhouse Gas Emissions", titleWidth=450),
  dashboardSidebar(width = 275,
                   sidebarMenu(id = "sidebarid", 
                               #style = "position:fixed; width:auto; overflow-x: clip; white-space: normal;",
                               menuItem("About our ShinyApp", tabName = "about"),
                               menuItem("Emissions By State Over Time",
                                        menuSubItem(
                                           selectInput("years", label="Select year", choices = 1970:2020, selected = 2020)
                                           ),
                                        menuSubItem(
                                          sliderInput("rangeyears", label = "Select range of years", min = 1970, max = 2020, value = c(2018, 2020), sep="")
                                          ),
                                        p("Click on state to generate graph of emissions over time"),
                                        menuSubItem("Total Emissions", tabName="totalemissions_map_plot"),
                                        menuSubItem("Emission per Capita", tabName="percapemissions_map_plot")),

                               menuItem("Emissions By Fuel Type Over Time", 
                                        menuSubItem(
                                          selectInput("pick_state", label="Select state", selected = "California", choices =  unique(emissions_total_allsectors$state_name))),
                                        menuSubItem("Fuel Type", tabName="emissions_by_fuel")
                                        )
                               )
                   ),
                                  
  dashboardBody(
    tabItems(
      tabItem(tabName = "about", 
              #tags$iframe(src="about_pg.html", width = '100%',  height = 1000,  style = "border:none;")), 
                p("This ShinyApp explores energy usage by both sector and state throughout the United States. 
                All energy usage data was recorded from 1970 to 2020 across a variety of different sectors.
                Emissions are presented in million metric tons of CO2 (MMT CO2). Specific fuels explored here are petroleum, 
                natural gas, coal, wind, wood, nuclear, and hydroelectric. Furthermore, here we explore how much 
                electricity was generated each year by these types of fuels across each state."), 
              p("All data was collected from the U.S. Energy Information Administration (EIA) and the Department of Energy (DOE)"),
           
              p("Citations: Total energy annual data - U.S. energy information administration (EIA). 
                Total Energy Annual Data - U.S. Energy Information Administration (EIA). 
                Retrieved March 3, 2023, from https://www.eia.gov/totalenergy/data/annual/ ")),
                      
### MAP: Total Emissions by State 
      tabItem(tabName = "totalemissions_map_plot",
        box(width=NULL, status="primary", solidHeader=T, title = "Total Emissions Maps", 
            leafletOutput("totalemissions"),
            br(),
        plotOutput("plot_totalemissions_state"))),

### MAP: Per Capita Emissions 
      tabItem(tabName = "percapemissions_map_plot",
              box(width=NULL, status="primary", solidHeader=T, title = "Per Capita Emissions Maps", leafletOutput("percapemissions"),
                  br(),
                  plotOutput("plot_percapemissions_state"))),

      tabItem(tabName = "emissions_by_fuel",
         box(width=NULL, status="primary", solidHeader=T, title="Emissions by Fuel", plotOutput("plot_fuel_emissions")))
    ))
)



########## SERVER ################
server <- function(input, output) {
st <- read_sf(here( "cb_2021_us_state_500k", "cb_2021_us_state_500k.shp")) %>% 
  st_transform('+proj=longlat +datum=WGS84')
  US <-  st %>%
    clean_names() %>%
    mutate(state_name=name)
  states_emissions <- inner_join(US, emissions_all_fuels, by="state_name")
#####################
  ggplot_totalstate_data <- reactive({
    states_emissions %>%
    filter(state_name %in% input$totalemissions_shape_click$id & period %in% input$rangeyears[1]:input$rangeyears[2])
  })
  
  
  ggplot_percapstate_data <- reactive({
    states_emissions %>% filter(state_name %in% input$percapemissions_shape_click$id)
  })
  
  ggplot_sector_data <- reactive({
    emissions_sector %>% filter(state_name %in% input$totalemissions_shape_click$id)
  })
  
  ggplot_fuel_data <- reactive({
    emissions_total_allsectors %>% 
     subset(state_name %in% input$pick_state)
  })
  
  # date_emissions <- reactive({
  #   states_emissions %>%
  #     filter(period %in% input$rangeyears[1]:input$rangeyears[2]) %>%
  #     group_by(state_name)
  #   })
  date_emissions  <- reactive({
    states_emissions %>%
    select(period, value, state_name) %>%
      filter(period %in% input$rangeyears[1]:input$rangeyears[2]) %>%
      group_by(state_name) %>%
      slice(c(1,n())) %>%
   mutate(diff = diff(c(NA,value))) %>%
   mutate(initial = rep(min(value))) %>%
   mutate(pct_change = diff/initial) %>%
      drop_na()
    
  })
  # 
  # filter_states <- states_emissions %>%
  #   select(period, value, state_name) %>%
  #   filter(period %in% 2000:2002)
  # dt_states <- data.table(filter_states)
  # table_emissions <- dt_states[, .(avg_change=mean(c(NA, diff(value)), na.rm = TRUE)), by=state_name]
  # date_emissions <- data.frame(table_emissions)
  # merge(filter_states, date_emissions, by="state_name") %>%
  # distinct(avg_change, .keep_all = T)
  
  
  # date_emissions <- reactive({
  #   dt_states <- data.table(filter_states())
  #   table_emissions <- dt_states[, .(avg_change=mean(c(NA, diff(value)), na.rm = TRUE)), by=state_name]
  #   date_emissions <- data.frame(table_emissions)
  #   merge(filter_states(), date_emissions, by="state_name") %>%
  #     distinct(avg_change, .keep_all = T)
  # })
  
######################

  output$totalemissions <- renderLeaflet({
    pal <- colorNumeric("YlOrRd", date_emissions()$pct_change)
    leaflet(date_emissions(), options=leafletOptions(doubleClickZoom=F)) %>%
      addTiles() %>%
      addPolygons(layerId = ~unique(state_name),
                  fillColor = ~pal(pct_change),
                  weight = 0.5,
                  fillOpacity = 0.5,
                  smoothFactor = 0.2,
                  label = ~pct_change,
                  stroke = T, color = "black") %>%
      addLegend("bottomright", pal = pal, values = date_emissions()$pct_change,
                title = "Carbon emissions", labFormat = labelFormat(suffix = "mmt")) %>%
      setView(lng = -96.25, lat = 39.50, zoom = 4)
  })
  
  output$percapemissions <- renderLeaflet({
    pal <- colorNumeric("YlOrRd", date_emissions()$emissions_per_capita_value)
    leaflet(date_emissions(), options=leafletOptions(doubleClickZoom=F)) %>%
      addTiles() %>%
      addPolygons(layerId = ~unique(state_name),
                  fillColor = ~pal(emissions_per_capita_value),
                  weight = 0.5,
                  fillOpacity = 0.5,
                  smoothFactor = 0.2,
                  label = ~emissions_per_capita_value,
                  stroke = T, color = "black") %>%
      addLegend("bottomright", pal = pal, values = date_emissions()$emissions_per_capita_value,
                title = "Carbon emissions", labFormat = labelFormat(suffix = "mt")) %>%
      setView(lng = -96.25, lat = 39.50, zoom = 4)
  })
  

  output$plot_totalemissions_state <- renderPlot({
    ggplot(data=ggplot_totalstate_data(), 
           aes(period, value)) +
      geom_line() +
      geom_point(size=3) +
      theme_minimal()+
      ylab(expression(paste("% change ", CO[2], " emissions (mmt)")))+
      xlab("Year") +
      ggtitle(paste("Total CO2 Emissions for", input$totalemissions_shape_click$id)) + 
      scale_x_continuous(breaks=seq(input$rangeyears[1], input$rangeyears[2], 5)) + 
      theme(plot.title = element_text(size=22))
    # emissions_allfuels_plot %>% ggplotly()
  })
  output$plot_percapemissions_state <- renderPlot({
    ggplot(data=ggplot_percapstate_data(), 
           aes(period, emissions_per_capita_value)) + 
      geom_line() + 
      theme_minimal()+
      ylab("CO2 emissions per capita (mt)")+
      xlab("Year")
    # emissions_allfuels_plot <- 
    # emissions_allfuels_plot %>% ggplotly()
  })
  
  output$plot_emissions_sector <- renderPlot({
    ggplot(data=ggplot_sector_data(),
           aes(period, value), color=sector) +
      geom_line() +
      theme_minimal()+
      ylab("CO2 Emissions per capita")
    # emissions_sector_plot <- 
    # emissions_sector_plot %>% ggplotly()
  })
  
  output$plot_fuel_emissions <- renderPlot({
    ggplot(data=ggplot_fuel_data(), 
           aes(x=period, y=value, color = fuel_name)) + 
      geom_line(size=2.5) + 
      theme_minimal()+
      theme(legend.key.size = unit(2, 'cm'), 
            legend.title = element_text(size=15, face="bold"), 
            legend.text = element_text(size=12),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold")) +
      labs(color = "Fuel Type")+
      ylab("CO2 Emissions (mmt)") +
      xlab("Year")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)