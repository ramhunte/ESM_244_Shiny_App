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
library(RColorBrewer)
library(shinycssloaders)
library(tmaptools)

source('data_cleaning.R')

########## UI ################
ui <- dashboardPage(

  dashboardHeader(title  = "U.S. Energy and Emissions Monitor", titleWidth = 400),
  dashboardSidebar(width = 275,
                   sidebarMenu(id = "sidebarid", 
                               menuItem("About our Shiny App", tabName = "about", icon=icon("question")),
                               menuItem("Emissions Maps", icon=icon("map"),
                                        menuSubItem("Total Emissions", tabName="totalemissions_map_plot"),
                                        menuSubItem("Emission per Capita", tabName="percapemissions_map_plot")),
                               menuItem("Emissions By Fuel and Sector", icon = icon("line-chart"),
                                        menuSubItem("Fuel and Sector", tabName="emissions_by_fuel")),
                                checkboxInput("colorblind", label="Enable colorblind assist")
                               ),
                   hr(),
                   conditionalPanel("input.sidebarid == 'totalemissions_map_plot'",
                  fluidRow(
                  column(1),
                  column(10,
                         sliderInput("rangeyears", label = "Select range of years", min = 1970, max = 2020, value = c(2015, 2020), sep="")))),
                  conditionalPanel("input.sidebarid == 'percapemissions_map_plot'",
                                   fluidRow(
                                     column(1),
                                     column(10,
                                            sliderInput("rangeyears", label = "Select range of years", min = 1970, max = 2020, value = c(2015, 2020), sep="")))),
                  conditionalPanel("input.sidebarid == 'emissions_by_fuel'",
                                   fluidRow(
                                     column(1),
                                     column(10,
                         selectInput("pick_state", label="Select state", selected = "California", choices =  unique(emissions_total_allsectors$state_name)))))
                  ),

  dashboardBody(
    tags$head(
      tags$style(HTML(".shiny-output-error-validation {color: black; font-size:200%;}"))),
    tabItems(
      tabItem(tabName = "about", 
              box(width=NULL,
                h2("About This Shiny App"),
                p(h4("This Shiny app explores energy usage across time by both sector and state throughout the United States. 
                All energy usage data was recorded from 1970 to 2020 across a variety of different sectors, including the industrial, 
                commerical, electric power, and transportation sectors.
                Emissions are presented in million metric tons of CO2 (MMT), and metric tons (MT). Specific fuels explored here are petroleum, 
                natural gas, coal, wind, wood, nuclear, and hydroelectric. Furthermore, here we explore how much 
                electricity was generated each year by these types of fuels across each state. All data was collected from the U.S.
                Energy Information Administration (EIA) and the Department of Energy (DOE)")),
                br(),
              
              p(h4("Authors: Zoe Rennie, Raymond Hunter, and Ignacio Requena are all Bren School of Environmental 
                   Science and Management students at the University of California, Santa Brarbara. This Shiny App 
                   was created out of their interest in energy for a data science course. ")),
                 br(),
              p(h4("Citations: Total energy annual data - U.S. energy information administration (EIA). 
                Total Energy Annual Data - U.S. Energy Information Administration (EIA). 
                Retrieved March 3, 2023, from https://www.eia.gov/totalenergy/data/annual/ "))
              )),
                      
### MAP: Total Emissions by State 
      tabItem(tabName = "totalemissions_map_plot",
        box(width=NULL, status="primary", solidHeader=T, title = "Total Emissions Maps", 
            withSpinner(leafletOutput("totalemissions")),
            br(),
            withSpinner(plotlyOutput("plot_totalemissions_state")))),

### MAP: Per Capita Emissions 
      tabItem(tabName = "percapemissions_map_plot",
              box(width=NULL, status="primary", solidHeader=T, title = "Per Capita Emissions Maps", 
                  withSpinner(leafletOutput("percapemissions")),
                  br(),
                  withSpinner(plotlyOutput("plot_percapemissions_state")))),

      tabItem(tabName = "emissions_by_fuel",
         box(width=NULL, status="primary", solidHeader=T, title="Emissions by Fuel",  withSpinner(plotlyOutput("plot_fuel_emissions"))),
         br(),
         box(width=NULL, status="primary", solidHeader = T, title="Emissions by Sector", withSpinner(plotlyOutput("plot_emissions_sector")))
         )
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
    states_emissions %>% 
    filter(state_name %in% input$percapemissions_shape_click$id & period %in% input$rangeyears[1]:input$rangeyears[2])
  })
  
  ggplot_emissions_sector_data <- reactive({
    emissions_persector %>% filter(state_name %in% input$pick_state)
  })

  
  ggplot_fuel_data <- reactive({
    emissions_total_allsectors %>%
      select(state_name, fuel_name, value, period, value_units, emissions_per_capita_units) %>%
     filter(state_name %in% input$pick_state)
  })
  
  date_emissions_total  <- reactive({
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

  date_emissions_capita  <- reactive({
    states_emissions %>%
      select(period, emissions_per_capita_value, state_name) %>%
      filter(period %in% input$rangeyears[1]:input$rangeyears[2]) %>%
      group_by(state_name) %>%
      slice(c(1,n())) %>%
      mutate(diff = diff(c(NA,emissions_per_capita_value))) %>%
      mutate(initial = rep(min(emissions_per_capita_value))) %>%
      mutate(pct_change = diff/initial) %>%
      drop_na()
  })

######################
  output$totalemissions <- renderLeaflet({
    pal <- colorNumeric(if(input$colorblind==T){"Blues"}else{"YlOrRd"},date_emissions_total()$pct_change)
    leaflet(date_emissions_total(), options=leafletOptions(doubleClickZoom=F)) %>%
      addTiles() %>%
      addPolygons(layerId = ~unique(state_name),
                  fillColor = ~pal(pct_change),
                  weight = 0.5,
                  fillOpacity = 0.5,
                  smoothFactor = 0.2,
                  label = ~pct_change,
                  stroke = T, color = "black") %>%
      addLegend("bottomright", pal = pal, values = date_emissions_total()$pct_change,
                title = "Carbon emissions", labFormat = labelFormat(suffix = "MMT")) %>%
      setView(lng = -96.25, lat = 39.50, zoom = 4)
  })
  
  output$percapemissions <- renderLeaflet({
    pal <- colorNumeric(if(input$colorblind==T){"Blues"}else{"YlOrRd"},date_emissions_capita()$pct_change)
        leaflet(date_emissions_capita(), options=leafletOptions(doubleClickZoom=F)) %>%
      addTiles() %>%
      addPolygons(layerId = ~unique(state_name),
                  fillColor = ~pal(pct_change),
                  weight = 0.5,
                  fillOpacity = 0.5,
                  smoothFactor = 0.2,
                  label = ~pct_change,
                  stroke = T, color = "black"
                    ) %>%
      addLegend("bottomright", pal = pal, values = date_emissions_capita()$pct_change,
                title = "Per capita carbon emissions", labFormat = labelFormat(suffix = "MT")) %>%
      setView(lng = -96.25, lat = 39.50, zoom = 4)
  })

      observe({
        if(length(input$totalemissions_shape_click$id)!=0){
         leafletProxy("totalemissions") %>%
            removeShape("highlighted_polygon") %>%
        addPolylines(stroke=TRUE, weight = 5,color="yellow",data=subset(date_emissions_total(),date_emissions_total()$state_name==input$totalemissions_shape_click$id),layerId="highlighted_polygon")
        }
      })
      
      observe({
        if(length(input$percapemissions_shape_click$id)!=0){
          leafletProxy("percapemissions") %>%
            removeShape("highlighted_polygon") %>%
            addPolylines(stroke=TRUE, weight = 5,color="yellow",data=subset(date_emissions_capita(),date_emissions_capita()$state_name==input$percapemissions_shape_click$id),layerId="highlighted_polygon")
        }
      })

  output$plot_totalemissions_state <- renderPlotly({
    shiny::validate(need(input$totalemissions_shape_click$id, "Click on state to generate plot"))
    ggplot(data=ggplot_totalstate_data(), 
           aes(period, value)) +
      geom_line(size=1) +
      geom_point(size=2)+
      theme_minimal()+
      theme(legend.key.size = unit(2, 'cm'), 
            legend.title = element_text(size=15, face="bold"), 
            legend.text = element_text(size=12),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12,face="bold"), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
  
      ylab("% Change in CO<sub>2</sub> Emissions (MMT)")+
      xlab("Year") +
      ggtitle(paste("% Change in Gross CO<sub>2</sub> Emissions Over Time For", input$totalemissions_shape_click$id)) + 
      scale_x_continuous(breaks=seq(input$rangeyears[1], input$rangeyears[2], 5)) + 
      theme(plot.title = element_text(size=22))
    ggplotly()
  })
  
  output$plot_percapemissions_state <- renderPlotly({
  shiny::validate(need(input$percapemissions_shape_click$id, "Click on state to generate plot"))
    ggplot(data=ggplot_percapstate_data(), 
           aes(period, emissions_per_capita_value)) + 
      geom_line(size=1) +
      geom_point(size=2)+
      theme_minimal()+
      theme(legend.key.size = unit(2, 'cm'), 
            legend.title = element_text(size=15, face="bold"), 
            legend.text = element_text(size=12),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12,face="bold"), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      scale_x_continuous(breaks=seq(2015,2020,1))+
      ylab("% Change in CO<sub>2</sub> Emissions Per Capita (MT)")+
      xlab("Year") + 
      ggtitle(paste("% Change in Per Capita CO<sub>2</sub> Emissions Over Time for", input$percapemissions_shape_click$id)) + 
      scale_x_continuous(breaks=seq(input$rangeyears[1], input$rangeyears[2], 5)) + 
      theme(plot.title = element_text(size=22))
    ggplotly()
  })
  
  output$plot_emissions_sector <- renderPlotly({
    safe_pal <- c("#9ECAE1", "#6BAED6","#4292C6", "#2171B5")
    names(safe_pal) <- unique(ggplot_emissions_sector_data()$sector_name)
    ggplot(data=ggplot_emissions_sector_data(),
           aes(period, value, color = as.factor(sector_name))) +
      geom_line(size=1) +
      geom_point(size=2)+
      theme_minimal()+
      theme(legend.key.size = unit(2, 'cm'), 
            legend.title = element_text(size=14, face="bold"), 
            legend.text = element_text(size=12),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"), 
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           axis.line = element_line(colour = "black")) +
      ggtitle(paste("Emissions By Sector for", input$pick_state)) + 
      labs(color = "Sector")+
      ylab("CO<sub>2</sub> emissions (MMT)") +
      xlab("Year") + 
      scale_color_manual(values=if(input$colorblind==T){safe_pal}else{as.factor(unique(ggplot_emissions_sector_data()$sector_name))})
    ggplotly() %>% layout(hoverlabel=list(bgcolor="white"))
  })

  
  output$plot_fuel_emissions <- renderPlotly({
    safe_pal <- c("#9ECAE1", "#6BAED6","#4292C6", "#2171B5")
    names(safe_pal) <- unique(ggplot_fuel_data()$fuel_name)
   ggplot(data=ggplot_fuel_data(), 
          aes(period, value, color = as.factor(fuel_name))) +
      geom_line(size=1) +
      geom_point(size=2)+
      theme_minimal()+
     ggtitle(paste("Emissions By Fuel Type for", input$pick_state)) +
      theme(legend.key.size = unit(2, 'cm'), 
            legend.title = element_text(size=14, face="bold"), 
            legend.text = element_text(size=12),
            #legend.background = element_rect(linetype = 2, size = 0.5, colour = 1),
            legend.box.background = element_rect(colour = "black"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
           axis.line = element_line(colour = "black")) +
      labs(color = "Fuel Type")+
      ylab("CO<sub>2</sub> emissions (MMT)") +
      xlab("Year") + 
        scale_color_manual(values=if(input$colorblind==T){safe_pal}else{as.factor(unique(ggplot_fuel_data()$fuel_name))})
     ggplotly() %>% layout(hoverlabel=list(bgcolor="white"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)