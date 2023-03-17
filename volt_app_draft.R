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
library(scales)
library(gridExtra)

source('data_cleaning.R')

########## UI ################
ui <- dashboardPage(

  dashboardHeader(title  = "U.S. Energy and Emissions Monitor", titleWidth = 400),
  dashboardSidebar(width = 275,
                   sidebarMenu(id = "sidebarid", 
                               menuItem("About our Shiny App", tabName = "about", icon=icon("question")),
                               menuItem("Emissions Maps", icon=icon("map"),
                                        menuSubItem("Total Emissions", tabName="totalemissions_map_plot", icon = icon("flag-usa")),
                                        menuSubItem("Emission per Capita", tabName="percapemissions_map_plot", icon = icon("person"))),
                               menuItem("Sector and Fuel Emissions Plots",
                                        menuSubItem("State Scale", tabName="emissions_by_fuel", icon = icon("line-chart")),
                                        menuSubItem("National Scale", tabName= "emissions_persector_fuel", icon = icon("chart-column"))),
                               menuItem("Energy Use by Sector", icon = icon("line-chart"), tabName="energy_use_by_sector"),
                               menuItem("Energy Generation by State", icon=icon("line-chart"), tabName="elec_generation"),
                               checkboxInput("colorblind", label="Enable colorblind assist"),
                               
                               
                               #data download
                              
                               selectInput("dataset", "Choose a dataset:",
                                           choices = "emissions data"),
                               downloadButton("downloadData", "Download")),
                              
                   
                 #  mainPanel(tableOutput("table")),
                   
                   
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
                                            sliderInput("rangeyears2", label = "Select range of years", min = 1970, max = 2020, value = c(2015, 2020), sep="")))),
                  conditionalPanel("input.sidebarid == 'emissions_by_fuel'",
                                   fluidRow(
                                     column(1),
                                     column(10,
                         selectInput("pick_state", label="Select state", selected = "California", choices =  unique(emissions_total_allsectors$state_name))))),
                 # Widget 3
                   conditionalPanel("input.sidebarid == 'energy_use_by_sector'",
                   fluidRow(
                     column(1),
                     column(10,
                            checkboxGroupInput(
                              inputId = "pick_sector3",
                              label = "Pick sector:",
                              selected = "Commercial",
                              choices = c("Commercial", "Industrial", "Transportation", "All sectors"))))),
                  conditionalPanel("input.sidebarid == 'emissions_persector_fuel'",
                                   fluidRow(
                                     column(1),
                                     column(10,
                                          checkboxGroupInput(inputId="whichPlot",
                                                             label = "Select sector",
                                                             choices = c("Industrial"= "industrial",
                                                                         "Electric power" = "electric_power",
                                                                         "Commercial" = "commercial",
                                                                         "Transportation" = "transportation"),
                                                             selected="industrial"
                                                                         )))),
                 conditionalPanel("input.sidebarid == 'elec_generation'",
                                  fluidRow(
                                    column(1),
                                    column(10,
                                    selectInput("pick_state2", label="Pick state", selected = "CA", choices =  unique(clean_power_generation_states$state)))))
                 ),
              
  dashboardBody(
    tags$head(
      tags$style(HTML(".shiny-output-error-validation {color: black; font-size:200%;}"))),
    tabItems(
      tabItem(tabName = "about", 
              box(width=NULL,
                h2(strong("About This Shiny App")),
                p(h4("This Shiny app explores energy usage across time by both sector and state throughout the United States. 
                All energy usage data was recorded from 1970 to 2020 across a variety of different sectors, including the industrial, 
                commerical, electric power, and transportation sectors.
                Emissions are presented in million metric tons of CO2 (MMT), and metric tons (MT). Specific fuels explored here are petroleum, 
                natural gas, coal, wind, wood, nuclear, and hydroelectric. Furthermore, here we explore how much 
                electricity was generated each year by these types of fuels across each state. All data was collected from the U.S.
                Energy Information Administration (EIA) and the Department of Energy (DOE)")),
                br(),
                h2(strong("User Information")),
                p(h4("The features of this application are interactive, meaning the user can select options, click on, and scroll over elements to view information. 
                     The maps section includes interactive choropleth maps that display percent emissions in gross and per capita measurments through 1970-2020 for 
                     every state in the U.S. The plots show different comparisons for states and sectors through time regarding carbon emissions and energy usage. Insights
                     from the visualization are written below the generated plots, and serve as a justification for the relavence of the application.")),
                br(),
              p(h4("Authors: Zoe Rennie, Raymond Hunter, and Ignacio Requena are all Bren School of Environmental 
                   Science and Management students at the University of California, Santa Brarbara. This Shiny App 
                   was created out of their interest in energy for a data science course. ")),
                 br(),
              p(h4("Citations: Total energy annual data - U.S. energy information administration (EIA). 
                Total Energy Annual Data - U.S. Energy Information Administration (EIA). 
                Retrieved March 3, 2023, from https://www.eia.gov/totalenergy/data/annual/ ")),
              br(),
              p(h4("This application was built in R version 4.2.1")))),
                      
### MAP: Total Emissions by State 
      tabItem(tabName = "totalemissions_map_plot",
        box(width=NULL, status="primary", solidHeader=T, title = "Total Emissions Maps", 
            withSpinner(leafletOutput("totalemissions")),
            br(),
            withSpinner(plotlyOutput("plot_totalemissions_state"))),
        br(),
        box(width=NULL, status="primary", solidHeader=T, title = "Quick Facts",
            p(h4("State emissions can be largely dependent on a number of factors: firstly, some states rely heavily on fossil 
                       fuel industries, such as coal, for income generation. A prime example of this is Wyoming, which produces more coal than any other state. 
                       Emissions also can be dependent on policy, with more stringent air quality and emissions policies leading to lower greenhouse gas emissions, which can 
                       be observed in the percent change in emissions.")))),

### MAP: Per Capita Emissions 
      tabItem(tabName = "percapemissions_map_plot",
              box(width=NULL, status="primary", solidHeader=T, title = "Per Capita Emissions Maps",
                  withSpinner(leafletOutput("percapemissions")),
                  br(),
                  withSpinner(plotlyOutput("plot_percapemissions_state"))),
                  br(),
              box(width=NULL, status="primary", solidHeader=T, title = "Quick Facts",
                  p(h4("State emissions can be largely dependent on a number of factors: firstly, some states rely heavily on fossil 
                       fuel industries, such as coal, for income generation. A prime example of this is Wyoming, which produces more coal than any other state. 
                       Emissions also can be dependent on policy, with more stringent air quality and emissions policies leading to lower greenhouse gas emissions, which can 
                       be observed in the percent change in emissions.")))),

      tabItem(tabName = "emissions_by_fuel",
         box(width=NULL, status="primary", solidHeader=T, title="Emissions by Fuel",  withSpinner(plotlyOutput("plot_fuel_emissions"))),
         br(),
         box(width=NULL, status="primary", solidHeader = T, title="Emissions by Sector", withSpinner(plotlyOutput("plot_emissions_sector"))),
         br(),
         box(width=NULL, status="primary", solidHeader = T, title="Quick Facts", 
             p(h4("Particular aspects of the transportation sector are notoriously difficult to decarbonize, as more than a third of emissions comes from heavy transport 
                  from trucks and freight aircraft, as well as heat-intensive material manufacturing. Electric power generation as well is a large contributor to emissions, 
                  however this is dependent on the grid mix used in the state, meaning the fuel composition used in power plants to produce electricity.")))),
      tabItem(tabName = "energy_use_by_sector",
        box(width=NULL, status="primary", solidHeader=T, title="Energy Use by Sector", withSpinner(plotlyOutput("plot_sector_energy"))),
        br(),
        box(width=NULL, status="primary", solidHeader=T, title="Proportion Electrical System Losses to Total Energy Use", withSpinner(plotlyOutput("plot_losses")))),
      #widget 4
      tabItem(tabName = "emissions_persector_fuel",
        box(width=NULL, status="primary", solidHeader=T, title="Emissions by Sector and Fuel Type", withSpinner(uiOutput("plot_emissions_persector_fuel"))),
        box(width=NULL, status="primary", solidHeader = T, title="Quick Facts", 
            p(h4("This tab explores the emissions of different fuel types by each sector from 1970-2020. Some of the notable trends are that coal emissions or electrical power 
                 peaked at about 2008 and have slowly decreased as natural gas emissions increased. Natural gas emissions are the most abundant for the commercial sector, and 
                 petroleum emissions dominate the transportation sector. ")))),
      tabItem(tabName="elec_generation",
              box(width=NULL, status="primary", solidHeader=T, title="Electricity Generation by State", withSpinner(plotlyOutput("plot_generation"))),
              box(width=NULL, status="primary", solidHeader = T, title="Quick Facts", 
                  p(h4("Text")))
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
#########REACTIVE OBJECTS############
  ggplot_totalstate_data <- reactive({
    states_emissions %>%
      filter(state_name %in% input$totalemissions_shape_click$id & period %in% input$rangeyears[1]:input$rangeyears[2])
  })
  
  
  ggplot_percapstate_data <- reactive({
    states_emissions %>% 
    filter(state_name %in% input$percapemissions_shape_click$id & period %in% input$rangeyears2[1]:input$rangeyears2[2])
  })
  
  ggplot_emissions_sector_data <- reactive({
    emissions_persector %>% filter(state_name %in% input$pick_state)
  })

  
  ggplot_fuel_data <- reactive({
    emissions_total_allsectors %>%
      select(state_name, fuel_name, value, period, value_units, emissions_per_capita_units) %>%
     filter(state_name %in% input$pick_state)
  })
  
  ## Widget 3
  ggplot_sector_data2 <- reactive({
    filtered_sector2 %>%
      subset(description %in% input$pick_sector3)
  })
  
  ggplot_sector_losses <- reactive({
    sector_total_losses %>%
      subset(sector%in% input$pick_sector3)
  })
  
  date_emissions_total  <- reactive({
    states_emissions %>%
    select(period, value, state_name) %>%
      filter(period %in% input$rangeyears[1]:input$rangeyears[2]) %>%
      group_by(state_name) %>%
      slice(c(1,n())) %>%
   mutate(diff = diff(c(NA,value))) %>%
   mutate(avg = mean(value)) %>%
   mutate(pct_change = (diff/avg)*100) %>%
      drop_na()
  })

  date_emissions_capita  <- reactive({
    states_emissions %>%
      select(period, emissions_per_capita_value, state_name) %>%
      filter(period %in% input$rangeyears[1]:input$rangeyears[2]) %>%
      group_by(state_name) %>%
      slice(c(1,n())) %>%
      mutate(diff = diff(c(NA,emissions_per_capita_value))) %>%
      mutate(avg = mean(emissions_per_capita_value)) %>%
      mutate(pct_change = (diff/avg)*100) %>%
      drop_na()
  })

  ggplot_emissions_persector_fuel <- reactive({
    emissions_persector_fuel %>%
      filter(sector_name %in% input$whichPlot)
  })
  
  ggplot_electricity_generation <- reactive({
    clean_power_generation_states  %>%
      clean_names() %>%
      filter(state %in% input$pick_state2)

  })

#########OUTPUTS#############
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
                title = "% change in carbon emissions", labFormat = labelFormat(suffix = "MMT")) %>%
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
                title = "% change in per capita <br> carbon emissions</br>", labFormat = labelFormat(suffix = "MT")) %>%
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
  
      ylab("Change in CO<sub>2</sub> Emissions (MMT)")+
      xlab("Year") +
      ggtitle(paste("Gross Change CO<sub>2</sub> Emissions Over Time For", input$totalemissions_shape_click$id)) + 
      scale_x_continuous(breaks=seq(input$rangeyears[1], input$rangeyears[2], 5)) + 
      scale_color_manual(values=if(input$colorblind==T){safe_pal}else{c("#1f77b4","black",  "#d62728", "#2ca02c")})
      theme(plot.title = element_text(size=22))
    ggplotly()
  })
  
  output$plot_percapemissions_state <- renderPlotly({
  shiny::validate(need(input$percapemissions_shape_click$id, "Click on state to generate plot"))
    ggplot(data=ggplot_percapstate_data(), 
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
      ylab("Change in CO<sub>2</sub> Emissions Per Capita (MT)")+
      xlab("Year") + 
      ggtitle(paste("Gross Change CO<sub>2</sub> Emissions Over Time For", input$percapemissions_shape_click$id)) + 
      scale_x_continuous(breaks=seq(input$rangeyears2[1], input$rangeyears2[2], 5)) + 
      theme(plot.title = element_text(size=22))
    ggplotly()
  })
  
  output$plot_emissions_sector <- renderPlotly({
    safe_pal <- c("#9ECAE1", "#6BAED6","#4292C6", "#2171B5")
    names(safe_pal) <- unique(ggplot_emissions_sector_data()$sector_name)
    ggplot(data=ggplot_emissions_sector_data(),
           aes(period, value, color = sector_name)) +
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
      scale_color_manual(values=if(input$colorblind==T){safe_pal}else{c("#1f77b4","black",  "#d62728", "#2ca02c")})
    ggplotly() %>% layout(hoverlabel=list(bgcolor="white"))
  })

  
  output$plot_fuel_emissions <- renderPlotly({
    safe_pal <- c("#9ECAE1", "#6BAED6","#4292C6", "#2171B5")
    names(safe_pal) <- unique(ggplot_fuel_data()$fuel_name)
   ggplot(data=ggplot_fuel_data(), 
          aes(period, value, color = fuel_name)) +
      geom_line(size=1) +
      geom_point(size=2)+
      theme_minimal()+
     ggtitle(paste("Emissions By Fuel Type for", input$pick_state)) +
      theme(legend.key.size = unit(2, 'cm'), 
            legend.title = element_text(size=14, face="bold"), 
            legend.text = element_text(size=12),
            legend.box.background = element_rect(colour = "black"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           axis.line = element_line(colour = "black")) +
      labs(color = "Fuel Type") +
      ylab("CO<sub>2</sub> emissions (MMT)") +
      xlab("Year") + 
     scale_color_manual(values=if(input$colorblind==T){safe_pal}else{c("#1f77b4","black",  "#d62728", "#2ca02c")})
     ggplotly() %>% layout(hoverlabel=list(bgcolor="white"))
  })
  
  output$plot_sector_energy <- renderPlotly({
    safe_pal <- c("#9ECAE1", "#6BAED6","#4292C6", "#2171B5")
    names(safe_pal) <- unique(ggplot_sector_data2()$description)
    ggplot(data=ggplot_sector_data2(),
           aes(year, value, color = description))+
      theme_minimal()+
      geom_point(size=2) + 
      geom_line(size=1)+
      theme(legend.key.size = unit(2, 'cm'), 
            legend.title = element_text(size=14, face="bold"), 
            legend.text = element_text(size=12),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"))+
      labs(color = "Sector")+
      ylab("Energy use (trillion BTU)") +
      xlab("Year")+
      expand_limits(y = 0) +
      scale_color_manual(values=if(input$colorblind==T){safe_pal}else{c("#1f77b4","black",  "#d62728", "#2ca02c")})
    ggplotly() %>% layout(hoverlabel=list(bgcolor="white"))
    
  })
  

  output$plot_losses <- renderPlotly({
    safe_pal <- c("#9ECAE1", "#6BAED6","#4292C6", "#2171B5")
    names(safe_pal) <- unique(ggplot_sector_losses()$sector)
    ggplot(ggplot_sector_losses(), 
    aes(year, proportion_losses, color=sector)) +
    geom_point(size=2) + 
    geom_line(size=1) +
      theme_minimal() + 
      theme(legend.key.size = unit(2, 'cm'),
            legend.title = element_text(size=14, face="bold"), 
            legend.text = element_text(size=12),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      labs(color = "Sector")+
      ylab("Proportion losses") +
      xlab("Year")+
    scale_color_manual(values=if(input$colorblind==T){safe_pal}else{c("#1f77b4","black",  "#d62728", "#2ca02c")})
    ggplotly() %>% layout(hoverlabel=list(bgcolor="white"))
  })
  
output$electric_power <- renderPlot({    
  safe_pal <- c("#9ECAE1", "#6BAED6","#4292C6", "#2171B5")
  names(safe_pal) <- unique(emissions_persector_fuel$fuel_name)
    if("electric_power" %in% input$whichPlot){
      emissions_persector_fuel %>%
        filter(sector_name %in% "electric_power") %>%
        ggplot(aes(period, total, fill=fuel_name)) +
        geom_col()+
        scale_y_continuous(labels = function(x) paste0(x/1000, " k"))+
        labs(x="Year", y="CO2 Emissions (MMT)", fill= "Fuel Type")+
        geom_bar(position="stack", stat="identity")+
        theme_minimal()+
        ggtitle("Emissions By Fuel Type for Electric Power") +
        scale_fill_manual(values=safe_pal)    }
  })
  
 output$commercial <- renderPlot({ 
   safe_pal <- c("#9ECAE1", "#6BAED6","#4292C6", "#2171B5")
   names(safe_pal) <- unique(emissions_persector_fuel$fuel_name)
    if("commercial" %in% input$whichPlot){
      emissions_persector_fuel %>%
        filter(sector_name %in% "commercial") %>%
        ggplot(aes(period, total, fill=fuel_name)) +
        geom_col()+
        scale_y_continuous(labels = function(x) paste0(x/1000, " k"))+
        labs(x="Year", y="CO2 Emissions (MMT)", fill= "Fuel Type")+
        geom_bar(position="stack", stat="identity")+
        theme_minimal()+
        ggtitle("Emissions By Fuel Type for Commercial") +
        scale_fill_manual(values=safe_pal)    }
  })
 
 output$industrial <- renderPlot({ 
   safe_pal <- c("#9ECAE1", "#6BAED6","#4292C6", "#2171B5")
   names(safe_pal) <- unique(emissions_persector_fuel$fuel_name)
   if("industrial" %in% input$whichPlot){
     emissions_persector_fuel %>%
       filter(sector_name %in% "industrial") %>%
       ggplot(aes(period, total, fill=fuel_name)) +
       geom_col()+
       scale_y_continuous(labels = function(x) paste0(x/1000, " k"))+
       labs(x="Year", y="CO2 Emissions (MMT)", fill= "Fuel Type")+
       geom_bar(position="stack", stat="identity")+
       theme_minimal()+
       ggtitle("Emissions By Fuel Type for Industrial") +
       scale_fill_manual(values=safe_pal)   }
 })
 
 output$transportation <- renderPlot({ 
   safe_pal <- c("#9ECAE1", "#6BAED6","#4292C6", "#2171B5")
   names(safe_pal) <- unique(emissions_persector_fuel$fuel_name)
   if("transportation" %in% input$whichPlot){
     emissions_persector_fuel %>%
       filter(sector_name %in% "transportation") %>%
       ggplot(aes(period, total, fill=fuel_name)) +
       geom_col()+
       scale_y_continuous(labels = function(x) paste0(x/1000, " k"))+
       labs(x="Year", y="CO2 Emissions (MMT)", fill= "Fuel Type")+
       geom_bar(position="stack", stat="identity")+
       theme_minimal()+
       ggtitle("Emissions By Fuel Type for Transportation") +
       scale_fill_manual(values=safe_pal)
   }
 })
  
  output$plot_emissions_persector_fuel <- renderUI({
    plot_output_list <- lapply(input$whichPlot, 
                               function(plotname) {
                                 column(width=5, plotOutput(plotname)) ##wrap the plotOutput in column to render side-by-side
                               })
    do.call(tagList, plot_output_list)
  })
  
  output$plot_generation <- renderPlotly({
    safe_pal <- c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B", "#2B15FC", "#0CA0FF", "#0465A3", "#75C9FF", "#055589")
    names(safe_pal) <- unique(clean_power_generation_states$energy_source)
    ggplot(ggplot_electricity_generation(),
           aes(year, generation_megawatthours, fill=energy_source)) +
      geom_col()+
      labs(x="Year", y="Generation (mWh)", fill= "Energy Source")+
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
      scale_fill_manual(values=if(input$colorblind==T){safe_pal}else{unique(clean_power_generation_states$energy_source)}) 
    ggplotly() %>% layout(hoverlabel=list(bgcolor="white"))

  })

  
  ####3####data downlad
  
  datasetInput <- reactive({
    switch(input$dataset,
           "emissions data" = emissions_complete_data)
  })

  output$table <- renderTable({
    datasetInput()
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )

 ########## 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)