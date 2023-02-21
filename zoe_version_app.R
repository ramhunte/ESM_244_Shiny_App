library(tidyverse)
library(httr)
library(jsonlite)
library(shiny)
library(shinydashboard)

path <- "https://api.eia.gov/v2/co2-emissions/co2-emissions-aggregates/data/?frequency=annual&data[0]=value&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000&api_key=mdOP5PhYCWMt6IrK7c2qcDCslb7MrPpyNahheLlF"
raw_data <- GET(path)
emissions_data <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
df_emissions <- emissions_data$response$data
df_emissions %>% filter(stateId=="CA", fuelId == "CO")


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title  = "Energy Usage and Greenhouse Gas Emissions", titleWidth=450),
    dashboardSidebar(width = 500,
                     sidebarMenu(id = "sidebarid",
                      style = "position:fixed; width:auto; overflow-x: clip;",
                      menuItem("Dashboard", tabName="dashboard"),
                      menuItem("Energy Usage", tabName="energy",
                      # Input: Simple integer interval ----
                      sliderInput("integer", "Integer:",
                                  min = 0, max = 1000,
                                  value = 500),
                      
                      # Input: Decimal interval with step value ----
                      sliderInput("decimal", "Decimal:",
                                  min = 0, max = 1,
                                  value = 0.5, step = 0.1),
                      
                      # Input: Specification of range within an interval ----
                      sliderInput("range", "Range:",
                                  min = 1, max = 1000,
                                  value = c(200,500))),
                      menuItem("Greenhouse Gas Emissions", tabname="emissions",
                      menuSubItem("Sub-item 1", tabName="subitem1"),
                      menuSubItem("Sub-item 2", tabName = "subitem2"))
                      )),
                      dashboardBody())

   
# Define server logic required to draw a histogram
server <- function(input, output) {


}

# Run the application 
shinyApp(ui = ui, server = server)

