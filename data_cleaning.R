library(here)
library(httr)
library(jsonlite)
library(janitor)
library(tidyverse)
library(dplyr)

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

#total CO2 from all sectors by year, fuel, state
emissions_total <- co2_emissions %>% 
                   clean_names() %>% 
                    filter(sector_name %in% "Total carbon dioxide emissions from all sectors") %>%
                    group_by(state_name, fuel_name, period) %>%
                    summarize(value)
#CO2 emissions by state, sector, and year
emissions_sector <- co2_emissions %>% 
  clean_names() %>% 
  filter(sector_name != "Total carbon dioxide emissions from all sectors") %>%
  group_by(state_name, sector_name, period) %>%
  summarize(value)
#total CO2 emissions from all fuels for every state per year
emissions_all_fuels <- emissions_total %>% filter(fuel_name %in% "All Fuels")
#look into changing the data structure to avoid having more than one shape data thing
#View(emissions_total)

co2_emissions_clean <- co2_emissions %>% clean_names()

