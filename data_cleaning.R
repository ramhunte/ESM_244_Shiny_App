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