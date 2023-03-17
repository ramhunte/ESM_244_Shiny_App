library(here)
library(httr)
library(jsonlite)
library(janitor)
library(tidyverse)
library(dplyr)
library(lubridate)
library(gridExtra)

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

###read in data
#emissions data - includes sector, fuel type, state, and emissions in metric tons CO2
co2_emissions <- read_csv(here("data", "co2_emissions_aggregates.csv"))
#total energy consumed by residential, commercial, and industrial sectors
energy_res_com_ind <- read_csv(here("data", "MER_T02_01A.csv"))
#total energy consumed by transportation and electric power sectors
energy_transport_elec <- read_csv(here("data", "MER_T02_01B.csv"))
#total electric generation by state by year - includes energy source
power_generation_states <- read_csv(here("data", "annual_generation_states.csv"))
#census population data from 1970-2021
population_US <-  read_csv(here("data", "population_data.csv"))

#adding per capita calculations to emissions data
emissions_complete_data <- inner_join(co2_emissions, population_US) %>%
                            mutate(emissions_per_capita_value=value/population*10^6) %>%
                            mutate(emissions_per_capita_units='metric tons of CO2') %>%
                            clean_names()

#total CO2 from all sectors by year, fuel type, and state
emissions_total_allsectors <- emissions_complete_data %>%
                    filter(sector_name %in% "Total carbon dioxide emissions from all sectors") %>%
                    mutate(fuel_name = as.factor(fuel_name)) %>%
                    group_by(state_name, fuel_name, period, value_units, emissions_per_capita_units) %>%
                    summarise(across(c(value, emissions_per_capita_value)))
#cumulative CO2 emissions from all fuels for every state per year
emissions_all_fuels <- emissions_total_allsectors %>% filter(fuel_name %in% "All Fuels")
avg <- emissions_all_fuels %>% group_by(period) %>% summarise(national_avg = mean(value))
emissions_all_fuels <- inner_join(emissions_all_fuels, avg)

#CO2 emissions by state, sector, and year
emissions_persector <- emissions_complete_data %>%
  filter(sector_name %in% c("Industrial carbon dioxide emissions", "Electric Power carbon dioxide emissions", "Commercial carbon dioxide emissions", "Transportation carbon dioxide emissions")) %>%
  group_by(state_name, sector_name, period, value_units) %>%
 summarise(value=sum(value))

emissions_persector[emissions_persector== 'Transportation carbon dioxide emissions'] <- 'Transportation'
emissions_persector[emissions_persector == 'Industrial carbon dioxide emissions'] <- 'Industrial'
emissions_persector[emissions_persector== 'Commercial carbon dioxide emissions'] <- 'Commercial'
emissions_persector[emissions_persector == 'Electric Power carbon dioxide emissions'] <- 'Electric Power'

emissions_persector <- emissions_persector %>% mutate(sector_name = as.factor(sector_name))

#Widget 3: Total energy consumed by sector
#combine all sectors
sector_energy_use <- rbind(energy_transport_elec, energy_res_com_ind)
#filter out yearly use and filter by total energy by sector (there are other options for this)
filtered_sector <- sector_energy_use %>%
  mutate(YYYYMM = as.character(YYYYMM)) %>%
  filter(endsWith(YYYYMM, "13") & YYYYMM >= 197013 & Description %in% c("Total Energy Consumed by the End-Use-Sectors", 
                                                                        "Total Electrical Energy System Losses Proportioned to the End-Use Sectors", 
                                                                        "Total Energy Consumed by the Transportation Sector", 
                                                                        "Electrical Energy System Losses Proportioned to the Transportation Sector", 
                                                                        "Total Energy Consumed by the Industrial Sector", 
                                                                        "Industrial Sector Electrical System Energy Losses", 
                                                                        "Total Energy Consumed by the Commercial Sector",
                                                                        "Commercial Sector Electrical System Energy Losses"))

#make new column in data for sector name
sector_data <- filtered_sector
sector_data$sector <- ifelse(sector_data$Description %in% c("Total Energy Consumed by the End-Use-Sectors", "Total Electrical Energy System Losses Proportioned to the End-Use Sectors"), "All sectors", 
                                 ifelse(sector_data$Description %in% c("Total Energy Consumed by the Transportation Sector", "Electrical Energy System Losses Proportioned to the Transportation Sector"), "Transportation",
                                        ifelse(sector_data$Description %in% c("Total Energy Consumed by the Industrial Sector","Industrial Sector Electrical System Energy Losses"), "Industrial", 
                                               ifelse(sector_data$Description %in% c("Total Energy Consumed by the Commercial Sector", "Commercial Sector Electrical System Energy Losses"), "Commercial", NA))))
#get losses and total energy to calculate proportion of losses to total energy used
sector_total_losses <- sector_data %>% filter(grepl("Losses", Description)) %>% 
  mutate(losses = Value) %>% select(-c(Description, Value, Column_Order, MSN)) %>%
  mutate(sector=as.factor(sector))
total <- sector_data %>% filter(grepl("Total Energy", Description)) %>% mutate(total = Value) %>% select(-c(Description, Value, Column_Order, MSN))
sector_total_losses$total <- total$total
sector_total_losses$proportion_losses <- sector_total_losses$losses/sector_total_losses$total
sector_total_losses$year <- as.numeric(substr(sector_total_losses$YYYYMM, 1, 4))
##################
#rename totals
filtered_sector[filtered_sector == 'Total Energy Consumed by the Transportation Sector'] <- 'Transportation'
filtered_sector[filtered_sector == 'Total Energy Consumed by the Industrial Sector'] <- 'Industrial'
filtered_sector[filtered_sector == 'Total Energy Consumed by the Commercial Sector'] <- 'Commercial'
filtered_sector[filtered_sector == 'Total Energy Consumed by the End-Use-Sectors'] <- 'All sectors'
#new column with yearly sum extracted
filtered_sector$year <- as.numeric(substr(filtered_sector$YYYYMM, 1, 4))
#filter and group to get total energy over sectors
filtered_sector2 <- filtered_sector %>%
  clean_names() %>%
  mutate(description = as.factor(description)) %>%
  group_by(description, year) %>%
  summarize(value)

#############
# Widget 4. Emissions by sector over time

# A. Wrangle Data
emissions_persector_fuel <- emissions_complete_data %>% 
  select(period, sector_name, fuel_name, value, value_units) %>%
  mutate(fuel_name = as.factor(fuel_name)) %>%
  group_by(period, sector_name, fuel_name) %>%
  summarize(total = sum(value))

# B. Exploratory plot
plot_w4 <- ggplot(data=emissions_persector_fuel, aes(x=period, y=total, fill=fuel_name)) +
  labs(x="Year", y="CO2 Emissions (MMT)", fill= "Fuel Type")+
  scale_y_continuous(labels = function(x) paste0(x/1000, " k"))+
  geom_col()+
  theme_minimal()

plot_w4

# C. Rename Sectors
emissions_persector_fuel[emissions_persector_fuel == 'Residential carbon dioxide emissions'] <- 'residential'
emissions_persector_fuel[emissions_persector_fuel == 'Industrial carbon dioxide emissions'] <- 'industrial'
emissions_persector_fuel[emissions_persector_fuel == 'Electric Power carbon dioxide emissions'] <- 'electric_power'
emissions_persector_fuel[emissions_persector_fuel == 'Transportation carbon dioxide emissions'] <- 'transportation'
emissions_persector_fuel[emissions_persector_fuel == 'Commercial carbon dioxide emissions'] <- 'commercial'
emissions_persector_fuel[emissions_persector_fuel == 'Total carbon dioxide emissions from all sectors'] <- 'all_sectors'

# D. Duplicate data

emissions_persector_fuelb <- emissions_persector_fuel
#########
clean_power_generation_states <- power_generation_states %>%
  clean_names() %>%
  filter(type_of_producer %in% "Total Electric Power Industry" & !energy_source %in% c("Total", "Other", "Other Gases", "Other Biomass") & !state %in% c("DC", "US-TOTAL")) %>%
  mutate(energy_source = as.factor(energy_source))
#replace abbreviations with names
clean_power_generation_states$state_name <- state.name[match(clean_power_generation_states$state, state.abb)]

