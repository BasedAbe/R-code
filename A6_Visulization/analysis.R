# Assignment 6 Visualizing Data
# Accessing the Data API
library(wbstats)
library(ggplot2)
library(tidyr)
library(dplyr)
library(maps)
library(RColorBrewer)
options(scipen = 999)

# 1. Loading and Understanding the Data 

#View(wbindicators())

top_10_co2_countries <- wb(country = "countries_only", indicator = c("EN.ATM.CO2E.KT"), mrv = 1) %>% # Access World Bank data
  arrange(-value) %>% 
  head(10)

# 2. CO2 Emissions by Country  

top_10_c02_plot <- ggplot(top_10_co2_countries) +
  geom_col(mapping = aes(x = reorder(iso3c, value), y = value)) +
  labs(title = "Top 10 Countries by CO2 Emissions", x = "Country (iso3)", y = top_10_co2_countries[["indicator"]])

############################################################################################################################

# 3. US Income Equality over Time 

us_income_years <- wb(country = "USA",                                                   # Access World Bank data
                      indicator = c("SI.DST.FRST.20","SI.DST.04TH.20","SI.DST.10TH.10"), 
                      mrv = 20, return_wide = TRUE) %>%                                  
                         mutate(date = as.numeric(date),                                 # Change dates into numeric value
                             "Top 10% of Pop." = SI.DST.10TH.10, 
                              "Bottom 40% of Pop." = SI.DST.04TH.20 + SI.DST.FRST.20 ) %>%  # Make column names readable
                               gather(key = category , value = "percent", c("Top 10% of Pop.", "Bottom 40% of Pop.")) 

us_wealth_plot <- ggplot(us_income_years, mapping = aes(x = date, y = percent, color = category)) +
  geom_point() +
  geom_line() +
  labs(
    title = "US Wealth Equality over Time", # Adding labels to the visualizations 
    x = "Year",                             # and changning legend title to group
    y = "Percentage of income held",
    color = "Group"                         
  )

###################################################################################################################

# 4. Health Expenditures by Country

high_income_countries <- wbcountries() %>%  # Accessing world bank to filter for
  filter(income == "High income") %>%       # high income countries   
  pull(iso3c)

updated_cache <- wbcache()  

health_costs <- wb(country = high_income_countries,                                                                # Created data frame
                   indicator = c("SH.XPD.OOPC.PC.CD","SH.XPD.PVTD.PC.CD","SH.XPD.GHED.PC.CD","SH.XPD.CHEX.PC.CD"), # using the high income 
                   mrv = 1, cache = updated_cache) %>%                                                             # data frame and updated cache
                   arrange(value) 

health_costs[health_costs$indicatorID == "SH.XPD.OOPC.PC.CD", "indicatorID"] <- "Out of Pocket Costs"  # Changing the indicator
health_costs[health_costs$indicatorID == "SH.XPD.CHEX.PC.CD", "indicatorID"] <- "Total Spending"       # idea values in the rows
health_costs[health_costs$indicatorID == "SH.XPD.GHED.PC.CD", "indicatorID"] <- "Government Spending"  # into readable values
health_costs[health_costs$indicatorID == "SH.XPD.PVTD.PC.CD", "indicatorID"] <- "Private Spending"

total_health_costs <- health_costs[health_costs$indicatorID == "Total Spending", ]      # Data frame for only total spending

healthcare_costs_plots <- ggplot(health_costs, mapping = aes(x = reorder(iso3c, value), y = value , color = indicatorID)) +
  geom_linerange(total_health_costs, mapping = aes(ymax = value, ymin = 0)) +
  geom_point(mapping = aes(shape = indicatorID)) +
  labs(
    title = "Health Care Expenditures (per capita)",
    x = "Country",
    y = "Current US$",
    color = "Type",
    shape = "Type"
  ) +
  theme(axis.text = element_text(size = 4, angle = 90), legend.position = c(.3, .75)) 


###################################################################################################################

# 5. Map: Changes in Forestation around the World 

forest_area <- wb(country = "countries_only", indicator = c("AG.LND.FRST.ZS"), mrv = 20) %>% 
  spread(key = date, value = value) 

forest_data <- forest_area %>% 
  mutate(forest_change = forest_area[["2016"]] - forest_area[["1997"]]) # Creating column with 2016 values minus
                                                                        # the 1997 values
#maps_data("world)

maps_df <- map_data("world") %>%                          # Creating data frame for map_data using iso3c value to match with the
  mutate(iso3c = iso.alpha(region, n = 3))                # country name

forest_and_map <- left_join(maps_df, forest_data, by = "iso3c") # Left joining maps data frame with my forest data

world_forest_plot <- ggplot(forest_and_map) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = forest_change)) +
  scale_fill_distiller(palette = "RdYlGn", direction = -1) + 
  coord_quickmap() +
  theme_void() +
  labs(
    title = "Change in Forested Area 1997-2016"
  )

###################################################################################################################

# 6. Access to Electricity 

access_to_electricity <- wb(country = "countries_only", indicator = c("EG.ELC.ACCS.RU.ZS"), mrv = 20) %>% 
  filter(value <= 75) %>%                                                                                 # Accessing database and filtering for the countries 
  arrange(-value) %>%                                                                                     # who have 75 percent or less access to electricty
  spread(key = date, value = value)                                                                       # then arranging by value

access_to_electricity_fifteen_years <- access_to_electricity %>% 
  mutate(change_in_access = access_to_electricity[["2015"]] - access_to_electricity[["2000"]]) # Creating column with 2015 values minus
                                                                                               # the 2000 values
maps_electricity_df <- map_data("world") %>% 
  mutate(iso3c = iso.alpha(region, n = 3))

access_and_map <- left_join(maps_electricity_df, access_to_electricity_fifteen_years, by = "iso3c")

global_access_to_electricity_plot <- ggplot(access_and_map) +                                 # Creating map plot for the change in
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = change_in_access)) +    # access to electricity from 2000-2015 
  scale_fill_distiller(palette = "Spectral", direction = -1) +                                 
  coord_quickmap() +                                                                          
  theme_void() +
  labs(
    title = "Growth in Access to Electricity 2000-2015"
  )