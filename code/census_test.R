## Install the tidycensus package if you haven't yet
#install.packages("tidycensus")

library(tidycensus)
library(ggplot2)
source("SECRETS.R")

census_api_key(APIKEY)
portland_tract_medhhinc <- get_acs(geography = "tract", 
                                   year = 2016, # 2012-2016
                                   variables = "B19013_001",  # Median Household Income in the Past 12 Months
                                   state = "OR", 
                                   county = c("Multnomah County", "Washington County", "Clackamas County"),
                                   geometry = TRUE) # load geometry/gis info

ggplot(portland_tract_medhhinc) + 
  geom_sf(aes(fill = estimate)) +
  coord_sf(datum = NA) + theme_minimal()

library(sf)
library(mapview)
library(dplyr)

mapview(portland_tract_medhhinc %>% select(estimate), 
        col.regions = sf.colors(10), alpha = 0.1)

