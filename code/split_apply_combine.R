library(readxl)
library(tidyverse)
library(lubridate)


load_bridge <- function(bridge_name) {
  bridge_data <- read_excel(
    "data/Hawthorne Tilikum Steel daily bike counts 073118.xlsx",
    sheet = bridge_name, skip = 1)
  bridge_data <- add_column(bridge_data, name = bridge_name) %>%
    mutate(date = as.Date(date, format = '%m/%d/%Y'))
  return(bridge_data)
}

bridges <- c("Hawthorne", "Tilikum", "Steel")
all_bridges <-  lapply(bridges, load_bridge)
Hawthorne <- all_bridges[[1]] %>% 
  rename(westbound = `north side (westbound)`) %>%
  rename(eastbound = `south side (eastbound)`) 

Tilikum <-  all_bridges[2]

Steel <- all_bridges[[3]] %>%
  select(-c(`River Walk (lower 2-way)`)) %>% 
  rename(westbound = `north side (westbound)`) %>%
  rename(eastbound = `south side (eastbound)`) 

weather_data_csv <- read_csv("data/NCDC-CDO-USC00356750.csv") %>% 
  select(-c(STATION, NAME))

consolidated_bridge <- bind_rows(Hawthorne, Tilikum, Steel)

all_data <- left_join(consolidated_bridge, weather_data_csv, 
                      by = c("date" = "DATE"))
