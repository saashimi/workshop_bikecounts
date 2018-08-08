library(readxl)
library(tidyverse)
library(lubridate)

load_bridge <- function(bridge_name) {
  bridge_data <- read_excel("data/Hawthorne Tilikum Steel daily bike counts 073118.xlsx",
             sheet = bridge_name, skip = 1)
  bridge_data <- add_column(bridge_data, name = bridge_name) %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y'))
  return(bridge_data)
}

filter_by_name <- function(bridge_name) {
  bridge_data <- all_data %>%
    filter(name == bridge_name)
    return(bridge_data)
}

total_days <- function(bridge_name) {
  data <- bridge_name %>%
    select(total) %>%
    filter(total > 0) %>%
    tally()
    return(data)
}

average_daily <- function(bridge_name) {
  data <- bridge_name %>%
    select(total) %>%
    summarize(Mean = mean(total, na.rm=TRUE))
  return(data)
}

average_monthly <- function(bridge_name) {
  data <- bridge_name %>%
    select(date, total) %>%
    group_by(month(date)) %>%
    summarize(Mean = mean(total, na.rm=TRUE))
    return(data)
}

bridges <- c("Hawthorne", "Tilikum", "Steel")
all_bridges <-  lapply(bridges, load_bridge)
Hawthorne <- all_bridges[1]
Tilikum <-  all_bridges[2]
Steel <- all_bridges[3]

weather_data_csv <- read_csv("data/NCDC-CDO-USC00356750.csv") 
consolidated_bridge <- bind_rows(Hawthorne, Tilikum, Steel)
all_data <- left_join(consolidated_bridge, weather_data_csv, by = c("date" = "DATE"))

bridges <- c("Hawthorne", "Tilikum", "Steel")
all_bridges <-  lapply(bridges, filter_by_name)
Hawthorne <- all_bridges[[1]]
Tilikum <-  all_bridges[[2]]
Steel <- all_bridges[[3]]


ggplot(Hawthorne, aes(x=PRCP, y=total)) + geom_point()
ggplot(Tilikum, aes(x=PRCP, y=total)) + geom_point()
ggplot(Steel, aes(x=PRCP, y=total)) + geom_point()


#result <- c(strsplit(str, " "))
#col_width <- lapply(result, str_length)
#weather_data_fixed <- read_fwf("data/NCDC-CDO-USC00356750.txt", fwf_widths(col_width[[1]]))
