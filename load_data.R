library(readxl)
library(tidyverse)
library(lubridate)

load_bridge <- function(bridge_name) {
  bridge_data <- read_excel("data/Hawthorne Tilikum Steel daily bike counts 073118.xlsx",
             sheet = bridge_name, skip = 1)
  return(bridge_data)
}

load_bridge_bind <-  function(bridge_name) {
  # create empty dataframe
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

# Create empty dataframe
# df <- data.frame(matrix(ncol = 3, nrow = 0))
# x <- c("date", "westbound", "eastbound", "total")
# colnames(df) <- x
