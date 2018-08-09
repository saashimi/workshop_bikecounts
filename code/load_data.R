library(readxl)
library(tidyverse)
library(lubridate)
library(leaflet)

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
Hawthorne <- all_bridges[[1]] %>%
  rename(westbound = `north side (westbound)`) %>%
  rename(eastbound = `south side (eastbound)`) %>% 
  mutate(Lat = 45.513180) %>% 
  mutate(Lng = -122.670602)
  
Tilikum <-  all_bridges[2]  %>% 
  mutate(Lat = 45.504938) %>% 
  mutate(Lng = -122.667013)  

Steel <- all_bridges[[3]] %>%
  select(-c(`River Walk (lower 2-way)`)) %>% 
  rename(westbound = `north side (westbound)`) %>%
  rename(eastbound = `south side (eastbound)`) %>% 
  mutate(Lat = 45.527574) %>% 
  mutate(Lng = -122.669062)

weather_data_csv <- read_csv("data/NCDC-CDO-USC00356750.csv") %>% 
  select(-c(STATION, NAME))
  
consolidated_bridge <- bind_rows(Hawthorne, Tilikum, Steel)

bike_counts <- consolidated_bridge %>% 
  gather(eastbound, westbound, total, key = "type", value = "counts") %>%
  spread(type, counts)

bikecounts_dow <- bike_counts %>%
  mutate(dow=wday(date, label=TRUE))

# summarize bike counts by bridge & day-of-week
bikecounts_dow <- bike_counts %>% 
  group_by(name, dow) %>% 
  summarize(avg_daily_counts=mean(total, na.rm = TRUE))

bikecounts_dow %>% 
  spread(dow, avg_daily_counts)

bikecounts_dow %>% 
  spread(name, avg_daily_counts)

all_data <- left_join(consolidated_bridge, weather_data_csv, by = c("date" = "DATE"))

all_data %>% 
  group_by(name) %>% 
  top_n(3, wt=total)

bike_timeseries <- ts(all_data$total, frequency=4, start=c(2012,12))
plot.ts(bike_timeseries)
bike_timeseriescomponents <- decompose(bike_timeseries)


#all_data %>%
# ggplot(aes(x=PRCP, y=total, color=name)) + 
# theme_minimal() +
# geom_point(size=.5) +
# stat_smooth(method=lm, se=FALSE, linetype="dashed", aes(group=1), 
#             size=1, color="gray") +
# labs(title= "Total Bike Counts by Precipitation") +
# scale_color_brewer(palette="Dark2")

tilikum_open <- all_data %>%
  filter(name == 'Hawthorne' | name == 'Tilikum') 

tilikum_open %>%  ggplot(aes(x=date, y=total, color=name)) + 
  theme_minimal() +
  geom_point(size=.5) +
  stat_smooth(method=lm, se=FALSE, linetype="dashed", aes(group=1), 
              size=1, color="black") +
  labs(title= "Total Bike Counts over Time") +
  scale_color_brewer(palette="Dark2")
  