#getwd()
#setwd("//Users/jakobaggers/Desktop")

# Load packages
library(tidyverse)

#load data, ACLED 1 is before 01/01/2004, and ACLED 2 is after 01/01/2004 (just need to hit a little bit of filtering, as seen below)
#acled2 <- read_csv("acled 2014-01-01-2024-02-05.csv", name_repair = "universal")
#acled1 <- read_csv("acled 1900-01-01-2004-01-01.csv", name_repair = "universal")

#acled1 <- 
  #acled1 %>% 
 # filter(event_date != "01 January 2004")

#acled <-
  # rbind(acled2, acled1)

######### Drone Strikes #############
drone <- read_csv("drone_strikes.csv", name_repair = "universal")

### filter for distinct events (cut out repeats for statistics)
drone_distinct <- distinct(drone, timestamp, .keep_all = TRUE)
   
drone_distinct_fatal <- 
  drone_distinct %>% 
  filter (fatalities > 0)

library(ggplot2)

ggplot(data = drone_distinct_fatal) +
  geom_boxplot(mapping = aes(y = fatalities)) +
  scale_y_log10()

drone_distinct_fatal %>% 
  group_by(region) %>% 
  summarize(mean = mean(fatalities),
            median = median(fatalities),
            sd = sd(fatalities),
            max = max(fatalities))

###### do count of number of drone strikes per year ########

ggplot(data = drone_distinct_fatal) +
  geom_line(mapping = aes(x = year, y = fatalities))



