#######################                         #######################
#######################                         #######################
#######################       Project Setup     #######################
#######################                         #######################

# Load packages
library(tidyverse)

#load data, ACLED 1 is before 01/01/2004, and ACLED 2 is after 01/01/2004 (just need to hit a little bit of filtering, as seen below)
# acled2 <- read_csv("acled 2014-01-01-2024-02-05.csv", name_repair = "universal") 
# acled1 <- read_csv("acled 1900-01-01-2004-01-01.csv", name_repair = "universal")

# acled1 <- 
acled1 %>% 
  filter(event_date != "01 January 2004")


# acled <-
rbind(acled2, acled1)

# ACLED files too large to commit to GITHUB. If needed, can use them


###################### New Datasets ######################
##########################################################

library(readxl)

SVAC <- read_excel("SVAC_3.2_conflictyears.xlsx")

acled_sexual_violence <- read_csv("acled_sexual_violence.csv", name_repair = "universal")

ucdp_actor <- read_excel("ucdp_actor.xlsx")

ucdp_issues <- read_excel("ucdp_issues.xlsx")

CNTSDATA <- read_excel("CNTSDATA.xlsx")

# Part 0: Exploratory Analysis

# Part 1: acled_sexual_violence: make a shiny R based on actor type (interaction) 
# one shiny map for state sexual violence
# another shiny map for individual cases

# Part 2: Create model for type of actor (region + ally (ucdp_actor), economic state of country (CNTSDATA)) or interaction (reason for conflict (ucdp_issues)) that incurs sexual violence

# Part 3: Set of Visualizations

# Part 4: Based on Part 2, write data journalism story that incorporate results from model, and uses visualizations from Part 3

# Poster, Final Paper
