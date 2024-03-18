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

SVAC <- read_excel("Data/SVAC_3.2_conflictyears.xlsx")

#acled_sexual_violence <- read_csv("acled_sexual_violence.csv", name_repair = "universal")

ucdp_actor <- read_excel("Data/ucdp_actor.xlsx")

ucdp_issues <- read_excel("Data/ucdp_issues.xlsx")

CNTSDATA <- read_excel("Data/CNTSDATA.xlsx")

# Part 0: Exploratory Analysis

# Part 1: acled_sexual_violence: make a shiny R based on actor type (interaction) 
# one shiny map for state sexual violence
# another shiny map for individual cases

# Part 2: Create model for type of actor (region + ally (ucdp_actor), economic state of country (CNTSDATA)) or interaction (reason for conflict (ucdp_issues)) that incurs sexual violence

# Part 3: Set of Visualizations

# Part 4: Based on Part 2, write data journalism story that incorporate results from model, and uses visualizations from Part 3

# Poster, Final Paper


#### 
SVAC %>% 
  group_by(state_prev, ai_prev, hrw_prev) %>% 
  summarize(n = n())

SVAC_prev <- SVAC[SVAC$state_prev == 1 | SVAC$ai_prev == 1 | SVAC$hrw_prev == 1, ]

SVAC_prev %>% 
  group_by(actor_type) %>% 
  summarize(n = n())

SVAC_prev %>% 
  group_by(type_of_conflict) %>% 
  summarize(n = n())

SVAC %>% 
  group_by(type_of_conflict) %>% 
  summarize(n = n())

SVAC_prev %>% 
  group_by(incompatibility) %>% 
  summarize(n = n())

SVAC_prev %>% 
  group_by(region) %>% 
  summarize(n = n())

SVAC_prev %>% 
  group_by(child_prev) %>% 
  summarize(n = n()) 

###### Let's fix the form variable
library(dplyr)

# Define the categories

# Create new columns for each category with initial value 0
SVAC_prev <- SVAC_prev %>%
  mutate(
    form_rape = ifelse(grepl("1", form), 1, 0),
    form_sexual_slavery = ifelse(grepl("2", form), 1, 0),
    form_forced_prostitution = ifelse(grepl("3", form), 1, 0),
    form_forced_pregnancy = ifelse(grepl("4", form), 1, 0),
    form_forced_sterilization_abortion = ifelse(grepl("5", form), 1, 0),
    form_sexual_mutilation = ifelse(grepl("6", form), 1, 0),
    form_sexual_torture = ifelse(grepl("7", form), 1, 0)
  )


# Calculate Cramer's V for each pair of categorical variables
install.packages("vcd")
library(vcd)
install.packages("corrplot")
library(corrplot)
install.packages("Hmisc")
library(Hmisc)

SVAC_prev_NA <- SVAC_prev %>% 
  select(!c("gwnoloc2", "child_prev", "form"))

SVAC_prev_NA <- na.omit(SVAC_prev_NA)

# Convert specified variables to factors
SVAC_prev_NA$state_prev <- factor(ifelse(SVAC_prev_NA$state_prev == -99, NA, SVAC_prev_NA$state_prev))
SVAC_prev_NA$ai_prev <- factor(ifelse(SVAC_prev_NA$ai_prev == -99, NA, SVAC_prev_NA$ai_prev))
SVAC_prev_NA$hrw_prev <- factor(ifelse(SVAC_prev_NA$hrw_prev == -99, NA, SVAC_prev_NA$hrw_prev))

library(forcats)

# Convert each specified column to a factor individually
SVAC_prev_NA <- SVAC_prev_NA %>%
  mutate(
    actor = as_factor(actor),
    actorid = as_factor(actorid),
    actor_type = as_factor(actor_type),
    type_of_conflict = as_factor(type_of_conflict),
    incompatibility = as_factor(incompatibility),
    region = as_factor(region),
    gwno_loc = as_factor(gwno_loc),
    conflictyear = as_factor(conflictyear),
    interm = as_factor(interm),
    postc = as_factor(postc),
    state_prev = as_factor(state_prev),
    ai_prev = as_factor(ai_prev),
    hrw_prev = as_factor(hrw_prev),
    form_rape = as_factor(form_rape),
    form_sexual_slavery = as_factor(form_sexual_slavery),
    form_forced_prostitution = as_factor(form_forced_prostitution),
    form_forced_pregnancy = as_factor(form_forced_pregnancy),
    form_forced_sterilization_abortion = as_factor(form_forced_sterilization_abortion),
    form_sexual_mutilation = as_factor(form_sexual_mutilation),
    form_sexual_torture = as_factor(form_sexual_torture)
  )

# Check the structure of your dataframe to ensure the conversion
str(SVAC_prev_NA)
SVAC_prev_NA <- na.omit(SVAC_prev_NA)

SVAC_prev_NA_factor <- SVAC_prev_NA_factor[complete.cases(SVAC_prev_NA_factor), ]

# Now Cramer's V using these variables.

SVAC_prev_NA_numeric <- as.data.frame(sapply(SVAC_prev_NA_factor, as.numeric))

# Calculate the correlation matrix
correlation_matrix <- cor(SVAC_prev_NA_numeric, use = "pairwise.complete.obs")

# Print the correlation matrix
print(correlation_matrix)

correlation_matrix[is.na(correlation_matrix)] <- 0

corrplot(correlation_matrix, 
         method = "color",          # Color method
         type = "upper",            # Upper triangle
         order = "hclust",         # Hierarchical clustering
         tl.col = "black",          # Label color
         tl.srt = 45,               # Label rotation
         addCoef.col = "black",     # Coefficient color
         number.cex = 0.5,          # Coefficient size
         tl.cex = 0.5               # Label size
         )

plot.new()

png("Images/SVAC_corr_plot.png", width = 800, height = 600)
dev.off()  # Close the PNG device



########################### SVAC correlations pt 2 ###############################

SVAC <- SVAC %>%
  mutate(overall_prev = pmax(state_prev, hrw_prev, ai_prev))

SVAC$overall_prev <- as.character(SVAC$overall_prev)

library(forcats)

SVAC$overall_prev <- as_factor(SVAC$overall_prev)

SVAC <- SVAC %>%
  mutate_all(~replace(., . == -99 | is.na(.), 0))

SVAC <- SVAC %>%
  mutate(
    form_rape = ifelse(grepl("1", form), 1, 0),
    form_sexual_slavery = ifelse(grepl("2", form), 1, 0),
    form_forced_prostitution = ifelse(grepl("3", form), 1, 0),
    form_forced_pregnancy = ifelse(grepl("4", form), 1, 0),
    form_forced_sterilization_abortion = ifelse(grepl("5", form), 1, 0),
    form_sexual_mutilation = ifelse(grepl("6", form), 1, 0),
    form_sexual_torture = ifelse(grepl("7", form), 1, 0)
  )

SVAC <- SVAC %>% 
  select(!form)

SVAC_model <- SVAC %>% 
  select(actor_type, type_of_conflict, incompatibility, region, conflictyear, interm, postc, overall_prev, actorid)

library(stats)

# Fit logistic regression model
model <- glm(overall_prev ~ actor_type + type_of_conflict + incompatibility + region + conflictyear + interm + postc + actorid,
             data = SVAC_model,
             family = binomial)

# Summarize the model
summary(model)

# Extract coefficients, standard errors, z-values, and p-values from summary object
coef_summary <- summary(model)$coefficients

# Extract p-values
p_values <- coef_summary[, "Pr(>|z|)"]

# Determine significance
Significance <- ifelse(p_values < 0.05, "Yes", "No")

# Create a dataframe with coefficients, standard errors, z-values, p-values, and significance
coef_summary <- data.frame(
  Coefficient = coef_summary[, "Estimate"],
  `Std. Error` = coef_summary[, "Std. Error"],
  `Z Value` = coef_summary[, "z value"],
  `P Value` = p_values,
  Significance = Significance
)

# Print the table
print(coef_summary)

library(grid)
library(gridExtra)

# Convert the dataframe to a table grob
table_grob <- tableGrob(coef_summary)

# Save the table as an image using ggsave
ggsave("Images/coef_summary_table.png", plot = table_grob, width = 8, height = 6, units = "in", dpi = 300)

