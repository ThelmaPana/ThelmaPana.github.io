#--------------------------------------------------------------------------#
# Project: ThelmaPana.github.io
# Script purpose: Test plots for a Quarto dashboard
# Date: 15/02/2025
# Author: Thelma Panaïotis (inspired by Florian Ricour’s code)
#--------------------------------------------------------------------------#

##TODO
# indoor cycling, need to read Zwift files?


library(tidyverse)
library(FITfileR)
library(sf)
library(parallel)
library(scales)

# List fit files
fit_files <- list.files("~/Desktop/HealthData/FitFiles/Activities", pattern = "*.fit", full.names = TRUE)
fit_files <- sort(fit_files)
#fit_files <- list.files("~/HealthData/FitFiles/Activities/", pattern = "*.fit", full.names = TRUE)
#fit_files <- fit_files[1000:1025]
#
#file <- fit_files[1023]
#
#
#fit_files[str_detect(fit_files, "18730458288")]
#
#fit_files <- c(
#  "/Users/thepan/Desktop/HealthData/FitFiles/Activities//11719964231_ACTIVITY.fit", # lap swimming
#  "/Users/thepan/Desktop/HealthData/FitFiles/Activities//18716290482_ACTIVITY.fit", # commuting
#  "/Users/thepan/Desktop/HealthData/FitFiles/Activities//18730458288_ACTIVITY.fit" # running
#)
#file <- fit_files[2]


# function that extracts fit files data (see https://msmith.de/FITfileR/articles/FITfileR.html)
extract_garmin_activities <- function(file){

  # read file
  f <- readFitFile(file)

  # get file id
  #f_id <- file_id(f)$serial_number
  f_id <- unlist(stringr::str_split(file, "/"))
  f_id <- unlist(stringr::str_split(f_id[length(f_id)], '_'))[1]

  #print(f_id)

  # if sport is available, read file content
  if ("sport" %in% listMessageTypes(f)){

    # get activity type
    activity_type <- try(getMessagesByType(f, "sport")$sport[1])
    activity_subtype <- try(getMessagesByType(f, "sport")$sub_sport[1])

    ## check activity type
    #if(class(activity_type) == 'try-error'){
    #  activity_type <- 'unknown'
    #  activity_subtype <- 'unknown'
    #}

    # Keep only swimming, cycling and running
    if (activity_type %in% c("swimming", "cycling", "running")) {
      # Get laps
      data <- laps(f) %>%
        summarise(
          # Get unique timestamp
          timestamp = first(timestamp),
          # Sum time and distance across laps
          total_time = sum(total_timer_time),
          total_distance = sum(total_distance)
        ) %>%
        mutate(
          # Add ID, activity type and subtype
          id = f_id,
          type = activity_type,
          subtype = activity_subtype,
          .before = total_time
        )

      return(data)
    }
  }
}


toto <- all_records %>%
  select(id, activity_type, activity_subtype, timestamp) %>%
  group_by(id, activity_type, activity_subtype) %>%
  slice_head(n = 1) %>%
  ungroup()

titi <- toto %>% select(activity_type, activity_subtype) %>% distinct() %>% arrange(activity_type)

# Running lap_swimming
toto <- all_records %>% filter(activity_type == "cycling" & activity_subtype == "mixed_surface") #15249921056
toto %>% select(id)

toto %>% filter(activity_subtype == "trail")

file <- fit_files[str_detect(fit_files, "15249921056")]
file <- fit_files[str_detect(fit_files, "11155341926")]



activities <- mclapply(1:length(fit_files), function(i) {
  extract_garmin_activities(fit_files[i])
}, mc.cores = 12) %>%
  bind_rows()






summary(activities)
# Keep only activities in 2025

df <- activities %>%
  #filter(year(timestamp) == 2024) %>%
  mutate(
    month = month(timestamp),
    month = factor(month),
    week = week(timestamp),
    week = factor(week)
  )

df %>% select(type, subtype) %>% distinct() %>% arrange(type)

## Clean activity names
types <- df %>% select(type, subtype) %>% distinct() %>% arrange(type)

types %>%
  mutate(
    new_type = c("cycling", "cycling", "cycling", NA, NA, "running", "running", "running", "swimming", "swimming"),
    new_subtype = c("commuting", "road", "gravel", NA, NA, "road", "trail", "treadmill", "pool", "open-water")
  )


## Duration plots
# By month
ggplot(df) +
  geom_col(aes(x = month, y = total_time, fill = activity_type), position = "dodge") +
  labs(x = "Month", y = "Duration (h)", fill = "Activity") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_time(expand = c(0, 0)) +
  scale_fill_brewer() +
  theme_minimal()


# For week, fill with zeros!
ggplot(df) +
  geom_col(aes(x = week, y = total_time, fill = activity_type), position = "dodge") +
  labs(x = "Week", y = "Duration (h)", fill = "Activity") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_time(expand = c(0, 0.1)) +
  scale_fill_brewer() +
  theme_minimal()


## Distance plots
# By month
ggplot(df) +
  geom_col(aes(x = month, y = total_distance / 1000, fill = activity_type), position = "dodge") +
  labs(x = "Month", y = "Distance (km)", fill = "Activity") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer() +
  theme_minimal()

# By week
ggplot(df) +
  geom_col(aes(x = week, y = total_distance / 1000, fill = activity_type), position = "dodge") +
  labs(x = "Week", y = "Distance (km)", fill = "Activity") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer() +
  theme_minimal()



