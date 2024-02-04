#--------------------------------------------------------------------------#
# Project: ThelmaPana.github.io
# Script purpose: Read and process garmin data
# Date: 02/02/2024
# Author: Thelma Panaïotis (inspired by Florian Ricour’s code)
#--------------------------------------------------------------------------#

library(tidyverse)
library(FITfileR)
library(sf)
library(chroma)

# List fit files
fit_files <- list.files("~/HealthData/FitFiles/Activities/", pattern = "*.fit", full.names = TRUE)

# function that extracts fit files data (see https://msmith.de/FITfileR/articles/FITfileR.html)
extract_garmin_data <- function(file){

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

    # check activity type
    if(class(activity_type) == 'try-error'){
      activity_type <- 'unknown'
      activity_subtype <- 'unknown'
    }

    data <- records(f) %>%
      bind_rows() %>%
      mutate(id = f_id, activity_type = activity_type, activity_subtype = activity_subtype, .before = timestamp) %>%
      arrange(timestamp)

    return(data)
  }
}

## extract all data
#all_activities <- map_dfr(fit_files, extract_garmin_data)
#unique(all_activities$activity_type)
#unique(all_activities$activity_subtype)


df_all <- tibble()
for(i in 1:length(fit_files)){
  df <- extract_garmin_data(fit_files[i])

  if (nrow(df_all) == 0){
    df_all <- df
  } else {
    df_all <- bind_rows(df_all, df)
  }

  if (i %% 10 == 0){print(paste0("Done with ", i , " out of ", length(fit_files)))}
}


#file = "/Users/oblade94/HealthData/FitFiles/Activities//11936919539_ACTIVITY.fit"

all_activities <- df_all

# Keep only certain activities
all_activities <- all_activities %>%
  select(id, activity_type, activity_subtype, time = timestamp, lat = position_lat, lon = position_long) %>%
  filter(activity_type %in% c("cycling", "running", "walking", "swimming", "hiking")) %>%
  drop_na(lon) %>%
  drop_na(lat) %>%
  filter(lon < 179) %>%
  filter(lat < 179)


# Save raw version
#save(all_activities, file = "garmin/all_activities.Rdata")
#load("garmin/all_activities.Rdata")

# Rename activities
clean_names <- read_csv("garmin/clean_names.csv", show_col_types = FALSE)
all_activities <- all_activities %>%
  left_join(clean_names, by = join_by(activity_type, activity_subtype)) %>%
  drop_na(activity) %>%
  select(id, activity, time, lon, lat)

# Remove beginning and end of each activity
all_activities <- all_activities %>%
  group_by(id) %>%
  filter(between(row_number(), 50, n() - 50)) %>%
  ungroup()

# Create LineString objects for each 'id'
lines_list <- all_activities %>%
  group_by(id, activity) %>%
  arrange(time) %>% # needed to avoid issues with trajectories (occurred due to (sometimes) several records in a single fit file)
  summarize(geometry = st_sfc(st_linestring(cbind(lon, lat))))

# Convert the summarized data to an sf dataframe with spatial lines
lines_sf <- st_as_sf(lines_list)

# Save
write_sf(lines_sf, "garmin/activities.geojson", quiet = TRUE)





