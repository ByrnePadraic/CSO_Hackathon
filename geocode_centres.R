# ============================================================
# Step 2: Geocode test centres to lat/long
# Run once, review output, keep centre_coords.csv in the repo
# ============================================================

library(dplyr)
library(tidygeocoder)
library(readr)

# Load the centres from the saved master table
master <- readRDS("data/master.rds")

centres <- tibble(
  centre_name = sort(unique(master$driving_test_centre))
)

message("Geocoding ", nrow(centres), " centres via OpenStreetMap...")

centre_coords <- centres %>%
  mutate(query = paste0(centre_name, " Driving Test Centre, Ireland")) %>%
  geocode(address = query, method = "osm",
          lat = latitude, long = longitude)

# Show anything that failed
failed <- centre_coords %>% filter(is.na(latitude))
if (nrow(failed) > 0) {
  message("\nThese centres failed geocoding - retrying with simpler query:")
  print(failed$centre_name)
  
  retried <- failed %>%
    select(centre_name) %>%
    mutate(query = paste0(centre_name, ", Ireland")) %>%
    geocode(address = query, method = "osm",
            lat = latitude, long = longitude)
  
  centre_coords <- centre_coords %>%
    filter(!is.na(latitude)) %>%
    bind_rows(retried)
}

# Sanity check - Ireland is roughly lat 51.4-55.5, lon -10.6 to -5.2
out_of_bounds <- centre_coords %>%
  filter(!is.na(latitude)) %>%
  filter(latitude  < 51.4 | latitude  > 55.5 |
           longitude < -10.6 | longitude > -5.2)

if (nrow(out_of_bounds) > 0) {
  message("\nWARNING - these geocoded outside Ireland, check manually:")
  print(out_of_bounds)
}

still_missing <- centre_coords %>% filter(is.na(latitude))
if (nrow(still_missing) > 0) {
  message("\nWARNING - these have no coordinates at all:")
  print(still_missing$centre_name)
}

# Save
centre_coords %>%
  select(centre_name, latitude, longitude) %>%
  write_csv("data/centre_coords.csv")

message("\nSaved data/centre_coords.csv - review it before running the app.")