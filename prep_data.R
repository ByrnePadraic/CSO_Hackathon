# # ============================================================
# # Step 1: Fetch CSO data and save to disk
# # Run this manually. Check the output at each stage.
# # ============================================================
# 
# library(csodata)
# library(dplyr)
# library(lubridate)
# 
# # --- Fetch the main pass-rate table ---
# raw <- cso_get_data("ROA30", pivot_format = "tidy") %>% as_tibble()
# 
# 
# # --- Build a simple master table ---
# # Only using pass rate for the minimal version
# master <- raw %>%
#   mutate(date = parse_date_time(Month,
#                                 orders = c("Y m", "Y b", "b Y", "Y B"),
#                                 quiet = TRUE)) %>%
#   select(
#     date,
#     driving_test_centre     = `Driving.Test.Centre`,
#     driving_test_categories = `Driving.Test.Categories`,
#     tests_delivered         = `Driving Tests Delivered`,
#     pass_rate_percent       = `Driving Test Pass Rate`
#   ) %>%
#   filter(!tolower(driving_test_centre) %in%
#            c("state", "all centres", "total", "all driving test centres"))
# 
# 
# # --- Build latest snapshot (last 12 months average) ---
# max_date <- max(master$date, na.rm = TRUE)
# 
# snapshot <- master %>%
#   filter(date >= max_date %m-% months(12)) %>%
#   group_by(driving_test_centre, driving_test_categories) %>%
#   summarise(
#     pass_rate       = mean(pass_rate_percent, na.rm = TRUE),
#     total_delivered = sum(tests_delivered,    na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   filter(!is.nan(pass_rate))
# 
# 
# # --- Save ---
# dir.create("data", showWarnings = FALSE)
# saveRDS(master,   "data/master.rds")
# saveRDS(snapshot, "data/snapshot.rds")


# ============================================================
# Step 1: Fetch CSO data and save to disk
# Run this manually. Check the output at each stage.
# ============================================================

library(csodata)
library(dplyr)
library(lubridate)

# --- Fetch pass-rate table (ROA30) ---
message("Fetching ROA30 (pass rate)...")
raw_pass <- cso_get_data("ROA30", pivot_format = "tidy") %>% as_tibble()

message("Columns in ROA30:")
print(names(raw_pass))

# --- Fetch wait time table (ROA36) ---
message("\nFetching ROA36 (wait times)...")
raw_wait <- cso_get_data("ROA36", pivot_format = "tidy") %>% as_tibble()

message("Columns in ROA36:")
print(names(raw_wait))

# --- Build master table ---

# --- Build master table ---
master <- raw_pass %>%
  mutate(date = parse_date_time(Month,
                                orders = c("Y m", "Y b", "b Y", "Y B"),
                                quiet = TRUE)) %>%
  select(
    date,
    driving_test_centre     = `Driving.Test.Centre`,
    driving_test_categories = `Driving.Test.Categories`,
    tests_delivered         = `Driving Tests Delivered`,
    pass_rate_percent       = `Driving Test Pass Rate`
  ) %>%
  filter(!tolower(driving_test_centre) %in%
           c("state", "all centres", "total", "all driving test centres"))

# --- Build wait time table (centre × month, no category) ---
# ASSUMPTION: "Estimated Time to Driving Test Invite at Month End" is in WEEKS.
# Check summary(wait$wait_weeks) after running - if values are 30-200 it's days, divide by 7.
wait <- raw_wait %>%
  mutate(date = parse_date_time(Month,
                                orders = c("Y m", "Y b", "b Y", "Y B"),
                                quiet = TRUE)) %>%
  select(
    date,
    driving_test_centre = `Driving.Test.Centre`,
    wait_weeks          = `Estimated Time to Driving Test Invite at Month End`
  ) %>%
  filter(!tolower(driving_test_centre) %in%
           c("state", "all centres", "total", "all driving test centres"))

message("\nWait table shape: ", nrow(wait), " rows")
message("Wait time summary (check units - should be weeks):")
print(summary(wait$wait_weeks))

message("\nMaster table shape: ", nrow(master), " rows × ", ncol(master), " columns")
message("Date range: ",
        format(min(master$date, na.rm = TRUE)), " to ",
        format(max(master$date, na.rm = TRUE)))
message("\nDistinct centres: ", length(unique(master$driving_test_centre)))
message("Distinct categories: ", length(unique(master$driving_test_categories)))

# --- Build latest snapshot (last 12 months average) ---
max_date <- max(master$date, na.rm = TRUE)

snapshot <- master %>%
  filter(date >= max_date %m-% months(12)) %>%
  group_by(driving_test_centre, driving_test_categories) %>%
  summarise(
    pass_rate       = mean(pass_rate_percent, na.rm = TRUE),
    total_delivered = sum(tests_delivered,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.nan(pass_rate))

message("\nSnapshot shape: ", nrow(snapshot), " rows")

# --- Build latest wait time per centre ---
wait_latest <- wait %>%
  filter(date == max(date, na.rm = F)) %>%
  select(driving_test_centre, wait_weeks)

message("Latest wait times: ", nrow(wait_latest), " centres")

# --- Save ---
dir.create("data", showWarnings = FALSE)
saveRDS(master,      "data/master.rds")
saveRDS(snapshot,    "data/snapshot.rds")
saveRDS(wait,        "data/wait.rds")
saveRDS(wait_latest, "data/wait_latest.rds")

message("\nSaved master.rds, snapshot.rds, wait.rds, wait_latest.rds")
message("Run geocode_centres.R next, then launch app.R")

