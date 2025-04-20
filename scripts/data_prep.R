library(tidyverse)
library(lubridate)
library(survival)

file_path <- "data/cleaned_global_water_consumption.csv"
water_data <- read_csv(file_path, show_col_types = FALSE)


water_data <- water_data %>%
  rename(
    country = `Country`,
    year = `Year`,
    total_consumption = `Total Water Consumption (Billion Cubic Meters)`,
    per_capita_use = `Per Capita Water Use (Liters per Day)`,
    scarcity_level = `Water Scarcity Level`,
    agri_use_pct = `Agricultural Water Use (%)`,
    industrial_use_pct = `Industrial Water Use (%)`,
    household_use_pct = `Household Water Use (%)`,
    rainfall_mm = `Rainfall Impact (Annual Precipitation in mm)`,
    groundwater_depletion_pct = `Groundwater Depletion Rate (%)`
  )

# Group by country and arrange by year to process time series
water_data_sorted <- water_data %>%
  group_by(country) %>%
  arrange(year, .by_group = TRUE) %>%
  ungroup()

print("Data sorted by country and year:")
print(head(water_data_sorted))


# Find the first year each country reaches high water scarcity
event_year_data <- water_data_sorted %>%
  filter(scarcity_level == "High") %>%
  group_by(country) %>%
  summarise(event_year = min(year), .groups = "drop")

# Get start and end years for each country's observation period
observation_periods <- water_data_sorted %>%
  group_by(country) %>%
  summarise(
    start_year = min(year),
    last_observation_year = max(year),
    .groups = "drop"
  )

# Combine observation periods with event year information
survival_summary <- observation_periods %>%
  left_join(event_year_data, by = "country") %>%
  mutate(
    event_status = ifelse(!is.na(event_year) & event_year <= last_observation_year, 1, 0),
    event_time_point = ifelse(event_status == 1, event_year, last_observation_year),
    time = event_time_point - start_year
  ) %>%
  mutate(time = ifelse(time < 0, 0, time)) %>%
  select(country, start_year, last_observation_year, event_year, time, event_status)

print("Summary of time-to-event and status per country:")
print(head(survival_summary))
print(paste("Total countries:", nrow(survival_summary)))
print(paste("Number of events (reaching High scarcity):", sum(survival_summary$event_status)))
print(paste("Censoring rate:", scales::percent(1 - mean(survival_summary$event_status))))



# Check NAs in the key predictors
missing_summary <- water_data_sorted %>%
  select(country, year, agri_use_pct, rainfall_mm, groundwater_depletion_pct) %>%
  summarise(across(everything(), ~ sum(is.na(.))))

print("Missing values summary for key covariates:")
print(missing_summary) # Result: No missing values found.


water_data_with_status <- water_data_sorted %>%
  left_join(survival_summary %>% select(country, start_year, time, event_status), by = "country")


final_survival_data <- water_data_with_status %>%
  group_by(country) %>%
  mutate(
    time_elapsed = year - first(start_year),
    t_start = time_elapsed,
    t_stop = time_elapsed + 1
  ) %>%
  filter(t_start < time) %>%
  mutate(t_stop = ifelse(t_stop > time, time, t_stop)) %>%
  mutate(interval_event_status = ifelse(t_stop == time & event_status == 1, 1, 0)) %>%
  ungroup() %>%
  select(
    country,
    t_start,
    t_stop,
    interval_event_status,
    agri_use_pct,
    rainfall_mm,
    groundwater_depletion_pct,
    year,
    overall_time = time,
    overall_event_status = event_status
  ) %>%
  filter(t_start < t_stop)

print("Preview of final data structure for time varying covariates:")
print(head(final_survival_data))



# Get unique country list
unique_countries <- unique(final_survival_data$country)


# Create mapping from country to region (Continent)
# Based on the 20 unique countries present in this dataset.
region_vector <- case_when(
  unique_countries %in% c("Argentina", "Brazil", "Mexico") ~ "Americas", # Combined North & South
  unique_countries %in% c("Australia") ~ "Oceania",
  unique_countries %in% c("Canada", "USA") ~ "Americas",
  unique_countries %in% c("China", "India", "Indonesia", "Japan", "Saudi Arabia", "Turkey", "South Korea") ~ "Asia",
  unique_countries %in% c("France", "Germany", "Italy", "Russia", "UK", "Spain") ~ "Europe",
  unique_countries %in% c("Egypt", "South Africa", "Nigeria") ~ "Africa",
  TRUE ~ "Unknown"
)


region_map <- data.frame(
  country = unique_countries,
  region = region_vector
)


# Merge the region information into the main survival data
final_survival_data <- final_survival_data %>%
  left_join(region_map, by = "country") %>%
  mutate(region = as.factor(region))

print("Preview of final data with Region column added:")
print(head(final_survival_data))
print("Summary of regions assigned:")
print(summary(final_survival_data$region))


saveRDS(final_survival_data, "data/processed_water_survival_data.rds")
print("Processed data saved to data/processed_water_survival_data.rds")
