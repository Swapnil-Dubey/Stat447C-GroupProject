# Load necessary libraries
library(tidyverse)
library(lubridate) # Useful for potential date/year manipulations
library(survival) # For Surv object and potentially tmerge later

# --- 1. Load Data ---
# Assuming the CSV is in the same directory or a subdirectory named 'data'
# Adjust the path if necessary
file_path <- "cleaned_global_water_consumption.csv" 
# Check if file exists, provide guidance if not
if (!file.exists(file_path)) {
  stop(paste("Error: Data file not found at", file_path, 
             ". Please ensure the file is in the correct directory."))
}
water_data <- read_csv(file_path, show_col_types = FALSE)

# --- 2. Initial Inspection ---
# print("Column Names:")
# print(colnames(water_data))
# glimpse(water_data)
# print("Unique Water Scarcity Levels:")
# print(unique(water_data$`Water Scarcity Level`)) # Confirmed: Low, Moderate, High

# --- 3. Define Survival Variables ---
# Goal: For each country, find the time until the first occurrence of "High" scarcity
#       using the original `Water Scarcity Level` column provided in the dataset.
#       Countries not reaching "High" by the last observation year are right-censored.

# Rename columns for easier use (remove spaces, special chars)
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

# --- 4. Calculate Time-to-Event and Event Status ---

# Find the first year each country reaches "High" scarcity
event_year_data <- water_data_sorted %>%
  filter(scarcity_level == "High") %>%
  group_by(country) %>%
  summarise(event_year = min(year), .groups = 'drop')

# Get start and end years for each country's observation period
observation_periods <- water_data_sorted %>%
  group_by(country) %>%
  summarise(
    start_year = min(year),
    last_observation_year = max(year),
    .groups = 'drop'
  )

# Combine observation periods with event year information
survival_summary <- observation_periods %>%
  left_join(event_year_data, by = "country") %>%
  mutate(
    # Event occurred if event_year is not NA and happened during observation
    event_status = ifelse(!is.na(event_year) & event_year <= last_observation_year, 1, 0),
    # Time is duration until event or end of observation
    # Handle cases where event happens *before* the recorded start_year (unlikely but possible)
    event_time_point = ifelse(event_status == 1, event_year, last_observation_year),
    time = event_time_point - start_year
  ) %>%
  # Ensure time is non-negative (edge case if event_year == start_year)
  # Also handle cases where a country might only have one year of data
  mutate(time = ifelse(time < 0, 0, time)) %>%
  # A time of 0 could mean event happened in the first year or only one year of data
  # Let's refine: if event_status=1 and time=0, it means event in start_year.
  # If event_status=0 and time=0, it means only one year of data and no event.
  select(country, start_year, last_observation_year, event_year, time, event_status)

print("Summary of time-to-event and status per country:")
print(head(survival_summary))
print(paste("Total countries:", nrow(survival_summary)))
print(paste("Number of events (reaching High scarcity):", sum(survival_summary$event_status)))
print(paste("Censoring rate:", scales::percent(1 - mean(survival_summary$event_status))))


# --- 5. Check for Missing Data in Covariates ---
# Check NAs in the key predictors
missing_summary <- water_data_sorted %>%
  select(country, year, agri_use_pct, rainfall_mm, groundwater_depletion_pct) %>%
  summarise(across(everything(), ~sum(is.na(.))))

print("Missing values summary for key covariates:")
print(missing_summary) # Result: No missing values found.

# If there are missing values, we need a strategy:
# - Imputation (e.g., mean/median imputation, more complex methods)
# - Removal (listwise deletion - removing entire country or specific years)
# This needs careful consideration based on the extent and pattern of missingness.


# --- 6. Reshape data for Time-Varying Covariates ---
# Create (start_time, stop_time, event_status_interval) format needed for Stan model

# Join the overall event status back to the yearly data
water_data_with_status <- water_data_sorted %>%
  left_join(survival_summary %>% select(country, start_year, time, event_status), by = "country")

# Create interval start and stop times relative to the country's start year
# For each country, the intervals are [0, 1), [1, 2), ..., [T-1, T)
# where T is the total time for that country from survival_summary

final_survival_data <- water_data_with_status %>%
  group_by(country) %>%
  mutate(
    # Calculate time elapsed since the country's start year for each row
    time_elapsed = year - first(start_year), # using first() as start_year is constant per group
    # Define interval start and stop times
    t_start = time_elapsed,
    t_stop = time_elapsed + 1
  ) %>%
  # Important: Only keep intervals up to the final event/censoring time
  filter(t_start < time) %>%
  # Adjust t_stop for the last interval to match the final event/censoring time
  mutate(t_stop = ifelse(t_stop > time, time, t_stop)) %>%
  # Create the event status for *this interval*
  # The event only occurs at the end of the *last* interval for that country
  mutate(interval_event_status = ifelse(t_stop == time & event_status == 1, 1, 0)) %>%
  ungroup() %>%
  # Select relevant columns for modeling
  select(
    country,
    t_start, 
    t_stop, 
    interval_event_status, 
    # Include the time-varying covariates for this interval (year)
    agri_use_pct, 
    rainfall_mm, 
    groundwater_depletion_pct,
    # Keep original year and overall status for reference/checking if needed
    year, 
    overall_time = time, 
    overall_event_status = event_status 
  ) %>%
  # Optional: Filter out intervals where t_start >= t_stop (can happen if time=0)
  filter(t_start < t_stop)

print("Preview of final data structure for time-varying covariates:")
print(head(final_survival_data))
# Check a specific country that had an event
print("Example: Brazil (event at time=13)")
print(filter(final_survival_data, country == "Brazil"))
# Check a specific country that was censored
print("Example: Argentina (censored at time=24)")
print(filter(final_survival_data, country == "Argentina"))


# --- 7. Add Regional Grouping ---

# Get unique country list
unique_countries <- unique(final_survival_data$country)
# print("Unique countries in the dataset:")
# print(unique_countries) # Used to verify mapping below

# Create mapping from country to region (Continent)
# Based on the 20 unique countries present in this dataset.
region_vector <- case_when(
    unique_countries %in% c("Argentina", "Brazil", "Mexico") ~ "Americas", # Combined North & South
    unique_countries %in% c("Australia") ~ "Oceania",
    unique_countries %in% c("Canada", "USA") ~ "Americas", # Using USA instead of United States if that's in unique_countries
    unique_countries %in% c("China", "India", "Indonesia", "Japan", "Saudi Arabia", "Turkey", "South Korea") ~ "Asia",
    unique_countries %in% c("France", "Germany", "Italy", "Russia", "UK", "Spain") ~ "Europe", # Using UK if that's in unique_countries
    unique_countries %in% c("Egypt", "South Africa", "Nigeria") ~ "Africa",
    # Add a default or error check if needed
    TRUE ~ "Unknown" 
)

# Now create the mapping dataframe
region_map <- data.frame(
  country = unique_countries,
  region = region_vector
)

# Check if all countries were mapped
if (any(region_map$region == "Unknown")) {
  warning("Some countries were not mapped to a region. Please update the region_map.")
  print(filter(region_map, region == "Unknown"))
}

# Merge the region information into the main survival data
final_survival_data <- final_survival_data %>%
  left_join(region_map, by = "country") %>%
  # Convert region to factor, important for modeling (especially random effects)
  mutate(region = as.factor(region))

print("Preview of final data with Region column added:")
print(head(final_survival_data))
print("Summary of regions assigned:")
print(summary(final_survival_data$region))


# --- Next Steps (Revised) ---
# 1-7: Data loading, cleaning, survival definition, TVC reshaping, region mapping DONE.
# 8. Handle Missing Data: DONE (no missing covariates found).
# 9. Perform EDA: See eda.R.
# 10. Implement Stan model: See stan_model.R and weibull_hierarchical_tvc.stan.


# --- Save the processed data ---
# Optional: Save the final data frame for use in modeling scripts
saveRDS(final_survival_data, "processed_water_survival_data.rds")
print("Processed data saved to processed_water_survival_data.rds") 