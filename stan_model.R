# Load necessary libraries
library(tidyverse)
library(cmdstanr)
library(posterior)

# --- Load Processed Data ---
file_path <- "processed_water_survival_data.rds"
if (!file.exists(file_path)) {
  stop(paste("Error: Processed data file not found at", file_path, 
             ". Please run data_prep.R first."))
}
survival_data <- readRDS(file_path)

# --- Prepare Data for Stan ---
print("Preparing data list for Stan...")

# Scale covariates (mean 0, sd 1) - Improves sampler efficiency and interpretation of priors
covariates <- c("agri_use_pct", "rainfall_mm", "groundwater_depletion_pct")
survival_data_scaled <- survival_data %>%
  mutate(across(all_of(covariates), ~ scale(.)[,1])) # scale() returns matrix, take first col

# Create covariate matrix X
X_matrix <- survival_data_scaled %>%
  select(all_of(covariates)) %>%
  as.matrix()

# Create integer IDs for countries and regions (Stan needs 1-based indices)
country_map <- data.frame(country = unique(survival_data$country), 
                          country_idx = 1:length(unique(survival_data$country)))
region_map_stan <- data.frame(region = levels(survival_data$region),
                            region_idx = 1:length(levels(survival_data$region)))

# Add indices to the data
survival_data_indexed <- survival_data_scaled %>%
  left_join(country_map, by = "country") %>%
  left_join(region_map_stan, by = "region")
  
# Create mapping from country index to region index (length = N_countries)
country_to_region_map <- survival_data_indexed %>%
  distinct(country_idx, .keep_all = TRUE) %>%
  arrange(country_idx) %>%
  pull(region_idx)
  
# Ensure region is factor before getting N_regions
survival_data_indexed <- survival_data_indexed %>%
    mutate(region = factor(region, levels = levels(survival_data$region)))

# Create the final data list for Stan
stan_data <- list(
  N_intervals = nrow(survival_data_indexed),
  N_countries = length(unique(survival_data_indexed$country)),
  N_regions = length(levels(survival_data_indexed$region)),
  K = ncol(X_matrix),
  t_start = survival_data_indexed$t_start,
  t_stop = survival_data_indexed$t_stop,
  event = survival_data_indexed$interval_event_status,
  country_id = survival_data_indexed$country_idx,
  region_id = country_to_region_map, # Map country index to region index
  X = X_matrix
)

# Verify list structure
print("Stan data list structure:")
str(stan_data)

# --- Compile and Run Stan Model ---

# Set path to Stan model file
stan_file <- "weibull_hierarchical_tvc.stan"
if (!file.exists(stan_file)) {
  stop(paste("Stan model file not found at", stan_file))
}

# Compile the model (this might take a minute the first time)
print("Compiling Stan model...")
model <- cmdstan_model(stan_file)
print("Model compiled.")

# Run the sampler
# Using defaults: 4 chains, 1000 warmup, 1000 sampling iterations
# Increase iterations if needed for convergence
print("Running Stan sampler...")
fit <- model$sample(
  data = stan_data,
  seed = 123, # for reproducibility
  chains = 4,
  parallel_chains = 4, # Run chains in parallel if possible
  iter_warmup = 1000,
  iter_sampling = 1500, # Increased sampling iterations slightly
  refresh = 500, # How often to print progress
  adapt_delta = 0.95 # Increased from default 0.85 to reduce divergences
)

print("Sampling finished.")

# --- Save Stan Fit Object ---
fit_file <- "stan_weibull_fit.rds"
fit$save_object(file = fit_file)
print(paste("Stan fit object saved to", fit_file))

# --- Basic Diagnostics ---
print("--- Stan Fit Summary (Key Parameters) ---")
# Use posterior::summarise_draws for specific parameters
fit$summary(variables = c("alpha", "beta", "mu_lambda", "sigma_lambda"))

print("--- Stan Diagnostics Summary ---")
fit$diagnostic_summary()

print("Stan model script finished.") 