library(tidyverse)
library(cmdstanr)
library(posterior)

file_path <- "data/processed_water_survival_data.rds"
survival_data <- readRDS(file_path)


covariates <- c("agri_use_pct", "rainfall_mm", "groundwater_depletion_pct")
survival_data_scaled <- survival_data %>%
  mutate(across(all_of(covariates), ~ scale(.)[, 1]))


X_matrix <- survival_data_scaled %>%
  select(all_of(covariates)) %>%
  as.matrix()

country_map <- data.frame(
  country = unique(survival_data$country),
  country_idx = 1:length(unique(survival_data$country))
)
region_map_stan <- data.frame(
  region = levels(survival_data$region),
  region_idx = 1:length(levels(survival_data$region))
)


survival_data_indexed <- survival_data_scaled %>%
  left_join(country_map, by = "country") %>%
  left_join(region_map_stan, by = "region")


country_to_region_map <- survival_data_indexed %>%
  distinct(country_idx, .keep_all = TRUE) %>%
  arrange(country_idx) %>%
  pull(region_idx)


survival_data_indexed <- survival_data_indexed %>%
  mutate(region = factor(region, levels = levels(survival_data$region)))


stan_data <- list(
  N_intervals = nrow(survival_data_indexed),
  N_countries = length(unique(survival_data_indexed$country)),
  N_regions = length(levels(survival_data_indexed$region)),
  K = ncol(X_matrix),
  t_start = survival_data_indexed$t_start,
  t_stop = survival_data_indexed$t_stop,
  event = survival_data_indexed$interval_event_status,
  country_id = survival_data_indexed$country_idx,
  region_id = country_to_region_map,
  X = X_matrix
)

print("Stan data list structure:")
str(stan_data)




stan_file <- "models/weibull_hierarchical_tvc.stan"


print("Compiling Stan model...")
model <- cmdstan_model(stan_file)
print("Model compiled.")


# Running sampler using defaults: 4 chains, 1000 warmup, 1000 sampling iterations
print("Running Stan sampler...")
fit <- model$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1500,
  refresh = 500,
  adapt_delta = 0.95
)

print("Sampling finished.")

# Save Stan Fit Object
fit_file <- "models/stan_weibull_fit.rds"
fit$save_object(file = fit_file)
print(paste("Stan fit object saved to", fit_file))

# Basic Diagnostics
print("Stan Fit Summary (Key Parameters) ")
fit$summary(variables = c("alpha", "beta", "mu_lambda", "sigma_lambda"))

print("Stan Diagnostics Summary ")
fit$diagnostic_summary()

print("Stan model script finished.")
