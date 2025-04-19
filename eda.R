# Load necessary libraries
library(tidyverse)
library(survival)
library(survminer) # For enhanced survival plots (ggsurvplot)
library(patchwork) # For combining plots
library(ggcorrplot) # For visualizing correlation matrix

# --- Load Processed Data (for KM plots) ---
processed_file_path <- "processed_water_survival_data.rds"
if (!file.exists(processed_file_path)) {
  stop(paste(
    "Error: Processed data file not found at", processed_file_path,
    ". Please run data_prep.R first."
  ))
}
final_survival_data <- readRDS(processed_file_path)

# --- Load Original Data (for histograms and boxplots of overall distributions) ---
original_file_path <- "cleaned_global_water_consumption.csv"
if (!file.exists(original_file_path)) {
  stop(paste("Error: Original data file not found at", original_file_path))
}
original_water_data <- read_csv(original_file_path, show_col_types = FALSE)

# Rename columns consistently with data_prep.R
original_water_data <- original_water_data %>%
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
  ) %>%
  # Ensure scarcity_level is a factor with desired order
  mutate(scarcity_level = factor(scarcity_level, levels = c("Low", "Moderate", "High")))


# --- Histograms of Key Predictors (using original full data) ---
print("Generating histograms...")
hist_agri <- ggplot(original_water_data, aes(x = agri_use_pct)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  ggtitle("Distribution of Agricultural Water Use (%)") +
  theme_minimal()

hist_rain <- ggplot(original_water_data, aes(x = rainfall_mm)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  ggtitle("Distribution of Annual Rainfall (mm)") +
  theme_minimal()

hist_gw <- ggplot(original_water_data, aes(x = groundwater_depletion_pct)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  ggtitle("Distribution of Groundwater Depletion Rate (%)") +
  theme_minimal()

# Combine and save histograms
plot_histograms <- hist_agri + hist_rain + hist_gw + plot_layout(ncol = 1)
png("histograms_predictors.png", width = 6, height = 10, units = "in", res = 300)
print(plot_histograms)
dev.off()
print("Histograms saved to histograms_predictors.png")

# --- Boxplots of Predictors vs. Original Scarcity Level (using original full data) ---
print("Generating boxplots vs scarcity level...")
box_agri <- ggplot(original_water_data, aes(x = scarcity_level, y = agri_use_pct, fill = scarcity_level)) +
  geom_boxplot() +
  ggtitle("Agricultural Use (%) vs. Scarcity Level") +
  theme_minimal() +
  theme(legend.position = "none")

box_rain <- ggplot(original_water_data, aes(x = scarcity_level, y = rainfall_mm, fill = scarcity_level)) +
  geom_boxplot() +
  ggtitle("Rainfall (mm) vs. Scarcity Level") +
  theme_minimal() +
  theme(legend.position = "none")

box_gw <- ggplot(original_water_data, aes(x = scarcity_level, y = groundwater_depletion_pct, fill = scarcity_level)) +
  geom_boxplot() +
  ggtitle("Groundwater Depletion (%) vs. Scarcity Level") +
  theme_minimal() +
  theme(legend.position = "none")

# Combine and save boxplots
plot_boxplots <- box_agri + box_rain + box_gw + plot_layout(ncol = 1)
png("boxplots_vs_scarcity.png", width = 6, height = 10, units = "in", res = 300)
print(plot_boxplots)
dev.off()
print("Boxplots saved to boxplots_vs_scarcity.png")

# --- Correlation Analysis of Potential Predictors (using original full data) ---
print("Calculating and plotting predictor correlations...")

# Select all potential numeric predictors
potential_predictors <- original_water_data %>%
  select(
    agri_use_pct,
    rainfall_mm,
    groundwater_depletion_pct,
    total_consumption,
    per_capita_use,
    industrial_use_pct,
    household_use_pct
  )

# Calculate correlation matrix (handling potential NAs just in case)
cor_matrix <- cor(potential_predictors, use = "complete.obs")

print("--- Correlation Matrix --- ")
print(round(cor_matrix, 2)) # Print rounded matrix

# Visualize the correlation matrix
plot_corr <- ggcorrplot(cor_matrix,
  method = "square", # Use squares for boxes
  type = "lower", # Show lower triangle
  lab = TRUE, # Show correlation coefficients
  lab_size = 3,
  title = "Correlation Matrix of Potential Predictors",
  ggtheme = theme_minimal()
) +
  theme(legend.position = "right")

png("correlation_matrix_predictors.png", width = 8, height = 7, units = "in", res = 300)
print(plot_corr)
dev.off()
print("Correlation matrix plot saved to correlation_matrix_predictors.png")


# --- Kaplan-Meier Survival Plots (using processed data) ---

# Create Surv object from processed data (one row per country summary)
km_data <- final_survival_data %>%
  distinct(country, .keep_all = TRUE)

surv_obj_overall <- Surv(time = km_data$overall_time, event = km_data$overall_event_status)

# --- Overall Kaplan-Meier Plot ---
print("Generating overall Kaplan-Meier plot...")
km_fit_overall <- survfit(surv_obj_overall ~ 1, data = km_data)

# Plot using ggsurvplot
plot_overall_km <- ggsurvplot(
  km_fit_overall,
  data = km_data,
  conf.int = TRUE,
  pval = FALSE,
  risk.table = TRUE,
  title = "Overall Kaplan-Meier Estimate of Time to High Water Scarcity",
  xlab = "Time (Years since observation start)",
  ylab = "Survival Probability (Not Reaching High Scarcity)",
  legend = "none",
  ggtheme = theme_minimal()
)

# Save the plot (robust method)
png("km_plot_overall.png", width = 8, height = 6, units = "in", res = 300)
print(plot_overall_km)
dev.off()
print("Overall KM plot saved to km_plot_overall.png")


# --- Kaplan-Meier Plot Stratified by Region ---
print("Generating Kaplan-Meier plot stratified by region...")

surv_obj_region <- Surv(time = km_data$overall_time, event = km_data$overall_event_status)

km_fit_region <- survfit(surv_obj_region ~ region, data = km_data)

# Plot using ggsurvplot
plot_region_km <- ggsurvplot(
  km_fit_region,
  data = km_data,
  conf.int = TRUE,
  pval = TRUE, # Show log-rank test p-value comparing regions
  risk.table = TRUE,
  title = "Kaplan-Meier Estimate by Region",
  xlab = "Time (Years since observation start)",
  ylab = "Survival Probability",
  legend.title = "Region",
  legend.labs = levels(km_data$region),
  ggtheme = theme_minimal()
)

# Save the plot (robust method)
png("km_plot_by_region.png", width = 10, height = 7, units = "in", res = 300)
print(plot_region_km)
dev.off()
print("Regional KM plot saved to km_plot_by_region.png")

# --- Optional: Examine Covariate Distributions ---
# e.g., Boxplots of baseline covariates by region or event status
# ggplot(km_data, aes(x = region, y = agri_use_pct)) + geom_boxplot()
# ggplot(km_data, aes(x = as.factor(overall_event_status), y = rainfall_mm)) + geom_boxplot()

print("EDA script finished.")
