library(tidyverse)
library(survival)
library(survminer)
library(patchwork)
library(ggcorrplot)

theme_report <- theme_minimal() +
  theme(
    text = element_text(colour = "black"),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "black"),
    plot.title = element_text(colour = "black", face = "bold"),
    plot.subtitle = element_text(colour = "black"),
    legend.text = element_text(colour = "black"),
    legend.title = element_text(colour = "black", face = "bold"),
    strip.text = element_text(colour = "black", face = "bold"),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "grey85"),
    panel.grid.minor = element_line(colour = "grey90")
  )

processed_file_path <- "data/processed_water_survival_data.rds"
final_survival_data <- readRDS(processed_file_path)

original_file_path <- "data/cleaned_global_water_consumption.csv"
original_water_data <- read_csv(original_file_path, show_col_types = FALSE)


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
  mutate(scarcity_level = factor(scarcity_level, levels = c("Low", "Moderate", "High")))


print("Generating histograms...")
hist_agri <- ggplot(original_water_data, aes(x = agri_use_pct)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  ggtitle("Distribution of Agricultural Water Use (%)") +
  theme_report

hist_rain <- ggplot(original_water_data, aes(x = rainfall_mm)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  ggtitle("Distribution of Annual Rainfall (mm)") +
  theme_report

hist_gw <- ggplot(original_water_data, aes(x = groundwater_depletion_pct)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  ggtitle("Distribution of Groundwater Depletion Rate (%)") +
  theme_report

# Combine and save histograms
plot_histograms <- hist_agri + hist_rain + hist_gw + plot_layout(ncol = 1)
ggsave("output/eda/histograms_predictors.png", plot_histograms, width = 10, height = 8, dpi = 300, bg = "white")
print("Histograms saved to output/eda/histograms_predictors.png")

print("Generating boxplots vs scarcity level...")
box_agri <- ggplot(original_water_data, aes(x = scarcity_level, y = agri_use_pct, fill = scarcity_level)) +
  geom_boxplot() +
  ggtitle("Agricultural Use (%) vs. Scarcity Level") +
  theme(legend.position = "none") +
  theme_report

box_rain <- ggplot(original_water_data, aes(x = scarcity_level, y = rainfall_mm, fill = scarcity_level)) +
  geom_boxplot() +
  ggtitle("Rainfall (mm) vs. Scarcity Level") +
  theme(legend.position = "none") +
  theme_report

box_gw <- ggplot(original_water_data, aes(x = scarcity_level, y = groundwater_depletion_pct, fill = scarcity_level)) +
  geom_boxplot() +
  ggtitle("Groundwater Depletion (%) vs. Scarcity Level") +
  theme(legend.position = "none") +
  theme_report

# Combine and save boxplots
plot_boxplots <- box_agri + box_rain + box_gw + plot_layout(ncol = 1)
ggsave("output/eda/boxplots_vs_scarcity.png", plot_boxplots, width = 10, height = 6, dpi = 300, bg = "white")
print("Boxplots saved to output/eda/boxplots_vs_scarcity.png")

print("Calculating and plotting predictor correlations...")
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

cor_matrix <- cor(potential_predictors, use = "complete.obs")

print("Correlation Matrix")
print(round(cor_matrix, 2))


plot_corr <- ggcorrplot(cor_matrix,
  method = "square",
  type = "lower",
  lab = TRUE,
  lab_size = 3,
  title = "Correlation Matrix of Potential Predictors"
)
plot_corr <- plot_corr + theme_report

ggsave("output/eda/correlation_matrix_predictors.png", plot_corr, width = 8, height = 7, dpi = 300, bg = "white")
print("Correlation matrix plot saved to output/eda/correlation_matrix_predictors.png")


# Kaplan-Meier Survival Plots (using processed data)

km_data <- final_survival_data %>%
  distinct(country, .keep_all = TRUE)

surv_obj_overall <- Surv(time = km_data$overall_time, event = km_data$overall_event_status)

print("Generating overall Kaplan-Meier plot...")
km_fit_overall <- survfit(surv_obj_overall ~ 1, data = km_data)

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

plot_overall_km$plot <- plot_overall_km$plot + theme_report
plot_overall_km$table <- plot_overall_km$table + theme_report

png("output/eda/km_plot_overall.png", width = 8, height = 6, units = "in", res = 300, bg = "white")
print(plot_overall_km)
dev.off()
print("Overall KM plot saved to output/eda/km_plot_overall.png")


#  Kaplan-Meier Plot Stratified by Region
print("Generating Kaplan-Meier plot stratified by region...")

surv_obj_region <- Surv(time = km_data$overall_time, event = km_data$overall_event_status)

km_fit_region <- survfit(surv_obj_region ~ region, data = km_data)

plot_region_km <- ggsurvplot(
  km_fit_region,
  data = km_data,
  conf.int = TRUE,
  pval = TRUE,
  risk.table = TRUE,
  title = "Kaplan-Meier Estimate by Region",
  xlab = "Time (Years since observation start)",
  ylab = "Survival Probability",
  legend.title = "Region",
  legend.labs = levels(km_data$region),
  ggtheme = theme_minimal()
)

plot_region_km$plot <- plot_region_km$plot + theme_report
plot_region_km$table <- plot_region_km$table + theme_report

png("output/eda/km_plot_by_region.png", width = 10, height = 8, units = "in", res = 300, bg = "white")
print(plot_region_km)
dev.off()
print("Regional KM plot saved to output/eda/km_plot_by_region.png")

print("EDA script finished.")
