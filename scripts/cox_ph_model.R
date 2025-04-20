# Load libraries
require(survival)
require(survminer)
require(dplyr)
require(ggplot2) # Explicitly load for theme_update

# Set theme defaults for standard plots (white background, black text)
theme_set(theme_minimal())
theme_update(
    text = element_text(colour = "black"),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "black"),
    plot.title = element_text(colour = "black"),
    plot.subtitle = element_text(colour = "black"),
    legend.text = element_text(colour = "black"),
    legend.title = element_text(colour = "black"),
    strip.text = element_text(colour = "black"),
    # Keep plot background white
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
)

# Load processed data
survival_data <- readRDS("data/processed_water_survival_data.rds")

# Ensure region is factor if needed for stratification (not used here)
survival_data <- survival_data %>%
    mutate(region = factor(region))

# Create a distinct dataset for the Cox model (one row per country)
cox_data <- survival_data %>%
    distinct(country, .keep_all = TRUE)

# Fit Cox proportional hazards model using overall time and status
# Covariates used are the values *at the time of event/censoring* (or baseline)
# Note: This uses the covariate values from the *last* time interval for each country
# A more complex time-dependent Cox model would use the interval format directly.
cox_model <- coxph(Surv(overall_time, overall_event_status) ~ groundwater_depletion_pct + rainfall_mm + agri_use_pct,
    data = cox_data
)

# Check proportional hazards assumption
schoenfeld_check <- cox.zph(cox_model)
print("Proportional Hazards assumption check:")
print(schoenfeld_check)

# Graph Schoenfeld plot
p_schoenfeld <- ggcoxzph(schoenfeld_check)
# Apply theme update manually to each plot in the list ggcoxzph returns
p_schoenfeld <- lapply(p_schoenfeld, function(p) p + theme_update())
print(p_schoenfeld)

# Inspect fitted model
print("Cox PH model summary:")
print(summary(cox_model))

# Extract hazard ratio confidence interval
ci <- confint(cox_model)
print("Hazard Ratio 95% confidence interval:")
print(exp(ci))

# Graph survival plot based on the fitted model
surv_fit_obj <- survfit(cox_model, data = cox_data) # Need to pass data here too
p_cox_surv <- ggsurvplot(surv_fit_obj,
    data = cox_data,
    # xlim = c(0, max(cox_data$overall_time)), # Use actual time range
    title = "Adjusted Survival Curve from Cox Model",
    xlab = "Time (Years)", ylab = "Survival Probability",
    ggtheme = theme_minimal()
) # Use minimal theme as base
# Apply theme update to plot and table components
p_cox_surv$plot <- p_cox_surv$plot + theme_update()
p_cox_surv$table <- p_cox_surv$table + theme_update()
print(p_cox_surv)

# Save survival plot
# ggsave should inherit the theme from theme_update applied to p_cox_surv$plot
ggsave("output/frequentist/cox_ph_survival_plot.png", plot = p_cox_surv$plot, width = 8, height = 6, bg = "white")

# Save Schoenfeld residuals plot
# ggsave should inherit the theme from theme_update applied to p_schoenfeld[[1]]
ggsave("output/frequentist/schoenfeld_plot.png", plot = p_schoenfeld[[1]], width = 10, height = 7, bg = "white") # ggcoxzph returns a list of plots

print("Cox PH model script finished.")
