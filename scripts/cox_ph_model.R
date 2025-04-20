# Load libraries
require(survival)
require(survminer)
require(dplyr)
require(ggplot2)

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


survival_data <- readRDS("data/processed_water_survival_data.rds")


survival_data <- survival_data %>%
    mutate(region = factor(region))


cox_data <- survival_data %>%
    distinct(country, .keep_all = TRUE)


cox_model <- coxph(Surv(overall_time, overall_event_status) ~ groundwater_depletion_pct + rainfall_mm + agri_use_pct,
    data = cox_data
)

# Check proportional hazards assumption
schoenfeld_check <- cox.zph(cox_model)
print("Proportional Hazards assumption check:")
print(schoenfeld_check)

# Graph Schoenfeld plot
p_schoenfeld_list <- ggcoxzph(schoenfeld_check)
p_schoenfeld_themed_list <- lapply(p_schoenfeld_list, function(p) p + theme_report)


print("Cox PH model summary:")
print(summary(cox_model))

# Graph survival plot based on the fitted model
surv_fit_obj <- survfit(cox_model, data = cox_data)
p_cox_surv <- ggsurvplot(surv_fit_obj,
    data = cox_data,
    title = "Adjusted Survival Curve from Cox Model",
    xlab = "Time (Years)", ylab = "Survival Probability",
    ggtheme = theme_minimal()
)

p_cox_surv$plot <- p_cox_surv$plot + theme_report
p_cox_surv$table <- p_cox_surv$table + theme_report



ggsave("output/frequentist/cox_ph_survival_plot.png", plot = p_cox_surv$plot, width = 8, height = 6, bg = "white")
ggsave("output/frequentist/schoenfeld_plot.png", plot = p_schoenfeld_themed_list[[1]], width = 10, height = 7, bg = "white")

print("Cox PH model script finished.")
