# Load libraries
require(survival)
require(survminer)

# Fit Cox proportional hazards model
# Survival object records "death" if year reaches high water scarcity level (scarcity_index > 0.5)
cox_model <- coxph(Surv(year, scarcity_index > 0.5) ~ groundwater_dep + rainfall_annual + agricultural,
                   data = df)

# Check proportional hazards assumption
check <- cox.zph(cox_model)
print("Proportional Hazards assumption check:")
print(check)

# Inspect fitted model
print("Cox PH model summary:")
print(summary(cox_model))

# Graph survival plot
ggsurvplot(survfit(cox_model), data = df, xlim = c(2000, 2024))






