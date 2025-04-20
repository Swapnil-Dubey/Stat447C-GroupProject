d# Bayesian Model Analysis Script

library(tidyverse)
library(posterior)
library(bayesplot)
library(patchwork)

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

fit_file <- "models/stan_weibull_fit.rds"
fit <- readRDS(fit_file)

print("Stan fit object loaded.")


print("Overall MCMC Diagnostics Summary")

fit$diagnostic_summary()

draws_df <- fit$draws(
    variables = c("alpha", "beta", "mu_lambda", "sigma_lambda", "lambda"),
    format = "df"
)


if ("beta[1]" %in% colnames(draws_df)) {
    draws_df <- draws_df %>%
        rename(
            beta_agri = `beta[1]`,
            beta_rain = `beta[2]`,
            beta_gw = `beta[3]`
        )
}

print("Posterior draws extracted.")
str(draws_df)


print("Posterior Summaries (Key Parameters)")
param_summary <- summarise_draws(
    draws_df,
    default_summary_measures(),
    default_convergence_measures()
)
print(param_summary)

print("Hazard Ratios (exp(beta))")

beta_draws <- draws_df %>% select(starts_with("beta_"))
hr_draws <- exp(beta_draws)
colnames(hr_draws) <- gsub("beta_", "HR_", colnames(beta_draws))


hr_summary <- summarise_draws(
    hr_draws,
    ~ median(.),
    ~ quantile(., probs = c(0.025, 0.975))
)

print(hr_summary)

print("Generating posterior distribution plots...")

# Density plots for beta coefficients
p_dens_betas <- mcmc_dens_overlay(draws_df, pars = vars(beta_agri, beta_rain, beta_gw)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    labs(
        title = "Posterior Distributions for Covariate Effects (beta)",
        subtitle = "Effects on Log-Hazard Rate (Scaled Covariates)"
    )

# Density plots for hierarchical parameters
p_dens_hier <- mcmc_dens(draws_df, pars = vars(mu_lambda, sigma_lambda)) +
    labs(
        subtitle = "Mean (mu) and Std Dev (sigma) of log Baseline Scale (lambda)"
    )

# Density plots for Weibull shape
p_dens_alpha <- mcmc_dens(draws_df, pars = vars(alpha)) +
    labs(title = NULL)


p_dens_betas <- p_dens_betas + theme_report
p_dens_hier <- p_dens_hier + theme_report
p_dens_alpha <- p_dens_alpha + theme_report

# Combine some plots
combined_dens_plots <- (p_dens_betas) / (p_dens_hier + p_dens_alpha) +
    plot_layout(heights = c(2, 1)) +
    plot_annotation(title = "Posterior Parameter Distributions")

ggsave("output/bayesian/posterior_densities.png", combined_dens_plots, width = 8, height = 8, dpi = 300, bg = "white")
print("Posterior density plots saved to output/bayesian/posterior_densities.png")


processed_data_path <- "data/processed_water_survival_data.rds"
region_map <- readRDS(processed_data_path) %>%
    distinct(country, region) %>%
    left_join(data.frame(
        country = unique(readRDS(processed_data_path)$country),
        country_idx = 1:length(unique(readRDS(processed_data_path)$country))
    ), by = "country") %>%
    left_join(data.frame(
        region = levels(readRDS(processed_data_path)$region),
        region_idx = 1:length(levels(readRDS(processed_data_path)$region))
    ), by = "region") %>%
    distinct(region, region_idx) %>%
    arrange(region_idx)

lambda_vars <- paste0("lambda[", 1:nrow(region_map), "]")
draws_lambda <- fit$draws(variables = lambda_vars, format = "df")
colnames(draws_lambda)[1:nrow(region_map)] <- paste0("lambda_", region_map$region)

p_lambda_regions <- mcmc_areas(draws_lambda, regex_pars = "lambda_") +
    labs(
        title = "Posterior Distributions for Region-Specific Scale (lambda)",
        subtitle = "Higher lambda implies shorter baseline time-to-event"
    )

p_lambda_regions <- p_lambda_regions + theme_report

ggsave("output/bayesian/posterior_lambda_regions.png", p_lambda_regions, width = 8, height = 6, dpi = 300, bg = "white")
print("Region-specific lambda plots saved to output/bayesian/posterior_lambda_regions.png")



print("Generating MCMC diagnostic plots...")

# Trace plots for key parameters
p_trace <- mcmc_trace(draws_df, pars = vars(alpha, beta_agri, beta_rain, beta_gw, mu_lambda, sigma_lambda)) +
    labs(title = "MCMC Trace Plots")


p_trace <- p_trace + theme_report

ggsave("output/bayesian/mcmc_trace_plots.png", p_trace, width = 10, height = 8, dpi = 300, bg = "white")
print("Trace plots saved to output/bayesian/mcmc_trace_plots.png")

# R-hat values
p_rhat <- mcmc_rhat_hist(rhat(fit)) +
    labs(title = "Histogram of R-hat values")

# Effective Sample Size (ESS)
p_neff <- mcmc_neff_hist(neff_ratio(fit)) +
    labs(title = "Histogram of ESS / Total Samples Ratio")

p_rhat <- p_rhat + theme_report
p_neff <- p_neff + theme_report

combined_diag_plots <- p_rhat + p_neff

ggsave("output/bayesian/mcmc_convergence_plots.png", combined_diag_plots, width = 10, height = 5, dpi = 300, bg = "white")
print("Convergence diagnostic plots saved to output/bayesian/mcmc_convergence_plots.png")


print("Performing Posterior Predictive Checks")

# Load observed data to get original event status and region info
obs_data <- readRDS("data/processed_water_survival_data.rds")
y_obs <- obs_data$interval_event_status # Observed event indicators per interval

# Extract replicated event data (y_rep) from Stan fit
y_rep_draws <- fit$draws(variables = "event_rep", format = "matrix")


print(paste("Dimensions of y_rep_draws:", paste(dim(y_rep_draws), collapse = " x ")))


p_ppc_total_events <- ppc_stat(
    y = y_obs,
    yrep = y_rep_draws,
    stat = "sum"
) +
    labs(
        title = "PPC: Total Number of Events",
        subtitle = "Observed Total Events vs. Distribution from Model Simulations"
    ) +
    xaxis_text(size = 10) + theme_report

print(p_ppc_total_events)
ggsave("output/bayesian/ppc_total_events.png", p_ppc_total_events, width = 6, height = 5, dpi = 300, bg = "white")
print("PPC plot for total events saved to output/bayesian/ppc_total_events.png")

region_group <- obs_data$region


p_ppc_events_region <- ppc_stat_grouped(
    y = y_obs,
    yrep = y_rep_draws,
    group = region_group,
    stat = "sum"
) +
    labs(
        title = "PPC: Total Number of Events per Region",
        subtitle = "Observed vs. Simulated Event Counts by Region"
    ) +
    xaxis_text(size = 10) + theme_report

print(p_ppc_events_region)
ggsave("output/bayesian/ppc_events_region.png", p_ppc_events_region, width = 8, height = 6, dpi = 300, bg = "white")
print("PPC plot for events per region saved to output/bayesian/ppc_events_region.png")


print("Bayesian analysis script finished.")
