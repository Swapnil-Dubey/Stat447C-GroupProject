# Bayesian Model Analysis Script

# --- 1. Load Libraries ---
library(tidyverse)
library(posterior)
library(bayesplot) # For MCMC diagnostics and posterior visualizations
library(patchwork) # To combine plots

# Set bayesplot theme
color_scheme_set("brightblue")
theme_set(theme_minimal())
# Update theme elements for standard plots (white background, black text)
theme_update(
    text = element_text(colour = "black"), # Change text color to black
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

# --- 2. Load Stan Fit Object ---
fit_file <- "models/stan_weibull_fit.rds"
if (!file.exists(fit_file)) {
    stop(paste(
        "Stan fit file not found:", fit_file,
        "\nPlease run stan_model.R first."
    ))
}
fit <- readRDS(fit_file)

print("Stan fit object loaded.")

# --- 3. Check Basic Diagnostics Summary ---
print("--- Overall MCMC Diagnostics Summary ---")
# Provides Rhat, ESS (Bulk and Tail)
fit$diagnostic_summary()
# Note: Check for warnings about divergences, Rhat > 1.01, low ESS.

# --- 4. Extract Posterior Draws ---
# Extract draws for key parameters into a convenient format
draws_df <- fit$draws(
    variables = c("alpha", "beta", "mu_lambda", "sigma_lambda", "lambda"),
    format = "df"
)

# Reminder: Mapping for beta coefficients (based on stan_model.R scaling order)
# beta[1]: agri_use_pct (scaled)
# beta[2]: rainfall_mm (scaled)
# beta[3]: groundwater_depletion_pct (scaled)

# Optional: Rename beta columns for clarity
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

# --- 5. Calculate Posterior Summaries ---
print("--- Posterior Summaries (Key Parameters) ---")
param_summary <- summarise_draws(
    draws_df,
    default_summary_measures(),
    default_convergence_measures()
)
print(param_summary)

# --- 5b. Calculate and Interpret Hazard Ratios (HR) ---
print("--- Hazard Ratios (exp(beta)) ---")

# Extract beta draws specifically
beta_draws <- draws_df %>% select(starts_with("beta_"))

# Calculate HR = exp(beta)
hr_draws <- exp(beta_draws)
colnames(hr_draws) <- gsub("beta_", "HR_", colnames(beta_draws))

# Summarize HRs (e.g., median and 95% Credible Interval)
hr_summary <- summarise_draws(
    hr_draws,
    ~ median(.),
    ~ quantile(., probs = c(0.025, 0.975))
)

print(hr_summary)

# Interpretation Guidance:
# - HR = 1: No effect on hazard compared to baseline.
# - HR > 1: Predictor increases the hazard (shortens time-to-event).
#           A one-unit increase in the predictor multiplies the hazard by HR.
# - HR < 1: Predictor decreases the hazard (lengthens time-to-event).
#           A one-unit increase in the predictor multiplies the hazard by HR.
# - Remember: Betas relate to *scaled* predictors (1 unit = 1 standard deviation).
# - Check if the 95% CI for HR overlaps 1. If it does, the effect is not statistically significant at the alpha=0.05 level.

# You can access specific HR summaries, e.g.:
# hr_summary %>% filter(variable == "HR_agri")

# --- 6. Visualize Posterior Distributions ---
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

# Combine some plots
combined_dens_plots <- (p_dens_betas) / (p_dens_hier + p_dens_alpha) +
    plot_layout(heights = c(2, 1)) +
    plot_annotation(title = "Posterior Parameter Distributions")

ggsave("output/bayesian/posterior_densities.png", combined_dens_plots, width = 8, height = 8, dpi = 300)
print("Posterior density plots saved to output/bayesian/posterior_densities.png")

# Visualize Region-specific baseline scales (lambda)
# Need region mapping
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

ggsave("output/bayesian/posterior_lambda_regions.png", p_lambda_regions, width = 8, height = 6, dpi = 300)
print("Region-specific lambda plots saved to output/bayesian/posterior_lambda_regions.png")


# --- 7. Visualize MCMC Diagnostics ---
print("Generating MCMC diagnostic plots...")

# Trace plots for key parameters
p_trace <- mcmc_trace(draws_df, pars = vars(alpha, beta_agri, beta_rain, beta_gw, mu_lambda, sigma_lambda)) +
    labs(title = "MCMC Trace Plots")

ggsave("output/bayesian/mcmc_trace_plots.png", p_trace, width = 10, height = 8, dpi = 300)
print("Trace plots saved to output/bayesian/mcmc_trace_plots.png")

# R-hat values
p_rhat <- mcmc_rhat_hist(rhat(fit)) +
    labs(title = "Histogram of R-hat values")

# Effective Sample Size (ESS)
p_neff <- mcmc_neff_hist(neff_ratio(fit)) +
    labs(title = "Histogram of ESS / Total Samples Ratio")

combined_diag_plots <- p_rhat + p_neff

ggsave("output/bayesian/mcmc_convergence_plots.png", combined_diag_plots, width = 10, height = 5, dpi = 300)
print("Convergence diagnostic plots saved to output/bayesian/mcmc_convergence_plots.png")

# --- 8. Interpretation Notes ---
# - Review the posterior summaries (means, medians, CIs) for effect sizes and uncertainty.
# - Examine the density plots: Are distributions wide/narrow? Do CIs for betas overlap zero?
# - Check trace plots for good mixing (should look like fuzzy caterpillars with no trends).
# - Ensure R-hat values are close to 1.0 (e.g., < 1.01).
# - Ensure ESS values are adequate (e.g., > 400 for reliable CI estimation, higher is better).
# - Recall and report the number of divergences from the original Stan run (if any were noted).
# - Interpret alpha: alpha > 1 means hazard increases over time, alpha < 1 means hazard decreases, alpha = 1 means constant hazard (Exponential).
# - Interpret betas: Positive beta means covariate increases hazard (shortens time-to-event), negative beta means covariate decreases hazard (lengthens time-to-event).
# - Interpret lambda: Higher lambda means shorter baseline time-to-event.
# - Interpret mu_lambda and sigma_lambda: Describe average baseline and variability across regions.

# --- 9. Posterior Predictive Checks (PPCs) ---
print("--- Performing Posterior Predictive Checks ---")

# Load observed data to get original event status and region info
obs_data <- readRDS("data/processed_water_survival_data.rds")
y_obs <- obs_data$interval_event_status # Observed event indicators per interval

# Extract replicated event data (y_rep) from Stan fit
# Resulting matrix: rows = draws, columns = parameters (event_rep[1], event_rep[2], ...)
# This format (N_draws x N_intervals) is directly usable by bayesplot
y_rep_draws <- fit$draws(variables = "event_rep", format = "matrix")

# Check dimensions (should be N_draws x N_intervals)
print(paste("Dimensions of y_rep_draws:", paste(dim(y_rep_draws), collapse = " x ")))

# --- PPC: Total Number of Events ---
# Calculate total events in observed data
# total_events_obs <- sum(y_obs) # Let ppc_stat calculate this

# Calculate total events for each replicated dataset
# total_events_rep <- rowSums(y_rep_draws) # Let ppc_stat calculate this

# Visualize observed vs replicated total events
# Pass the full y_obs and y_rep_draws, specify stat = "sum"
p_ppc_total_events <- ppc_stat(
    y = y_obs,
    yrep = y_rep_draws,
    stat = "sum"
) +
    labs(
        title = "PPC: Total Number of Events",
        subtitle = "Observed Total Events vs. Distribution from Model Simulations"
    ) +
    xaxis_text(size = 10) # Adjust size as needed

print(p_ppc_total_events)
ggsave("output/bayesian/ppc_total_events.png", p_ppc_total_events, width = 6, height = 5, dpi = 300)
print("PPC plot for total events saved to output/bayesian/ppc_total_events.png")

# --- PPC: Number of Events per Region ---
# Get region factor from observed data
region_group <- obs_data$region

# Function to calculate sum per group
# sum_by_group <- function(y, group) { # Let ppc_stat_grouped handle this
#   tapply(y, group, sum)
# }

# Visualize observed vs replicated events per region
# Pass the full y_obs and y_rep_draws, specify stat = "sum"
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
    xaxis_text(size = 10) # Adjust size as needed

print(p_ppc_events_region)
ggsave("output/bayesian/ppc_events_region.png", p_ppc_events_region, width = 8, height = 6, dpi = 300)
print("PPC plot for events per region saved to output/bayesian/ppc_events_region.png")

# Interpretation Guidance for PPCs:
# - Look at the ppc_stat plots. The dark line/point represents the statistic calculated from the *observed* data.
# - The histogram shows the distribution of the same statistic calculated from the *replicated* datasets simulated by the model.
# - Good Fit: If the observed statistic falls comfortably within the bulk of the distribution of replicated statistics, it suggests the model captures this aspect of the data well.
# - Poor Fit: If the observed statistic falls far in the tails (or outside) of the replicated distribution, the model fails to capture this aspect of the data (model misspecification).
# - Consider if the model systematically over- or under-predicts the statistic.

print("Bayesian analysis script finished.")
print("Please review the generated plots and summary table.")
