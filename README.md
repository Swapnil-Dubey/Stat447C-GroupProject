# Stat447C Group Project: Bayesian and Frequentist Survival Analysis of Water Scarcity

This project analyzes the time until countries reach high water scarcity levels using data from the Kaggle "Global Water Consumption Dataset (2000-2024)". It implements and compares two survival analysis approaches:

1.  A **Bayesian hierarchical Weibull model** with time-varying covariates, implemented in Stan, to account for regional variations.
2.  A standard **Cox Proportional Hazards model** (frequentist approach) for comparison.

## Team Members

*   Swapnil Dubey
*   Lawrence Ma

## Project Structure

```
.
├── data/
│   ├── cleaned_global_water_consumption.csv  # Raw input data
│   └── processed_water_survival_data.rds     # Processed data for modeling
├── models/
│   ├── weibull_hierarchical_tvc.stan         # Stan code for the Bayesian model
│   └── stan_weibull_fit.rds                # Saved MCMC results from Stan
├── output/
│   ├── eda/                                # Plots from Exploratory Data Analysis
│   │   ├── *.png
│   ├── bayesian/                           # Plots from Bayesian Analysis
│   │   ├── *.png
│   └── frequentist/                        # Plots from Cox Model Analysis
│       ├── *.png
├── scripts/
│   ├── data_prep.R                         # Data loading, cleaning, survival definition, TVC processing
│   ├── eda.R                               # Exploratory Data Analysis and plotting
│   ├── stan_model.R                        # Bayesian model fitting using Stan
│   ├── bayesian_analysis.R                 # Analysis of Bayesian model results (diagnostics, PPCs, plots)
│   └── cox_ph_model.R                      # Frequentist Cox PH model fitting and diagnostics
├── .gitignore
└── README.md                             # This file
```

## Workflow & How to Run

**Prerequisites:**

*   R installed.
*   Required R packages: `tidyverse`, `lubridate`, `survival`, `survminer`, `patchwork`, `ggcorrplot`, `cmdstanr`, `posterior`, `bayesplot`. You can install them using `install.packages(c("tidyverse", "lubridate", ...))`.
*   `cmdstanr` configured (run `cmdstanr::install_cmdstan()` if needed).

**Execution Order:**

Run the following scripts from the **project root directory** using `Rscript`:

1.  `Rscript scripts/data_prep.R`
    *   Reads `data/cleaned_global_water_consumption.csv`.
    *   Defines the survival outcome (time to first "High" scarcity level).
    *   Processes time-varying covariates.
    *   Adds regional groupings.
    *   Saves processed data to `data/processed_water_survival_data.rds`.

2.  `Rscript scripts/eda.R`
    *   Loads processed and original data.
    *   Generates exploratory plots (histograms, boxplots, correlations, Kaplan-Meier curves).
    *   Saves plots to `output/eda/`.

3.  `Rscript scripts/stan_model.R`
    *   Loads processed data (`data/processed_water_survival_data.rds`).
    *   Scales covariates and prepares data for Stan.
    *   Compiles (`models/weibull_hierarchical_tvc.stan`) and runs the Stan sampler (this may take several minutes).
    *   Saves the MCMC fit object to `models/stan_weibull_fit.rds`.

4.  `Rscript scripts/bayesian_analysis.R`
    *   Loads the Stan fit object (`models/stan_weibull_fit.rds`).
    *   Performs MCMC diagnostics.
    *   Calculates posterior summaries and Hazard Ratios.
    *   Generates posterior distribution plots and Posterior Predictive Checks (PPCs).
    *   Saves plots to `output/bayesian/`.

5.  `Rscript scripts/cox_ph_model.R`
    *   Loads processed data (`data/processed_water_survival_data.rds`).
    *   Fits a standard Cox Proportional Hazards model.
    *   Performs proportional hazards assumption checks (Schoenfeld residuals).
    *   Generates survival and diagnostic plots.
    *   Saves plots to `output/frequentist/`.

## Models

*   **Bayesian Model (`models/weibull_hierarchical_tvc.stan`):** A hierarchical Weibull survival model accommodating time-varying covariates using a piecewise exponential approximation. It includes random intercepts for regions (`lambda`) to capture baseline hazard variations.
*   **Cox Model (`scripts/cox_ph_model.R`):** A standard semi-parametric Cox Proportional Hazards model fitted using `survival::coxph`.

## Outputs

*   **Processed Data:** `data/processed_water_survival_data.rds` contains the data formatted for both survival models.
*   **Bayesian Fit:** `models/stan_weibull_fit.rds` contains the full MCMC output.
*   **Plots:** Located in `output/eda/`, `output/bayesian/`, and `output/frequentist/`, visualizing data exploration, model diagnostics, and results.
