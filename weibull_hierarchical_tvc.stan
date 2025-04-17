// Hierarchical Weibull Survival Model with Time-Varying Covariates

// Data Block
data {
  int<lower=1> N_intervals;       // Total number of intervals (rows in final_survival_data)
  int<lower=1> N_countries;       // Total number of unique countries
  int<lower=1> N_regions;         // Total number of unique regions
  int<lower=1> K;                 // Number of covariates
  
  vector[N_intervals] t_start;    // Start time of each interval (relative to country start)
  vector[N_intervals] t_stop;     // Stop time of each interval
  array[N_intervals] int<lower=0, upper=1> event; // Event indicator for *each interval* (1 if event happens at t_stop, 0 otherwise)
  
  array[N_intervals] int<lower=1, upper=N_countries> country_id; // Country ID for each interval
  array[N_countries] int<lower=1, upper=N_regions> region_id;  // Region ID for each *country* (length N_countries)
  
  matrix[N_intervals, K] X;      // Covariate matrix for each interval
}

// Parameters Block
parameters {
  real<lower=0> alpha;             // Weibull shape parameter (common across regions)
  
  vector[K] beta;                // Coefficients for covariates (common across regions)
  
  // Hierarchical part for baseline hazard (intercept)
  real mu_lambda;                 // Mean log baseline scale parameter across regions 
  real<lower=0> sigma_lambda;      // Standard deviation of log baseline scale parameter across regions
  vector[N_regions] eta_lambda;    // Region-specific deviations (standardized)
}

// Transformed Parameters Block
transformed parameters {
  // Calculate region-specific log scale parameters (lambda) from hierarchical structure
  // lambda = exp(log_lambda) is the Weibull scale parameter
  vector[N_regions] log_lambda = mu_lambda + sigma_lambda * eta_lambda;
  
  // Map region-specific log_lambda to each interval via country_id
  vector[N_intervals] interval_log_lambda; 
  for (i in 1:N_intervals) {
    interval_log_lambda[i] = log_lambda[region_id[country_id[i]]];
  }
  
  // Calculate linear predictor for each interval
  vector[N_intervals] log_hazard_ratio = X * beta;
}

// Model Block
model {
  // --- Priors ---
  alpha ~ gamma(1, 1);             // Weakly informative prior for shape
  beta ~ normal(0, 2);           // Regularizing priors for coefficients
  
  mu_lambda ~ normal(0, 5);        // Prior for mean log baseline scale
  sigma_lambda ~ cauchy(0, 2.5);     // Prior for std dev of log baseline scale (half-Cauchy recommended)
  eta_lambda ~ std_normal();       // Prior for standardized region deviations
  
  // --- Likelihood ---
  // Using the piecewise exponential formulation for Weibull likelihood 
  // to handle time-varying covariates defined over intervals [t_start, t_stop].
  // See, e.g., Stan User's Guide section on Survival Analysis.
  // Integrated hazard H(t_start, t_stop) = exp(log_lambda_r + X*beta) * (t_stop^alpha - t_start^alpha)
  
  vector[N_intervals] log_hazard_contribution = interval_log_lambda + log_hazard_ratio;
  
  for (i in 1:N_intervals) {
    // Add small epsilon to t_stop for numerical stability when alpha is near 1 and t_stop might be 0?
    // No, t_stop should always be > t_start >= 0.
    // Check for t_stop == 0? No, t_stop=time_elapsed+1 or time.
    // Only potential issue is log(t_stop[i]) if t_stop[i] is 0. This happens only if time=0 and event=1.
    // Let's guard log(t_stop)
    real log_t_stop = (t_stop[i] > 0) ? log(t_stop[i]) : -1e9; // Assign large negative log if t_stop is 0
    
    if (event[i] == 1) {
      // Contribution for intervals ending in an event
      // log(h(t_stop)) + log(S(t_stop)) - log(S(t_start))
      // log(h(t_stop)) = log(alpha) + (alpha - 1) * log(t_stop) + log_lambda_r[i] + X_i * beta
      // Simplified using integrated hazard:
      // log_lik += log(h(t_stop)) - H(t_start, t_stop)
      target += log(alpha) + (alpha - 1) * log_t_stop + log_hazard_contribution[i] 
                - exp(log_hazard_contribution[i]) * (pow(t_stop[i], alpha) - pow(t_start[i], alpha));
    } else {
      // Contribution for censored intervals (event[i] == 0)
      // log(S(t_stop)) - log(S(t_start))
      // log_lik += -H(t_start, t_stop)
      target += - exp(log_hazard_contribution[i]) * (pow(t_stop[i], alpha) - pow(t_start[i], alpha));
    }
  }
}

// Generated Quantities Block (Optional)
generated quantities {
  vector[N_regions] lambda = exp(log_lambda); // Weibull scale parameter per region
  // Can calculate hazard ratios, posterior predictions etc.
} 