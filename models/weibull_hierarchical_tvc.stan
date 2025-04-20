// Hierarchical Weibull Survival Model with Time-Varying Covariates

data {
  int<lower=1> N_intervals;       // Total number of intervals
  int<lower=1> N_countries;       // Total number of unique countries
  int<lower=1> N_regions;         // Total number of unique regions
  int<lower=1> K;                 // Number of covariates
  
  vector[N_intervals] t_start;    // Start time of each interval (relative to country start)
  vector[N_intervals] t_stop;     // Stop time of each interval
  array[N_intervals] int<lower=0, upper=1> event; // indicator for each interval (1 if event happens at t_stop, 0 otherwise)
  
  array[N_intervals] int<lower=1, upper=N_countries> country_id; // Country ID for each interval
  array[N_countries] int<lower=1, upper=N_regions> region_id;  //Region ID for each country (length N_countries)
  
  matrix[N_intervals, K] X;      //Covariate matrix for each interval
}


parameters {
  real<lower=0> alpha;             // Weibull shape parameter (common across regions)
  vector[K] beta;                // Coefficients for covariates (common across regions)
  
  // Hierarchical part for baseline hazard (intercept)
  real mu_lambda;                 // Mean log baseline scale parameter across regions 
  real<lower=0> sigma_lambda;      // Standard deviation of log baseline scale parameter across regions
  vector[N_regions] eta_lambda;    // Region specific deviations (standardized)
}


transformed parameters {
  // Calculate region specific log scale parameters (lambda) from hierarchical structure
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


model {
  // Priors 
  alpha ~ gamma(1, 1);             // Weakly informative prior for shape
  beta ~ normal(0, 2);           // Regularizing priors for coefficients
  
  mu_lambda ~ normal(0, 5);        // Prior for mean log baseline scale
  sigma_lambda ~ normal(0, 1);     // Prior for std dev of log baseline scale (Half-Normal)
  eta_lambda ~ std_normal();       // Prior for standardized region deviations
  

  // Using the piecewise exponential formulation for Weibull likelihood 
  // to handle time-varying covariates defined over intervals [t_start, t_stop].
  
  vector[N_intervals] log_hazard_contribution = interval_log_lambda + log_hazard_ratio;
  
  for (i in 1:N_intervals) {
    real log_t_stop = (t_stop[i] > 0) ? log(t_stop[i]) : -1e9; // Assign large negative log if t_stop is 0
    
    if (event[i] == 1) {
      // Contribution for intervals ending in an event
      target += log(alpha) + (alpha - 1) * log_t_stop + log_hazard_contribution[i] 
                - exp(log_hazard_contribution[i]) * (pow(t_stop[i], alpha) - pow(t_start[i], alpha));
    } else {
      // Contribution for censored intervals (event[i] == 0)
      target += - exp(log_hazard_contribution[i]) * (pow(t_stop[i], alpha) - pow(t_start[i], alpha));
    }
  }
}


generated quantities {
  vector[N_regions] lambda = exp(log_lambda); // Weibull scale parameter per region
  
  // Posterior Predictive Checks 
  // Simulate event indicator for each interval based on model parameters
  array[N_intervals] int<lower=0, upper=1> event_rep;
  {
    // Recalculate interval-specific log_lambda and log_hazard_ratio 
    // These are based on the current posterior draw of parameters
    vector[N_regions] current_log_lambda = mu_lambda + sigma_lambda * eta_lambda;
    vector[N_intervals] current_interval_log_lambda; 
    vector[N_intervals] current_log_hazard_ratio = X * beta;
    vector[N_intervals] current_log_hazard_contribution;
    
    for (i in 1:N_intervals) {
      current_interval_log_lambda[i] = current_log_lambda[region_id[country_id[i]]];
    }
    current_log_hazard_contribution = current_interval_log_lambda + current_log_hazard_ratio;

    // Loop through intervals and simulate event status
    for (i in 1:N_intervals) {
      // Calculate integrated hazard for the interval H(t_start, t_stop)
      real H_interval = exp(current_log_hazard_contribution[i]) * 
                         (pow(t_stop[i], alpha) - pow(t_start[i], alpha));
                         
      // Calculate probability of event within this interval, given survival up to t_start
      real prob_event_interval = 1.0 - exp(-fmax(0.0, H_interval)); 
      
      // Simulate the event indicator for this interval
      event_rep[i] = bernoulli_rng(prob_event_interval);
    }
  }
} 