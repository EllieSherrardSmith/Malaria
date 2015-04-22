data {
  // Number of control rounds/bites = 16 rounds 1 to 4 and bites 2 to 5
  // (as all ATV were 0 for bite 1)
  int<lower=0> N_C;
  
  // Number of ATV treatment rounds (16)
  int<lower=0> N_T;
  
  // Number of oocysts in each round (45) - reduced to 24
  // to get rid of NAs from some groups
  int<lower=0> N_ooc;
  
  // Number of mice (5)
  int<lower=0> N_mice;
  
  // Raw data counts of the oocysts for each group of controls
  int<lower=0> ooc_count_C[N_ooc, N_C];
  
  // Raw data counts of the oocysts for each group of ATV treatments
  int<lower=0> ooc_count_T[N_ooc, N_T];
  
  // Infection prevalence of each mouse in each control group
  int<lower=0> prev_C[N_mice, N_C];
  
  // Infection prevalence of each mouse in each ATV treatment group
  int<lower=0> prev_T[N_mice, N_T];

/*
  // Sporozoite count binning
  int<lower=0> N_bin;
  int<lower=0> bin_edge[N_bin + 1];
  
  // Binned sporozoite counts for each control/treatment group
  int<lower=0> s_count_C[N_C, N_bin];
  int<lower=0> s_count_T[N_T, N_bin];
*/
}

transformed data {
  int N_bite;
  int N_round;
  real offset;
  
  N_bite <- 4;
  N_round <- 4;
  offset <- 4;
}

parameters {
  real rho__log_mean_ooc_C;
  real<lower=0> tau_bite__log_mean_ooc_C;
  real rho_bite__log_mean_ooc_C[N_bite];
  real<lower=0> tau_round__log_mean_ooc_C;
  real rho_round__log_mean_ooc_C[N_round];
  
  real rho__log_od_ooc_C;
  real<lower=0> tau_bite__log_od_ooc_C;
  real rho_bite__log_od_ooc_C[N_bite];
  real<lower=0> tau_round__log_od_ooc_C;
  real rho_round__log_od_ooc_C[N_round];
  
  real rho__log_mean_ooc_T;
  real<lower=0> tau_bite__log_mean_ooc_T;
  real rho_bite__log_mean_ooc_T[N_bite];
  real<lower=0> tau_round__log_mean_ooc_T;
  real rho_round__log_mean_ooc_T[N_round];
  
  real rho__log_od_ooc_T;
  real<lower=0> tau_bite__log_od_ooc_T;
  real rho_bite__log_od_ooc_T[N_bite];
  real<lower=0> tau_round__log_od_ooc_T;
  real rho_round__log_od_ooc_T[N_round];
  
  // Slopes and intercepts of GLM modeling conversion from oocyst counts
  // to sporozoite counts.  Common slopes for control and treatment groups
  // as the underlying process should be independent of treatment.
  vector[2] beta_mu;
  vector[2] beta_phi;
  real alpha_mu;
  real alpha_phi;
  
  // Slopes and intercept of GLM modeling infection probability from
  // sporozite count distribution.  Again, common parameters for control
  // and treatment as the underlying process should be independent of
  // treatment.
  vector[2] beta_theta;
  real alpha_theta;
}
  
model {
  
  // Log mean and log precision of control group oocyst count NegBin distribution
  vector[N_C] log_mu_ooc_C;
  vector[N_C] log_phi_ooc_C;
  
  // Log mean and log precision of treatment group oocyst count NegBin distribution
  vector[N_T] log_mu_ooc_T;
  vector[N_T] log_phi_ooc_T;
  

  // Log mean and log precision of control group sporozoite count NegBin distribution
  //vector[N_C] log_mu_s_C;
  //vector[N_C] log_phi_s_C;
  
  // Log mean and log precision of ATV treatment group sporozoite count NegBin distribution
  //vector[N_T] log_mu_s_T;
  //vector[N_T] log_phi_s_T;

  // Logit infection probability for each control/treatment group
  vector[N_C] logit_theta_C;
  vector[N_T] logit_theta_T;

  // Sporozoite count censoring model parameters
##  real sum_p;
##  vector[N_bin] p;
  
  // Priors
  rho__log_mean_ooc_C ~ normal(0, 2);
  tau_bite__log_mean_ooc_C ~ cauchy(0, 2.5);
  rho_bite__log_mean_ooc_C ~ normal(0, 1);
  tau_round__log_mean_ooc_C ~ cauchy(0, 2.5);
  rho_round__log_mean_ooc_C ~ normal(0, 1);
  
  rho__log_od_ooc_C ~ normal(0, 2);
  tau_bite__log_od_ooc_C ~ cauchy(0, 2.5);
  rho_bite__log_od_ooc_C ~ normal(0, 1);
  tau_round__log_od_ooc_C ~ cauchy(0, 2.5);
  rho_round__log_od_ooc_C ~ normal(0, 1);
  
  rho__log_mean_ooc_T ~ normal(0, 2);
  tau_bite__log_mean_ooc_T ~ cauchy(0, 2.5);
  rho_bite__log_mean_ooc_T ~ normal(0, 1);
  tau_round__log_mean_ooc_T ~ cauchy(0, 2.5);
  rho_round__log_mean_ooc_T ~ normal(0, 1);
  
  rho__log_od_ooc_T ~ normal(0, 2);
  tau_bite__log_od_ooc_T ~ cauchy(0, 2.5);
  rho_bite__log_od_ooc_T ~ normal(0, 1);
  tau_round__log_od_ooc_T ~ cauchy(0, 2.5);
  rho_round__log_od_ooc_T ~ normal(0, 1);
  
  beta_mu ~ normal(0, 4);
  beta_phi ~ normal(0, 4);
  beta_theta ~ normal(0, 4);
  
  alpha_mu ~ normal(0, 8);
  alpha_phi ~ normal(0, 8);
  alpha_theta ~ normal(0, 4);

  // Oocyst measureuments (control)
  for (g in 1:N_C) {
    log_mu_ooc_C[g] <-   rho__log_mean_ooc_C
                       + tau_bite__log_mean_ooc_C
                         * rho_bite__log_mean_ooc_C[g];
    log_phi_ooc_C[g] <-  rho__log_od_ooc_C
                       + tau_bite__log_od_ooc_C
                         * rho_bite__log_od_ooc_C[g];
  }
  
  for(n in 1:N_ooc)
    ooc_count_C[n] ~ neg_binomial_2_log(log_mu_ooc_C, exp(log_phi_ooc_C));
  
  // Oocyst measureuments (treatment)
  for (g in 1:N_T) {
    log_mu_ooc_T[g] <-   rho__log_mean_ooc_T
                       + tau_bite__log_mean_ooc_T
                         * rho_bite__log_mean_ooc_T[g];
    log_phi_ooc_T[g] <-  rho__log_od_ooc_T
                       + tau_bite__log_od_ooc_T
                         * rho_bite__log_od_ooc_T[g];
  }
  
  for(n in 1:N_ooc)
      ooc_count_T[n] ~ neg_binomial_2_log(log_mu_ooc_T, exp(log_phi_ooc_T));
  
  // Censored sporozoite measurements (control)
  for (n in 1:N_C) {
    real log_mu;
    real log_phi;
    
    // GLM of sporozoite count distribution in place of intractable convolution
    log_mu <-  beta_mu[1] * log_mu_ooc_C[n]
             + beta_mu[2] * log_phi_ooc_C[n]
             + alpha_mu + offset;
    log_phi <-  beta_phi[1] * log_mu_ooc_C[n]
              + beta_phi[2] * log_phi_ooc_C[n]
              + alpha_phi + offset;
    
    // GLM of infection probability in place of intractable convolution
    logit_theta_C[n] <-    beta_theta[1] * log_mu
                         + beta_theta[2] * log_phi
                         + alpha_theta;
    /*
    // Compute censored multinomial probabilities
    p[1] <- neg_binomial_2_cdf(bin_edge[1], exp(log_mu), exp(log_phi));
    sum_p <- p[1];

    for(b in 2:(N_bin - 1)) {
      p[b] <-   neg_binomial_2_cdf(bin_edge[b],     exp(log_mu), exp(log_phi))
              - neg_binomial_2_cdf(bin_edge[b - 1], exp(log_mu), exp(log_phi));
      sum_p <- sum_p + p[b];
    }
    p[N_bin] <- 1 - sum_p;
    
    // Censored measurement
    s_count_C[n] ~ multinomial(p);
    */
  }
  
  // Censored sporozoite measurements (ATV treatment)
  // Treatment group distributions
  for (n in 1:N_T) {
    real log_mu;
    real log_phi;
    
    // GLM of sporozoite count distribution in place of intractable convolution
    log_mu <-  beta_mu[1] * log_mu_ooc_T[n]
             + beta_mu[2] * log_phi_ooc_T[n]
             + alpha_mu + offset;
    log_phi <-  beta_phi[1] * log_mu_ooc_T[n]
              + beta_phi[2] * log_phi_ooc_T[n]
              + alpha_phi + offset;
    
    // GLM of infection probability in place of intractable convolution
    logit_theta_T[n] <-   beta_theta[1] * log_mu
                        + beta_theta[2] * log_phi
                        + alpha_theta;
    
    // Compute censored multinomial probabilities
    /*
    p[1] <- neg_binomial_2_cdf(bin_edge[1], exp(log_mu), exp(log_phi));
    sum_p <- p[1];
    
    for(b in 2:(N_bin - 1)) {
      p[b] <-   neg_binomial_2_cdf(bin_edge[b],     exp(log_mu), exp(log_phi))
              - neg_binomial_2_cdf(bin_edge[b - 1], exp(log_mu), exp(log_phi));
      sum_p <- sum_p + p[b];
    }
    p[N_bin] <- 1 - sum_p;
   
    
    // Censored measurement
    s_count_T[n] ~ multinomial(p);
    */
  }
  
  // Mice infection prevalence measurements
  for(n in 1:N_mice) {
    prev_C[n] ~ bernoulli_logit(logit_theta_C);
    prev_T[n] ~ bernoulli_logit(logit_theta_T);
  }
  
}
    
generated quantities {
  int<lower=0> sim_ooc_count_C[N_C];
  int<lower=0> sim_ooc_count_T[N_T];
  
##  int<lower=0> sim_s_count_C[N_C, N_bin];
##  int<lower=0> sim_s_count_T[N_T, N_bin];

  real<lower=0, upper=1> theta_C[N_C];
  real<lower=0, upper=1> theta_T[N_T];
  
  int<lower=0, upper=N_mice> sim_prev_C[N_C];
  int<lower=0, upper=N_mice> sim_prev_T[N_T];

  {
  ##  real sum_p;
  ##  vector[N_bin] p;
  
    for (g in 1:N_C) {
      real log_mu_ooc;
      real log_phi_ooc;
      real log_mu_s;
      real log_phi_s;
      
      // Simulate oocyst count measurement
      log_mu_ooc <-  rho__log_mean_ooc_C
                   + tau_bite__log_mean_ooc_C
                   * rho_bite__log_mean_ooc_C[(g - 1) / N_round + 1]
                   + tau_round__log_mean_ooc_C
                   * rho_round__log_mean_ooc_C[(g - 1) % N_round + 1];
      log_phi_ooc <-  rho__log_od_ooc_C
                    + tau_bite__log_od_ooc_C
                    * rho_bite__log_od_ooc_C[(g - 1) / N_round + 1]
                    + tau_round__log_od_ooc_C
                    * rho_round__log_od_ooc_C[(g - 1) % N_round + 1];
      
      sim_ooc_count_C[g]
        <- neg_binomial_2_log_rng(log_mu_ooc, exp(log_phi_ooc));
      
      // Simulate sporozoite count measurement
      log_mu_s <-  beta_mu[1] * log_mu_ooc
                 + beta_mu[2] * log_phi_ooc
                 + alpha_mu + offset;
      log_phi_s <-  beta_phi[1] * log_mu_ooc
                  + beta_phi[2] * log_phi_ooc
                  + alpha_phi + offset;
      
      /*
      p[1] <- neg_binomial_2_cdf(bin_edge[1], exp(log_mu_s), exp(log_phi_s));
      sum_p <- p[1];
      
      for(b in 2:(N_bin - 1)) {
        p[b] <-   neg_binomial_2_cdf(bin_edge[b],     exp(log_mu_s), exp(log_phi_s))
                - neg_binomial_2_cdf(bin_edge[b - 1], exp(log_mu_s), exp(log_phi_s));
        sum_p <- sum_p + p[b];
      }
      p[N_bin] <- 1 - sum_p;
      
      sim_s_count_C[g] <- multinomial_rng(p, sum(s_count_C[g]));
      */
      theta_C[g] <- inv_logit(  beta_theta[1] * log_mu_s
                              + beta_theta[2] * log_phi_s
                              + alpha_theta);
      
      // Simulate infection prevelance measurement
      sim_prev_C[g] <- binomial_rng(N_mice, theta_C[g]);
      
    }
    
    for (g in 1:N_T) {
      real log_mu_ooc;
      real log_phi_ooc;
      real log_mu_s;
      real log_phi_s;
      
      // Simulate oocyst count measurement
      log_mu_ooc <-  rho__log_mean_ooc_T
                   + tau_bite__log_mean_ooc_T
                   * rho_bite__log_mean_ooc_T[(g - 1) / N_round + 1]
                   + tau_round__log_mean_ooc_T
                   * rho_round__log_mean_ooc_T[(g - 1) % N_round + 1];
      log_phi_ooc <-  rho__log_od_ooc_T
                    + tau_bite__log_od_ooc_T
                    * rho_bite__log_od_ooc_T[(g - 1) / N_round + 1]
                    + tau_round__log_od_ooc_T
                    * rho_round__log_od_ooc_T[(g - 1) % N_round + 1];
      
      sim_ooc_count_T[g]
        <- neg_binomial_2_log_rng(log_mu_ooc, exp(log_phi_ooc));
      
      // Simulate sporozoite count measurement
      log_mu_s <-  beta_mu[1] * log_mu_ooc
                 + beta_mu[2] * log_phi_ooc
                 + alpha_mu + offset;
      log_phi_s <-  beta_phi[1] * log_mu_ooc
                  + beta_phi[2] * log_phi_ooc
                  + alpha_phi + offset;
      
  ##    /*
  ##    p[1] <- neg_binomial_2_cdf(bin_edge[1], exp(log_mu_s), exp(log_phi_s));
  ##    sum_p <- p[1];
  ##    
    ##  for(b in 2:(N_bin - 1)) {
    ##    p[b] <-   neg_binomial_2_cdf(bin_edge[b],     exp(log_mu_s), exp(log_phi_s))
    ##            - neg_binomial_2_cdf(bin_edge[b - 1], exp(log_mu_s), exp(log_phi_s));
    ##    sum_p <- sum_p + p[b];
    ##  }
    ##  p[N_bin] <- 1 - sum_p;
      
    ##  sim_s_count_T[g] <- multinomial_rng(p, sum(s_count_T[g]));
    ##  */
      theta_T[g] <- inv_logit(  beta_theta[1] * log_mu_s
                              + beta_theta[2] * log_phi_s
                              + alpha_theta);
      
      // Simulate infection prevelance measurement
      sim_prev_T[g] <- binomial_rng(N_mice, theta_T[g]);
      
    }
    
  }
  
}