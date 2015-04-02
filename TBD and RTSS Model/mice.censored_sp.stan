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

  // Sporozoite count binning
  int<lower=0> N_bin;
  int<lower=0> bin_edge[N_bin + 1];
  
  // Binned sporozoite counts for each control/treatment group
  int<lower=0> s_count_C[N_C, N_bin];
  int<lower=0> s_count_T[N_T, N_bin];
}

parameters {
  // Log mean and log precision of control group oocyst count NegBin distribution
  real log_mu_ooc_C;
  real log_sigma_ooc_C;
  
  // Log mean and log precison of ATV treatment group oocyst count NegBin distribution
  real log_mu_ooc_T;
  real log_sigma_ooc_T;
  
  // Slopes and intercepts of GLM modeling conversion from oocyst counts
  // to sporozoite counts.  Common slopes for control and treatment groups
  // as the underlying process should be independent of treatment.
  vector[2] beta_mu;
  vector[2] beta_sigma;
  real alpha_mu;
  real alpha_sigma;
  
  // Slopes and intercept of GLM modeling infection probability from
  // sporozite count distribution.  Again, common parameters for control
  // and treatment as the underlying process should be independent of
  // treatment.
  vector[2] beta_theta;
  real alpha_theta;
}

model {
  // Log mean and log precisoin of control group sporozoite count NegBin distribution
  vector[N_C] log_mu_s_C;
  vector[N_C] log_sigma_s_C;
  
  // Log mean and log precision of ATV treatment group sporozoite count NegBin distribution
  vector[N_T] log_mu_s_T;
  vector[N_T] log_sigma_s_T;
  
  // Logit infection probability for each control/treatment group
  vector[N_C] logit_theta_C;
  vector[N_T] logit_theta_T;

  // Sporozoite count censoring model parameters
  real sum_p;
  vector[N_bin] p;
  
  // Control group distributions
  for (n in 1:N_C) {
    
    // GLM of sporozoite count distribution in place of intractable convolution
    log_mu_s_C[n] <-   beta_mu[1] * log_mu_ooc_C
                     + beta_mu[2] * log_sigma_ooc_C
                     + alpha_mu;
    log_sigma_s_C[n] <-   beta_sigma[1] * log_mu_ooc_C
                        + beta_sigma[2] * log_sigma_ooc_C
                        + alpha_sigma;
    
    // GLM of infection probability in place of intractable convolution
    logit_theta_C[n] <-    beta_theta[1] * log_mu_s_C[n]
                         + beta_theta[2] * log_sigma_s_C[n]
                         + alpha_theta;
  }
  
  // Treatment group distributions
  for (n in 1:N_T) {
    
    // GLM of sporozoite count distribution in place of intractable convolution
    log_mu_s_T[n] <-   beta_mu[1] * log_mu_ooc_T
                     + beta_mu[2] * log_sigma_ooc_T
                     + alpha_mu;
    log_sigma_s_T[n] <-   beta_sigma[1] * log_mu_ooc_T
                        + beta_sigma[2] * log_sigma_ooc_T
                        + alpha_sigma;
    
    // GLM of infection probability in place of intractable convolution
    logit_theta_T[n] <-   beta_theta[1] * log_mu_s_T[n]
                        + beta_theta[2] * log_sigma_s_T[n]
                        + alpha_theta;
  }
  
  // Priors
  log_mu_ooc_C ~ normal(0, 5);
  log_sigma_ooc_C ~ normal(0, 5);
  log_mu_ooc_T ~ normal(0, 5);
  log_sigma_ooc_T ~ normal(0, 5);
  
  beta_mu ~ normal(0, 2);
  beta_sigma ~ normal(0, 2);
  beta_theta ~ normal(0, 2);
  
  alpha_mu ~ normal(0, 2);
  alpha_sigma ~ normal(0, 2);
  alpha_theta ~ normal(0, 2);

  // Oocyst measureuments
  for(n in 1:N_ooc) {
    ooc_count_C[n] ~ neg_binomial_2(exp(log_mu_ooc_C), exp(log_sigma_ooc_C));
    ooc_count_T[n] ~ neg_binomial_2(exp(log_mu_ooc_T), exp(log_sigma_ooc_T));
  }
  
  // Censored sporozoite measurements (control)
  for(n in 1:N_C) {
    
    // Compute censored multinomial probabilities
    sum_p <- 0;
    for(b in 1:(N_bin - 1)) {
      p[b] <-   neg_binomial_2_cdf(bin_edge[b + 1], exp(log_mu_s_C[n]), exp(log_sigma_s_C[n]))
              - neg_binomial_2_cdf(bin_edge[b],     exp(log_mu_s_C[n]), exp(log_sigma_s_C[n]));
      sum_p <- sum_p + p[b];
    }
    p[N_bin] <- 1 - sum_p;
  
    // Censored measurement
    s_count_C[n] ~ multinomial(p);
  }
   
  // Censored sporozoite measurements (ATV treatment)
  for(n in 1:N_T) {
    
    // Compute censored multinomial probabilities
    sum_p <- 0;
    for(b in 1:(N_bin - 1)) {
      p[b] <-   neg_binomial_2_cdf(bin_edge[b + 1], exp(log_mu_s_T[n]), exp(log_sigma_s_T[n]))
              - neg_binomial_2_cdf(bin_edge[b],     exp(log_mu_s_T[n]), exp(log_sigma_s_T[n]));
      sum_p <- sum_p + p[b];
    }
    p[N_bin] <- 1 - sum_p;
    
    // Censored measurement
    s_count_T[n] ~ multinomial(p);
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
  
  //real<lower=0> mu_s_C[N_C];
  //real<lower=0> sigma_s_C[N_C];
  //real<lower=0> mu_s_T[N_T];
  //real<lower=0> sigma_s_T[N_T];
    
  //real<lower=0, upper=1> theta_C[N_C];
  //real<lower=0, upper=1> theta_T[N_T];

  for (n in 1:N_C) {
    sim_ooc_count_C[n] <- neg_binomial_2_rng(exp(log_mu_ooc_C), exp(log_sigma_ooc_C));
  }
  
  for (n in 1:N_C) {
    sim_ooc_count_T[n] <- neg_binomial_2_rng(exp(log_mu_ooc_T), exp(log_sigma_ooc_T));
  }
  
  /*
  for (n in 1:N_C) {
    mu_s_C[n] <- exp(  beta_mu[1] * log_mu_ooc_C
                     + beta_mu[2] * log_sigma_ooc_C
                     + alpha_mu);
    sigma_s_C[n] <- exp(  beta_sigma[1] * log_mu_ooc_C
                        + beta_sigma[2] * log_sigma_ooc_C
                        + alpha_sigma);
    theta_C[n] <- inv_logit(  beta_theta[1] * log(mu_s_C[n])
                            + beta_theta[2] * log(sigma_s_C[n])
                            + alpha_theta);
  }
  
  for (n in 1:N_T) {
    mu_s_T[n] <- exp(  beta_mu[1] * log_mu_ooc_T
                     + beta_mu[2] * log_sigma_ooc_T
                     + alpha_mu);
    sigma_s_T[n] <- exp(  beta_sigma[1] * log_mu_ooc_T
                        + beta_sigma[2] * log_sigma_ooc_T
                        + alpha_sigma);
    theta_T[n] <- inv_logit(  beta_theta[1] * log(mu_s_T[n])
                            + beta_theta[2] * log(sigma_s_T[n])
                            + alpha_theta);
  }
  */
  
}