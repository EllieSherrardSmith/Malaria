data{
  int<lower=0> N_C; ##number of control rounds = 20
  int<lower=0> N_T; ##number of treatment rounds = 20
  int<lower=0> N_ooc; ##number of oocysts in each round
  int<lower=0> N_mice; ##number of mice = 5
  
  int<lower=0> ooc_count_C[N_ooc,N_C];
  int<lower=0> ooc_count_T[N_ooc,N_T];
  
  int<lower=0> prev_C[N_mice,N_C];
  int<lower=0> prev_T[N_mice,N_T];

  int<lower=0> N_bin;
  int<lower=0> bin_edge[N_bin+1];
  int<lower=0> s_count_C[N_C,N_bin];
  int<lower=0> s_count_T[N_T,N_bin];

}
parameters{
  real logmu_ooc_C;
  real logsigma_ooc_C;
  real logmu_ooc_T;
  real logsigma_ooc_T;
  vector[2] beta_mu;
  vector[2] beta_sigma;
  vector[2] beta_theta;
  real alpha_mu;
  real alpha_sigma;
  real alpha_theta;
}
model{
  vector[N_C] logmu_s_C;
  vector[N_C] logsigma_s_C;
  vector[N_T] logmu_s_T;
  vector[N_T] logsigma_s_T;
  
  vector[N_C] logit_theta_C;
  vector[N_T] logit_theta_T;

  vector[N_bin] p;
  real Sum;
  for (n in 1:N_C){
    logmu_s_C[n] <- (beta_mu[1] * logmu_ooc_C + beta_mu[2] * logsigma_ooc_C + alpha_mu);
    logsigma_s_C[n] <- (beta_sigma[1] * logmu_ooc_C + beta_sigma[2] * logsigma_ooc_C + alpha_sigma);
    logit_theta_C[n] <- (beta_theta[1] * logmu_s_C[n] + beta_theta[2] * logsigma_s_C[n] + alpha_theta);
  }
  for (n in 1:N_T){
    logmu_s_T[n] <- (beta_mu[1] * logmu_ooc_T + beta_mu[2] * logsigma_ooc_T + alpha_mu);
    logsigma_s_T[n] <- (beta_sigma[1] * logmu_ooc_T + beta_sigma[2] * logsigma_ooc_T + alpha_sigma);
    logit_theta_T[n] <- (beta_theta[1] * logmu_s_T[n] + beta_theta[2] * logsigma_s_T[n] + alpha_theta);
  }
  logmu_ooc_C ~ normal(0,10);
  logsigma_ooc_C ~ normal(0,10);
  logmu_ooc_T ~ normal(0,10);
  logsigma_ooc_T ~ normal(0,10);
  
  beta_mu ~ normal(0,2);
  beta_sigma ~ normal(0,2);
  beta_theta ~ normal(0,2);
  alpha_mu ~ normal(0,2);
  alpha_sigma ~ normal(0,2);
  alpha_theta ~ normal(0,2);

  for(n in 1:N_ooc){
    ooc_count_C[n] ~ neg_binomial(exp(logmu_ooc_C),exp(logsigma_ooc_C));//NEED TO CHECK AS NOT THE SAME NEG BIN AS IN r
    ooc_count_T[n] ~ neg_binomial(exp(logmu_ooc_T),exp(logsigma_ooc_T));//NEED TO CHECK AS NOT THE SAME NEG BIN AS IN r
    }
  Sum <- 0;
  for(n in 1:(N_bin-1)){
    p[n] <- neg_binomial_cdf(bin_edge[n+1],exp(logmu_s_C),exp(logsigma_s_C))
                        -neg_binomial_cdf(bin_edge[n],exp(logmu_s_C),exp(logsigma_s_C));
    Sum <- Sum+p[n];
  }
  p[N_bin] <- 1-Sum;
  
  for(n in 1:N_C){
    s_count_C[n] ~ multinomial(p);
  }
    Sum <- 0;
  for(n in 1:N_bin){
    p[n] <- neg_binomial_cdf(bin_edge[n+1],exp(logmu_s_T),exp(logsigma_s_T))
                        -neg_binomial_cdf(bin_edge[n],exp(logmu_s_T),exp(logsigma_s_T));
      Sum <- Sum+p[n];
  }
  p[N_bin] <- 1-Sum;
  
  for(n in 1:N_T){
    s_count_T[n] ~ multinomial(p);
  }

  for(n in 1:N_mice){
    prev_C[n] ~ bernoulli_logit(logit_theta_C);
    prev_T[n] ~ bernoulli_logit(logit_theta_T);
    }
  }
generated quantities{
  real<lower=0> mu_s_C[N_C];
  real<lower=0> sigma_s_C[N_C];
  real<lower=0> mu_s_T[N_T];
  real<lower=0> sigma_s_T[N_T];
    
  real<lower=0, upper=1> theta_C[N_C];
  real<lower=0, upper=1> theta_T[N_T];

  for (n in 1:N_C){
    mu_s_C[n] <- exp(beta_mu[1] * logmu_ooc_C + beta_mu[2] * logsigma_ooc_C + alpha_mu);
    sigma_s_C[n] <- exp(beta_sigma[1] * logmu_ooc_C + beta_sigma[2] * logsigma_ooc_C + alpha_sigma);
    theta_C[n] <- inv_logit(beta_theta[1] * log(mu_s_C[n]) + beta_theta[2] * log(sigma_s_C[n]) + alpha_theta);
  }
  for (n in 1:N_T){
    mu_s_T[n] <- exp(beta_mu[1] * logmu_ooc_T + beta_mu[2] * logsigma_ooc_T + alpha_mu);
    sigma_s_T[n] <- exp(beta_sigma[1] * logmu_ooc_T + beta_sigma[2] * logsigma_ooc_T + alpha_sigma);
    theta_T[n] <- inv_logit(beta_theta[1] * log(mu_s_T[n]) + beta_theta[2] * log(sigma_s_T[n]) + alpha_theta);
  }
}



