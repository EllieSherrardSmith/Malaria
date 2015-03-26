data{
  int<lower=0> N_Comb; ##number of rounds/bites = 32 - control then atvs rounds 1 to 4 and bites 2 to 5 (as all atv were 0 for bite 1)
  int<lower=0> N_ooc; ##number of oocysts in each round (45) - reduced to 24 to get rid of NAs from some groups
  ##int<lower=0> N_mice; ##number of mice = 5
  
  int<lower=0> ooc_count[N_ooc,N_Comb]; ##the raw data counts of the oocysts for each group
  
  ##int<lower=0> prev[N_mice,N_Comb]; ##the prevalence in the mice for each group e.g proportion infected out of the 5 mice
  
  int<lower=0> N_bin;
  int<lower=0> bin_edge[N_bin+1];
  int<lower=0> s_count[N_Comb,N_bin]; ##the frequency of sporozoites in each band 0-1, 1-2, 2-3, 3-4 and 4+
  
}
parameters{
  real logmu_ooc;
  real logsigma_ooc;
  
  vector[2] beta_mu;
  vector[2] beta_sigma;
  vector[2] beta_theta;
  
  real alpha_mu;
  real alpha_sigma;
  real alpha_theta;
}
model{
  vector[N_Comb] logmu_s;
  vector[N_Comb] logsigma_s;
  
  vector[N_bin] p;
  real Sum;
  
  for (n in 1:N_Comb){
    logmu_s[n] <- (beta_mu[1] * logmu_ooc + beta_mu[2] * logsigma_ooc + alpha_mu);
    logsigma_s[n] <- (beta_sigma[1] * logmu_ooc + beta_sigma[2] * logsigma_ooc + alpha_sigma);
    }
  
  logmu_ooc ~ normal(0,10);
  logsigma_ooc ~ normal(0,10);
  
  beta_mu ~ normal(0,2);
  beta_sigma ~ normal(0,2);

  alpha_mu ~ normal(0,2);
  alpha_sigma ~ normal(0,2);
  
  for(n in 1:N_ooc){
    ooc_count[n] ~ neg_binomial_2(exp(logmu_ooc),exp(logsigma_ooc));
  }
  Sum <- 0;
  for(n in 1:(N_bin-1)){
    p[n] <- neg_binomial_2_cdf(bin_edge[n+1],exp(logmu_s),exp(logsigma_s))
    -neg_binomial_2_cdf(bin_edge[n],exp(logmu_s),exp(logsigma_s));
    
    Sum <- Sum+p[n];
  }
  p[N_bin] <- 1-Sum;
  
  for(n in 1:N_Comb){
    s_count[n] ~ multinomial(p);
  }
}
generated quantities{
  real<lower=0> mu_s[N_Comb];
  real<lower=0> sigma_s[N_Comb];

  for(n in 1:(N_Comb)){
    mu_s[n] <- exp(beta_mu[1] * logmu_ooc + beta_mu[2] * logsigma_ooc + alpha_mu);
    sigma_s[n] <- exp(beta_sigma[1] * logmu_ooc + beta_sigma[2] * logsigma_ooc + alpha_sigma);
  }
}
