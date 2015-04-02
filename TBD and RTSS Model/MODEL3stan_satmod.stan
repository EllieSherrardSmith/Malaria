data {
int<lower=0> N;
vector[N] ooc_count;
vector[N] s_count;
}
parameters {
real alpha;
real beta;
real delta;
real<lower=0> sigma;
}
model {
  s_count ~ normal(alpha * exp (beta * exp(delta *  ooc_count)),sigma);
      }