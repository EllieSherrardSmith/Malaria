data {
int<lower=0> N;
vector[N] ooc_count;
vector[N] s_count;
}
parameters {
real beta;
real<lower=0> sigma;
}
model {
s_count ~ normal(0 + beta * ooc_count, sigma);
}
