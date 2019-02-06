data {  // data and other constants
  int<lower=0> n;
  vector[n] x;
  vector[n] y;
}

parameters {  // parameters to be estimated
  real beta0;
  real beta1;
  real<lower=0> sigma;
}

model {// here we specify priors and likelihood for a single generic observation
  beta0 ~ normal(35,5);  // beta0 prior
  beta1 ~ normal(-10,3);  // beta1 prior
  sigma ~ normal(0,10); //  sigma prior: actually this is a half-normal distribution on the positive real line, since real<lower=0> sigma
  y ~ normal(beta0 + beta1*x, sigma);  // Stan will take care of producing a likelihood using all observations
}
