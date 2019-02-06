// Here we speficy the model structure to perform MCMC inference using Stan
// This is NOT R code, but rstan is able to intepret it

data {  // this field is for constants and other observed quantities
  int N; //the number of observations (an integer)
  real y[N]; //the data vector, i.e. the heights
}

parameters {  // here define the unknown parameters to estimate
  real mu;  // mu is a real-valued quantity
  real<lower=0> sigma; // sigma is a positive real-valued quantity (lower bound is 0)
}

model {  // here we specify priors and likelihood for a single generic observation
  mu ~ normal(180, 5);   // prior for mu
  sigma ~ uniform(0.1, 20);  // prior for sigma
  y ~ normal(mu, sigma);  // our assumed model for a given height.
  // Stan will take care of producing a likelihood using all observations
}