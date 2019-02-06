# "datasets" package is automatically loaded in Rstudio
mtcars #comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973--74 models).

#mpg 	Miles/(US) gallon 
#wt 	Weight (1000 lbs) 

plot(mtcars$wt,mtcars$mpg)
# standard linear regression
linreg<-lm(mpg ~ wt, mtcars)
summary(linreg)
abline(linreg)

library(rstan)

# draw 10000 samples from the posterior:
# see carsdata_withpriors.stan
draws_stan <- stan(file='carsdata_withpriors.stan', 
                   data = list(n = nrow(mtcars),
                               x = mtcars$wt, 
                               y = mtcars$mpg),
                   seed = 47,
                   iter = 10000, chains=1)

draws_stan

#        mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
#beta0  37.42    0.04 1.75  34.02  36.26  37.38  38.54  40.82  1583    1
#beta1  -5.40    0.01 0.53  -6.44  -5.74  -5.39  -5.04  -4.38  1578    1
#sigma   3.17    0.01 0.43   2.48   2.87   3.12   3.41   4.14  2075    1
#lp__  -52.41    0.03 1.29 -55.78 -53.00 -52.07 -51.47 -50.98  1379    1

traceplot(draws_stan,inc_warmup = TRUE)  # plot sequences of draws with burnin
traceplot(draws_stan,inc_warmup = FALSE)  # plot sequences of draws without burnin

params <- extract(draws_stan) # extract the MCMC output for further examination via R standard functions

#:::: plot priors then superimpose smoothed posterior densities, for comparison :::::::::::::::::::
# define support set of beta0, to plot its prior
x = seq(20, 50, by = 0.1)
unif_beta0 = dnorm(x, mean = 35, sd = 5) # prior density for beta0
plot(x,unif_beta0,type="l",col="blue",main="prior for beta0 (blue), posterior (black)",xlab="beta0",xlim=c(20, 50),ylim=c(0,0.25)) # plot prior
# add smoothed posterior for beta0
lines(density(params$beta0),xlab="beta0",main="posterior")  # posterior draws for beta0 (after warmup)

# define support set of beta1, to plot its prior
x = seq(-20, -1, by = 0.1)
unif_beta1 = dnorm(x, mean = -10, sd = 3) # prior density for beta1
plot(x,unif_beta1,type="l",col="blue",main="prior for beta1 (blue), posterior (black)",xlab="beta1",xlim=c(-20, -1),ylim=c(0,0.8)) # plot prior
# add smoothed posterior for beta0
lines(density(params$beta1),xlab="beta1",main="posterior")  # posterior draws for beta1 (after warmup)

hist(params$beta1,xlab="beta1") # posterior draws for beta1 (after warmup)
hist(params$sigma,xlab="sigma") # posterior draws for sigma (after warmup)

draws_stan_df <- as.data.frame(draws_stan)
posterior <- as.matrix(draws_stan)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
library("bayesplot")
mcmc_areas(posterior, pars = c("beta0"),
           prob = 0.8) + plot_title
mcmc_areas(posterior, pars = c("beta1"),
           prob = 0.8) + plot_title
mcmc_areas(posterior, pars = c("sigma"),
           prob = 0.8) + plot_title
