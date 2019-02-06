# Bayesian inference using rstan. Heights of the last 11 US presidents.
# Coded by Umberto Picchini 

library(rstan)   # for online use just go to https://rstudio.cloud/project/56157
# then check the slides for the lecture on how to proceed.
# Otherwise, for offline use:
# install by following https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

#::: optional: detect number of cores of your computer
# useful to exploit parallel computation 
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# height (cm) of the last 11 US presidents from Kennedy to Trump
data <- c(183, 192, 182, 183, 177, 185, 188, 188, 182, 185, 188)

# optional: specify starting parameter values
initparam <- function() {
  list(mu = 190, sigma = 5)
} 

# generate 10000 posterior draws via MCMC: the model (likelihood and priors) is specified in the .stan file
fit <- stan(file='USheights_model.stan', data=list(N=length(data),y=data), init=initparam, seed=194838, iter=10000,chains=1)

# plot the sequence of generated random numbers
traceplot(fit,pars="mu",inc_warmup=T)    # traceplot for mu, including warmup (default warmup=iter/2)
traceplot(fit,pars="sigma",inc_warmup=T) # traceplot for sigma, including warmup (default warmup=iter/2)

params<-extract(fit) # extract the MCMC output for further examination via R standard functions
hist(params$mu,xlab="mu", main = "posterior mean height")  # posterior for mu (after warmup)
hist(params$sigma,xlab="sigma", main = "posterior std. dev. for height") # posterior for sigma (after warmup)

print(fit)  # basic summaries at convergence (ie after iter/2 iterations)
# theoretical population of US presidents has the following:
# posterior mean height = 184.4 cm
# posterior standard deviation for the height = 4.68 cm

# we can also compute any quantile,
# e.g.
quantile(params$mu,probs=0.01)  
# what is the probability that the mean height is higher than 189 cm?
# use 1 - Prob(height<189) = 1- ecdf = 1- empirical cumulative distr funct
1-ecdf(params$mu)(189)  # it's about 0.2%
# what is the 95% credible interval for mu? 
# Just look at print(fit), find the values for mu at 2.5% and 97.5%, so it's about [182,187]
