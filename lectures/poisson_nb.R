## R-code for lecture 10:

## Plot some Poisson-distributions:
y<-seq(0,20,1)
Py02<-dpois(y,.2)  # Poisson distribution with mean 0.2
Py1<-dpois(y,1)    # Poisson distribution with mean 1
Py5<-dpois(y,5)    # Poisson distribution with mean 5
Py10<-dpois(y,10)  # Poisson distribution with mean 10
# notice for graphical reasons I subtract a small quantity 0.1 before plotting (so lines for different plots do not overwrite the previous ones)
plot(y-.1,Py02,type="h",ylim=c(0,1),xlab="y",ylab="P(Y=y)",
     main="Poisson distributions")
lines(y,Py1,type="h",col="green")
lines(y+.1,Py5,type="h",col="red") # here add a small quantity before plotting only to make lines distinguishable
lines(y+.2,Py10,type="h",col="blue") # here add a small quantity before plotting
legend("topright",c("mu=0.2","mu=1","mu=5","mu=10"),
       lty=c(1,1,1,1),col=c("black","green","red","blue"))

######## Poisson-regression ##################################
##############################################################

data <- read.table('f10.txt',header=T)
attach(data)
hist(data$y)
plot(data$x,data$y)

# Fit a poisson regression
model<-glm(y ~ x, family="poisson",data=data)

# Test for over/under-dispersion
library(AER)
dispersiontest(model,alternative="two.sided") # <-- ok, we fail to reject H0:mean=variance

sum<-summary(model)
sum
sum$cov.unscaled  # gives covariance matrix for parameters

# standard errors
se_b0 = sqrt(sum$cov.unscaled[1,1]) # SE for beta0
se_b1 = sqrt(sum$cov.unscaled[2,2]) # SE for beta1

# compute 95% CI for parameters
model$coefficients[1]+c(1,-1)*qnorm(0.025)*se_b0  # CI for beta0
model$coefficients[2]+c(1,-1)*qnorm(0.025)*se_b1  # CI for beta1
# compare with 
confint.default(model)

# Calculate estimate of the linear predictor, ln mu_i = X_i*b, with S.E.(X_i*b):
xb_hat<-predict(model,se.fit=T)

# Calculate the estimated mu_i:
mu_hat<-predict(model,type="response")
# same as mu_hat<-exp(xb_hat$fit)

# Calculate 95% confidence interval for linear predictor X_i*b:
cixb_lo<-xb_hat$fit-1.96*xb_hat$se.fit
cixb_hi<-xb_hat$fit+1.96*xb_hat$se.fit

# ... and transform into interval for mu_i:
cimu_lo<-exp(cixb_lo)
cimu_hi<-exp(cixb_hi)

# Plot data and estimates + conf.int.:
plot(data$x,data$y,xlab="x",ylab="y",main="Poisson regression")
lines(data$x,mu_hat,col="blue")
lines(data$x,cimu_lo,col="blue",lty=2)
lines(data$x,cimu_hi,col="blue",lty=2)

# Influence measures:
infl<-influence(model)
d<-cooks.distance(model)

# Pearsons residuals
r_pear <- infl$pear.res

## standardized Pearson residuals:
r_std<-r_pear/sqrt(1-infl$hat)
plot(r_std,xlab="i",ylab="r_i",ylim=c(-4,4),main="Standardised Pearson residuals")
z<-qnorm(0.05/2)
abline(h=0)
abline(h=-z,col="red")
abline(h=z,col="red")

# Cook's distance:
plot(d,ylim=c(0,1.2),xlab="i",ylab="D",main="Cook's distance")



## Negative binomial regression ##############################
##############################################################

## Plot some:
y<-seq(0,20,1)
Py1<-dnbinom(y,mu=5,size=5)
Py2<-dnbinom(y,mu=5,size=50)
Py3<-dnbinom(y,mu=5,size=0.5)
Py4<-dnbinom(y,mu=5,size=0.05)
plot(y-.1,Py4,type="h",ylim=c(0,1),xlab="y",ylab="P(Y=y)",
     main="Negative binomial distributions")
lines(y,Py3,type="h",col="green")
lines(y+.1,Py1,type="h",col="red")
lines(y+.2,Py2,type="h",col="blue")
legend("topright",c("theta=0.05","theta=0.5","theta=5","theta=50"),
       lty=c(1,1,1,1),col=c("black","green","red","blue"))

## Read the data and fit the model:
data<-read.table('f10b.txt',header=T)
plot(data$x,data$y)  # data seem more dispersed than Poisson data

library(AER)
# POisson
model.pois<-glm(y ~ x, family="poisson",data=data)
dispersiontest(model.pois,alternative = "two.sided") # as expected, we have overdispersion. Use NB instead if Poisson

# Needs R library for the glm.nb-commands:
library(MASS) # MASS = Modern Applied Statistics with S ("S" was the father of "R"...)
# Negative binomial
model.nb<-glm.nb(y ~ x, data=data)
model.nb
sum<-summary(model.nb)
sum
# Notice the additional parameter THETA at the bottom of the summary output. This is the parameter that enlarges the variance
# in a negative-binomial distribution (compared to Poisson)

mu_hat<-predict(model.nb,type="response")
xb_hat<-predict(model.nb,se.fit=T)

cixb_lo<-xb_hat$fit-1.96*xb_hat$se.fit
cixb_hi<-xb_hat$fit+1.96*xb_hat$se.fit
cimu_lo<-exp(cixb_lo)
cimu_hi<-exp(cixb_hi)

plot(data$x,data$y,xlab="x",ylab="y",main="Negative binomial regression")
lines(data$x,mu_hat,col="blue")
lines(data$x,cimu_lo,col="blue",lty=2)
lines(data$x,cimu_hi,col="blue",lty=2)

## Compare with a poisson model
model.pois<-glm(y ~ x, family="poisson",data=data)
mupois_hat<-predict(model.pois,type="response")
xbpois_hat<-predict(model.pois,se.fit=T)
plot(data$x,data$y,xlab="x",ylab="y",main="Poisson regression")
lines(data$x,mupois_hat,col="red")
ci_xbpois_lo<-xbpois_hat$fit-1.96*xbpois_hat$se.fit
ci_xbpois_hi<-xbpois_hat$fit+1.96*xbpois_hat$se.fit
ci_mupois_lo<-exp(ci_xbpois_lo)
ci_mupois_hi<-exp(ci_xbpois_hi)
lines(data$x,ci_mupois_lo,col="blue",lty=2)
lines(data$x,ci_mupois_hi,col="blue",lty=2)

AIC(model.nb) # recall NB has 1 more parameter compared to POisson
AIC(model.pois)

# Likelihood ratio test = difference between deviances
-2*(logLik(model.pois)[1]-logLik(model.nb)[1])  # 259.29
# notice I did NOT use "anova" as anova tests two models having observations following the same distribution,
# but this is not our case, as we are comparing models were data are assumed to belong once
# to a POisson distribution (model.pois) and another time to a NB distrib. (model.nb)
# Hence anova() breaks down, and I compute the deviances difference "by hand"

qchisq(1-0.05,1) # 3.84 < 259 
# Difference between deviances is large: I reject the simple Poisson model at 0.05 significance

## Poisson gives a very similar mu as Negative Binomial regression, but the fit does not account for increasing variability for increasing x.
## We need that theta.
