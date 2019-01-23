## Some R-commands for exercise 1.9 in Rawlings, J.O., Pantula, S.G., Dickey, D.A.: Applied Regression Analysis - A Research Tool, 2ed, Springer

x <- c(29.7,68.4,120.7,217.2,313.5,419.1,535.9,641.5)
y <- c(16.6,49.1,121.7,219.6,375.5,570.8,648.2,755.6)
meanx = mean(x)

plot(x,y,xlab="time (weeks)",ylab="plant biomass (grams)")

model <- lm(y ~ x)
abline(model)
# parameters confidence intervals
confint(model)  # default uses 95% confidence level, otherwise use e.g. confint(model,level=0.99) etc.

beta0 = model$coefficients[1]
beta1 = model$coefficients[2]

# construct 95% confidence intervals for parameters "by hand":
# we need 's', the square root of the estimate of the error variance
n = length(y)   # the sample size
e = model$residuals  # select the field 'residuals' from the object "model"
s = sqrt(sum(e^2)/(n-2))  # or model$sigma
t_quant <- qt(0.025,n-2)  # 2.5%-quantile for a Student's distribution with n-2 degrees of freedom
ci_beta0 = beta0 + c(1,-1) * t_quant * s * sqrt(1/n + meanx^2/sum((x-meanx)^2))
ci_beta1 = beta1 + c(1,-1) * t_quant * s / sqrt(sum((x-meanx)^2))

###############################

# Let's plot confidence and prediction intervals for E(Y0) and Y0 respectively
xx0 <- seq(0,700,0.1)  # just a grid of values for the predictor
predx<-data.frame(x = xx0)
y1 <- predict(model,predx)
y1ci <- predict(model,predx,interval="confidence")  # confidence interval for the expected response
y1pi <- predict(model,predx,interval="prediction")  # prediction interval for hypothetical new observations
plot(x,y,xlab="time (weeks)",ylab="plant biomass (g)",xlim=c(0,700),ylim=c(-100,1000))
abline(model)   # add the fitted line to the previous plot
lines(xx0,y1ci[,2],lty=2,col="blue",lwd=2)  # add confidence interval lines
lines(xx0,y1ci[,3],lty=2,col="blue",lwd=2)  # add confidence interval lines
lines(xx0,y1pi[,2],lty=3,col="blue",lwd=2)  # add prediction interval lines
lines(xx0,y1pi[,3],lty=3,col="blue",lwd=2)  # add prediction interval lines
