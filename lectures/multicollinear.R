# example of multicollinearity

# artificially create responses
x1 = seq(1,100,by=1)
x2 = x1 + 3
y = x1 + x2 + rnorm(length(x1),0,10)  # artificially created responses (intercept=0, x1 parameter = 1, x2 parameter = 1, measurement error standard deviation = 10)
# ok we have now artificially created  some responses y, based on some covariates.

pairs(~y+x1+x2) # always look at a scatter plot matrix when you have several predictors

# Let's now pretend we do not know how we obtained the y and fit a linear regression model
summary(lm(y~x1+x2))  # Notice the NA's. Reason is that the design matrix is ill-ranked because predictors are highly (perfectly) correlated

# let's see what happens when x2 is not perfectly correlated with x1 (as in real datasets)
# I am artificially adding some very small noise to x2
x2 = x1 + 3 + rnorm(length(x1),0,0.01)
summary(lm(y~x1+x2)) # we solve the numerical problem. But as you can see now no regressor is significant. This should not be the case.
# Also huge standard errors. 

# let's make the relation between x1 and x2 not as perfectly correlated (note the increased standard deviation)
x2 = x1 + 3 + rnorm(length(x1),0,5)
summary(lm(y~x1+x2)) 
# as you can see while now x1 is significant, all parameter estimates are wrong. We should have 
# intercept=0, beta1=1, beta2=1