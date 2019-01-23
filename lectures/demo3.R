## Some R-commands for exercise 1.9 in Rawlings, J.O., Pantula, S.G., Dickey, D.A.: Applied Regression Analysis - A Research Tool, 2ed, Springer

x <- c(29.7,68.4,120.7,217.2,313.5,419.1,535.9,641.5)
y <- c(16.6,49.1,121.7,219.6,375.5,570.8,648.2,755.6)

plot(x,y,xlab="time (weeks)",ylab="plant biomass")

model1 <- lm(y ~ x)
abline(model1)

###################################
## t-test of H0: beta1 = 0       ##
###################################

# Estimate the model y = beta0 + beta1*x:
sum1 <- summary(model1)

sum1
#R-squared = 0.99

## Reject H0 since Pr(>|t|) = 4.36*10^(-7) which is ridiculously small

###################################
## t-test of H0: beta1 = 0 "by hand" ##
###################################
# The slope estimate: 
beta1 <- model1$coefficients[2]
## same as
# beta1 <- sum1$coefficients[2,1]

# The sigma-estimate:
s <- sum1$sigma  # also given as "Residual standard error" in sum1


# s(beta_1):  # standard error for beta_1
s_beta1 <- s/sqrt(sum((x-mean(x))^2))
## same as
# s_beta1 <- sum1$coefficients[2,2]

# Test-value:
t <- (beta1-0)/s_beta1
## same as
# t <- sum1$coefficients[2,3]
# t-quantile for comparison: (here alpha=0.05)
t_alfa <- qt(1-0.05/2,8-2)  # same magnitude as (but opposite sign of) qt(0.05/2,6)

abs(t)
t_alfa
## Reject H0 at alpha=0.05 since abs(t) > t_alfa.
