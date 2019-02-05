# Example of Poisson regression with offset term.
##
# Here we have number of incidents measured for several types of cargo vessels.
# For more info load the MASS package then type data(ships) and then type ?ships
##
library(MASS)
data(ships)
# These are "aggregated data", it means that each line of the record is NOT
# representing data for a single vessel. For example: the first line in the record
# shows the total number of months of service (127) summed across ALL vessels
# of type A that have been operating between 1960-74 and have been built between 1960 and 1964.
# SImilarly, the second line in the record shows the total number of months of service (63) summed across ALL vessels
# of type A that have been operating between 1960-74 and have been built between 1975 and 1979.

ships
# data coding convention:
# period = 60 for ships operating between 1960-74
# period = 75 for ships operating between 1975-79
# year = 60 for ships built between 1960-1964
# year = 65 for ships built between 1965-1969
# year = 70 for ships built between 1970-1974
# year = 75 for ships built between 1975-1979

# We first exclude ships with 0 months of service and convert the period and year variables to factors:
ships2 <- subset(ships, service > 0)
ships2$year <- as.factor(ships2$year)
ships2$period <- as.factor(ships2$period)

glm1 <- glm(formula = incidents~type + year + period,
            family = "poisson", data = ships2,
            offset = log(service),x=TRUE)

dispersiontest(glm1,alternative="two.sided") # <-- ok, we fail to reject H0:mean=variance

# verify that indeed an offset is taken into account by comparing the cofficients of glm1
# with the coefficients of another model glm0 fitted without offset
glm0 <- glm(formula = incidents~type + year + period,
            family = "poisson", data = ships2)
glm0
glm1
# yes the coefficients are different. glm1 uses the offset

# interpret coefficients for glm1:
summary(glm1)
# Ships of type B and C have the lowest risk (significantly smaller than baseline). E the highest. D and A have similar risk.
# The incident rate increased by a factor of exp(0.384) = 1.47, i.e. increased by 47% after 1974 (see coefficient period75)
# The ships built between 1960 and 1964 seem to be the safest (since coefficients for year65, year70, year75 are positive hence those built in years 65-69, 70-74 and 75-79 are more risky)
# with ships operating between 1970 and 1974 having the highest risk (coefficient for year70)

beta <- glm1$coefficients  # coefficients when using the offset term

# suppose we wish to predict rates. DO NOT USE PREDICT(). It seems predict() does not take into account the offset!!!
design <- glm1$x   # extract the design matrix from glm1. This works because I wrote "x=TRUE" inside glm1()
rate=exp(design%*%beta) # predicted rates
plot(rate,ships2$incidents/ships2$service,xlab="estimated rates",ylab="observed rates")
abline(0,1)
