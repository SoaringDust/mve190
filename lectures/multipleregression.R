# the tension-temperature-pressure example 
data=matrix(c(152, 180, 450,
              150, 180, 450,
              103 ,190, 375,
              99, 190, 375,
              88, 200, 350,
              89, 200, 350,
              122, 210, 375,
              120, 210, 375,
              162, 220, 450,
              161, 220, 450),10,3)
tension<-data[,1]
temperature<-data[,2]
pressure<-data[,3]

pairs(~tension+temperature+pressure) # always look at a scatter plot matrix when you have several predictors

# want to plot 3d scatterplots? you need the package "scatterplot3d"
install.packages("scatterplot3d")  # download & install the package (needed only once)
library(scatterplot3d)  # load the package
scatterplot3d(temperature,pressure,tension)

# however you cannot rotate the plot above using e.g. the mouse. Use instead the plot3d function (needs the package "rgl")
install.packages("rgl")  # download & install the package (needed only once)
library(rgl)  # load the package
plot3d(temperature,pressure,tension)


# define the model 
mymodel<-lm(tension~temperature+pressure)
summary(mymodel)
# Rsquared = 0.989 all highly significant

# However let's look at pairwise plots
plot(temperature,tension) # not great
plot(1/temperature,tension) # better
mymodel2<-lm(tension~I(1/temperature)+pressure)
summary(mymodel2) # Multiple R-squared:  0.9909

plot(pressure,tension) # seems to suggest a quadratic relationship
plot(pressure^2,tension) # not great but better
mymodel3<-lm(tension~I(1/temperature)+I(pressure^2))
summary(mymodel3)  # Multiple R-squared:  0.9929

# we got beta0 = 107.84, beta1 = 34558, beta2=-0.00089, all highly significant
# Estimated expected response for temperature = 195 and pressure = 450
x0=data.frame(temperature=195,pressure=450)
y0<-predict(mymodel3,x0)
# same as doing the following (...up to rounding)
y0=107.84 +34558/195 -0.00089*450^2

# 95% confidence interval for beta1 "by hand":
# let's first compute the residual standard error: this can be found from summary(mymodel3)$sigma
# that is s = 13.15
s = 13.15  # also found via sqrt(sum(residuals(mymodel3)^2)/(10-3)) and summary(mymodel3)$sigma
# now we need the inverse of X'X
invXtX = summary(mymodel3)$cov.unscaled
# for beta1 we need to pick second element on its diagonal
a22 = invXtX[2,2]
# therefore the standard error for beta1 is
se_beta1 = sqrt(s^2*a22)
# the 95% confidence interval is
34558 +c(1,-1)*qt(0.025,10-3)*se_beta1
# or, see the second line of
confint(mymodel3)

