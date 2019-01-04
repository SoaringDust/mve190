#### 5 basic assumptions? ####
install.packages("ISLR")
library(ISLR)
#
#data(Auto) #comprise fuel consumption and aspects of automobile design from cars in the period 1970-1982.
# For example 
#mpg 	Miles/(US) gallon 
#wt 	Weight (1000 lbs) 
#cylinders 	Number of cylinders
#displacement, 	Displacement (cu.in.)
#horsepower 	Gross horsepower 
#head(Auto)
#
#plot(Auto$weight,Auto$mpg)
#
#plot(Auto$weight,1/Auto$mpg)


######

sleeptab<-read.table('sleeptab.dat',header=T)

# data includes brain and body  weight,  life  span,  gestation  time,  time 
# sleeping, and predation and danger indices for 62 mammals (from "Sleep  in Mammals: Ecological and Constitutional Correlates" by Allison, T.  and 
# Cicchetti, D. (1976), _Science_, November 12, vol. 194,  pp.  732-734. )
#
# Variables are:
#
# species of animal
# body weight in kg
# brain weight in g
# slow wave ("nondreaming") sleep (hrs/day)
# paradoxical ("dreaming") sleep (hrs/day)
# total sleep (hrs/day)  (sum of slow wave and paradoxical sleep)
# maximum life span (years)
# gestation time (days)
# predation index (1-5) 1 = minimum (least likely to be preyed upon), 5 = maximum (most likely to be preyed upon)
# sleep exposure index (1-5), 1 = least exposed (e.g. animal sleeps in well-protected den), 5 = most exposed
# overall danger index (1-5) (based on the above two indices and other information) 1 = least danger (from other animals) 5 = most danger (from other animals)

#Note: Missing values denoted by -999.0

head(sleeptab)
plot(sleeptab$bwt,sleeptab$brwt)
identify(sleeptab$bwt,sleeptab$brwt,row.names(sleeptab),pos=T)
## identify() allows you to mark observations in the data with a left mouse click
## end the identify session by pressing ESC
plot(sqrt(sleeptab$bwt),sqrt(sleeptab$brwt))
plot(log(sleeptab$bwt),log(sleeptab$brwt))




