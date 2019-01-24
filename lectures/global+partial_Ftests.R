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

# define the model 
mymodel<-lm(tension~temperature+pressure)
summary(mymodel)

##### F test by hand #########################
residuals<-mymodel$residuals
n<-length(tension)
SSerr <- sum(residuals^2)
MSerror <- SSerr/(n-3)
fit<-mymodel$fit
SSRegr <- sum((fit-mean(tension))^2)
MSRegr <- SSRegr/2  # 2 = # parameters to test (we exclude the intercept)
F <- MSRegr/MSerror #compare with F-statistic in summary(mymodel)
fquantile <- qf(1-0.05,2,n-3)
# F >> fquantile, reject H0 at 95% confidence level
# see also p-value = 1.32e-7 in summary function

#### Partial F test by hand #################
fullmodel <- lm(tension~temperature+pressure)
reducedmodel <- lm(tension~temperature)
SSerr_full <- sum(fullmodel$residuals^2)
SSerr_reduc <- sum(reducedmodel$residuals^2)
Q <- SSerr_reduc - SSerr_full
s_full <- summary(fullmodel)$s
PartialF <- (Q/(1))/s_full^2  # = 382.99
quant_f <- qf(1-0.05,1,n-3) # = 5.59
# reject H_0 --> we keep the larger model

#### Faster Partial F test  #################
anova(reducedmodel,fullmodel) # warning! Only for nested models!
# creates an ANOVA table comparing the two models (smaller first)


##### (sequential) ANOVA table #########################
anova(mymodel)
# SS(Regr) = SS(Temp)+SS(Press)=67454+100818
# SS(Tot_corr) = SS(Regr)+SS(Err) = (67454+100818)+1843

# order matters!
mymodel2<-lm(tension~pressure+temperature)
anova(mymodel2)
