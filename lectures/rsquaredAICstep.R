# AUCTION DATA

## Read data from file with headers in the first row:
w<-read.table('auction.dat',header=T)

w
## Type as a factor, not a number:
w$type<-factor(w$type,labels=c("bowl","cass","dish","tray","plate"))


## make a data-frame to collect all criteria :
crit<-data.frame(nbr=c(1,2,3,4,5),R2=c(0,0,0,0,0),R2adj=c(0,0,0,0,0),
                 aic=c(0,0,0,0,0),bic=c(0,0,0,0,0))

## 1: intercept only:
mod1<-lm(price ~ 1,data=w)
sum1<-summary(mod1)
crit$R2[1]<-sum1$r.squared  # computes the R^2
crit$R2adj[1]<-sum1$adj.r.squared # computes the adjusted R^2
crit$aic[1]<-AIC(mod1)  # computes the AIC
crit$bic[1]<-AIC(mod1,k=log(nrow(w)))  # computes the BIC

## 2: intercept + type:
mod2<-lm(price ~ type,data=w)
sum2<-summary(mod2)
crit$R2[2]<-sum2$r.squared
crit$R2adj[2]<-sum2$adj.r.squared
crit$aic[2]<-AIC(mod2)
crit$bic[2]<-AIC(mod2,k=log(nrow(w)))

## 3: intercept + diameter:
mod3<-lm(price ~ diam,data=w)
sum3<-summary(mod3)
crit$R2[3]<-sum3$r.squared
crit$R2adj[3]<-sum3$adj.r.squared
crit$aic[3]<-AIC(mod3)
crit$bic[3]<-AIC(mod3,k=log(nrow(w)))

## 4: intercept + type + diameter:
mod4<-lm(price ~ type+diam,data=w)
sum4<-summary(mod4)
crit$R2[4]<-sum4$r.squared
crit$R2adj[4]<-sum4$adj.r.squared
crit$aic[4]<-AIC(mod4)
crit$bic[4]<-AIC(mod4,k=log(nrow(w)))

## 5: intercept + type + diam + type*diam:
mod5<-lm(price ~ type*diam,data=w)
sum5<-summary(mod5)
crit$R2[5]<-sum5$r.squared
crit$R2adj[5]<-sum5$adj.r.squared
crit$aic[5]<-AIC(mod5)
crit$bic[5]<-AIC(mod5,k=log(nrow(w)))

plot(crit$nbr,crit$R2,type="b",xlab="model number")
lines(crit$nbr,crit$R2adj,col="red")
title("R^2 (black), R^2 adjusted (red)")

plot(crit$nbr,crit$aic,type="b",xlab="model number")
lines(crit$nbr,crit$bic,col="red")
title("AIC (black), BIC (red)")

####################################
# Backward elimination

# we start with the "full model" mod5
# by default step uses AIC
step(mod5,direction="backward") # it just returns the largest model
# try with BIC
step(mod5,direction="backward",k=log(dim(w)[1])) # it returns model 4
