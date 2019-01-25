###################################################
### code chunk number 10: cars1
###################################################
cars<-data.frame(read.table("cars.dat",header=T)) ## read in the data
print(dim(cars)) ## dimensions
print(names(cars)) ## variable names


###################################################
### code chunk number 11: cars1b
###################################################
## Creating a training and test data set. 
ntrain<-60
ii<-sample(seq(1,dim(cars)[1]),ntrain)
# ntrain cars = training set
cars.train<-cars[ii,]
row.names(cars.train)<-seq(1,ntrain)
cars.test<-cars[-ii,]
row.names(cars.test)<-seq(1,dim(cars.test)[1])


###################################################
### code chunk number 12: cars2a
###################################################
par(mfrow=c(2,2))
plot(cars.train$ci,cars.train$mid,main="price on citympg")
plot(cars.train$hw,cars.train$ci,main="citympg on hwmpg")
plot(cars.train$le,cars.train$ci,main="citympg on length")
plot(cars.train$engsize,cars.train$ci,main="citympg on enginesize")
p<-locator()


###################################################
### code chunk number 13: cars2b
###################################################
par(mfrow=c(2,2))
plot(cars.train$le,cars.train$wi,main="width on length")
plot(cars.train$wi,cars.train$we,main="weight on width")
plot(cars.train$le,cars.train$wh,main="wheelbase on length")
plot(cars.train$lu,cars.train$rea,main="rearroom on luggage room")
p<-locator()


###################################################
### code chunk number 14: cars3a
###################################################
par(mfrow=c(2,2))
plot(cars.train$ci,cars.train$mid,main="price on citymg")
plot(cars.train$we,cars.train$mid,main="price on weight")
plot(cars.train$ho,cars.train$mid,main="price on horsepower")
plot(cars.train$man,cars.train$mid,main="price on transmission")
p<-locator()


###################################################
### code chunk number 15: cars3b
###################################################
par(mfrow=c(2,2))
plot(1/cars.train$ci,log(cars.train$mid),main="log(price) on 1/citympg")
plot(cars.train$we,log(cars.train$mid),main="log(price) on weight")
plot(cars.train$ho,log(cars.train$mid),main="log(price) on horsepower")
plot(cars.train$man,log(cars.train$mid),main="log(price) on transmission")
p<-locator()


###################################################
### code chunk number 16: cars4
###################################################
cars.train[,1]<-log(cars.train$mid)
cars.train[,2]<-1/cars.train$ci
cars.train[,3]<-1/cars.train$hw
cars.test[,1]<-log(cars.test$mid)
cars.test[,2]<-1/cars.test$ci
cars.test[,3]<-1/cars.test$hw
names(cars.train)[c(1,2,3)]<-c("log.mid.price","city.gpm","hw.gpm")
names(cars.test)[c(1,2,3)]<-c("log.mid.price","city.gpm","hw.gpm")


###################################################
### code chunk number 17: cars5
###################################################
## Fitting the model after transforming the data
mm<-lm(log.mid.price~city.gpm+hw.gpm+airbagstd+cylnbr+
         engsize+horsepwr+rpm.at.max+engrev.high+mantrans.op+
         fueltank+passengers+length+width+uturn+rearroom+
         luggage+weight+domestic,data=cars.train)
print(ms<-summary(mm))


###################################################
### code chunk number 18: cars5b
###################################################
par(mfrow=c(2,2))
plot(mm)
p<-locator()


###################################################
### code chunk number 19: cars6
###################################################
## Checking the residuals.
par(mfrow=c(1,1))
plot(mm$fit,mm$res,xlab="fitted values",ylab="residuals")
abline(h=0)
id1<-identify(mm$fit,mm$res,pos=T)



###################################################
### code chunk number 21: cars7
###################################################
## Checking normal error assumption
qq<-seq(.5/ntrain,(ntrain-.5)/ntrain,length=ntrain)
normq<-qnorm(p=qq)
rsort<-sort(rstandard(mm))
rlist<-sort.list(rstandard(mm))
plot(normq,rsort,xlab="Theoretical quantiles",ylab="Standardized residuals")
qr<-quantile(rstandard(mm))
qn<-quantile(qnorm(p=qq))
b<-(qr[4]-qr[2])/(qn[4]-qn[2])
a<-qr[4]-b*qn[4]
abline(a,b)
id2<-identify(normq,sort(rstandard(mm)),label=rlist,pos=T)



###################################################
### code chunk number 23: cars8
###################################################
## Checking constant error variance using absolute residuals
plot(mm$fit,abs(rstandard(mm)),xlab="fitted values", ylab="|standardized residuals|")
id3<-identify(mm$fit,abs(rstandard(mm)),pos=T)




###################################################
### code chunk number 25: cars9
###################################################
## Checking the Cooks distance
lm1<-lm.influence(mm)
cooksd<-cooks.distance(mm)
plot(cooksd,main="Cooks Distance",type="h")
abline(h=qf(.95,1,mm$df),lty=2)
idc<-identify(cooksd,pos=T)




###################################################
### code chunk number 27: cars10
###################################################
## Checking leverage and impact on slopes
plot(lm1$hat,main="Leverage")
idlev<-identify(lm1$hat,pos=T)



###################################################
### code chunk number 29: cars11
###################################################
plot(lm1$coeff[,4],main="change in slope 4")
id4<-identify(lm1$coeff[,4],pos=T)




###################################################
### code chunk number 31: cars12
###################################################
plot(lm1$coeff[,7],main="change in slope 7")
id7<-identify(lm1$coeff[,7],pos=T)



###################################################
### code chunk number 33: cars13
###################################################
plot(lm1$sig,main="change in sigma")
ids<-identify(lm1$sig,pos=T)



###################################################
### code chunk number 35: cars14
###################################################
indvec<-sort(c(id1$ind,rlist[id2$ind],id3$ind,idlev$ind,id4$ind,id7$ind,ids$ind))
print(table(indvec)) ## how many of each?
maxid<-max(table(indvec))
indout<-unique(indvec)[table(indvec)==max(table(indvec))]
# Here I just identify the "worst" outliers - when you run the code you might want to drop
# all unique observations in indvec. 
# In that case, use indout<-unique(indvec)


###################################################
### code chunk number 36: cars15
###################################################
## regression without the identified outlier
mmb<-lm(log.mid.price~city.gpm+hw.gpm+airbagstd+cylnbr+
          engsize+horsepwr+rpm.at.max+engrev.high+mantrans.op+
          fueltank+passengers+length+width+uturn+rearroom+
          luggage+weight+domestic,data=cars.train,subset=-indout)
print(summary(mmb))


###################################################
### code chunk number 37: cars15b
###################################################
par(mfrow=c(2,2))
plot(mmb)
p<-locator()


###################################################
### code chunk number 38: cars16
###################################################
selectstep<-step(mmb,trace=F) # backward selection


###################################################
### code chunk number 39: cars17
###################################################
print(summary(selectstep))


###################################################
### code chunk number 40: cars18
###################################################
predval<-predict(selectstep,newdata=cars.test) ## Prediction 
prederror<-sum((cars.test[,1]-predval)^2)  ## Prediction MSE
#
## Note that the outcome Price is in the first column of
## cars.test. If you use this code on other data sets, make 
## sure you keep track of which column contains your y-variable. 


###################################################
### code chunk number 41: cars19
###################################################
# compare refit
mmtest<-lm(log.mid.price~city.gpm+hw.gpm+airbagstd+cylnbr+
             engsize+horsepwr+rpm.at.max+engrev.high+mantrans.op+
             fueltank+passengers+length+width+uturn+rearroom+
             luggage+weight+domestic,data=cars.test)
print(selecttest<-step(mmtest,trace=F))
fiterror<-sum(summary(selecttest)$res^2)


###################################################
### code chunk number 42: cars20
###################################################
yy<-cars.train[,1] ## training data
xx<-as.matrix(cars.train[,-1])
yyt<-cars.test[,1] ## test data
xxt<-as.matrix(cars.test[,-1])
###
#install.packages("leaps") ## You only have to do this once
library(leaps)  ## But you have to do this everytime you start a new session
rleaps<-regsubsets(xx,yy,int=T,nbest=500,nvmax=dim(cars)[2],really.big=T,method=c("ex")) 
## all subset models
cleaps<-summary(rleaps,matrix=T) 
## True/False matrix. The r-th row is a True/False statement 
## about which variables are included in model r.
tt<-apply(cleaps$which,1,sum) ## size of each model in the matrix
mses<-cleaps$rss/length(yy) ## corresponding MSEs


###################################################
### code chunk number 43: cars20b
###################################################
## First add the intercept-only model to the list
tt<-c(1,tt)
nullrss<-sum((yy-mean(yy))^2)/length(yy)
mses<-c(nullrss,mses)
plot(tt,mses,xlab="number of parameters",ylab="MSE",main="MSE for all subset models")
tmin<-min(tt) ## smallest model tried
tmax<-max(tt) ## biggest model tried
tsec<-seq(tmin,tmax)
msevec<-rep(0,length(tsec))
for (tk in 1:length(tsec)) {
  msevec[tk]<-min(mses[tt==tsec[tk]])} ## the best model for each size
lines(tsec,msevec,lwd=2,col=2) ## a line connecting the best models.
p<-locator()


###################################################
### code chunk number 44: cars20c
###################################################
plot(tsec,msevec,xlab="number of parameters",
     ylab="MSE",main="MSE for best model of each size",type="b",col=4,lwd=2)
p<-locator()


###################################################
### code chunk number 45: cars21
###################################################
pmses<-rep(0,dim(cleaps$which)[1]) ## creates an empty vector to store pMSE in
for (ta in (1:dim(cleaps$which)[1])) {
  mmr<-lm(yy~xx[,cleaps$which[ta,-1]==T]) ## Fit each of the stored models to training data
  Xmat<-cbind(rep(1,dim(xxt)[1]),xxt[,cleaps$which[ta,-1]==T])
  PEcp<-sum((yyt-Xmat%*%mmr$coef)^2)/length(yyt) ## Compute the prediction error
  ## Note, I create the design matrix Xmat for the current model by
  ## including a column of 1s first (for the intercept) and then
  ## tagging on xxt[,cleaps$which[ta,-1]==T] which contains the 
  ## x-variables for which row ta of the cleaps$which model matrix has T for True.
  pmses[ta]<-PEcp }
nullpmse<-sum((yyt-mean(yy))^2)/length(yyt)
pmses<-c(nullpmse,pmses)
pmsevec<-rep(0,length(tsec))
for (tk in 1:length(tsec)) {
  pmsevec[tk]<-min(pmses[tt==tsec[tk]])} ## winning model for each size


###################################################
### code chunk number 46: cars21b
###################################################
plot(tsec,pmsevec,xlab="number of parameters",
     ylab="pMSE",main="prediction MSE", type="b",lwd=2,col=2)


###################################################
### code chunk number 47: cars22
###################################################
ptmin<-which.min(pmses)
## which model has the smallest pmse?
ModMat<-rbind(c("TRUE", rep("FALSE",dim(cleaps$which)[2]-1)),cleaps$which)
# First row of ModMat is the Intercept only model
pmod<-ModMat[ptmin,]
winsize<-sum(pmod==T)
## Winning model
print(names(pmod[pmod==T])[-1])
print(names(selectstep$model)[-1])
## Compare to backward selection model
