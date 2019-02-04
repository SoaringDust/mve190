setwd("~/RStudioProjects/MVE190/projects/mini3/")

housedata<-data.frame(read.csv("kc_house_data.csv",header=T)) ## read in the data
print(dim(housedata)) ## dimensions
print(names(housedata)) ## variable names

# select training and test data
ntrain<-500
ii<-sample(seq(1,dim(housedata)[1]),ntrain)
train<-housedata[ii,]
row.names(train)<-seq(1,ntrain)
remainingdata<-housedata[-ii,]
ii<-sample(seq(1,dim(remainingdata)[1]),ntrain)
test<-remainingdata[ii,]
row.names(test)<-seq(1,dim(test)[1])

# plot variables vs price
par(mfrow=c(2,2))
plot(train$bedrooms,train$price,main = "price on bedrooms")
plot(train$bathrooms,train$price,main = "price on bathrooms")
plot(train$sqft_living,train$price,main = "price on sqft_living")
plot(train$sqft_lot,train$price,main = "price on sqft_lot")

par(mfrow=c(2,2))
plot(train$floors,train$price,main = "price on floors")
plot(train$sqft_above,train$price,main = "price on sqft_above")
plot(train$yr_built,train$price,main = "price on yr_built")

par(mfrow=c(2,2))
plot(train$zipcode,train$price,main = "price on zipcode")
plot(train$lat,train$price,main = "price on lat")
plot(train$long,train$price,main = "price on long")

# plot transformations
par(mfrow=c(2,2))
plot(train$bedrooms,log10(train$price),main = "log10(price) on bedrooms")
plot(train$bathrooms,log10(train$price),main = "log10(price) on bathrooms")
plot(log10(train$sqft_living),log10(train$price),main = "log10(price) on log10(sqft_living)")
plot(log10(train$sqft_lot),log10(train$price),main = "log10(price) on log10(sqft_lot)")

par(mfrow=c(2,2))
plot(train$floors,log10(train$price),main = "log10(price) on floors")
plot(log10(train$sqft_above),log10(train$price),main = "log10(price) on log10(sqft_above)")
plot(train$yr_built,log10(train$price),main = "log10(price) on yr_built")

par(mfrow=c(2,2))
plot(1/train$zipcode,log10(train$price),main = "log10(price) on 1/zipcode")
plot(train$lat,log10(train$price),main = "log10(price) on lat")
plot(train$long,log10(train$price),main = "log10(price) on long")

# transform columns
train$price<-log10(train$price)
train$sqft_living<-log10(train$sqft_living)
train$sqft_lot<-log10(train$sqft_lot)
train$sqft_above<-log10(train$sqft_above)
train$zipcode<-1/train$zipcode

test$price<-log10(test$price)
test$sqft_living<-log10(test$sqft_living)
test$sqft_lot<-log10(test$sqft_lot)
test$sqft_above<-log10(test$sqft_above)
test$zipcode<-1/test$zipcode

names(train)[c(4,7,8,14,18)]<-
  c("log.price","log.sqft_living","log.sqft_lot","log.sqft_above","zipcode.gmp")
names(test)[c(4,7,8,14,18)]<-
  c("log.price","log.sqft_living","log.sqft_lot","log.sqft_above","zipcode.gmp")

# fit a model to the transformed data
mm<-lm(log.price~bedrooms+bathrooms+log.sqft_living+log.sqft_lot+floors+log.sqft_above+
         yr_built+zipcode.gmp+lat+long,data = train)
print(ms<-summary(mm))

# lm summary plots
par(mfrow=c(2,2))
plot(mm)

# check residuals
par(mfrow=c(1,1))
plot(mm$fit,mm$res,xlab="fitted values",ylab="residuals")
abline(h=0)
id1<-identify(mm$fit,mm$res,pos=T)

## check normal error assumption
qq<-seq(.5/ntrain,(ntrain-.5)/ntrain,length=ntrain)
normq<-qnorm(p=qq)
rsort<-sort(rstandard(mm))
rlist<-sort.list(rstandard(mm))
plot(normq,rsort,xlab="Theoretical quantiles",ylab="Standardized residuals")
qqline(rstandard(mm))
id2<-identify(normq,sort(rstandard(mm)),label=rlist,pos=T)

# check constant error variance using absolute residuals
plot(mm$fit,abs(rstandard(mm)),xlab="fitted values", ylab="|standardized residuals|")
id3<-identify(mm$fit,abs(rstandard(mm)),pos=T)

# check the Cook's distance
cooksd<-cooks.distance(mm)
plot(cooksd,main="Cooks Distance",type="h")
idc<-identify(cooksd,pos=T)

# check the leverage and impact on slopes
lm1<-lm.influence(mm)
plot(lm1$hat,main="Leverage")
idlev<-identify(lm1$hat,pos=T)

# check the change in residual std deviation when the i-th observation is dropped
plot(lm1$sig,main="change in sigma")
ids<-identify(lm1$sig,pos=T)

# summing up the worst outliers
indvec<-sort(c(id1$ind,rlist[id2$ind],id3$ind,idlev$ind,ids$ind))
print(table(indvec)) ## how many of each?
maxid<-max(table(indvec))
indout<-unique(indvec)[table(indvec)==max(table(indvec))]

# regression without the identified outlier
mmb<-lm(log.price~bedrooms+bathrooms+log.sqft_living+log.sqft_lot+floors+log.sqft_above+
                 yr_built+zipcode.gmp+lat+long,data = train,subset=-indout)
print(summary(mmb))

# summary plots
par(mfrow=c(2,2))
plot(mmb)

# backward selection
selectstep<-step(mmb,trace=F)
print(summary(selectstep))

# prediction
predval<-predict(selectstep,newdata=test)
prederror<-sum((test[,4]-predval)^2)  # prediction MSE
print(prederror)

# compare refit
mmtest<-lm(log.price~bedrooms+bathrooms+log.sqft_living+log.sqft_lot+floors+log.sqft_above+
          yr_built+zipcode.gmp+lat+long,data = test)
print(selecttest<-step(mmtest,trace=F))
fiterror<-sum(summary(selecttest)$res^2)
print(fiterror)


yy<-train[,1] ## training data
xx<-as.matrix(train[,-1])
yyt<-test[,1] ## test data
xxt<-as.matrix(test[,-1])

library(leaps)
# all subset models
#rleaps<-regsubsets(xx,yy,int=T,nbest=500,nvmax=dim(housedata)[2],really.big=T,method=c("ex")) 
