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

names(train)[c(2,5,6,12,14)]<-
  c("log.price","log.sqft_living","log.sqft_lot","log.sqft_above","zipcode.gmp")
names(test)[c(2,5,6,12,14)]<-
  c("log.price","log.sqft_living","log.sqft_lot","log.sqft_above","zipcode.gmp")

# fit a model to the transformed data
mm<-lm(log.price~bedrooms+bathrooms+log.sqft_living+log.sqft_lot+floors+log.sqft_above+
         yr_built+zipcode.gmp+lat+long,data = train)
print(ms<-summary(mm))

# lm summary plots
par(mfrow=c(2,2))
plot(mm)

