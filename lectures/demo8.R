###################################################
### code chunk number 11: cvldl1
###################################################
library(leaps)
SA <- data.frame(read.table("SA.dat", header=T)) ## read in the data
SAuse <- SA
SAuse$ldl <- log(SA$ldl)
SAuse$obesity <- log(SA$obesity)
SAuse$age <- log(SA$age)
## Applying the transformations from Lecture 6
yy <- SAuse[,12]
xx <- SAuse[,-12]
## the 12th column is the outcome
rleaps <- regsubsets(xx, yy, int = T, nbest = 1000, nvmax = 250, really.big = T, method = c("ex")) 
## all subset models
cleaps <- summary(rleaps, matrix=T) 
## True/False matrix. The r-th is a True/False statement 
## about which variables are included
Models <- cleaps$which
Models <- rbind(c(T,rep(F,dim(xx)[2])), Models)

###################################################
### code chunk number 12: cvldl2
###################################################
K <- 10
ii <- sample(seq(1,length(yy)),length(yy))
foldsize <- floor(length(yy)/K)
sizefold <- rep(foldsize,K)
restdata <- length(yy)-K*foldsize
if (restdata > 0) {
  sizefold[1:restdata]<-sizefold[1:restdata]+1 }
## creates the size for each fold

###################################################
### code chunk number 13: cvldl3
###################################################
# Here follows K-fold CV (NOTICE: further below follows a second version using predict())
Prederrors <- matrix(0,dim(Models)[1],K)
# a matrix to store the prediction errors in
iused <- 0
Xmat <- as.matrix(cbind(rep(1,dim(xx)[1]),xx))
for (k in (1:K)) {
  itest<-ii[(iused+1):(iused+sizefold[k])] ## the k-fold test set
  itrain<-ii[-c((iused+1):(iused+sizefold[k]))] ## the k-fold training set
  iused<-iused+length(itest)
  for (mm in (1:dim(Models)[1])) {
    betahat<-solve(t(Xmat[itrain,Models[mm,]])%*%Xmat[itrain,
                                                      Models[mm,]])%*%t(Xmat[itrain,Models[mm,]])%*%yy[itrain]
    ypred<-Xmat[itest,Models[mm,]]%*%betahat ## predictions
    Prederrors[mm,k]<-sum((yy[itest]-ypred)^2) } }
PE <- apply(Prederrors,1,sum)/length(yy)  ## final prediction errors, average across all folds.


# HERE FOLLOWS ANOTHER VERSION OF THE CODE ABOVE, USING PREDICT()
Prederrors <- matrix(0,dim(Models)[1],K)
# a matrix to store the prediction errors in
iused <- 0
Xmat <- as.matrix(cbind(rep(1,dim(xx)[1]),xx))
for (k in (1:K)) {
  itest <- ii[(iused + 1):(iused + sizefold[k])]
  itrain <- ii[-c((iused + 1):(iused + sizefold[k]))]
  iused <- iused + length(itest)
  # error for the model with intercept only
  Prederrors[1,k] <- sum((yy[itest] - mean(yy[itrain]))^2)
  # error for the models including slopes
  for (mm in (2:dim(Models)[1])) {
    xi <- Xmat[itrain, Models[mm,]]
    # remove the intercept, lm does not take intercept arguments
    xi <- xi[,-1]
    yi <- yy[itrain]
    lin.mod <- lm(yi ~ xi)
    xi <- Xmat[itest, Models[mm,]]
    # remove the intercept, lm does not take intercept arguments
    xi <- xi[,-1]
    yhat <- predict(lin.mod, as.data.frame(xi))
    Prederrors[mm,k] <- sum((yy[itest] - yhat)^2)    
  }
}

PE <- apply(Prederrors,1,sum)/length(yy)  # final prediction errors.

###################################################
### code chunk number 14: cvldl4
###################################################
jj<-sort.list(PE)[1:5]
print(as.matrix(Models[jj,]))

plot(c(1,apply(cleaps$which,1,sum)),PE,xlab="model size",ylab="CV error")

###################################################
### code chunk number 15: cvldl4b
###################################################
z<-data.frame(as.matrix(cbind(Prederrors[jj,],PE[jj])))
colnames(z) <- c("Fold1","Fold2","Fold3","Fold4","Fold5","Fold6","Fold7","Fold8",
                 "Fold9","Fold10","PE")
row.names<-c(seq(1,5))
xtable(z, digits=c(0, rep(3,10),5),
       caption="Prederrors in different folds and total",label="tab:CVldl")

###################################################
### code chunk number 16: cvldl5
###################################################
winmod<-Models[which.min(PE),]
print((names(SA)[-12])[winmod[-1]==T])

###################################################
### code chunk number 17: cvldl6
###################################################
mm<-lm(log(ldl)~log(age)+sbp+adiposity+log(obesity)+typea+alcohol+
         alcind+tobacco+tobind+as.factor(chd)+as.factor(famhist),data=SA)
ss<-step(mm,trace=F)
print(ss)

###################################################
### code chunk number 18: cvldl6
###################################################
#LOOCV
PE<-rep(0,dim(Models)[1])
PEwrong<-rep(0,dim(Models)[1])
for (mm in (1:dim(Models)[1])) {
  modfit<-lm(yy~Xmat[,Models[mm,]]-1)
  # Umberto: below I fix a buggy code by commenting it out
  # PE[mm] <- (sum(resid(modfit)^2/(1-hatvalues(modfit))^2))/length(yy)
  # rstud<-rstudent(modfit)*summary(modfit)$sig
  # PE[mm]<-sum((rstud)^2)/length(yy) 
  # Here follows the fixed code
  PE[mm] <- (sum(resid(modfit)^2/(1-hatvalues(modfit))^2))/length(yy)
}

###################################################
### code chunk number 19: cvldl4
###################################################
jj<-sort.list(PE)[1:5]
print(as.matrix(Models[jj,]))
