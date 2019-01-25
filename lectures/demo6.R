### R code from vignette source 'Lecture6-2015.Rnw'

###################################################
### code chunk number 1: ldl1
###################################################
SA<-data.frame(read.table('SA.dat',sep='\t',header=T)) ## read in the data
print(dim(SA)) ## dimensions
print(names(SA)) ## variable names


###################################################
### code chunk number 2: ldl2
###################################################
## scatter plots
par(mfrow=c(2,2))
plot(SA$obesity,SA$ldl,main="LDL on obesity")
plot(SA$sbp,SA$ldl,main="LDL on blood pressure")
plot(SA$tobacco,SA$ldl,main="LDL on tobacco usage")
plot(SA$alcohol,SA$ldl,main="LDL on alcohol consumption")
p<-locator()


###################################################
### code chunk number 3: ldl3
###################################################
## scatter plots
par(mfrow=c(2,2))
boxplot(SA$ldl~SA$chd,main="LDL on coronary heart disease status")
boxplot(SA$ldl~as.factor(SA$famhist),main="LDL on family history of same")
plot(SA$age,SA$ldl,main="LDL on age")
plot(SA$typea,SA$ldl,main="LDL on type A behaviour")
p<-locator()


###################################################
### code chunk number 4: ldl4
###################################################
## trying transformations
par(mfrow=c(2,2))
plot(log(SA$obesity),log(SA$ldl),main="log(LDL) on log(obesity)")
plot(log(SA$age),log(SA$ldl),main="log(LDL) on log(age)")
plot(SA$adi,log(SA$ldl),main="log(LDL) on adiposity")
plot(SA$sbp,log(SA$ldl),main="log(LDL) on blood pressure")
p<-locator()


###################################################
### code chunk number 5: ldl5
###################################################
## linear model fit and summary
mm1<-lm(log(ldl)~log(age)+log(obesity)+as.factor(chd)+as.factor(famhist)+as.factor(tobind)+as.factor(alcind)+tobacco+alcohol+adiposity+typea+sbp,data=SA)
#library(xtable)
#xtable(summary(mm1), caption="Regression summary", label="tab:ldl")
print(summary(mm1))


###################################################
### code chunk number 6: ldl5b
###################################################
## diagnostic plots
par(mfrow=c(2,2))
plot(mm1)
p<-locator()


###################################################
### code chunk number 7: ldl-out1
###################################################
## cooks distance
par(mfrow=c(1,1))
lm1<-lm.influence(mm1)
cooksd<-cooks.distance(mm1)
plot(cooksd,main="Cook's Distance",type="h")
abline(h=qf(.95,1,mm1$df),lty=2)
id<-identify(cooksd,pos=T)


###################################################
### code chunk number 8: ldl-out1fig
###################################################
plot(cooksd,main="Cook's Distance",type="h")
abline(h=qf(.95,1,mm1$df),lty=2)
if (max(id$ind)!=-Inf) {text(id$ind,cooksd[id$ind],id$ind,pos=id$pos) }


###################################################
### code chunk number 9: ldl-out2
###################################################
## leverage
plot(lm1$hat,main="Leverage")
idlev<-identify(lm1$hat,pos=T)


###################################################
### code chunk number 10: ldl-out2fig
###################################################
plot(lm1$hat,main="Leverage")
abline(h=3*length(mm1$coef)/dim(SA)[1])
if (max(idlev$ind)!=-Inf) {text(idlev$ind,lm1$hat[idlev$ind],idlev$ind,pos=idlev$pos)}


###################################################
### code chunk number 11: ldl-out3
###################################################
## several plots that summarize effect on slope estimates
plot(lm1$coeff[,4],main="change in slope 4")
id4<-identify(lm1$coeff[,4],pos=T)


###################################################
### code chunk number 12: ldl-out3fig
###################################################
plot(lm1$coeff[,4],main="change in slope 4")
if (max(id4$ind)!=-Inf) {text(id4$ind,lm1$coeff[id4$ind,4],id4$ind,pos=id4$pos)}


###################################################
### code chunk number 13: ldl-out4
###################################################
plot(lm1$coeff[,6],main="change in slope 6")
id6<-identify(lm1$coeff[,6],pos=T)


###################################################
### code chunk number 14: ldl-out4fig
###################################################
plot(lm1$coeff[,6],main="change in slope 6")
if (max(id6$ind)!=-Inf) {text(id6$ind,lm1$coeff[id6$ind,6],id6$ind,pos=id6$pos)}


###################################################
### code chunk number 15: ldl-out5
###################################################
plot(lm1$coeff[,9],main="change in slope 9")
id9<-identify(lm1$coeff[,9],pos=T)


###################################################
### code chunk number 16: ldl-out5fig
###################################################
plot(lm1$coeff[,9],main="change in slope 9")
if (max(id9$ind)!=-Inf) {text(id9$ind,lm1$coeff[id9$ind,9],id9$ind,pos=id9$pos) }


###################################################
### code chunk number 17: ldl-out6
###################################################
plot(lm1$coeff[,10],main="change in slope 10")
id10<-identify(lm1$coeff[,10],pos=T)


###################################################
### code chunk number 18: ldl-out6fig
###################################################
plot(lm1$coeff[,10],main="change in slope 10")
if (max(id10$ind)!=-Inf) {text(id10$ind,lm1$coeff[id10$ind,10],id10$ind,pos=id10$pos)}


###################################################
### code chunk number 19: ldl-outlier
###################################################
## I collect all the identified observations from the above plots
indvec<-c(id$ind,idlev$ind,id4$ind,id6$ind,id9$ind,id10$ind)
print(table(indvec)) ## how many of each? 
indout<-unique(sort(indvec))[sort.list(table(indvec),decreasing=T)[1]] ## pick the one most frequently identified


###################################################
### code chunk number 20: ldl-rmout
###################################################
## regression without the identified outlier
mm1b<-lm(log(ldl)~log(age)+log(obesity)+as.factor(chd)+as.factor(famhist)+as.factor(tobind)+as.factor(alcind)+tobacco+alcohol+adiposity+typea+sbp,data=SA,subset=-indout)
print(summary(mm1b))


###################################################
### code chunk number 21: ldl-rmoutfig
###################################################
par(mfrow=c(2,2))
plot(mm1b)
p<-locator()


###################################################
### code chunk number 22: ldl-cormat
###################################################
## a graphical summary of collinearity
SA2<-SA
SA2$obesity<-log(SA$obesity)
SA2$age<-log(SA$age)
SA2$ldl<-log(SA$ldl)
distmat<-1-cor(SA2[,-12])
library(gplots)
hh<-heatmap.2(as.matrix(distmat), col=redgreen(75),cexRow=.5,key=TRUE, symkey=FALSE, density.info="none", trace="none")


###################################################
### code chunk number 23: ldl-step
###################################################
## backward selection of variables
step(mm1b,directions='backward')


###################################################
### code chunk number 24: ldl-rmout-2
###################################################
## regression without the identified outlier
mm1b<-lm(log(ldl)~log(age)+log(obesity)+as.factor(chd)+as.factor(famhist)+as.factor(tobind)+as.factor(alcind)+tobacco+alcohol+adiposity+typea+sbp,data=SA,subset=-indout)
ss<-step(mm1b,trace=0,k=500)
print(ss$anova)


###################################################
### code chunk number 25: ldl-anova-backward
###################################################
RSSvec<-ss$anova[,5]
deltaRSSvec<-diff(RSSvec)
MSEvec<-RSSvec[2:length(RSSvec)]/ss$anova[2:length(RSSvec),4]
Fvec<-deltaRSSvec/MSEvec
pvals<-1-pf(Fvec,1,ss$anova[2:length(RSSvec),4])
out<-cbind(ss$anova[2:length(RSSvec),1],round(Fvec,3),round(pvals,5))
colnames(out)<-c("variable drop","F","p-value")
print(out)
