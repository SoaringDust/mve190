TVdat<-read.table("TV.dat") ## reading the data into R
print(dim(TVdat)) ## printing the dimensions of the data
print(names(TVdat))  ## printing the names of the variables
print(row.names(TVdat)) ## printing the names of the countries included
plot(TVdat$ppD,TVdat$ppT,xlab="people per Dr",ylab="people per TV")
id<-identify(TVdat$ppD,TVdat$ppT,row.names(TVdat),pos=T)
## identify() allows you to mark observations in the data with a left mouse click
## end the identify session with a right mouse click

## this code is just a way for me to allow for output to be put in my handouts on the go
## the if-statement allows for the figure to be created even if there are no observations marked
plot(TVdat$ppD,TVdat$ppT,xlab="people per Dr",ylab="people per TV")
if (max(id$ind)!=-Inf) {
  text(TVdat$ppD[id$ind],TVdat$ppT[id$ind],row.names(TVdat)[id$ind],pos=id$pos) }

plot(log(TVdat$ppD),TVdat$ppT) ## taking logs of the x-variable
id<-identify(log(TVdat$ppD),TVdat$ppT,row.names(TVdat),pos=T)

plot(log(TVdat$ppD),TVdat$ppT,xlab="people per Dr",ylab="people per TV")
if (max(id$ind)!=-Inf) {text(log(TVdat$ppD)[id$ind],TVdat$ppT[id$ind],row.names(TVdat)[id$ind],pos=id$pos)}

plot(log(TVdat$ppD),log(TVdat$ppT)) ## taking logs of both x and y
id<-identify(log(TVdat$ppD),log(TVdat$ppT),row.names(TVdat),pos=T)
mm<-lm(log(TVdat$ppT)~log(TVdat$ppD))
# missing values for ppT for two countries
lines(sort(log(TVdat$ppD)[is.na(TVdat$ppT)==F]),mm$fit[sort.list(log(TVdat$ppD)[is.na(TVdat$ppT)==F])])
## here I use is.na() to identify the observations with missing values, I only plot those that had
## is.na==F for FALSE

plot(log(TVdat$ppD),log(TVdat$ppT))
if (max(id$ind)!=-Inf) {text(log(TVdat$ppD)[id$ind],log(TVdat$ppT)[id$ind],row.names(TVdat)[id$ind],pos=id$pos)}
lines(sort(log(TVdat$ppD)[is.na(TVdat$ppT)==F]),mm$fit[sort.list(log(TVdat$ppD)[is.na(TVdat$ppT)==F])])

## here I create a regression summary in a LateX table format
## you can just use the command summary(mm) in R directly
library(xtable)
xtable(summary(mm), caption="Regression summary", label="tab:ch4")

induse<-seq(1,dim(TVdat)[1])[is.na(TVdat$ppT)==F] ##id the data with non-missing y-values
plot(log(TVdat$ppD)[induse],mm$res)
abline(h=0)
id<-identify(log(TVdat$ppD)[induse],mm$res,row.names(TVdat)[induse],pos=T) ## mark large residuals

plot(log(TVdat$ppD)[induse],mm$res)
abline(h=0)
if (max(id$ind)!=-Inf) {text((log(TVdat$ppD)[induse])[id$ind],mm$res[id$ind],(row.names(TVdat)[induse])[id$ind],pos=id$pos)}

lmi<-lm.influence(mm)
plot(log(TVdat$ppD)[induse],lmi$hat,ylab="leverage")
id<-identify(log(TVdat$ppD)[induse],lmi$hat, row.names(TVdat)[induse],pos=T)

plot(log(TVdat$ppD)[induse],lmi$hat,ylab="leverage")
if (max(id$ind)!=-Inf) {text((log(TVdat$ppD)[induse])[id$ind],lmi$hat[id$ind], (row.names(TVdat)[induse])[id$ind],pos=id$pos)}
p<-locator()

plot(induse,lmi$coef[,2],ylab="Impact on Slope")
abline(h=0)
id<-identify(induse,lmi$coef[,2],label=row.names(TVdat)[induse],pos=T)

plot(induse,lmi$coef[,2],ylab="Impact on Slope")
abline(h=0)
if (max(id$ind)!=-Inf) {text(induse[id$ind],(lmi$coef[,2])[id$ind],label=(row.names(TVdat)[induse])[id$ind],pos=id$pos)}

plot(induse,lmi$sig,ylab="Impact on Sum of Squares")
id<-identify(induse,lmi$sig,label=row.names(TVdat)[induse],pos=T)

plot(induse,lmi$sig,ylab="Impact on Sum of Squares")
if (max(id$ind)!=-Inf) {text(induse[id$ind],lmi$sig[id$ind],label=(row.names(TVdat)[induse])[id$ind],pos=id$pos)}

print(summary(mm))

ms<-summary(mm)

library(xtable)
## the confint() function computes the t-based confidence intervals for the intercept and slope
## I use the xtable() function to create a table for LaTeX 
z<-data.frame(matrix(confint(mm),2,2))
dimnames(z) <- list(c("intercept","slope"), c("2.5%", "97.5%"))
xtable(z, digits=c(0, 3, 3),caption="95 percent confidence intervals",label="tab:ci")
p<-locator()

z<-data.frame(matrix(ms$fstat,1,3))
dimnames(z) <- list(c(" "), c("value", "df1", "df2"))
xtable(z, digits=c(0, 3, 3, 3),caption="F-statistic",label="tab:fsumTV")
pval<-1-pf(ms$fs[1],ms$fs[2],ms$fs[3])
ord<-3+abs(floor(log10(pval)))
p<-locator()

ii<-sample(seq(1,dim(TVdat)[1]),10) ## withholding 10 samples for prediction
x<-sort(log(TVdat$ppD[-ii]))
y<-log(TVdat$ppT[-ii])[sort.list(log(TVdat$ppD[-ii]))]
plot(x,y,xlab="log-ppDr",ylab="log-ppTV")
mm<-lm(y~x)
ms<-summary(mm)
lines(x[is.na(y)==F],mm$fitted,lwd=2)
xp<-seq(min(log(TVdat$ppD)),max(log(TVdat$ppD)),by=.1)
xuse<-x[is.na(y)==F]
hats<-1/length(xuse)+(xp-mean(xuse))^2/sum((xuse-mean(xuse))^2)
lines(xp,mm$coef[1]+mm$coef[2]*xp + qt(.975,length(xuse)-2) * sqrt(ms$sigma^2 * hats),lwd=2,lty=2)
lines(xp,mm$coef[1]+mm$coef[2]*xp - qt(.975,length(xuse)-2) * sqrt(ms$sigma^2 * hats),lwd=2,lty=2)
lines(xp,mm$coef[1]+mm$coef[2]*xp + qt(.975,length(xuse)-2) * sqrt(ms$sigma^2 * (1+hats)),lwd=2,col=3,lty=3)
lines(xp,mm$coef[1]+mm$coef[2]*xp - qt(.975,length(xuse)-2) * sqrt(ms$sigma^2 * (1+hats)),lwd=2,col=3,lty=3)
points(log(TVdat$ppD)[ii],log(TVdat$ppT)[ii],col=3,pch=2)
points(log(TVdat$ppD)[ii],mm$coef[1]+mm$coef[2]*log(TVdat$ppD)[ii],col=3,pch=8)
id<-identify(log(TVdat$ppD)[ii],log(TVdat$ppT)[ii],row.names(TVdat)[ii],pos=T)

plot(x,y,xlab="log-ppDr",ylab="log-ppTV")
lines(x[is.na(y)==F],mm$fitted,lwd=2)
lines(xp,mm$coef[1]+mm$coef[2]*xp + qt(.975,length(xuse)-2) * sqrt(ms$sigma^2 * hats),lwd=2,lty=2)
lines(xp,mm$coef[1]+mm$coef[2]*xp - qt(.975,length(xuse)-2) * sqrt(ms$sigma^2 * hats),lwd=2,lty=2)
lines(xp,mm$coef[1]+mm$coef[2]*xp + qt(.975,length(xuse)-2) * sqrt(ms$sigma^2 * (1+hats)),lwd=2,col=3,lty=3)
lines(xp,mm$coef[1]+mm$coef[2]*xp - qt(.975,length(xuse)-2) * sqrt(ms$sigma^2 * (1+hats)),lwd=2,col=3,lty=3)
points(log(TVdat$ppD)[ii],log(TVdat$ppT)[ii],col=3,pch=2)
points(log(TVdat$ppD)[ii],mm$coef[1]+mm$coef[2]*log(TVdat$ppD)[ii],col=3,pch=8)
if (max(id$ind)!=-Inf) {text((log(TVdat$ppD)[ii])[id$ind],(log(TVdat$ppT)[ii])[id$ind],(row.names(TVdat)[ii])[id$ind],pos=id$pos)}