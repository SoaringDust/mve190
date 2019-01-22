TVdat<-read.table("TV.dat") ## reading the data into R
print(dim(TVdat)) ## printing the dimensions of the data
print(names(TVdat))  ## printing the names of the variables
print(row.names(TVdat)) ## printing the names of the countries included
plot(TVdat$ppD,TVdat$ppT,xlab="people per Dr",ylab="people per TV")
id<-identify(TVdat$ppD,TVdat$ppT,row.names(TVdat),pos=T)
## identify() allows you to mark observations in the data with a left mouse click
## end the identify session by pressing ESC

## this code is just a way for me to allow for output to be put in my handouts on the go
## the if-statement allows for the figure to be created even if there are no observations marked
#plot(TVdat$ppD,TVdat$ppT,xlab="people per Dr",ylab="people per TV")
#if (max(id$ind)!=-Inf) {
#text(TVdat$ppD[id$ind],TVdat$ppT[id$ind],row.names(TVdat)[id$ind],pos=id$pos) }

plot(log(TVdat$ppD),TVdat$ppT) ## taking logs of the x-variable
id<-identify(log(TVdat$ppD),TVdat$ppT,row.names(TVdat),pos=T)

#plot(log(TVdat$ppD),TVdat$ppT,xlab="log(people per Dr)",ylab="people per TV")
#if (max(id$ind)!=-Inf) {text(log(TVdat$ppD)[id$ind],TVdat$ppT[id$ind],row.names(TVdat)[id$ind],pos=id$pos)}

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

