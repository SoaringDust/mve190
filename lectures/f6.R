## R-code for lecture 6.

w <- read.table('f6data.txt',header=T)
plot(w$x,w$y,xlab="x",ylab="y",xlim=c(-5,30),ylim=c(-10,50))

# Identify the points to single out:
Iout <- which(w$x>15 & w$y<10)  # get the outlier
Iinf <- which(w$x>22)           # get the potentially influential point
points(w$x[Iout],w$y[Iout],col="red",pch=19)
points(w$x[Iinf],w$y[Iinf],col="green",pch=19)

# Estimate models with and without the two points:
mod <- lm(y ~ x,data=w)
mod2 <- lm(y[-c(Iout,Iinf)] ~ x[-c(Iout,Iinf)],data=w)
abline(mod,col="red")
abline(mod2,col="black",lty=2)

sum <- summary(mod)
res <- sum$residuals
plot(w$x,res,xlim=c(-10,30),xlab="x_i",ylab="residuals e_i")
points(w$x[Iout],res[Iout],col="red",pch=19)
points(w$x[Iinf],res[Iinf],col="green",pch=19)
abline(h=0)

##  Plots for page 4:
v <- hatvalues(mod) # leverage, see below for a version computed "by hand"
plot(w$x,v,xlim=c(-10,30),ylim=c(0,0.3),xlab="x_i",ylab="v_ii",
     main="Leverage v_ii. Black=1/n; Red=2(p+1)/n")
points(w$x[Iout],v[Iout],col="red",pch=19)
points(w$x[Iinf],v[Iinf],col="green",pch=19)
abline(h=1/nrow(w))
abline(h=2*(1+1)/nrow(w),col="red")

# computation of leverage by hand
X <- matrix(c(rep(1,nrow(w)),w$x),nrow=nrow(w))
xtx <- t(X) %*% X
invxtx <- solve(xtx)
P <- X %*% invxtx %*% t(X)
v <- diag(P)  # same as v <- hatvalues(mod)


infl <- lm.influence(mod) # will calculate basic influence measures
# type ?lm.influence for more info
s_i <- infl$sigma #a vector whose i-th element contains the estimate of the residual standard deviation obtained when the i-th case is dropped from the regression        
# v <- infl$hat

#plot(s_i,ylim=c(0,6))  # <--this work just as well as the code below
plot(s_i,ylim=c(0,6),xlab="i",ylab="s_(i)",
     main="s_(i)")
points(Iout,s_i[Iout],col="red",pch=19)
points(Iinf,s_i[Iinf],col="green",pch=19)
abline(h=sum$sigma)

# studentised residuals
r_stud <- rstudent(mod)

plot(r_stud,ylim=c(-7,7),xlab="i",ylab="r*_i",
     main="stud. residuals, +/- 2")
points(Iout,r_stud[Iout],col="red",pch=19)
points(Iinf,r_stud[Iinf],col="green",pch=19)
abline(h=0)
abline(h=2,col="red")
abline(h=-2,col="red")


D<-cooks.distance(mod)
plot(D,ylim=c(0,1.2),xlab="i",ylab="D_i",
     main="Cook's distance with D=4/n and D=1")
points(Iout,D[Iout],col="red",pch=19)
points(Iinf,D[Iinf],col="green",pch=19)
abline(h=4/nrow(w))
abline(h=1)
## Standardized residuals:
# r_std <- rstandard(mod)
# r_std <- res/(sum$sigma*sqrt(1-v))
# D <- r_std^2/2*v/(1-v)

## Standardized change in beta-estimates (DFBETAS):
# you may use dfbetas(mod)
beta0_i <- infl$coefficients[,1] # gives the numerator for the DFBETA of beta0, type "?lm.influence" for details
beta1_i <- infl$coefficients[,2] # gives the numerator for the DFBETA of beta1, type "?lm.influence" for details
dfbeta_0=beta0_i/s_i/sqrt(invxtx[1,1])
dfbeta_1=beta1_i/s_i/sqrt(invxtx[2,2])

plot(dfbeta_0,ylim=c(-.5,.5),xlab="i",ylab="dfbeta_0(i)",
     main="dfbeta_0 +/- 2/sqrt(n)")
points(Iout,dfbeta_0[Iout],col="red",pch=19)
points(Iinf,dfbeta_0[Iinf],col="green",pch=19)
abline(h=0)
abline(h=2/sqrt(nrow(w)),col="red")
abline(h=-2/sqrt(nrow(w)),col="red")

plot(dfbeta_1,ylim=c(-1,1),xlab="i",ylab="dfbeta_1(i)",
     main="dfbeta_1 +/- 2/sqrt(n)")
points(Iout,dfbeta_1[Iout],col="red",pch=19)
points(Iinf,dfbeta_1[Iinf],col="green",pch=19)
abline(h=0)
abline(h=2/sqrt(nrow(w)),col="red")
abline(h=-2/sqrt(nrow(w)),col="red")

