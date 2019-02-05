## Read data from file with headers in the first row:
w<-read.table('auction.dat',header=T)

w
## Type as a factor, not a number:
w$type<-factor(w$type,labels=c("bowl","cass","dish","tray","plate"))
plot(w$type,w$price)

#############################
# no intercept 
#############################

mod1a<-lm(price ~ type-1,data=w)
summary(mod1a)
# check how the design matrix X has been defined for this model
# we need an additional option, x=TRUE
mod1a<-lm(price ~ type-1,data=w, x=TRUE)
mod1a$x  # prints the design matrix X
X=mod1a$x
# verify that the typical formula used for multiple regression works also for categorical variables.
# The following prints beta = (X'X)^(-1) * X' * Y
summary(mod1a)$cov.unscaled %*% t(X) %*% w$price

#############################################
# intercept and reference category 
#############################################

mod1b<-lm(price ~ type,data=w)
summary(mod1b)
# let's look at how the design matrix has been coded
mod1b<-lm(price ~ type,data=w, x=TRUE)
mod1b$x
X=mod1b$x
# check that we get the same estimates as from the summary
summary(mod1b)$cov.unscaled %*% t(X) %*% w$price

##############################
# correlations among predictors
##############################
pairs(w$price~w$diam+w$time)
# intuitively, the larger the size the more it takes to polish.
# also, the larger the size, the higher the selling price.
# IMPORTANT point though: it's not that the more you polish the higher the price.
# It's just that larger times are associated with larger dimensions (and presumably higher 
# starting price for the bidding)

###########################
# Parallel lines 
###########################

mod2a<-lm(price ~ type + diam-1,data=w)
mod2b<-lm(price ~ type + diam,data=w)
# by introducing 'diam', we see that the real discriminator in selling price is the dimension of the item (diam).
# it also says that (regardless the dimension) plates are signif. less expensive than bowls and casseroles signif.
# more expensive than bowls
summary(mod2a)
summary(mod2b)

# as an example let's consider the model with intercept
coef_bowl<-mod2a$coefficients[c(1,6)] # select coeff. for bowl and diameter
coef_cass<-mod2a$coefficients[c(2,6)] # select coeff. for cass and diameter
coef_dish<-mod2a$coefficients[c(3,6)] # etc
coef_tray<-mod2a$coefficients[c(4,6)]
coef_plat<-mod2a$coefficients[c(5,6)]

plot(w$diam[w$type=="bowl"],w$price[w$type=="bowl"],
     xlim=c(0,30),ylim=c(0,300),pch=19,col="blue",
     xlab="diameter", ylab="price")
points(w$diam[w$type=="cass"],w$price[w$type=="cass"],col="green",pch=20)
points(w$diam[w$type=="dish"],w$price[w$type=="dish"],col="red",pch=21)
points(w$diam[w$type=="tray"],w$price[w$type=="tray"],col="yellow",pch=22)
points(w$diam[w$type=="plate"],w$price[w$type=="plate"],col="magenta",pch=23)
legend(1,250,lty=c(1,1),legend=c("bowl","cass.", "dish", "tray","plate"),col=c("blue", "green", "red","yellow","magenta"))
abline(coef_bowl,col="blue")  # abline basically only needs some intercept and some slope to plot a line
abline(coef_cass,col="green")
abline(coef_dish,col="red")
abline(coef_tray,col="yellow")
abline(coef_plat,col="magenta")


################################################
# Interaction between type and diameter
################################################

# the ":" operator allows to specifiy interactions between variables
mod3a<-lm(price ~ type + diam + type:diam -1,data=w)
# the '*' operator allows to specify interactions and simultaneously include all main effects (i.e. the non interactive terms)
mod3a<-lm(price ~ type*diam-1,data=w) # this is the same as writing lm(price ~ type + diam + type:diam -1,data=w)
mod3b<-lm(price ~ type*diam,data=w) # and similarly for a model with intercept and interactions
summary(mod3b)
# it seems like slopes do not vary that much between types of items, as diameter increases

# let's see it graphically
xx<-c(0,30)
y0_bowl<-predict(mod3a,data.frame(type=factor(c("bowl","bowl")),diam=xx))
y0_cass<-predict(mod3a,data.frame(type=factor(c("cass","cass")),diam=xx))
y0_dish<-predict(mod3a,data.frame(type=factor(c("dish","dish")),diam=xx))
y0_tray<-predict(mod3a,data.frame(type=factor(c("tray","tray")),diam=xx))
y0_plat<-predict(mod3a,data.frame(type=factor(c("plate","plate")),diam=xx))

plot(w$diam[w$type=="bowl"],w$price[w$type=="bowl"],
     xlim=c(0,30),ylim=c(0,300),pch=19,col="blue",
     xlab="diam", ylab="price")
points(w$diam[w$type=="cass"],w$price[w$type=="cass"],col="green",pch=20)
points(w$diam[w$type=="dish"],w$price[w$type=="dish"],col="red",pch=21)
points(w$diam[w$type=="tray"],w$price[w$type=="tray"],col="yellow",pch=22)
points(w$diam[w$type=="plate"],w$price[w$type=="plate"],col="magenta",pch=23)
legend(1,250,lty=c(1,1),legend=c("bowl","cass.", "dish", "tray","plate"),col=c("blue", "green", "red","yellow","magenta"))
lines(xx,y0_bowl,col="blue")
lines(xx,y0_cass,col="green")
lines(xx,y0_dish,col="red")
lines(xx,y0_tray,col="yellow")
lines(xx,y0_plat,col="magenta")
# interestingly, the plots seem to show that slopes are different but not ENOUGH to justify adding an interaction.
# Let see if, indeed, we can be content with mod2b (no interactions)

anova(mod2b,mod3b) # yes it seems mod2b is enough
