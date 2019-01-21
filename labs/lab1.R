setwd("~/RStudioProjects/MVE190/labs")

repair1 <- data.frame(units=c(1,2,3,4,4,5,6,6,7,8,9,9,10,10,15,18),
                      minutes=c(23,29,49,64,74,87,96,97,109,119,149,145,154,166,180,190))
write.table(repair1,file="repair.txt",sep="\t")

# repair1 <- read.table("https://tinyurl.com/y8av4xfn",header=TRUE,sep="\t")

x <- repair1$units
y <- repair1$minutes
plot(x,y)

model <- lm(minutes~units, data = repair1)
lines(x,model$fit,lwd=2,col=4)

b1 <- c((x-mean(x))%*%(y-mean(y)))/c((x-mean(x))%*%(x-mean(x)))
b0 <- mean(y)-b1*mean(x)

# the slope is minutes/units; the time per number of units in the computer

service_12 <- b0+b1*12

new.df <- data.frame(units=c(12))
predict(model,new.df)

pre <- predict(model)
segments(x,y,x,pre,col = "red")
