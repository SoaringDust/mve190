setwd("~/RStudioProjects/MVE190/projects/mini2/")

require(ggplot2)
require(reshape2)
require(gridExtra)
require(purrr)
require(tidyr)

housedata <- read.csv("kc_house_data.csv", header = TRUE)

ii <- sample(seq(1,dim(housedata)[1]),500)  # obtain a random sample of 500 indices
mydata <- housedata[ii,]                    # select a random subset of your full dataset
mydata <- subset(mydata, select = -c(id, date, waterfront, view, condition, grade))
row.names(mydata) <- NULL            # assign new IDs to each row
induce <- seq(1,dim(mydata)[1])[is.na(mydata$price)==F]


plot(log10(mydata$sqft_living), log10(mydata$price))
id1 <- identify(log10(mydata$sqft_living), log10(mydata$price), row.names(mydata), pos = T)

# mydata %>%
  # gather(-price, key = "var", value = "value") %>%
  # ggplot(aes(x=value, y=price)) + geom_point() + facet_wrap(~ var, scales = "free") + theme_bw()

mm <- lm(log10(price)~log10(sqft_living), data = mydata)
lines(sort(log10(mydata$sqft_living)[is.na(mydata$price)==F]), 
      mm$fit[sort.list(log10(mydata$sqft_living)[is.na(mydata$price)==F])])

plot(log10(mydata$sqft_living)[induce],mm$res)
id2 <- identify(log10(mydata$sqft_living)[induce], mm$res, row.names(mydata)[induce], pos = T)
abline(h=0)

lmi <- lm.influence(mm)
plot(log10(mydata$sqft_living)[induce], lmi$hat, ylab = "leverage")
id3 <- identify(log10(mydata$sqft_living)[induce], lmi$hat, row.names(mydata)[induce], pos = T)

plot(induce, lmi$coef[,2], ylab = "impact in slope")
abline(h=0)
id4 <- identify(induce, lmi$coef[,2], label = row.names(mydata)[induce], pos = T)

plot(induce, lmi$sig, ylab = "impact on sum of squares")
id5 <- identify(induce, lmi$sig, labels = row.names(mydata), pos = T)

summary(mm)
# R-squared for the fit
rsqa<-round(summary(mm)$r.sq,2)

plot(log10(mydata$sqft_lot), log10(mydata$price))
mm1 <- lm(log10(price)~log10(sqft_lot), data = mydata)
lines(sort(log10(mydata$sqft_lot)[is.na(mydata$price)==F]), 
      mm1$fit[sort.list(log10(mydata$sqft_lot)[is.na(mydata$price)==F])])

mm_tot <- lm(log10(price)~log10(sqft_living)+log10(sqft_lot), data = mydata)
plot(mm_tot)