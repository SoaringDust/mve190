setwd("~/RStudioProjects/MVE190/projects/mini1/")

housing <- read.csv("kc_house_data.csv", header = TRUE)

plot(price~sqft_living, data = housing)
plot(log10(price)~log10(sqft_living), data = housing)

mm <- lm(log10(price)~log10(sqft_living), data = housing)
abline(mm, col = 2)

summary(mm)
lmi <- lm.influence(mm)
plot(log10(housing$sqft_living), lmi$hat)

induce <- seq(1,dim(housing)[1])[is.na(housing$price)==F]
plot(log10(housing$sqft_living),mm$residuals)
abline(h=0, col=2)
# id <- identify(log10(housing$sqft_living), mm$residuals, row.names(housing), pos = T)