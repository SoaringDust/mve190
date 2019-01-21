setwd("~/RStudioProjects/MVE190/projects/mini2/")

require(ggplot2)
require(reshape2)
require(gridExtra)
require(purrr)
require(tidyr)

housedata <- read.csv("kc_house_data.csv", header = TRUE)

ii <- sample(seq(1,dim(housedata)[1]),500)  # obtain a random sample of 500 indices
mydata <- housedata[ii,]                    # select a random subset of your full dataset
mydata <- subset(mydata, select = -c(waterfront, view, condition, grade))
row.names(mydata) <- NULL            # assign new IDs to each row

plot(log10(price)~log10(sqft_living), data = mydata)

plot(mydata)

mydata %>%
  gather(-price, key = "var", value = "value") %>%
  ggplot(aes(x=value, y=price)) + geom_point() + facet_wrap(~ var, scales = "free") + theme_bw()
