setwd("~/RStudioProjects/MVE190/projects/mini3/")

housedata<-data.frame(read.csv("kc_house_data.csv",header=T)) ## read in the data
print(dim(housedata)) ## dimensions
print(names(housedata)) ## variable names

housedata$view <- factor(housedata$view, labels = c("no", "poor", "fair", "good", "excellent"))
housedata$condition <- factor(housedata$condition, labels = c("poor", "acceptable", "good", "great", "excellent"))



housedata$zipcode <- factor(housedata$zipcode)
housedata$lat <- factor(housedata$lat)
housedata$long <- factor(housedata$long)
