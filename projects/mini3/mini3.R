setwd("~/RStudioProjects/MVE190/projects/mini3/")

housedata<-data.frame(read.csv("kc_house_data.csv",header=T)) ## read in the data
print(dim(housedata)) ## dimensions
print(names(housedata)) ## variable names

