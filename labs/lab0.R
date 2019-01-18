setwd("~/RStudioProjects/MVE190/labs")

# create a data frame
emission <- data.frame(vehicles = c(28, 36, 15, -19, -24, 8, 25, 40, 63, 12, -6, 21),
                       pollution = c(22, 26, 15, -18, -21, 7, 21, 31, 52, 8, -7, 20))

# check that it looks ok
emission
summary(emission)
