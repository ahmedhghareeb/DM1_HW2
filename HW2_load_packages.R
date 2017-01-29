# Data Mining I
# HW 2
# Linear Regression
# Boston Housing Data

##### install packages #####


packages <- c("MASS","ggplot2","GGally","rpart","caret","leaps","boot","reshape2","stringr")

install.packages(packages)

lapply(as.list(packages),library, character.only = TRUE)
