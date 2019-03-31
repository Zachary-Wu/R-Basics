setwd("D:\\R programming\\Default working directory")
getwd()
rm(list=ls())
##set.library
.libPaths("D:\\R programming\\ R software\\library")

#data partitioning
library(caret)
mtcars_root     <- mtcars%>%
  as.tibble()
inTrain_mtcars  <- createDataPartition(
  y = mtcars_root$mpg,    #data$Vector
  p = 0.75,              
  list = FALSE
  )

install.packages("caret")
train_mtcars <- mtcars_root[inTrain_mtcars,]
test_mtcars  <- mtcars_root[-inTrain_mtcars,]