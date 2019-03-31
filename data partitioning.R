setwd("D:/R/R Basics")
getwd()
rm(list=ls())

#data partitioning
library(tidyverse)
library(caret)
mtcars_root     <- mtcars %>%
  as.tibble()
inTrain_mtcars  <- createDataPartition(
  y = mtcars_root$mpg,    #data$Vector
  p = 0.75,              
  list = FALSE
)


train_mtcars <- mtcars_root[inTrain_mtcars,]
test_mtcars  <- mtcars_root[-inTrain_mtcars,]