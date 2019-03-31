library(tidyverse)
library(cowplot)
library(randomForest)


url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data <- read.csv(url, header=FALSE)

#data <- rename(data,age = "V1", sex = "V2")
colnames(data) <- c("age","sex","cp","trestbps","chol","fbs",
  "restecg","thalach","exang" , "oldpeak","slope","ca" ,"thal",
  "hd" 
)

data1 <- within(data, {
  sex[1] <- "M"
  sex[2] <- "F"
  sex <- as.factor(sex)
  cp <- as.factor(cp)
  fbs <- as.factor(fbs)
  restecg <- as.factor(restecg)
  exang <- as.factor(exang)
  slope <- as.factor(slope)
  ca <- as.integer(ca)
  ca <- as.factor(ca)
  thal <- as.integer(thal)
  thal <- as.factor(thal)
  hd <- ifelse(hd == 0, yes = "Healthy", no = "Unhealthy")
  hd <- as.factor(hd)
})

na.fail(data1)
str(data1)

## NOTE: For most machine learning methods, you need to divide the data
## manually into a "training" set and a "test" set. This allows you to train
## the method using the training data, and then test it on data it was not
## originally trained on.
##
## In contrast, Random Forests split the data into "training" and "test" sets
## for you. This is because Random Forests use bootstrapped
## data, and thus, not every sample is used to build every tree. The
## "training" dataset is the bootstrapped data and the "test" dataset is
## the remaining samples. The remaining samples are called
## the "Out-Of-Bag" (OOB) data


model <- randomForest(hd ~ ., data=data1, ntree=1000)

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"],
          model$err.rate[,"Healthy"],
          model$err.rate[,"Unhealthy"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))




