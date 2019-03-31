is.na(data)
#figure out whether there is a missing value.
na.fail(data)
na.omit(data)

#The manyNAs() function gives you the row numbers that
  #have more than 20% of the columns with an NA.
manyNAs(data , 0.2)

#Filling in the Unknown Values by Exploring Correlations
cor(data , use = "complete.obs")

# the k-nearest neighbours to fill in 
  #the unknown (NA) values by some means(median,weighAvg).
library(DMwR)
cleanData <- knnImputation(data, k = 10, scale = T, 
              meth = "weighAvg", distData = NULL)

#The missing data is interpolated with 
  #the median of the non-missing samples.
cleanData <- centralImputation(data)

## impute any missing values in the training set using 
   # proximities
data.imputed <- rfImpute(variable1 ~ ., data = data, iter=6)


