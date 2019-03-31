#R squared is a way to assess how two quantitative variables might be related
setwd("D:/R programming/statistics")
getwd()
library(ggplot2)
data <- read.csv("R-squared.csv")
pre <- lm(var2~var1, data = data)


ggplot(data = data) + theme_classic() +
  geom_point(aes(x = var1, y = var2))+ 
  geom_hline(yintercept = mean(data$var2), color = "Red") +
  geom_segment(aes(x = var1, 
                   xend = var1,
                   y = mean(var2),
                   yend =var2),color = "red")
#How do we calculate the variation around the var2/var1 relationship? 
#We just calculate the distence around the red line that we drew before.
#We usually calculate it by the variance.
var(data$var2)
#It represents the total variation of var2.



ggplot(data = data) + theme_classic() + 
  geom_point(aes(x = var1, y = var2)) + 
  labs(title = "
       Residual schematic diagram",
       subtitle = "NOTICE:It is not the residual Illustration ", hjust = 0.5) +
  theme(plot.title=element_text(size=12,
                                color="red",face="italic",
                                hjust=0.5,lineheight=0.5)) +
  geom_smooth(aes(x = var1, y = var2), method = 'lm', se = F, color = "Blue") + 
  geom_segment(aes(x = var1, 
                   xend = var1,
                   y = predict(pre),
                   yend =var2))
#How do we calculate the variation around the var2/var1 relationship? 
#We just do the same thing.
residuals(pre)
deviance(pre)
#residual sum of squares/sum squared residual
#It is means that how much of total variation is not described by the regression.
#It tells us whether the fit is good or not.



#what about R squared?
#It is means that how much of total variation is described by the regression.
#R squared = 1 - [deviance(pre) / var(data$var2)]
#R squared = [var(data$var2) - deviance(pre)] / var(data$var2)




#R-squared is the percentage of variation in the data that the relationship accounts for.
#For example,
#let's say the R squared equal to 90 percent.
#it means that, the relationship between the two variables explians  90% 
#of the variation of the data.


#R squared is just the square of the statistically significant R.
#Namely, when someone says:"the statistically significant R was 0.8."
#you should know by now that the relationship between the two variables explians  
#0.8*0.8 * 100% = 64% of the variation of the data.
#and,there are other things accounting for the remaining 36%.
