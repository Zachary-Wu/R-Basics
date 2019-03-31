setwd("D:/R/R Basics")
###Graphics for Communication with ggplot2
library(ggplot2)
#themes
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  theme_classic()

#plot title
windowsFonts(myFont = windowsFont("Times New Roman"))
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  theme_classic()+
  labs(
    title =
      "Fuel efficiency generally decreases with engine size",
    subtitle = 
      "Two seaters (sports cars) are an exception
    because of their light weight",
    caption = "Data from fueleconomy.gov")+
  theme(plot.title=element_text(family="myFont",size=12,
                                color="red",face="italic",
                                hjust=0.2,lineheight=0.5),
        plot.subtitle = element_text(hjust = 0.5)) 



#axis titles
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  theme_classic()+
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)"
  )
#legend title
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  theme_classic()+
  labs(
    colour = "Car type"
  )


#use mathematical equations instead of text strings
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  theme_classic()+
  labs(
    title = quote(sum(x[i] ^ 2, i == 1, n)),
    y     = quote(alpha + beta + frac(delta, theta)))

    
    


