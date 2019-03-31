setwd("D:\\R programming\\Daiyi")

library(readxl)
library(cluster)
library(tidyverse)

data <- read_xlsx("ellips.xlsx")
data[data==0] <- NA
data <- na.omit(data)
data <- as.matrix(data)

xy1 <- unname(data[c(1:33),c(1,2)])
xy4 <- unname(data[c(34:109),c(1,2)])
exy1 <- ellipsoidhull(xy1)
exy4 <- ellipsoidhull(xy4)

var1 <- c(1:100, rep=1)
# >> calling print.ellipsoid()
windowsFonts(myFont = windowsFont("Times New Roman"))
#xaxs = "i", yaxs = "i"

title(main= "ABC", sub = "BCD")

plot(xy1, pch = 20, bty = 'l', xlab = "CP/(CP+EE+Tannin)", ylab = "EE/(CP+EE+Tannin)", cex = 0.2,
     xlim=c(70,100), ylim=c(0,40),
     xaxs = "i", yaxs = "i",
     col= "white",family = "myFont")

lines(predict(exy1), lty = 1)
points(rbind(exy1$loc), cex = 2, pch = 1 )


# points(xy4, cex = 0.5, pch = 21)
lines(predict(exy4), lty = 2)
points(rbind(exy4$loc), cex = 2, pch = 19)
legend('topleft',c('Spring','Winter'), 
       pch = c(1,19), ncol=2, lty = c(1,2),
       bty = "n")




exy <- ellipsoidhull(xy, tol = 1e-7, ret.wt = TRUE, ret.sq = TRUE)
str(exy)              # had small `tol(Time On Line)', hence many iterations
(ii <- which(zapsmall(exy $ wt) > 1e-6))
## --> only about 4 to 6  "spanning ellipsoid" points
round(exy$wt[ii],3); sum(exy$wt[ii]) # weights summing to 1
points(xy[ii,], pch = 21, cex = 2)





eg <- eigen(exy$cov)
axes <- sqrt(eg$values)
# angle of major axis with x axis
angle <- atan(eg$vectors[1,1]/eg$vectors[2,1])
x0 <- exy$loc[1] # centroid locations
y0 <- exy$loc[2]  
a <- sqrt(exy$d2) * axes[1]  # major axis length
b <- sqrt(exy$d2) * axes[2]  # minor axis length
print(xy[ii,])




pexy1 <- predict(ellipsoidhull(xy1))
center1 <- colMeans((pexy1))
dist2center1 <- sqrt(rowSums((t(t(pexy1)-center1))^2))
# lines(rbind(center1,pexy1[dist2center1 == min(dist2center1),]))
lines(rbind(center1, pexy1[dist2center1 == max(dist2center1),]))
lines(var1, sum(center1)- var1)


pexy4 <- predict(ellipsoidhull(xy4))
center4 <- colMeans((pexy4))
dist2center4 <- sqrt(rowSums((t(t(pexy4)-center4))^2))
#lines(rbind(center4,pexy4[dist2center4 == min(dist2center4),]))
lines(rbind(center4, pexy4[dist2center4 == max(dist2center4),]), lty = 2)
lines(var1, sum(center4)-var1, lty = 2)


###++++++++++++++++++++++++++++++++++++++#
##
##=======================================#
Spr_CF_TA <- unname(data[c(1:33),c(4,5)])
win_CF_TA <- unname(data[c(34:109),c(4,5)])

eSpr_CF_TA <- ellipsoidhull(Spr_CF_TA)
ewin_CF_TA <- ellipsoidhull(win_CF_TA)

plot(Spr_CF_TA, pch = 20, bty = 'l', xlab = "CF/(CF+TA+TF)", ylab = "EE/(CF+TA+TF)", cex = 0.2,
     xlim=c(70,100), ylim=c(0,40),
     xaxs = "i", yaxs = "i",
     col= "white",family = "myFont")

lines(predict(eSpr_CF_TA), lty = 1)
points(rbind(eSpr_CF_TA$loc), cex = 2, pch = 1 )
lines(predict(ewin_CF_TA), lty = 2)
points(rbind(ewin_CF_TA$loc), cex = 2, pch = 19)
legend('topleft',c('Spring','Winter'), 
       pch = c(1,19), ncol=2, lty = c(1,2),
       bty = "n")


pSpr_CF_TA <- predict(ellipsoidhull(Spr_CF_TA))
Spr_CF_TA_center <- colMeans((pSpr_CF_TA))
dist2Spr_CF_TA <- sqrt(rowSums((t(t(pSpr_CF_TA)- Spr_CF_TA_center))^2))
# lines(rbind(center1,pexy1[dist2center1 == min(dist2center1),]))
lines(rbind(Spr_CF_TA_center, pSpr_CF_TA[dist2Spr_CF_TA == max(dist2Spr_CF_TA),]))
lines(var1, sum(Spr_CF_TA_center)- var1)


pwin_CF_TA <- predict(ellipsoidhull(win_CF_TA))
win_CF_TA_center <- colMeans((pwin_CF_TA))
dist2win_CF_TA <- sqrt(rowSums((t(t(pwin_CF_TA)- win_CF_TA_center))^2))
# lines(rbind(center4,pexy4[dist2center4 == min(dist2center4),]))
lines(rbind(win_CF_TA_center, pwin_CF_TA[dist2pwin_CF_TA == max(dist2pwin_CF_TA),]), lty = 2)
lines(var1, sum(win_CF_TA_center)-var1, lty = 2)
