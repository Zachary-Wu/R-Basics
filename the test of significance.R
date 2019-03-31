getwd()
setwd("D:/R programming/R-Basics")
                      ###############
                          #T.test#
                  ######################

  #1.t.test n<30，总体标准差σ未知的正态分布 t.test {stats}
  #独立样本t检验
  #配对样本t检验:具有时间先后格局的数据，如：吃药前血压和吃药后血压
  #同样的数据，选择配对比较与不配对比较的结果是不同的，一定对数据有
  #较为清晰的认知，选择合适的方法
  #具体请参考：??t.test()
  #Notice:The formula interface is only applicable for the 2-sample tests
  #如果不知道该选两尾还是一尾，推荐更为保守的两尾，让数据自己说话。
sample <- c("sp1","sp2","sp3","sp4")
Detection1 <- c(1,3,5,7)
Detection2 <- c(20,40,60,80)
Detection3 <- c(1,4,6,9)

data1 <- data.frame(sample, Detection1, Detection2, Detection3)
head(data1)
t.test(data1$Detection1,data1$Detection2)
t.test(data1$Detection1,data1$Detection2, paired = TRUE)


  #如果有超过两组数据，且不满足方差检验前提
  #不满足正态性：
  #1.独立分组：Kruskal-Wallis H Test(kruskal.test {stats})
  #2.相关分组：Friedman test(friedman.test {stats}, friedman {agricolae})
  #3.任推荐使用单因素方差分析
  #4.数据转换：如log或者平方（Rummel, Applied factor analysis，1988),但结果基于变换后数据
  #1.不满足方差齐性：Welch‘s anova, Brown and Forsythe test, Kruskal-Wallis H Test
states <- data.frame(state.region, state.x77)
kruskal.test(Illiteracy ~ state.region, data = states)
  #通过上述代码，我们得出出Illiteracy 在不同state.region中有显著差异的结论
  #但state.region具有很多分组，差异显著的区域不明确
  #3.具体差异显著的区域：wmc()
source("http://www.statmethods.net/RiA/wmc.txt")
wmc(Illiteracy ~ state.region, data = states, method = "holm")



  #前提：1.正态性 2.方差齐性 3. 独立性
  #1.正态性
  #Shaprio-Wilks normality test – if your data is mainly unique values
  #D'Agostino-Pearson normality test – if you have lots of repeated values
  #Lilliefors normality test – mean and variance are unknown
  #Spiegelhalter's T' normality test – powerful non-normality is due to kurtosis, but bad if skewness is responsible
  # 推荐Q-Q 图：确定一组数据是否与另一组或者待测统计分布近似表示
  # 如果两组数据分布相似，QQ图为一条直线
ggplot(data = Moore, aes(sample = conformity)) + 
        stat_qq(distribution = qnorm) +
        stat_qq_line()
ggplot(data = Moore, aes(sample = conformity, color = fcategory)) + 
        stat_qq(distribution = qnorm) +
        stat_qq_line()

  #2.方差齐性
  #Bartlett检验 - 对于服从正态分布的数据最为适用 bartlett.test {stats}
  #Levene检验 - 相较于Bartlett检验更为稳健 leveneTest {car}
  #Fligner-Killeen检验 - 不依赖于对分布的假设 fligner.test {stats}
  # ~ 后面是分组变量，细节可看： ??bartlett.test
bartlett.test(conformity ~ fcategory, data = Moore)
  #当我们要考察的分组变量多于一组，需要interaction函数
  #interaction(fcategory, partner.status) =
  #low+ low, low + medium, low + high,
  #high + low, high + medium, high + high
bartlett.test(conformity ~ interaction(fcategory, partner.status),
              data = Moore)
  #协方差齐性
    #Box??s M test: boxM{biotools}


  #独立性
  #可能不独立数据：
  #1.重复测量
  #2.观测值间有时间相关关系
  #3.观测值间有空间相关关系



#单因素方差分析:单一因素影响的多组样本的因变量均值差异是否显著
#如：不同喂食剂量下的体重#
library(car)
library(tidyverse)
library(agricolae)
help("agricolae")

set.seed(13)
low <- rnorm(40, mean = 10, sd = 1)
middle <- rnorm(40, mean = 10.3, sd = 1)
high <- rnorm(40, mean = 10.5, sd = 1)

data2 <- data.frame(low , middle , high )
data3 <- data2%>%
     gather(key = "does", value = "weight",
            low , middle, high)


data3.aov <- aov(weight ~ does, data = data3)
summary(data3.aov)
#线性模型
data3.lm <- lm(weight ~ does, data = data3)
summary(data3.lm)
#具体分组中差异显著的区域:多重比较
  #1.TukeyHSD {stats}    
  TukeyHSD(data3.aov)
  #2.(agricolae)
  result <- LSD.test(data3.aov , "does", p.adj="bonferroni")
  result
  #除LSD外的其他方法：duncan.test, SNK.test, SNK.test, etc

 

#??????Э????????:????Э??��???޷????ݣ???Ӱ??ʵ????????
  #??litter{multcomp}????��Ϊ?Ա?��??С??????ʱ??ΪЭ??��??
  #??ͬιʳ??��?µ?????????Ϊ????��??

    #????1??????��??��????��??
    #????2???Ա?��????2???????????顣
    #????3??Э??��??��????��??
    #????4?????о?????֮???????໥??��?Ĺ۲?ֵ??
    #????5????????Э??��??????��֮?????????Թ?ϵ??
    #????6????????Э??��??????��?Ļع?ֱ??ƽ?С?
    #????7????????????��?Ĳв????Ʒ?????̬?ֲ???
    #????8????????????��?Ĳв????еȷ????ԡ?
    #????9????????????��?Ĳв???롣
    #????10??????��û???????쳣ֵ??
library(car)
library(tidyverse)
library(agricolae)
library(multcomp)

litter.aov1 <- aov(weight ~ gesttime + dose, data = litter)
summary(litter.aov1)
#????ģ??
litter.lm <- lm(weight ~ gesttime + dose, data = litter)
summary(litter.lm)
#???رȽ?
result.aov1 <- LSD.test(litter.aov1 , "dose", p.adj="bonferroni")
TukeyHSD(litter.aov1, "dose")







#˫???ط?????????????��????????��

name <- seq(1:40)
set.seed(13)
rep1 <- rnorm(40, mean = 0, sd = 1)
rep2 <- rnorm(40, mean = 0, sd = 1)
rep3 <- rnorm(40, mean = 11, sd = 1)
group <- rep(c(1:4), 10)
#ת??Ϊ???Ӹ?ʽ
group <- factor(group)
data6 <- data.frame(name, rep1, rep2, rep3, group)
data7 <- data6 %>%
  gather(key = "rep", value = "test",
         rep1, rep2, rep3)

str(data7)
fit3 <- aov(test ~ rep*group, data = data7)
summary(fit3)
result3 <- LSD.test(fit3 , "rep", "group", p.adj="bonferroni")
??LSD.test

