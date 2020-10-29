## Author - Susan Rico
## HW3 select data set and do initial exploration

## The dataset selected is an insurance based dataset.  It contains 7 varibles including
## age, gender, region, smoker (y/n), body mass index, childnre, and insurance charges


#set wd
setwd("C:/Users/Susan Rico/Documents")

# call in file name
fname <- "insurance.csv"
insurance<- read.csv(fname
                   , header = TRUE
                   , stringsAsFactors = TRUE)


## view struture of dataset
str(insurance)

##calculate formula to ensure dataset has enough observations
calculation <- (ncol(insurance)*4) * (nrow(insurance)/100) 
calculation

## view summary
summary(insurance)


library(wesanderson)


## create a histogram for body mass index 
hist(insurance$bmi
      , xlab = "Body Mass Index %"
      , main = "Histogram of Body Mass Index"
      , col = c("#D91828", "#A61C35", "#012840", "#6DA0A6", "#F2DFA7"))


## barplot to view frequencies of children per person
childcount <- table(insurance$children)
childcount

barplot(childcount
        , main = "Distribution of Children Per Person"
        , xlab = "Number of Children"
        , ylab = "Frequency"
        , ylim = c(0,600)
        , col = c("#D91828", "#A61C35", "#012840", "#6DA0A6", "#F2DFA7"))



## scatter plot 
#plot(insurance$charges, insurance$bmi
 #    , pch = 16
  #   , cex = 1
   #  , col = "blue")


## violin plot for charges
library(vioplot)
vioplot(insurance$)


## density plot for charges
dplot3 <- density (insurance$charges)
plot(dplot3
     , main = "Density Plot for Insurance Charges"
     , xlab = "Charges") 

polygon(dplot3, col = "#A61C35")


## use tapply to get mean of charges according to gender and smoker status
charges <-tapply(insurance$charges, list(insurance$smoker, insurance$sex), mean)
charges



## create a bar plot comparing smokers/non smokers in each gender and their mean ins charges
barplot(charges
        , beside = TRUE
        , col = c("#A61C35", "#012840")
        , names.arg = c("Female", "Male")
        , xlim = c(0,7)
        , ylim= c(0, 35000)
        , main = "Average Insurance Charges by Gender and Smoker Status"
        , legend = TRUE
        , ylab = "Charges in $"
        , args.legend=list(title="Smoker"))

## adds a density factor to plot
ggplot(insurance) + aes(x = insurance$children, y = insurance$charges) + stat_bin_2d()+
  stat_density_2d(col = "red")


## multiple boxplots per region
ggplot(insurance) + aes(x= region, y = charges) +
  geom_boxplot() #+ geom_jitter(width = 0.1, height = 0.1, size = 1, col = "grey")                


ggplot(data = insurance, aes (x = children, y = charges, fill = as.factor(children))) + geom_violin(position = "dodge")



## plot charges based on BMI and smoker status
ggplot(insurance) +
  aes(x=bmi, y = charges
      , color = smoker
      , alpha = age 
      , shape = smoker) +
  geom_point()

ggplot(insurance) +
  aes(x=bmi, y = charges) + geom_point(color = smoker, alpha = age , shape = insurance$smoker, size = 3)

## plot charges based on BMI and smoker status ########## MODIFIED ######
p1 <- ggplot(insurance) + aes(x=bmi, y = charges, alpha = age)
p1 + geom_point(aes(shape = smoker, color = smoker), size = 3) +
  scale_shape_manual(values = c(16, 17)) +
  scale_color_manual(values = c("cadetblue", "orangered"))+
  theme_minimal() +
  theme(legend.position = "right")



#boxplots with charges based on children
ggplot(insurance, aes(x = factor(children), y = charges))+
  geom_boxplot(aes(fill = smoker))+
  xlab('Children') + 
  scale_fill_brewer(palette = 'BrBG', name = 'Smoker')

#boxplots with charges based on children
ggplot(insurance, aes(x = factor(region), y = charges))+
  geom_boxplot(aes(fill = smoker))+
  xlab('Region') +  scale_fill_manual(values = c("cadetblue", "orangered"))   


ggplot(insurance) + aes(x= age, y = charges) + geom_point()

## begin our correlation analysis
library(corrplot)


## convert the smoker column into 0 and 1 (0= not smoker, 1 = smoker)
ins2 <- as.data.frame(insurance)

require(dplyr)
ins2 <- ins2 %>%
  mutate(smoker = ifelse(smoker == "no",0,1))

## changed sex to 0 and 1 (female = 0, male = 1)
ins2 <- ins2 %>%
  mutate(sex = ifelse(sex == "female",0,1))

## removed the region column
ins2[6] <- NULL


corrplot.mixed(m, lower.col = "black", number.cex = 1)


m <- cor(ins2)

corrplot(m, type="upper")


install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

chart.Correlation(ins2, histogram=TRUE, pch=19)

# Basic violin plot
p <- ggplot(insurance, aes(x=bmi, y=len)) + 
  geom_violin()
p
# Rotate the violin plot
p + coord_flip()
# Set trim argument to FALSE
ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_violin(trim=FALSE)

library(corrr)
install.packages("corrr")

ins2 %>% correlate() %>% network_plot()

# It can also be called using the traditional method
# network_plot(correlate(mydata), min_cor=0.5)

## create a new column with bmi category
insurance$bmicat <- cut(insurance$bmi, breaks=c(0,18.5,24.9,30, Inf), labels=c("Underweight","Normal","Overweight", "Obese"))
ggplot(data = insurance, aes (x = bmicat, y = charges, fill = as.factor(bmicat))) + geom_violin(position = "dodge")

# Horizontal version
ggplot(insurance, aes(x=children, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )


pie(children, main = "Distribution of Children per Person", col = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#666666"))
legend("right", c("0","1","2","3","4", "5"), cex = 0.8, fill = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#666666"))



p <- ggplot(insurance, aes(x=region, y=charges)) + 
  geom_boxplot()

p
ggplot(insurance, aes(x=charges)) + 
  geom_histogram(aes(y=..density..), colour="white", fill="orangered")+
  geom_density(alpha=.5, fill="gray") + ggtitle("Distribution of Charges")
options(scipen=100)


