#### Assignment 3 ####
rm(list = ls())
setwd("~/Biostats")
# 1. [3 points] You modeled the Color Vision and RE experiment as a multiple regression. You found the best regression model. You can use the boostrap to get confidence intervals on the parameters for the best model. What are the boostrap confidence intervals for the parameters on the full model? 
library(ggplot2)

#### read in data from the web ####
dFull <- read.csv('https://raw.githubusercontent.com/hashtagcpt/biostats2/master/full_data.csv')

dFull <- read.csv('full_data.csv')

dFull$subject <- factor(dFull$subject)


RE <- dFull$RE
subject <- dFull$subject

t1<-log10(dFull$thr1)           
t2<-log10(dFull$thr2)
t3<-log10(dFull$thr3)
t4<-log10(dFull$thr4)
t5<-log10(dFull$thr5)
t6<-log10(dFull$thr6)


dTransform <- data.frame(subject,RE,t1,t2,t3,t4,t5,t6) 
colnames(dTransform) <- c('subject','RE','A','L','M','Sneg','Spos','S')

library(boot)

bootReg <- function(formula, data, i) {
  d <- data[i,]
  fit <- lm(formula, data)
  return(coef(fit))
}

bootResults <- boot(statistic = bootReg, formula = RE ~ S + Spos, data = dTransform, R = 1000)

boot.ci(bootResults, type = "perc", index = 1) # intercept
boot.ci(bootResults, type = "perc", index = 2) # S
boot.ci(bootResults, type = "perc", index = 3) # Spos

# 2. [3 points] In question 4 of Assignment #2 you found the correlation of a variable S_summed  which was the square root of the squared and summed threshold values for S.positive and S.negative and found the correlation using the cor.test() function. Write a function to bootstrap the correlation of this value. What is the confidence interval on this correlation?


write.csv(dTransform, file = "data_ex3final.csv")

Ex3_data <- read.csv('data_ex3final.csv')
S_summed <- sqrt(Ex3_data$Spos^2 + Ex3_data$Sneg^2)
S_summed_plot <- data.frame(Ex3_data$S, S_summed)
colnames(S_summed_plot) <- c('S', 'S_summed')

pA <- ggplot(data = S_summed_plot, aes(x=S, y=S_summed)) + geom_point(size=1) + stat_smooth(method = 'lm')
pA
cor.test(x=Ex3_data$S, y=S_summed)

library(boot)

bootCor <- function(data, i) {
  d <- data[i,]
  fit <- cor.test(d$S, d$Spos)
  return(fit$estimate)
}

bootCorResults <- boot(statistic = bootCor, data = Ex3_data, R = 1000)

plot(bootCorResults)

boot.ci(bootCorResults, type = "perc", index = 1) # intercept


# 3. [4 points] For this question you'll need the dataset:

dat <- read.csv('https://raw.githubusercontent.com/hashtagcpt/biostats2/master/Monitor_Birds.csv')

# Within this data set, you'll find three categorical variables that define three groups of birds, Control, Flicker, and Noise. 
#Create three bar graph with error bars that represent standard error of the mean for three dependent measures: postCh, postAx, and postRE. 
#Run three 1-way anovas on each of these variables, first using Levene's test to check the homogeneity of variance. 
#Finally, run Tukey's Honestly Significant Difference Post-hoc test. Which pairs of conditions are different from one another according to this test at an alpha of p < .1?


datac1 <- summarySE(data=dat, measurevar="postCh", groupvars="condition")
datac2 <- summarySE(data=dat, measurevar="postAx", groupvars="condition")
datac3 <- summarySE(data=dat, measurevar="postRE", groupvars="condition")

# bar graphs with error bars
pBar_1 <- ggplot(datac1, aes(x=condition, y=postCh)) + geom_bar(stat = 'identity') + geom_errorbar(width=.1, aes(ymin=postCh-ci, ymax=postCh+ci)) + xlab('Condition') + ylab('Choroidal Thickness') +theme(text = element_text(size=12), axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))
pBar_1

pBar_2 <- ggplot(datac2, aes(x=condition, y=postAx)) + geom_bar(stat = 'identity') + geom_errorbar(width=.1, aes(ymin=postAx-ci, ymax=postAx+ci)) + xlab('Condition') + ylab('Axial Length') +theme(text = element_text(size=12), axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))
pBar_2

pBar_3 <- ggplot(datac3, aes(x=condition, y=postRE)) + geom_bar(stat = 'identity') + geom_errorbar(width=.1, aes(ymin=postRE-ci, ymax=postRE+ci)) + xlab('Condition') + ylab('Refractive Error') +theme(text = element_text(size=12), axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))
pBar_3

library(car)

#postCh
leveneTest(dat$postCh, dat$condition, center = median)
d_aov1 <- aov(data = dat, formula = postCh ~ condition)
summary(d_aov1)
TukeyHSD(d_aov1)

#postAx
leveneTest(dat$postAx, dat$condition, center = median)
d_aov2 <- aov(data = dat, formula = postAx ~ condition)
summary(d_aov2)
TukeyHSD(d_aov2)

#postRE
leveneTest(dat$postRE, dat$condition, center = median)
d_aov3 <- aov(data = dat, formula = postRE ~ condition)
summary(d_aov3)
TukeyHSD(d_aov3)

# p<0.1:
# postCh Flicker-Control
# postAx Flicker-Control and Noise-Flicker
# postRE Flicker-Control and Noise-Flicker
