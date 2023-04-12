#### Assignment 3 ####
rm(list = ls())
setwd("~/Downloads")

library(reshape2)
library(plyr)
library(ggplot2)
library(boot)
library(plyr)
library(car)

# 1. [3 points] You modeled the Color Vision and RE experiment as a multiple regression. You found the best regression model. You can use the boostrap to get confidence intervals on the parameters for the best model. What are the boostrap confidence intervals for the parameters on the full model? 

dFull <- read.csv('https://raw.githubusercontent.com/hashtagcpt/biostats2/master/full_data.csv')
# grab threshhold values and log
t1<-log10(dFull$thr1)           
t2<-log10(dFull$thr2)
t3<-log10(dFull$thr3)
t4<-log10(dFull$thr4)
t5<-log10(dFull$thr5)
t6<-log10(dFull$thr6)

# factor will change a numeric variable to a factor which is useful for creating categorical variables
dFull$subject <- factor(dFull$subject)
# assign RE variable
RE <- dFull$RE

subject <- dFull$subject

# make a data frame
dTransform <- data.frame(subject,RE,t1,t2,t3,t4,t5,t6)
colnames(dTransform) <- c('subject','RE','A','L','M','Sneg','Spos','S')


bootReg <- function(formula, data, i) {
  d <- data[i,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

bootResults <- boot(statistic = bootReg, formula = RE ~ S + Spos, data = dTransform, R = 1000)

boot.ci(bootResults, type = "perc", index = 1) 

bootResults <- boot(statistic = bootReg, formula = RE ~ A + L + M + Sneg + S + Spos, data = dTransform, R = 1000)

boot.ci(bootResults, type = "perc", index = 1)

# 2. [3 points] In question 4 of Assignment #2 you found the correlation of a variable S_summed  which was the square root of the squared and summed threshold values for S.positive and S.negative and found the correlation using the cor.test() function. Write a function to bootstrap the correlation of this value. What is the confidence interval on this correlation?
minimaldata <- read.csv('minimaldata.csv')
boot.cor <- function(data, i) {
  d <- data[i,]
  S_summed <- sqrt(d[,2]^2 + d[,3]^2)
  corS_summed <- cor.test(x=d[,4], y=S_summed)
  return(corS_summed$estimate)
}
bootS_summed <- boot(statistic = boot.cor, data = minimaldata, R = 1000)
boot.ci(bootS_summed, type = "perc", index =1) 

# 3. [4 points] For this question you'll need the dataset:

dat <- read.csv('https://raw.githubusercontent.com/hashtagcpt/biostats2/master/Monitor_Birds.csv')

# Within this data set, you'll find three categorical variables that define three groups of birds, Control, Flicker, and Noise. Create three bar graph with error bars that represent standard error of the mean for three dependent measures: postCh, postAx, and postRE. Run three 1-way anovas on each of these variables, first using Levene's test to check the homogeneity of variance. Finally, run Tukey's Honestly Significant Difference Post-hoc test. Which pairs of conditions are different from one another according to this test at an alpha of p < .1?       
d1 <- subset(dat, condition == "Control", select = c("postCh", "postAx", "postRE"))
d2 <- subset(dat, condition == "Flicker", select = c("postCh", "postAx", "postRE"))
d3 <- subset(dat, condition == "Noise", select = c("postCh", "postAx", "postRE"))

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar)) 
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

datac1 <- summarySE(data=dat, measurevar='postCh', groupvars='condition')
datac2 <- summarySE(data=dat, measurevar='postAx', groupvars='condition')
datac3 <- summarySE(data=dat, measurevar='postRE', groupvars='condition')

# bar graphs with error bars
pBar1 <- ggplot(datac1, aes(x=condition, y=postCh)) + geom_bar(stat = 'identity') + geom_errorbar(width=.1, aes(ymin=postCh-ci, ymax=postCh+ci)) + xlab('Condition') + ylab('Choroidal Thickness') +theme(text = element_text(size=12), axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))
pBar1

pBar2 <- ggplot(datac2, aes(x=condition, y=postAx)) + geom_bar(stat = 'identity') + geom_errorbar(width=.1, aes(ymin=postAx-ci, ymax=postAx+ci)) + xlab('Condition') + ylab('Axial Length') +theme(text = element_text(size=12), axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))
pBar2

pBar3 <- ggplot(datac3, aes(x=condition, y=postRE)) + geom_bar(stat = 'identity') + geom_errorbar(width=.1, aes(ymin=postRE-ci, ymax=postRE+ci)) + xlab('Condition') + ylab('Refractive Error') +theme(text = element_text(size=12), axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))
pBar3


#levene test choroid
leveneTest(dat$postCh, dat$condition, center = median)
d_aov1 <- aov(data = dat, formula = postCh ~ condition)
summary(d_aov1)

#levene test axial length
leveneTest(dat$postAx, dat$condition, center = median)
d_aov2 <- aov(data = dat, formula = postAx ~ condition)
summary(d_aov2)

#levene test refractive error
leveneTest(dat$postRE, dat$condition, center = median)
d_aov3 <- aov(data = dat, formula = postRE ~ condition)
summary(d_aov3)

TukeyHSD(d_aov1)
# pairs that are below p<0.1:
# Within Choroid: Flicker-Control

TukeyHSD(d_aov2)
# Within Axial Length: Flicker-Control and Noise-Flicker

TukeyHSD(d_aov3)
# Within RE: Flicker-Control and Noise-Flicker