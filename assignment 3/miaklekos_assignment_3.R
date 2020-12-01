#miaklekos_assignment_3
#### Assignment 3 ####
setwd("~/Documents/Bio Stats II Fall 2019")

dFull <- read.csv('https://raw.githubusercontent.com/hashtagcpt/biostats2/master/full_data.csv')

t1<-log10(dFull$thr1)           
t2<-log10(dFull$thr2)
t3<-log10(dFull$thr3)
t4<-log10(dFull$thr4)
t5<-log10(dFull$thr5)
t6<-log10(dFull$thr6)

dFull$subject <- factor(dFull$subject)
# assign RE variable
RE <- dFull$RE

subject <- dFull$subject

# make a data.frame
dTransform <- data.frame(subject,RE,t1,t2,t3,t4,t5,t6)
colnames(dTransform) <- c('subject','RE','A','L','M','Sneg','Spos','S')

library(QuantPsyc)


# 1. [3 points] You modeled the Color Vision and RE experiment as a multiple regression. You found the best regression model. You can use the boostrap to get confidence intervals on the parameters for the best model. What are the boostrap confidence intervals for the parameters on the full model? 

REModel <- lm(RE ~ A + L + M + Sneg + Spos + S, data =dTransform)
summary(REModel)

REModel <- lm(RE ~ Spos, data = dTransform)
summary(REModel)

REModel <- lm(RE ~ S, data =dTransform)
summary(REModel)


Best.3 <- lm(RE ~ S + Spos + Sneg, data = dTransform)
Best.2 <- lm(RE ~ S + Spos, data = dTransform)


library(boot)
bootReg <- function(formula, data, i) {
  d <- data[i,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

bootResults <- boot(statistic = bootReg, formula = RE ~ S + Spos, data = dTransform, R = 1000)

boot.ci(bootResults, type = "perc", index = 1) # intercept
boot.ci(bootResults, type = "perc", index = 2) # S
boot.ci(bootResults, type = "perc", index = 3) # Spos


# 2. [3 points] In question 4 of Assignment #2 you found the correlation of a variable S_summed  which was the square root of the squared and summed threshold values for S.positive and S.negative and found the correlation using the cor.test() function. Write a function to bootstrap the correlation of this value. What is the confidence interval on this correlation?   
# refers back to last assignent - S+ and S- thresholds--do some bootstrapping of the correlation of pearson's R, instead of fitting one function-use boootstrap to get confidence interval on corrleation statistic 
#instead of linear regression--you want to do cor.test which will spit out regressino coefficient--trick is just getting that user defined function to spit out the pearson's R and only pearson's R to the bootstrap --then have it spit out confidence interval 
library(ggplot2)
Exercise3.data <- read.csv('Exercise_3.csv')
S_summed <- sqrt(Exercise3.data$S_positive^2 + Exercise3.data$S_negative^2)
S_summed_plot <- data.frame(Exercise3.data$S, S_summed)
colnames(S_summed_plot) <- c('S', 'S_summed')

pA <- ggplot(data = S_summed_plot, aes(x= S, y=S_summed)) + geom_point(size= 1) + stat_smooth(method = 'lm')
pA

cor.test(x=Exercise3.data$S, y=S_summed)
Exercise3.data$S_summed <- S_summed

#bootstrap the correlation of this value:

library(boot)
bootCor <- function(data, i) {
  d <- data[i,]
  fit <- cor.test(d$S, d$S_positive)
  return(fit$estimate)
}

bootCorResults <- boot(statistic = bootCor, data= Exercise3.data, R = 1000)

plot(bootCorResults)


boot.ci(bootCorResults, type = "perc", index = 1) # intercept

# 3. [4 points] For this question you'll need the dataset:
#asks you to look at 3 groups of birds, control group-stare at blanck screen, and some stared at flickering, and noise screen--create bar graphs--3 different bar graphs, 3 ANOVAs

dat <- read.csv('https://raw.githubusercontent.com/hashtagcpt/biostats2/master/Monitor_Birds.csv')

# Within this data set, you'll find three categorical variables that define three groups of birds, Control, Flicker, and Noise. Create three bar graph with error bars that represent standard error of the mean for three dependent measures: postCh, postAx, and postRE. Run three 1-way anovas on each of these variables, first using Levene's test to check the homogeneity of variance. Finally, run Tukey's Honestly Significant Difference Post-hoc test. Which pairs of conditions are different from one another according to this test at an alpha of p < .1?     
# run Tukey HSD--homogeneity of variance will be tested--if violated calls into question the use of the ANOVA for those birds, if you set alpha to p <0.1 --> its asking you to report in comments which ones are less than P< 0.1 --Tukey HSD will give you an adjusted p value 


#### USER-DEFINED FUNCTIONS: RUN the code below to get the functions normDataWithin, summarySEwithin, summarySE ####

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
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
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# Confidence interval multiplier for standard error
# Calculate t-statistic for confidence interval: 
# e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1

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
#run Tukey's Honestly Significant Difference Post-hoc test. 

#postCh
leveneTest(dat$postCh, dat$condition, center = median)
# one-way ANOVA--to determine if there are any differences among our conditions 
d_aov <- aov(data = dat, formula = postCh ~ condition)
summary(d_aov)
TukeyHSD(d_aov)

#postAx
leveneTest(dat$postAx, dat$condition, center = median)

d_aov <- aov(data = dat, formula = postAx ~ condition)
summary(d_aov)
TukeyHSD(d_aov)

#postRE
leveneTest(dat$postRE, dat$condition, center = median)

d_aov <- aov(data = dat, formula = postRE ~ condition)
summary(d_aov)
TukeyHSD(d_aov)

#Which pairs of conditions are different from one another according to this test at an alpha of p < .1?    

# postCh: Flicker-Control
# postAx: Flicker-Control and Noise-Flicker
# postRE: Flicker-Control and Noise-Flicker