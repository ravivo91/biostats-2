#### Assignment 3 ####

# 1. [3 points] You modeled the Color Vision and RE experiment as a multiple regression. You found the best regression model. You can use the bootstrap to get confidence intervals on the parameters for the best model. What are the boostrap confidence intervals for the parameters on the full model? 

#The following is copied from the exercise from Lecture 5

library(reshape2)
dFull <- read.csv('https://raw.githubusercontent.com/hashtagcpt/biostats2/master/full_data.csv')

# strip thresh values for data frame and log transform
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

# make a data.frame
dTransform <- data.frame(subject,RE,t1,t2,t3,t4,t5,t6)
colnames(dTransform) <- c('subject','RE','A','L','M','Sneg','Spos','S')

#I start answering the question from here: 
library(boot)

bootReg <- function(formula, data, i) {
  d <- data[i,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

bootResults <- boot(statistic = bootReg, formula = RE ~ Spos + S, data = dTransform, R = 1000)

boot.ci(bootResults, type = "bca", index = 1) # intercept
boot.ci(bootResults, type = "bca", index = 2) # Spos
boot.ci(bootResults, type = "bca", index = 3) # S

# 2. [3 points] In question 4 of Assignment #2 you found the correlation of a variable S_summed  which was the square root of the squared and summed threshold values for S.positive and S.negative and found the correlation using the cor.test() function. Write a function to bootstrap the correlation of this value. What is the confidence interval on this correlation?  Pearson's R?  

#From question 4 of assignment #2
Exercise3_data <- read.csv('exercise3.csv')
S_summed <- sqrt(Exercise3_data$S_positive^2 + Exercise3_data$S_negative^2)
S_summed_plot <- data.frame(Exercise3_data$S, S_summed)
colnames(S_summed_plot) <- c('S', 'S_summed')

pA <- ggplot(data = S_summed_plot, aes(x=S, y=S_summed)) + geom_point(size=1) + stat_smooth(method = 'lm')
pA
cor.test(x=Exercise3_data$S, y=S_summed)
Exercise3_data$S_summed <- S_summed

#Write a function to bootstrap the correlation of this value:
library(boot)
BootCorTest <- function(data, i){
  d <- data[i,]
  output <- cor.test(d$S, d$S_positive)
  return(output$estimate)
}

BootCorResults <- boot(statistic = BootCorTest, data = Exercise3_data, R = 1000)

plot(BootCorResults)

boot.ci(BootCorResults, type = "perc", index = 1) 

# 3. [4 points] For this question you'll need the dataset:

dat <- read.csv('https://raw.githubusercontent.com/hashtagcpt/biostats2/master/Monitor_Birds.csv')

# Within this data set, you'll find three categorical variables that define three groups of birds, Control, Flicker, and Noise. 
#Create three bar graph with error bars that represent standard error of the mean for three dependent measures: postCh, postAx, and postRE. 
#Run three 1-way anovas on each of these variables, first using Levene's test to check the homogeneity of variance. 
#Finally, run Tukey's Honestly Significant Difference Post-hoc test. Which pairs of conditions are different from one another according to this test at an alpha of p < .1?

#From User defined functions from lecture 6
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
  datac$se <- datac$sd / sqrt(datac$N)  
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

datac <- summarySE(data=re_datLonger, measurevar="RE", groupvars="cond")

devtools::install_github("thomasp85/patchwork")
library(patchwork)
pBox + pBar

# Confidence interval multiplier for standard error
# Calculate t-statistic for confidence interval: 
# e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
ciMult <- qt(conf.interval/2 + .5, datac$N-1)
datac$ci <- datac$se * ciMult
  
  return(datac)

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
# To determine if there are any differences among our conditions we can use a one-way ANOVA on threshold condition.
d_aov <- aov(data = dat, formula = postCh ~ condition)
summary(d_aov)
TukeyHSD(d_aov)

#postAx
leveneTest(dat$postAx, dat$condition, center = median)
# To determine if there are any differences among our conditions we can use a one-way ANOVA on threshold condition.
d_aov <- aov(data = dat, formula = postAx ~ condition)
summary(d_aov)
TukeyHSD(d_aov)

#postRE
leveneTest(dat$postRE, dat$condition, center = median)
# To determine if there are any differences among our conditions we can use a one-way ANOVA on threshold condition.
d_aov <- aov(data = dat, formula = postRE ~ condition)
summary(d_aov)
TukeyHSD(d_aov)
