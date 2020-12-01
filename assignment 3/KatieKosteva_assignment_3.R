#### Assignment 3 ####
rm(list = ls())
# 1. [3 points] You modeled the Color Vision and RE experiment as a multiple regression. You found the best regression model. You can use the boostrap to get confidence intervals on the parameters for the best model. What are the boostrap confidence intervals for the parameters on the full model? 
#Import and set up data
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

#Bootstrap CI
library(boot)
bootReg <- function(formula, data, i) {
  d <- data[i,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

#Best multiple regression
bootCVResults <- boot(statistic = bootReg, formula = RE ~ S + Spos, data = dTransform, R = 1000)

boot.ci(bootCVResults, type = "perc", index = 1) # intercept
boot.ci(bootCVResults, type = "perc", index = 2) # S
boot.ci(bootCVResults, type = "perc", index = 3) # Spos

#Full multiple regression
bootCV_Full_Results <- boot(statistic = bootReg, formula = RE ~ A + L + M + Sneg + Spos + S, data = dTransform, R = 1000)

boot.ci(bootCV_Full_Results, type = "perc", index = 1) # intercept
boot.ci(bootCV_Full_Results, type = "perc", index = 2) # A
boot.ci(bootCV_Full_Results, type = "perc", index = 3) # L
boot.ci(bootCV_Full_Results, type = "perc", index = 4) # M
boot.ci(bootCV_Full_Results, type = "perc", index = 5) # Sneg
boot.ci(bootCV_Full_Results, type = "perc", index = 6) # Spos
boot.ci(bootCV_Full_Results, type = "perc", index = 6) # S


# 2. [3 points] In question 4 of Assignment #2 you found the correlation of a variable S_summed  which was the square root of the squared and summed threshold values for S.positive and S.negative and found the correlation using the cor.test() function. Write a function to bootstrap the correlation of this value. What is the confidence interval on this correlation? 

minimal_data <- read.csv('minimal_data.csv')

library(boot)
bootCor <- function(data,i) {
  d <- data[i,]
  d_summed<- sqrt((d[,3]*d[,3])+(d[,2]*d[,2]))
  d_cor<-cor.test(x=d[,4],y=d_summed)
  return(d_cor$estimate)
}
colnames(minimal_data) <- c('subject','Sneg','Spos','S')
bootCVCor <- boot(statistic = bootCor, data = minimal_data, R = 1000)
boot.ci(bootCVCor, type = "perc", index = 1)


# 3. [4 points] For this question you'll need the dataset:

dat <- read.csv('https://raw.githubusercontent.com/hashtagcpt/biostats2/master/Monitor_Birds.csv')

# Within this data set, you'll find three categorical variables that define three groups of birds, Control, Flicker, and Noise. Create three bar graph with error bars that represent standard error of the mean for three dependent measures: postCh, postAx, and postRE. Run three 1-way anovas on each of these variables, first using Levene's test to check the homogeneity of variance. Finally, run Tukey's Honestly Significant Difference Post-hoc test. Which pairs of conditions are different from one another according to this test at an alpha of p < .1? '

library(data.table)
dat_postCh <- dat[,c(3,6)]
dat_postAx <- dat[,c(3,7)]
dat_postRE <- dat[,c(3,9)]

library(ggplot2)
#postCh Graph
bar_SE1 <- summarySE(data=dat_postCh, measurevar='postCh', groupvars ='condition')
postCh_Bar <- ggplot(bar_SE1, aes(x=condition, y=postCh)) + geom_bar(stat = 'identity') + geom_errorbar(data=bar_SE1, width=.1, aes(ymin=postCh-ci, ymax=postCh+ci)) + xlab('Condition') + ylab('Choroidal Thickness') + theme(text = element_text(size=15), axis.text.x = element_text(size = 9), axis.text.y = element_text(size=16))
postCh_Bar

#postAx Graph
bar_SE2 <- summarySE(data=dat_postAx, measurevar='postAx', groupvars = 'condition')
postAx_Bar <- ggplot(bar_SE2, aes(x=condition, y=postAx)) + geom_bar(stat = 'identity') + geom_errorbar(data=bar_SE2, width=.1, aes(ymin=postAx-ci, ymax=postAx+ci)) + xlab('Condition') + ylab('Axial Length')+ theme(text = element_text(size=15), axis.text.x = element_text(size = 9), axis.text.y = element_text(size=16))
postAx_Bar

#postRE
bar_SE2 <- summarySE(data=dat_postRE, measurevar='postRE', groupvars = 'condition')
postRE_Bar <- ggplot(bar_SE2, aes(x=condition, y=postRE)) + geom_bar(stat = 'identity') + geom_errorbar(data=bar_SE3, width=.1, aes(ymin=postRE-ci, ymax=postRE+ci)) + xlab('Condition') + ylab('Refractive Error') + theme(text = element_text(size=15), axis.text.x = element_text(size = 9), axis.text.y = element_text(size=16))
postRE_Bar

library(patchwork)
postCh_Bar + postAx_Bar + postRE_Bar

library(car)
#ReorganizeData
dat_long <- melt(dat, id.vars = c('Bird','condition')) 
dat_long <- dat_long[211:490,]
#Check homogenity of varaince
leveneTest(as.numeric(dat_long$value), dat_long$variable, center = median)
#ANOVA
d_aov3 <- aov(data = dat_long, formula = value ~ variable)
summary(d_aov3)
#Tukey Post Hoc 
TukeyHSD(d_aov3)


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
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  library(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}
## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}

# The functions above are from:
# utils::browseURL('http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#Helper%20functions')


