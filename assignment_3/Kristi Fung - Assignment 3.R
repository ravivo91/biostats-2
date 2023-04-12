#### Assignment 3 ####

# 1. [3 points] You modeled the Color Vision and RE experiment as a multiple regression. You found the best regression model. You can use the boostrap to get confidence intervals on the parameters for the best model. What are the boostrap confidence intervals for the parameters on the full model? 

library(boot)
bootReg <- function(formula, data, i) {
  d <- data[i,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

bootResults <- boot(statistic = bootReg, formula = RE.2Simple3 <- lm(RE ~ Sneg + S, data = dTransform), R = 1000)

boot.ci(bootResults, type = "bca", index = 1) # intercept
boot.ci(bootResults, type = "bca", index = 2) # Sneg
boot.ci(bootResults, type = "bca", index = 3) # S

# 2. [3 points] In question 4 of Assignment #2 you found the correlation of a variable S_summed  which was the square root of the squared and summed threshold values for S.positive and S.negative and found the correlation using the cor.test() function. Write a function to bootstrap the correlation of this value. What is the confidence interval on this correlation?    

E3_data <- read.csv('exercise3.csv')
S_summed <- sqrt(E3_data$S_positive^2 + E3_data$S_negative^2)
S_summed_plot <- data.frame(E3_data$S, S_summed)
colnames(S_summed_plot) <- c('S', 'S_summed')

pA <- ggplot(data = S_summed_plot, aes(x=S, y=S_summed)) + geom_point(size=1) + stat_smooth(method = 'lm')
pA
cor.test(x=E3_data$S, y=S_summed)

bootCorTest <- function(data, i) {
  d <- data[i,]
  output <- cor.test(d$S, d$S_positive)
  return(output$estimate)
}

bootCorResults <- boot(statistic = bootCorTest, data = E3_data, R = 1000)

plot(bootCorResults)

boot.ci(bootCorResults, type = "perc", index = 1) 

# 3. [4 points] For this question you'll need the dataset:

dat <- read.csv('https://raw.githubusercontent.com/hashtagcpt/biostats2/master/Monitor_Birds.csv')

# Within this data set, you'll find three categorical variables that define three groups of birds, Control, Flicker, and Noise. Create three bar graph with error bars that represent standard error of the mean for three dependent measures: postCh, postAx, and postRE. Run three 1-way anovas on each of these variables, first using Levene's test to check the homogeneity of variance. Finally, run Tukey's Honestly Significant Difference Post-hoc test. Which pairs of conditions are different from one another according to this test at an alpha of p < .1?       

d1 <- subset(dat, condition == "Control", select = c("postCh", "postAx", "postRE"))
d2 <- subset(dat, condition == "Flicker", select = c("postCh", "postAx", "postRE"))
d3 <- subset(dat, condition == "Noise", select = c("postCh", "postAx", "postRE"))

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

datac1 <- summarySE(data=dat, measurevar="postCh", groupvars="condition")
datac2 <- summarySE(data=dat, measurevar="postAx", groupvars="condition")
datac3 <- summarySE(data=dat, measurevar="postRE", groupvars="condition")

# bar graphs with error bars
library(ggplot2)
pBar1 <- ggplot(datac1, aes(x=condition, y=postCh)) + geom_bar(stat = 'identity') + geom_errorbar(width=.1, aes(ymin=postCh-ci, ymax=postCh+ci)) + xlab('Condition') + ylab('Choroidal Thickness') +theme(text = element_text(size=12), axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))
pBar1

pBar2 <- ggplot(datac2, aes(x=condition, y=postAx)) + geom_bar(stat = 'identity') + geom_errorbar(width=.1, aes(ymin=postAx-ci, ymax=postAx+ci)) + xlab('Condition') + ylab('Axial Length') +theme(text = element_text(size=12), axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))
pBar2

pBar3 <- ggplot(datac3, aes(x=condition, y=postRE)) + geom_bar(stat = 'identity') + geom_errorbar(width=.1, aes(ymin=postRE-ci, ymax=postRE+ci)) + xlab('Condition') + ylab('Refractive Error') +theme(text = element_text(size=12), axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))
pBar3

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
#postCh Flicker-Control
#postAx Flicker-Control and Noise-Flicker
#postRE Flicker-Control and Noise-Flicker
