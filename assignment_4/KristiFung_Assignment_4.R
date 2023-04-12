#### Lecture 8 - Bayes' Factors ####

# be careful -- all the variables are wiped here
rm(list = ls())

## define a function to make dTransform
dFull <- read.csv('https://raw.githubusercontent.com/hashtagcpt/biostats2/master/full_data.csv')

make_dTransform <- function(dFull) {
  # strip thresh values for data frame and log transform
  t1<-log10(dFull$thr1)           
  t2<-log10(dFull$thr2)
  t3<-log10(dFull$thr3)
  t4<-log10(dFull$thr4)
  t5<-log10(dFull$thr5)
  t6<-log10(dFull$thr6)
  
  # factor will change a numeric variable to a factor which is   useful for creating categorical variables
  dFull$subject <- factor(dFull$subject)
  # assign RE variable
  RE <- dFull$RE
  
  subject <- dFull$subject
  
  # make a data.frame
  dTransform <- data.frame(subject,RE,t1,t2,t3,t4,t5,t6)
  colnames(dTransform) <- c('subject','RE','A','L','M','Sneg','Spos','S')
  
  return(dTransform)
}
dTransform <- make_dTransform(dFull)

#### Assignment 4 ####

# 1. [4 Points] The BayesFactor package contains a function regressionBF. Use regressionBF on the dTransform data -- use the threshold values (S, M, L, and so on) to predict RE (refractive error). Look at the Bayes Factor output, what does it seem to tell you? The help shows an example of how the BF for each model can be tested against the full model (all thresholds). Put this line in the script with a comment that notes the best model. How does this model compare to what we saw with the stepwise model selection we did in previous lectures? 

output= regressionBF(RE~A+L+M+Sneg+Spos+S, data = dTransform)
head(output/output[63])

#BayesFactor greater (Spos and S) than 1 demonstrates higher likelyhood that the null hypothesis is a playing factor for RE and this model is faster than the stepwise model seen in previous lecture

#2. [3 Points] A colleague is going to run an experiment where they are going to compute a correlation test. Beforehand, they ask you how many subjects they need to run. Based on previous work, they believe the r for their experiment could be between .2 and .6, their funders want a *Type II error* probability of no more than 0.4, and they will use a conventional alpha of .05. Calculate what the minimum and the maximum number of subjects they need to run for their experiment to be adequately powered, given their funder's constraints.   
library(pwr)

#Max number of subjects
pwr.r.test(n = NULL, r = 0.2, sig.level = 0.05, power = 1-0.4)

#Min number of subjects
pwr.r.test(n = NULL, r = 0.6, sig.level = 0.05, power = 1-0.4)

#Max number of subjects is 121, and the min is 12 subjects, there's a large range

# 3. [3 Points]
# This function will help you create some simulated psychometric data... 
datasim <- function(contrasts, pcorrect, ntrials) {
  for (tmp in 1:length(contrasts)) {
    contrast <- rep(contrasts[tmp], ntrials)
    correct <- rbinom(ntrials, 1, pcorrect[tmp])
    if (tmp == 1) {
      data <- data.frame(contrast, correct)
    } else {
      data <- rbind(data, data.frame(contrast, correct))
    }
  }
  return(data)  
}

# Create some data with contrast levels for the variable "contrasts" 0.01 to 0.1 in 10 steps. Create a vector that goes from 0 to 1 for "pcorrect". Set ntrials equal to 20. Fit and plot the resulting psychometric function. What is 50% threshold level for this resulting function?
library(tidyverse)
library(psyphy)

#create vectors
contrast <- seq(0.01, 0.1, length.out = 10)
pcorrect <- seq(0.0, 1.0,length.out = 10)

# Create data
dat.contast <- datasim(contrast, pcorrect, ntrials=20)

# Summarize data
k_summary <- dat.contast %>% group_by(contrast,correct) %>% tally() %>%  ungroup() %>% complete(contrast, correct, fill = list(n = 1))

k_summary <- subset(k_summary, correct == 1)
k_summary$incorrect <- 20 - k_summary$n
k_summary$correct <- k_summary$n

# Take the absolute value of negative contrast values
k_summary$contrast <- abs(k_summary$contrast)

# Fit the psychometric function
psymet_fit <- glm(formula = cbind(correct, incorrect) ~ log(contrast), family = binomial, data = k_summary)

xseq <- seq(0.01, 0.1, len = 100) 
psymet_fit.pred <- predict(psymet_fit, newdata = data.frame(contrast = xseq), type = "response", se.fit = TRUE)  # obtain predicted values 
psymet_pred_df <- data.frame(xseq, psymet_fit.pred$fit)
colnames(psymet_pred_df) <- c('contrast','fit')

psymet_plot <- ggplot(data = k_summary, aes(x = contrast, y = correct / 20)) + geom_point(size = 4) + geom_line(data = psymet_pred_df, aes(x = contrast, y = fit), size = 2) + ylab('proportion correct') + theme(text = element_text(size=16), axis.text.x = element_text(size = 16), axis.text.y = element_text(size=16))

psymet_plot
 

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