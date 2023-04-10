#### BIOSTATS 2 - WEEK 3 SCRIPT ####
#rm(list = ls())
library(ggplot2)
library(utils)

#### read in data from the web ####
dFull <- read.csv('https://raw.githubusercontent.com/hashtagcpt/biostats2/master/full_data.csv')

#### Exercise 1 - download data, setwd in code and read in locally
#
# setwd("C:/Users/chris/Dropbox/Teaching/Teaching_2023/biostats-2-master/biostats-2")
#dFull <- read.csv('full_data.csv')

# The function factor will change a numeric variable to a factor this creates categorical variables.
dFull$subject <- factor(dFull$subject)

# What do the two lines of code below do?
RE <- dFull$RE
subject <- dFull$subject

# When working with contrast threshold data, we often take the log transform. This goes back to Fechner who noted that sensory systems often work on a log scale.
t1<-log10(dFull$thr1)           
t2<-log10(dFull$thr2)
t3<-log10(dFull$thr3)
t4<-log10(dFull$thr4)
t5<-log10(dFull$thr5)
t6<-log10(dFull$thr6)

# Rather than working with the entire data set we can make a more manageably sized data set with data.frame()
dTransform <- data.frame(subject,RE,t1,t2,t3,t4,t5,t6)

# It is useful to rename columns such that they have meaningful names. That is, meaningful to future you... here L, M, and S are meaningful because they refer to cone types. 
colnames(dTransform) <- c('subject','RE','A','L','M','Sneg','Spos','S')

utils::browseURL('https://iovs.arvojournals.org/article.aspx?articleid=2702942')

#### SCATTER PLOT - ggplot2 ####

# We can create a simple scatter plot using ggplot2 for RE versus A

pA <- ggplot(data = dTransform, aes(x=RE,y=A)) + geom_point() 
pA

# What is going on here? All ggplots take an initialization command that uses an aesthetic or aes() to initialize. Then you need to add least one geom layer to the plot. Here we are using geom_point... but we can use geom_bar, geom_boxplot, geom_errorbar and more. Go to https://ggplot2.tidyverse.org/reference/. Wait we can do that in RStudio...
browseURL('https://ggplot2.tidyverse.org/reference/')

utils::browseURL('https://ggplot2.tidyverse.org/reference/')


# ... that's a long, but ultimately useful, list. 

# Axes always need to be labeled. 
pA <- pA + xlab('Refractive Error') + ylab(expression(log[10](threshold)))
pA # show the graph with the new axes

# OK. We have a scatter plot of the data from one condition. At this point you realize that you want another type of graph. We just want to know how thresholds vary across the different conditions. Let's use a bar chart.

dFull_wide <- dTransform 
colnames(dFull_wide) <- c('subject','RE','A','L','M','S-','S+','S')

library(reshape2) # load in the reshape2 library to get the function melt
dFull_long <- melt(dFull_wide, id.vars = c('subject','RE'))

# Hold up. What is this wide business? 
View(dFull_long)
# Let's compare it to dFull_wide
View(dFull_wide)

#### RUN THE FUNCTIONS AT THE BOTTOM OF THE SCRIPT ####

# This is a within-subjects design -- each of A, L, M, S-, S+, and S were measured within subjects. 


#### bar-graph with within subject error-bars ####
pBar <- ggplot(datac, aes(x=variable, y=value, group=1)) + geom_bar(aes(fill = variable), stat = 'identity') + geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci))
pBar

#### box-plot ####
pBox <- ggplot(data = dFull_long, aes(x = variable, y=value)) + geom_boxplot() + xlab('stimulus') + ylab(expression(log[10](threshold)))
print(pBox)

# What about writing a plot instead of viewing it?
ggsave('boxplot.tif', device = 'tiff', plot = pBox)

# With that, we've covered a large portion of the types of plots that you'd want to do. What would you do to these graph to make it communicate the data better? We'd definitely want to make the text bigger. 

# Exercise 2A - Label the axes appropriately then make the axes labels and axes text bigger for each of the three graphs, by adding a theme layer to the plot...
#
# theme(text = element_text(size=20), axis.text.x = element_text(size = 16), axis.text.y = element_text(size=16))
#
# Try adding theme_classic() to the plot, what does that do? If anything... 

# Exercise 2B - Our error bars look a little thin on our bar chart, how do we fix this?

# Exercise 2C - Our scatter plot points look a little small, how do we fix this?

# Exercise 2D - Our scatter plot could use a regression line. Try stat_smooth -- for help ?stat_smooth -- this is another type of layer that you can add to a ggplot. The default inputs for stat smooth might not be the correct one. Try the defaults and then try 'lm'.

# Exercise 3 -- Create a minimal data.frame with S+, S-, and S from the data BEFORE we took the log10 of threshold (i.e., dFull). Create two scatter plots. One of S+ versus S thresholds, the second S- versus S thresholds. Save this data.frame with write.csv('my_filename_with_data.csv') -- you'll use this for a task in assignment #2

write.csv()

#### RUN the code below to get the functions normDataWithin, summarySEwithin, summarySE ####

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
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL, idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  

  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE], FUN=is.factor, FUN.VALUE=logical(1))
  
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

# The functions above are derived from:
# utils::browseURL('http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#Helper%20functions')
