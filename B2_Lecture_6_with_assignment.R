#### BIOSTATS 2 - WEEK 3 SCRIPT ####
#rm(list = ls())
library(ggplot2)

#### read in data from the web ####
dFull <- read.csv('https://raw.githubusercontent.com/hashtagcpt/biostats2/master/full_data.csv')

#### Exercise 1 - download data, setwd in code and read in locally
#
#
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

#### SCATTER PLOT - ggplot2 ####

# We can create a simple scatter plot using ggplot2 for RE versus A. 
pA <- ggplot(data = dTransform, aes(x=RE,y=A)) + geom_point() 

# What is going on here? All ggplots take an initialization command that uses an aesthetic or aes() to initialize. Then you need to add least one geom layer to the plot. Here we are using geom_point... but we can use geom_bar, geom_boxplot, geom_errorbar and more. Go to https://ggplot2.tidyverse.org/reference/. Wait we can do that in RStudio...
#utils::browseURL('https://ggplot2.tidyverse.org/reference/')
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
datac <- summarySEwithin(dFull_long, measurevar='value', withinvars='variable', idvar='subject')
datac

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

#### Exercise 3 ####
# Exercise 3 -- Create a minimal data.frame with S+, S-, and S from the data BEFORE we took the log10 of threhsold (i.e., dFull). Create two scatter plots. One of S+ versys S thresholds, the second S- versus S thresholds. Save this data.frame with write.csv('my_filename_with_data.csv') -- you'll use this for a task in assignment #2. Note, you'll want to name S+ something like S_positive, as you want to avoid symbols in variable names.

#### TODAY ####

### Patchwork -- making graphs on one plot ####

# install.packages("devtools")
devtools::install_github("thomasp85/patchwork")
library(patchwork)
pA + pBox + pBar

#### Lecture 4 - t-tests ####
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

Snew <- dTransform$S / dTransform$A
ts<-t.test(Snew, mu = 1)

Mnew <- dTransform$M / dTransform$A
tm<-t.test(Mnew, mu = 1)

Lnew <- dTransform$L / dTransform$A
tl<-t.test(Lnew, mu = 1)

# put p-values for the three one-sample t-tests into a vector
vals <- c(ts$p.value, tm$p.value, tl$p.value)

# What problem might arise from doing t-tests this way? Each test has a Type I error rate. If we don't control for the Type I error rate, then we are likely to make a false positive. That is, we'll likely reject the null hypothesis of no difference even though we don't have evidence of a differnce.  To control for the false positive rate we can use the p.adjust function. 

# Let's look at the p.adjust() function
?p.adjust

# The p.adjust function takes your three p-values for S/A, M/A, and L/A and adjusts them for multiple comparisons. Put your three p-values in a vector using c() -- and then try the corrections for "bonferroni", "holm", and "fdr". Which one of these is the most conservative (least likely to make a Type I error)? Which is the most liberal (most likely to make a Type I error)?    

# Perform two sample t.tests on S versus Spos / S neg.

# In this experiment we were not chiefly concerned with the difference among the conditions. That there was different sensitivity between the conditions was well known and we did not expect a difference between the Spos and Sneg conditions. We were interested in whether there were correlations with refractive error. The function cor.test can provide us with Pearson correlation tests, among others. First perform a cor.test between RE and Spos.   

# The options on cor.test default to a two sided test (test whether the correlation is positive or negative). What is the p-value if we use a directional hypothesis as our alternative (see help for the input parameter)?

# Sometimes our data our not normally distrubuted. In that case we'll want a non-parametric test. The R function cor.test() provides an option for a non-parametric test, Spearman's Rho. Perform a Spearman's test on RE versus Spos -- what happens to the p-value? (Hint: R will complain about ties, basically checking to see if you know that this is what you want, set the parameter exact = FALSE to stop it from complaining)

##### Summary statistics ####

dUntrans <- dFull # keep untransformed data

# Often you'll want to be able to summarize your data using measures of central tendency and variability. Here we'll look at few different measures of central tendency.

# Means are the most common way of communicating central tendency. They're easy to obtain in R. You'll want to be careful about how you express them and express them in a way that will make sense to your readership. Communicating untransformed means (as opposed to the log transformed means) is clearer to a general audience. 

mA <- mean(dUntrans$thr1)
mL <- mean(dUntrans$thr2)
mM <- mean(dUntrans$thr3)
mSpos <- mean(dUntrans$thr4)
mSneg <- mean(dUntrans$thr5)
mS <- mean(dUntrans$thr6)

# With real data you often have to deal with outliers. What is an outlier can be tricky to determine. The mean function allows for a trimming option. The trim option in the mean function throws away a certain proportion of the data before computing the mean. 

# For assignment #1 you created a trimming function. However, trimming is not an elegant soltion. It merely tosses data aside as if you did not collect it. A robust measure of central tendency called winsorized means have been developed. For winsorized means, there is a trim paramater but winsorizing does not throw the data away. A winsorized mean transforms the data and compresses outliers to be bounded -- the highest/lowest values are compressed. The mean is then computed on the winsorized values. Winsorized means are available in the psych package.

#install.packages('psych')
library(psych)
wmA <- winsor.mean(dUntrans$thr1, trim = 0.05)
wmL <- winsor.mean(dUntrans$thr2, trim = 0.05)
wmM <- winsor.mean(dUntrans$thr3, trim = 0.05)
wmSpos <- winsor.mean(dUntrans$thr4, trim = 0.05)
wmSneg <- winsor.mean(dUntrans$thr5, trim = 0.05)
wmS <- winsor.mean(dUntrans$thr6, trim = 0.05)

# Medians are another helpful measure of central tendency that is robust. They allow us to determine what the middle score is of a set of data. The median is the middle score in a data set.

medA <- median(dUntrans$thr1)
medL <- median(dUntrans$thr2)
medM <- median(dUntrans$thr3)
medSpos <- median(dUntrans$thr4)
medSneg <- median(dUntrans$thr5)
medS <- median(dUntrans$thr6)

# Measures of variablity are key for communicating uncertainty. Variance is a common values but more commonly communicated is the standard deviation - which is the square root of variance. This has to do with the units that variance is expressed in and that variance is what is known as a biased estimator. Standard deviations can be computed easily with the sd() function.

sdA <- sd(dUntrans$thr1)
sdL <- sd(dUntrans$thr2)
sdM <- sd(dUntrans$thr3)
sdSpos <- sd(dUntrans$thr4)
sdSneg <- sd(dUntrans$thr5)
sdS <- sd(dUntrans$thr6)

# Lastly, the co-efficient of variation allows us to express a measure of variability expressed in units of the mean. This allows us to see how variable scores are once we factor in that the means of the data among our conditions could be quite different.

cvA <- sdA / mA
cvL <- sdL / mL
cvM <- sdM / mM
cvSpos <- sdSpos / mSpos
cvSneg <- sdSneg / mSneg
cvS <- sdS /mS

#### Tables ####

# Install the knitr and kableExtra packages
#install.packages('knitr')
#install.packages('kableExtra')

library(knitr)
library(kableExtra)
library(tidyverse) # you may need to install this...

# Let's get some data to play with...
data("mtcars")
dt <- mtcars[1:5,1:6]

# The %>% is a new syntax style that was introduced by Hadley Wickham in his tidyverse. It tends to make code more readable - this operator feeds through to functions.

dt %>% kable() %>% kable_styling()

# If you look at the Viewer tab in the bottom right pane of R Studio you should see a nice table. This table can be output to HTML (which can be imported to word) or even PDF.

# Let's load in the full_data.csv from the color experiment...

# We can make table of descriptive measures using knitr and then pretty it up somewhat...

condition <- c('Achromatic Gabor','L-Gabor','M-Gabor','S-Gabor','S-Positive Gaussian','S-Negative Gaussian')
stim_means <- c(mA, mL, mM, mS, mSpos, mSneg)
stim_median <- c(medA, medL, medM, medS, medSpos, medSneg)
stim_sd <- c(sdA, sdL, sdM, sdS, sdSpos, sdSneg)
stim_win_means <- c(wmA, wmL, wmM, wmS, wmSpos, wmSneg)
stim_cv <- c(cvA, cvL, cvM, cvS, cvSpos, cvSneg)

desc_table <- data.frame(condition, stim_means, stim_median, stim_win_means,stim_sd, stim_cv)

# The parameter digits defines the number of significant digits for us.
desc_table %>% kable(digits = 3) %>% kable_styling()

# We can now save the table with the Export tab in the viewer. However, we might want to change the column names. Let's do that using the colnames function.

colnames(desc_table) <- c('stimulus','mean','median','winsorized mean','standard deviation', 'co-efficient of variation')

# Let's re-run the table.
desc_table %>% kable(digits = 3) %>% kable_styling()

#### Assignment #2 ####

#setwd('C://Users//taylo//Dropbox//Exp//ruckerlab//Burke_Monitor_Exp//rgb-noise')
#setwd('/home/taylorcp/Dropbox/Exp/ruckerlab/Burke_Monitor_Exp/rgb-noise')

### For this assignment we'll use the following datasets. 

dat <- read.csv('rgb_monitor.csv')
re_dat <- read.csv('rbg_noise_ir_data.csv')

# Here's how to change the change the name of factors.
dat$Light <- revalue(as.factor(dat$Light), c("rgb"="RGB", "white"="White"))
re_dat$condition <- revalue(as.factor(re_dat$condition), c("rgb"="RGB", "white"="White"))

# How do we take a subset of the data? 
re_data_white <- subset(re_dat, re_dat$condition == 'White')

#### Assignment 2 ####

# 1. [4 Points] Make a box-plot and a bar-graph of the data in re_dat for both preD and postD. The variables preD and postD are the refractive error (in Diopters) measured in chickens. White and RGB are different lighting types. Label the axes in a descriptive way. Add errorbars to the plot. The axes of the plot should have a suitable font size for a scientific poster. The graph should be in grayscale (no color), the error bars should be thick enough to be visible. Combine the two plots together in a single plot with patchwork.  

# 2. [2 Points] Compute a t.test on the preD for the RGB birds. Do the same for preD for White. Do the same thing with PostD. Adjust the p.values with the false discovery rate option p.adjust(p, method = 'fdr'). Hint: 
#
# You can get the p-value from a t-test from the function t.test test if you assign it to a variable as follows:
# tresult <- t.test(re_dat$preD, re_dat$postD)
# tresult$p.value
#
# FYI - on many variables you can use the function ls() to get their component variables.

# 3. [2 Points] Make a table using kable of the mean, winsorized mean, median, standard deviation, and coef of variation for the variable Mean_vit_diff, Mean_chor_diff, and Mean_cac_diff. One row should be the birds that were under White and the second RGB light. 

# 4. [2 Points] Use the data.frame you created in Exercise 3 above. Load it in from your saved file. Create a new variable called S_summed that is square root of the squared and summed threshold values for S.positive and S.negative  Make a scatterplot of this variable versus the thresholds from the S condition with a stat_smooth(method = 'lm') line. To test to see if there is a correlation run the function cor.test() on S thresholds versus S_summed.  

#### Lecture 5 - Regression, ANOVA and Post-Hoc Tests ####

#install.packages('car')
#install.packagaes('QuantPsyc')
album1 <- read.delim("http://www.discoveringstatistics.com/docs/Album%20Sales%201.dat", header = TRUE)

# The line below will fail, but it provides you with the general form of the linear model function. The linear model is behind many/most of the Frequentist tests you will use on your data.
newModel <- lm(fomula = outcome ~ predictor, data = dataFrame)

#newModel is an object that can be summarized with the summary() function which will give you the results of the inferential test.

#If we apply this to our example data...
albumSales.1 <- lm(sales ~ adverts, data = album1)

# Summarize our simple regression model...
(modelSummary <- summary(albumSales.1))

# We can check for the indivudal variables within our regression model. 
ls(modelSummary)
sqrt(modelSummary$r.squared)

#### Multiple Regression ####
album2 <- read.delim("http://www.discoveringstatistics.com/docs/Album%20Sales%202.dat", header = TRUE)

# We can compute the simple restricted model first as a baseline.
albumSales.2 <- lm(sales ~ adverts, data = album2)
# Now we can grow the model to include multiple predictors.
albumSales.3 <- lm(sales ~ adverts + airplay + attract, data = album2)

# Now compare the summaries of the two models...
summary(albumSales.2)
summary(albumSales.3)

# What happened to our ability to predict album sales when we added more predictors? How do we interpret the model? The order matters, as when we add an additional predictor to the model we are holding the effects of the previous terms constant.

# To check how our coefficients change as we can look at the standarized beta coefficients which is in the QuantPsyc package
#install.packages('QuantPsyc')
library(QuantPsyc)

lm.beta(albumSales.3)

# We can calculate confidence intervals on our coefficients the narrower the better in their ability to predict our dependent variable.

confint(albumSales.3)

# How do we compare models? We can apply an ANOVA to models as we add or substract parameters. This is known as a step-wise procedure.

#### Exercises - Lecture 5 ####
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

# Lecture 5 exercises. 

# 1. The data set we have from the Color Vision and RE experiment can be modeled as a multiple regression. Compare multiple regression models for the full model where RE is the dependent variable and all of A, L, M, S, Sneg, and Spos are predictors.

# 2. Find the model that maximizes R-square from the best three predictors. Then the best two. Perform an ANOVA that compares the best 3 predictor model to the best 2 predictor model.

# 3. Report the beta coefficients and confidence intervals for the best two predictor model.

#### Lecture 6 ANOVA ####

# NB Run code from the Lecture 5 exercises.
dTransform_long <- melt(dTransform, id.vars = c('subject','RE')) 
colnames(dTransform_long) <- c('subject','RE','condition', 'threshold')

# There is an assumption of homogenity of variance behind the ANOVA. We can test this using Levene's Test from the car pakacage. The test checks the variance of the *residuals* for each group.  
library(car)
leveneTest(dTransform_long$threshold, dTransform_long$condition, center = median)

# To determine if there are any differences among our conditions we can use a one-way ANOVA on threshold condition.
d_aov <- aov(data = dTransform_long, formula = threshold ~ condition)
summary(d_aov)

TukeyHSD(d_aov)

#### Bootstrap -- Lecture 6 ####

library(boot)

# bootstrap CI

b.mean <- function(d,i) {
  mean_boot <- mean(d[i])
  return(mean_boot)
}

# cards
full_set <- c(rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4),rep(7,4),rep(8,4),rep(9,4),rep(10,4))

# take half the deck
shuffle_set <- sample(full_set)
samp_1_set <- shuffle_set[1:18]
samp_2_set <- shuffle_set[19:36]

# boot function
boot_card_1 <- boot(data = samp_1_set, statistic = b.mean, R = 999)

# bootstrap confidence intervals
(boot_card_1_ci <- boot.ci(boot.out = boot_card_1, type = c("perc"), index = 1))

# the bootstrap on regression
album1 <- read.delim("http://www.discoveringstatistics.com/docs/Album%20Sales%202.dat", header = TRUE)

library(boot)

bootReg <- function(formula, data, i) {
  d <- data[i,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

bootResults <- boot(statistic = bootReg, formula = sales ~ adverts + airplay, data = album1, R = 1000)

boot.ci(bootResults, type = "bca", index = 1) # intercept
boot.ci(bootResults, type = "bca", index = 2) # adverts
boot.ci(bootResults, type = "bca", index = 3) # airplay


#### Assignment 3 ####

# 1. [3 points] You modeled the Color Vision and RE experiment as a multiple regression. You found the best regression model. You can use the boostrap to get confidence intervals on the parameters for the best model. What are the boostrap confidence intervals for the parameters on the full model? 

# 2. [3 points] In question 4 of Assignment #2 you found the correlation of a variable S_summed  which was the square root of the squared and summed threshold values for S.positive and S.negative and found the correlation using the cor.test() function. Write a function to bootstrap the correlation of this value. What is the confidence interval on this correlation?    

# 3. [4 points] For this question you'll need the dataset:

dat <- read.csv('https://raw.githubusercontent.com/hashtagcpt/biostats2/master/Monitor_Birds.csv')

# Within this data set, you'll find three categorical variables that define three groups of birds, Control, Flicker, and Noise. Create three bar graph with error bars that represent standard error of the mean for three dependent measures: postCh, postAx, and postRE. Run three 1-way anovas on each of these variables, first using Levene's test to check the homogeneity of variance. Finally, run Tukey's Honestly Significant Difference Post-hoc test. Which pairs of conditions are different from one another according to this test at an alpha of p < .1?       


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
