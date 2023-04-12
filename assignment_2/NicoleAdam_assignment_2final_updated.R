#### Assignment 2 ####

rm(list = ls())

library(ggplot2)
library(reshape2)
library(plyr)
library(psych)
library(goeveg)

setwd("~/Biostats")

re_dat <- read.csv('rbg_noise_ir_data.csv')
dat <- read.csv('rgb_monitor.csv')

# 1. [4 Points] Make a box-plot and a bar-graph of the data in re_dat for both preD and postD. The variables preD and postD are the refractive error (in Diopters) measured in chickens. White and RGB are different lighting types. Label the axes in a descriptive way. Add errorbars to the plot. The axes of the plot should have a suitable font size for a scientific poster. The graph should be in grayscale (no color), the error bars should be thick enough to be visible. Combine the two plots together in a single plot with patchwork.  

# retaining the set of columns labeled bird and condition and melding the data into new columns called timing and refractive error
re_datLong <- melt(re_dat, id.vars = c("bird", "condition"),
                   variable.name = "Timing",
                   value.name = "Refractive Error")

# creating a variable for refractive errors containing preD and white as the deciding factors and so on for each 
#subset conditions
d1 <- subset(re_datLong, condition == "white" & Timing == "preD", select = "Refractive Error")
d2 <- subset(re_datLong, condition == "white" & Timing == "postD", select = "Refractive Error")
d3 <- subset(re_datLong, condition == "rgb" & Timing == "preD", select = "Refractive Error")
d4 <- subset(re_datLong, condition == "rgb" & Timing == "postD", select = "Refractive Error")

#creating a new data frame using the new variables we just created and calling them new labels
#then creating a new table with merged columns
re_datSub <- data.frame(d1, d2, d3, d4)
colnames(re_datSub) <- c("White (Pre)", "White (Post)", "RGB (Pre)", "RGB (Post)")
re_datLonger <- melt(re_datSub, variable.name = "cond",
                     value.name = "RE")

#Creating a boxplot of the previous cleaned up data 
#BoxPlot
pBox <- ggplot(data = re_datLonger, aes(x=cond, y=RE)) + geom_boxplot() + xlab('Lighting') + ylab('Refractive Error (D)')  + xlab('Lighting') + ylab('Refractive Error (D)') +theme(text = element_text(size=12), axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))

#Define functions for bar graph/error bars
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
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

datac <- summarySE(data=re_datLonger, measurevar="RE", groupvars="cond")

# bar graph with error bars
pBar <- ggplot(datac, aes(x=cond, y=RE)) + geom_bar(stat = 'identity') + geom_errorbar(width=.1, aes(ymin=RE-ci, ymax=RE+ci)) + xlab('Lighting') + ylab('Refractive Error (D)') +theme(text = element_text(size=12), axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))

devtools::install_github("thomasp85/patchwork")
library(patchwork)
pBox + pBar

# 2. [2 Points] Compute a t.test on the preD for the RGB birds. Do the same for preD for White. Do the same thing with PostD. Adjust the p.values with the false discovery rate option p.adjust(p, method = 'fdr'). Hint: 
#
# You can get the p-value from a t-test from the function t.test test if you assign it to a variable as follows:
# tresult <- t.test(re_dat$preD, re_dat$postD)
# tresult$p.value
#
# FYI - on many variables you can use the function ls() to get their component variables.

t1 <- t.test(d3) # preD RGB birds 1 sample t-test
t2 <- t.test(d1) # preD White
t3 <- t.test(d4) # postD RGB
t4 <- t.test(d2) # postD White

p.vals <- c(t1$p.value, t2$p.value, t3$p.value, t4$p.value)
p.adjust(p.vals, method = 'fdr')

# 3. [2 Points] Make a table using kable of the mean, winsorized mean, median, standard deviation, and coef of variation for the variable Mean_vit_diff, Mean_chor_diff, and Mean_cac_diff. One row should be the birds that were under White and the second RGB light. 

library(knitr)
#subset white birds
white_sub <- subset(dat, Light == "white", select = c("Mean_vit_diff", "Mean_chor_diff", "Mean_cac_diff"))
#subset rgb birds
rgb_sub <- subset(dat, Light == "rgb", select = c("Mean_vit_diff", "Mean_chor_diff", "Mean_cac_diff"))
#mean, winsorized mean, SD, coef of variation for each subset
white_Mvit <- c(mean(white_sub$Mean_vit_diff), winsor.mean(white_sub$Mean_vit_diff), median(white_sub$Mean_vit_diff), sd(white_sub$Mean_vit_diff), cv(white_sub$Mean_vit_diff))
white_Mchor <- c(mean(white_sub$Mean_chor_diff), winsor.mean(white_sub$Mean_chor_diff), median(white_sub$Mean_chor_diff), sd(white_sub$Mean_chor_diff), cv(white_sub$Mean_chor_diff))
white_Mcac <- c(mean(white_sub$Mean_cac_diff), winsor.mean(white_sub$Mean_cac_diff), median(white_sub$Mean_cac_diff), sd(white_sub$Mean_cac_diff), cv(white_sub$Mean_cac_diff))
rgb_Mvit <- c(mean(rgb_sub$Mean_vit_diff), winsor.mean(rgb_sub$Mean_vit_diff), median(rgb_sub$Mean_vit_diff), sd(rgb_sub$Mean_vit_diff), cv(rgb_sub$Mean_vit_diff))
rgb_Mchor <- c(mean(rgb_sub$Mean_vit_diff), winsor.mean(rgb_sub$Mean_vit_diff), median(rgb_sub$Mean_vit_diff), sd(rgb_sub$Mean_vit_diff), cv(rgb_sub$Mean_vit_diff))
rgb_Mcac <- c(mean(rgb_sub$Mean_vit_diff), winsor.mean(rgb_sub$Mean_vit_diff), median(rgb_sub$Mean_vit_diff), sd(rgb_sub$Mean_vit_diff), cv(rgb_sub$Mean_vit_diff))

#stack rows vertically
data_stack <-rbind(white_Mvit, white_Mchor, white_Mcac, rgb_Mvit, rgb_Mchor, rgb_Mcac)
#make table
table <- kable(x=data_stack, col.names = c('Mean', 'Winsorized Mean', 'Median', 'Standard Deviation', 'Coefficient of Variance'))
table
table %>% kable_styling()
# 4. [2 Points] Use the data.frame you created in Exercise 3 above. Load it in from your saved file. Create a new variable called S_summed that is square root of the squared and summed threshold values for S.positive and S.negative  Make a scatterplot of this variable versus the thresholds from the S condition with a stat_smooth(method = 'lm') line. To test to see if there is a correlation run the function cor.test() on S thresholds versus S_summed.  

write.csv(dTransform, file = "data_ex3final.csv")

Ex3_data <- read.csv('data_ex3final.csv')
S_summed <- sqrt(Ex3_data$S_positive^2 + Ex3_data$S_negative^2)
S_summed_plot <- data.frame(Ex3_data$S, S_summed)
colnames(S_summed_plot) <- c('S', 'S_summed')

pA <- ggplot(data = S_summed_plot, aes(x=S, y=S_summed)) + geom_point(size=1) + stat_smooth(method = 'lm')
pA
cor.test(x=Ex3_data$S, y=S_summed)