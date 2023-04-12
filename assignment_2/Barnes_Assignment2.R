#### Assignment 2 ####

rm(list = ls())

library(ggplot2)
library(reshape2)
library(plyr)
library(psych)
library(goeveg)

setwd("~/Downloads")

re_dat <- read.csv('rbg_noise_ir_data.csv')
dat <- read.csv('rgb_monitor.csv')

# 1. [4 Points] Make a box-plot and a bar-graph of the data in re_dat for both preD and postD. The variables preD and postD are the refractive error (in Diopters) measured in chickens. White and RGB are different lighting types. Label the axes in a descriptive way. Add errorbars to the plot. The axes of the plot should have a suitable font size for a scientific poster. The graph should be in grayscale (no color), the error bars should be thick enough to be visible. Combine the two plots together in a single plot with patchwork.  
#diff is the change post minus pre
diff <- re_dat$postD - re_dat$preD
#reassign condition as 
re_dat$condition <- revalue(as.factor(re_dat$condition), c("rgb"="RGB", "white"="White"))
tmp_dat = data.frame(bird=re_dat$bird, diff, condition=re_dat$condition)
tmp_dat_white <- subset(tmp_dat, tmp_dat$condition == 'White')
tmp_dat_rgb <- subset(tmp_dat, tmp_dat$condition == 'RGB')

#boxplot
WhiteBox <- ggplot(tmp_dat_white, aes(x=condition, y=diff)) + geom_boxplot() + xlab('Lighting') + ylab('Refractive Error (D)')  + xlab('Lighting') + ylab('Refractive Error (D)') +theme(text = element_text(size=10), axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))
RGBBox <- ggplot(tmp_dat_rgb, aes(x=condition, y=diff)) + geom_boxplot() + xlab('Lighting') + ylab('Refractive Error (D)')  + xlab('Lighting') + ylab('Refractive Error (D)') +theme(text = element_text(size=10), axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))

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

RGBdatac <- summarySE(tmp_dat_rgb, measurevar="diff", groupvars="condition")
Whitedatac <- summarySE(tmp_dat_white, measurevar="diff", groupvars="condition")

# bar graph with error bars
RGBBar <- ggplot(RGBdatac, aes(x=condition, y=diff)) + geom_bar(stat = 'identity') + geom_errorbar(width=.1, aes(ymin=diff-ci, ymax=diff+ci)) + xlab('Lighting') + ylab('Refractive Error (D)') +theme(text = element_text(size=12), axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))
WhiteBar <- ggplot(Whitedatac, aes(x=condition, y=diff)) + geom_bar(stat = 'identity') + geom_errorbar(width=.1, aes(ymin=diff-ci, ymax=diff+ci)) + xlab('Lighting') + ylab('Refractive Error (D)') +theme(text = element_text(size=12), axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))
library(patchwork)
WhiteBox+RGBBox+RGBBar+WhiteBar

# 2. [2 Points] Compute a t.test on the preD for the RGB birds. Do the same for preD for White. Do the same thing with PostD. Adjust the p.values with the false discovery rate option p.adjust(p, method = 'fdr').

re_data_white <- subset(re_dat, re_dat$condition == 'White')
re_data_rgb <- subset(re_dat, re_dat$condition == 'RGB')

#t-test white pre
t1 <- t.test(re_data_white$preD) 
#t-test white post
t2 <- t.test(re_data_white$postD)
#t-test rgb pre
t3 <- t.test(re_data_rgb$preD)
#t-test rgb post
t4 <- t.test(re_data_rgb$postD) 

p.vals <- c(t1$p.value, t2$p.value, t3$p.value, t4$p.value)
p.adjust(p.vals, method = 'fdr')

# 3. [2 Points] Make a table using kable of the mean, winsorized mean, median, standard deviation, and coef of variation for the variable Mean_vit_diff, Mean_chor_diff, and Mean_cac_diff. One row should be the birds that were under White and the second RGB light. 

library(knitr)
#subset white birds
white_sub <- subset(dat, Light == "white", select = c("Mean_vit_diff", "Mean_chor_diff", "Mean_cac_diff"))
#subset rgb birds
rgb_sub <- subset(dat, Light == "rgb", select = c("Mean_vit_diff", "Mean_chor_diff", "Mean_cac_diff"))

#White vit
white_Mvit <- c(mean(white_sub$Mean_vit_diff), winsor.mean(white_sub$Mean_vit_diff), median(white_sub$Mean_vit_diff), sd(white_sub$Mean_vit_diff), cv(white_sub$Mean_vit_diff))
#White chor
white_Mchor <- c(mean(white_sub$Mean_chor_diff), winsor.mean(white_sub$Mean_chor_diff), median(white_sub$Mean_chor_diff), sd(white_sub$Mean_chor_diff), cv(white_sub$Mean_chor_diff))
#White anterior chamber
white_Mcac <- c(mean(white_sub$Mean_cac_diff), winsor.mean(white_sub$Mean_cac_diff), median(white_sub$Mean_cac_diff), sd(white_sub$Mean_cac_diff), cv(white_sub$Mean_cac_diff))
#RGB vit
rgb_Mvit <- c(mean(rgb_sub$Mean_vit_diff), winsor.mean(rgb_sub$Mean_vit_diff), median(rgb_sub$Mean_vit_diff), sd(rgb_sub$Mean_vit_diff), cv(rgb_sub$Mean_vit_diff))
#RGB chor
rgb_Mchor <- c(mean(rgb_sub$Mean_vit_diff), winsor.mean(rgb_sub$Mean_vit_diff), median(rgb_sub$Mean_vit_diff), sd(rgb_sub$Mean_vit_diff), cv(rgb_sub$Mean_vit_diff))
#RGB anterior chamber
rgb_Mcac <- c(mean(rgb_sub$Mean_vit_diff), winsor.mean(rgb_sub$Mean_vit_diff), median(rgb_sub$Mean_vit_diff), sd(rgb_sub$Mean_vit_diff), cv(rgb_sub$Mean_vit_diff))

#stack data
data_stack <-rbind(white_Mvit, white_Mchor, white_Mcac, rgb_Mvit, rgb_Mchor, rgb_Mcac)
#make table with column names
table <- kable(x=data_stack, col.names = c('Mean', 'Winsorized Mean', 'Median', 'Standard Deviation', 'Coef of Variance'))
table

# 4. [2 Points] Use the data.frame you created in Exercise 3 above. Load it in from your saved file. Create a new variable called S_summed that is square root of the squared and summed threshold values for S.positive and S.negative  Make a scatterplot of this variable versus the thresholds from the S condition with a stat_smooth(method = 'lm') line. To test to see if there is a correlation run the function cor.test() on S thresholds versus S_summed.  

#read in csv file
data <- read.csv('minimaldata.csv')
#square root of the squared spos and sneg values
S_summed <- sqrt(data$S.positive^2 + data$S.negative^2)
#data frame for plot
splot <- data.frame(data$S, S_summed)
#rename columns
colnames(splot) <- c('S', 'S_summed')
#scatter plot of s_summed vs S
S_scatter <- ggplot(data = splot, aes(x=S, y=S_summed)) + geom_point(size=1) + stat_smooth(method = 'lm')
S_scatter
cor.test(x=data$S, y=S_summed)