#### BIOSTATS 2 - ASSIGNMENT 2 ####
# Katie Kosteva 
# September 2019

#setwd('C://Users//taylo//Dropbox//Exp//ruckerlab//Burke_Monitor_Exp//rgb-noise')
#setwd('/home/taylorcp/Dropbox/Exp/ruckerlab/Burke_Monitor_Exp/rgb-noise')
dat <- read.csv('rgb_monitor.csv')
re_dat <- read.csv('rbg_noise_ir_data.csv')
dat$Light <- revalue(as.factor(dat$Light), c("rgb"="RGB", "white"="White"))
re_dat$condition <- revalue(as.factor(re_dat$condition), c("rgb"="RGB", "white"="White"))
re_data_white <- subset(re_dat, re_dat$condition == 'White')
re_data_rgb <- subset(re_dat, re_dat$condition == 'RGB')

#### Assignment 2 ####

# 1. [4 Points] Make a box-plot and a bar-graph of the data in re_dat for both preD and postD. The variables preD and postD are the refractive error (in Diopters) measured in chickens. White and RGB are different lighting types. Label the axes in a descriptive way. Add errorbars to the plot. The axes of the plot should have a suitable font size for a scientific poster. The graph should be in grayscale (no color), the error bars should be thick enough to be visible. Combine the two plots together in a single plot with patchwork.
library(ggplot2)
re_dat_long_white <- melt(re_data_white, id.vars = c("bird","condition"))
re_dat_long_rgb <- melt(re_data_rgb, id.vars = c("bird","condition"))
stacked_long <- rbind(re_dat_long_white,re_dat_long_rgb)
stacked_long_2 <- stacked_long %>% unite("cond", condition:variable, remove = TRUE)

# Box Plots
pBox_birds <- ggplot(data = stacked_long_2, aes(x = cond,condition, y=value)) + geom_boxplot() + xlab('Condition') + ylab('Diopters')
print(pBox_birds)

# Bar-Graph
stacked_long_2 <- stacked_long_2[,2:3]
colnames(stacked_long_2) <- c('RE','cond')
bar_SE <- summarySE(data=stacked_long_2, measurevar='RE', groupvars = 'cond')

pBar_birds <- ggplot(stacked_long_2, aes(x=cond, y=value, group=1)) + geom_bar(aes(fill = variable), stat = 'identity') + geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci)) + theme(text = element_text(size=20), axis.text.x = element_text(size = 16), axis.text.y = element_text(size=16))
pBar_birds

# 2. [2 Points] Compute a t.test on the preD for the RGB birds. Do the same for preD for White. Do the same thing with PostD. Adjust the p.values with the false discovery rate option p.adjust(p, method = 'fdr'). Hint: 
#
# You can get the p-value from a t-test from the function t.test test if you assign it to a variable as follows:
# tresult <- t.test(re_dat$preD, re_dat$postD)
# tresult$p.value
#
# FYI - on many variables you can use the function ls() to get their component variables.
t_preD_white <- t.test(re_data_white$preD)
t_preD_RGB <- t.test(re_data_rgb$preD)
t_postD_white <- t.test(re_data_white$postD)
t_postD_RGB <- t.test(re_data_rgb$postD)

t_preD_white$p.value 
t_preD_RGB$p.value
t_postD_white$p.value
t_postD_RGB$p.value

p.adjust(t_preD_white$p.value, method = 'fdr')
p.adjust(t_preD_RGB$p.value, method = 'fdr')
p.adjust(t_postD_white$p.value, method = 'fdr')
p.adjust(t_postD_RGB$p.value, method = 'fdr')

# 3. [2 Points] Make a table using kable of the mean, winsorized mean, median, standard deviation, and coef of variation for the variable Mean_vit_diff, Mean_chor_diff, and Mean_cac_diff. One row should be the birds that were under White and the second RGB light. 
data_white <- subset(dat, dat$Light == 'White')
data_rgb <- subset(dat, dat$Light == 'RGB')

M1 <- mean(data_white$Mean_vit_diff)
M2 <- mean(data_rgb$Mean_chor_diff)

WM1 <- winsor.mean(data_white$Mean_vit_diff)
WM2 <- winsor.mean(data_rgb$Mean_chor_diff) 

Me1 <- median(data_white$Mean_vit_diff)
Me2 <- median(data_rgb$Mean_chor_diff)

sd1 <- sd(data_white$Mean_vit_diff)
sd2 <- sd(data_rgb$Mean_chor_diff)

var1 <- sd1/M1
var2 <- sd2/M2

white_total <- c(M1,WM1,Me1,sd1,var1)
rgb_total <- c(M2,WM2,Me2,sd2,var2)
pre_table <- data.frame(rbind(white_total,rgb_total))
colnames(pre_table) <- c("Mean","Winsor Mean","Median","Standard Deviation","Variance")
pre_table %>% kable() %>% kable_styling()

# 4. [2 Points] Use the data.frame you created in Exercise 3 above. Load it in from your saved file. Create a new variable called S_summed that is square root of the squared and summed threshold values for S.positive and S.negative  Make a scatterplot of this variable versus the thresholds from the S condition with a stat_smooth(method = 'lm') line. To test to see if there is a correlation run the function cor.test() on S thresholds versus S_summed
library(ggplot2)
minimal_data <- read.csv('minimal_data.csv')
S_summed <- sqrt((minimal_data$S.*minimal_data$S.)+(minimal_data$S..1*minimal_data$S..1))
S <- minimal_data$S
new_minimal <- data.frame(S_summed,S)
pPLOT_S <- ggplot(data = new_minimal, aes(x=S,y=S_summed)) + geom_point() + stat_smooth(method = 'lm')
pPLOT_S
cor.test(S,S_summed)

# user defined function 
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
  datac <- plyr:rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


