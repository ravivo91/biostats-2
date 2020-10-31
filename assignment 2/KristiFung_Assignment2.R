rm(list=ls())

setwd("~/NECO 3/Fall 2019/Biostats")

dat <-read.csv('rgb_monitor.csv')
re_dat<-read.csv('rbg_noise_ir_data.csv')

library(reshape2)
library(ggplot2)
library(plyr)
library(psych)

re_datLong <- melt(re_dat, id.vars = c("bird", "condition"),
                   variable.name = "Time",
                   value_name = "Refractive Error")

View(re_datLong)

s1 <- subset(re_datLong, condition == "white" & Time == "preD",select=value)
s2 <- subset(re_datLong, condition == "white" & Time == "postD",select=value)
s3 <- subset(re_datLong, condition == "rgb" & Time == "preD",select=value)
s4 <- subset(re_datLong, condition == "rgb" & Time == "postD",select=value)

re_datSub <-data.frame(s1, s2, s3, s4)

colnames(re_datSub) <- c("White_Pre", "White_Post", "RGB_Pre", "RGB_Post")

re_datLonger <-melt(re_datSub, variable.name="condition",
                    value.name="RE")
#BoxPlot
pBox <- ggplot(data = re_datLonger, aes(x=condition, y=RE)) + geom_boxplot() + xlab('Lighting') + ylab('Refractive Error (D)')  + xlab('Lighting Conditions') + ylab('Refractive Error (Diopters)') +theme(text = element_text(size=10), axis.text.x = element_text(size = 8), axis.text.y = element_text(size=8))

print(pBox)

#Errorbar functions####
summarySE<-function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE){
  
library(plyr) 

length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }

#Summary

datac <- ddply(data, groupvars, .drop=.drop,
               .fun = function(xx, col) {
                 c(N    = length2(xx[[col]], na.rm=na.rm),
                   mean = mean   (xx[[col]], na.rm=na.rm),
                   sd   = sd     (xx[[col]], na.rm=na.rm)
                 )
               },
               measurevar
)

#Mean Column
datac<-plyr::rename(datac,c("mean"= measurevar))

datac$se <- datac$sd / sqrt(datac$N) 

#CI
ciMult <- qt(conf.interval/2 + .5, datac$N-1)
datac$ci <- datac$se * ciMult

return(datac)
}

datac <- summarySE(data=re_datLonger, measurevar="RE", groupvars="condition")

pBar <- ggplot(datac, aes(x=condition, y=RE)) + geom_bar(stat = 'identity') + geom_errorbar(width=.1, aes(ymin=RE-ci, ymax=RE+ci)) + xlab('Lighting Conditions') + ylab('Refractive Error (Diopters)') +theme(text = element_text(size=10), axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))

print(pBar)

library(patchwork)
print(pBox+pBar)

#t.test

# tresult <- t.test(re_dat$preD, re_dat$postD)
# tresult$p.value

t1 <- t.test(s3) # preD RGB birds 1 sample t-test
t2 <- t.test(s1) # preD White
t3 <- t.test(s4) # postD RGB
t4 <- t.test(s2) # postD White

p.vals <- c(t1$p.value, t2$p.value, t3$p.value, t4$p.value)
p.adjust(p.vals, method = 'fdr')

#Table of means
library(knitr)
library(kableExtra)

#subset white birds
white_sub <- subset(dat, Light == "white", select = c("Mean_vit_diff", "Mean_chor_diff", "Mean_cac_diff"))

#subset rgb birds
rgb_sub <- subset(dat, Light == "rgb", select = c("Mean_vit_diff", "Mean_chor_diff", "Mean_cac_diff"))

#mean, winsorized mean, SD, coef of variation for each subset
white_Mvit <- c(mean(white_sub$Mean_vit_diff), winsor.mean(white_sub$Mean_vit_diff), median(white_sub$Mean_vit_diff), sd(white_sub$Mean_vit_diff), c(white_sub$Mean_vit_diff))
white_Mchor <- c(mean(white_sub$Mean_chor_diff), winsor.mean(white_sub$Mean_chor_diff), median(white_sub$Mean_chor_diff), sd(white_sub$Mean_chor_diff), c(white_sub$Mean_chor_diff))
white_Mcac <- c(mean(white_sub$Mean_cac_diff), winsor.mean(white_sub$Mean_cac_diff), median(white_sub$Mean_cac_diff), sd(white_sub$Mean_cac_diff), c(white_sub$Mean_cac_diff))
rgb_Mvit <- c(mean(rgb_sub$Mean_vit_diff), winsor.mean(rgb_sub$Mean_vit_diff), median(rgb_sub$Mean_vit_diff), sd(rgb_sub$Mean_vit_diff), c(rgb_sub$Mean_vit_diff))
rgb_Mchor <- c(mean(rgb_sub$Mean_vit_diff), winsor.mean(rgb_sub$Mean_vit_diff), median(rgb_sub$Mean_vit_diff), sd(rgb_sub$Mean_vit_diff), c(rgb_sub$Mean_vit_diff))
rgb_Mcac <- c(mean(rgb_sub$Mean_vit_diff), winsor.mean(rgb_sub$Mean_vit_diff), median(rgb_sub$Mean_vit_diff), sd(rgb_sub$Mean_vit_diff), c(rgb_sub$Mean_vit_diff))

#stack rows vertically
data_stack <-rbind(white_Mvit,white_Mchor,white_Mcac,rgb_Mvit, rgb_Mchor,rgb_Mcac)

#make table
table <- kable(x=data_stack, col.names = c('Mean', 'Winsorized Mean', 'Median', 'Standard Deviation', 'Coefficient of Variance'))
table


#Excerise 3
E3_data <- read.csv('exercise3.csv')
S_summed <- sqrt(E3_data$S_positive^2 + E3_data$S_negative^2)
S_summed_plot <- data.frame(E3_data$S, S_summed)
colnames(S_summed_plot) <- c('S', 'S_summed')

pA <- ggplot(data = S_summed_plot, aes(x=S, y=S_summed)) + geom_point(size=1) + stat_smooth(method = 'lm')
pA
cor.test(x=E3_data$S, y=S_summed)
