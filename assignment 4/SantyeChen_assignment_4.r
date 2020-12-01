#### Assignment 4 ####
rm(list = ls())
setwd("C:/Users/stick/OneDrive/2019 Fall OD3/Biostats")
library(R.matlab)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(stringr)
library(dplyr)
library(pwr)
library(BayesFactor)


dFull <- read.csv('https://raw.githubusercontent.com/hashtagcpt/biostats2/master/full_data.csv')

dFull$subject <- factor(dFull$subject)


RE <- dFull$RE
subject <- dFull$subject

t1<-log10(dFull$thr1)           
t2<-log10(dFull$thr2)
t3<-log10(dFull$thr3)
t4<-log10(dFull$thr4)
t5<-log10(dFull$thr5)
t6<-log10(dFull$thr6)

dTransform <- data.frame(subject,RE,t1,t2,t3,t4,t5,t6)

colnames(dTransform) <- c('subject','RE','A','L','M','Sneg','Spos','S')


# 1. [4 Points] The BayesFactor package contains a function regressionBF. Use regressionBF on the dTransform data -- use the threshold values (S, M, L, and so on) to predict RE (refractive error). Look at the Bayes Factor output, what does it seem to tell you? The help shows an example of how the BF for each model can be tested against the full model (all thresholds). Put this line in the script with a comment that notes the best model. How does this model compare to what we saw with the stepwise model selection we did in previous lectures? 
output = regressionBF(RE ~ A + M + L + Sneg + Spos + S, data = dTransform, progress=FALSE)
head(output / output[63])

#2. [3 Points] A colleague is going to run an experiment where they are going to compute a correlation test. Beforehand, they ask you how many subjects they need to run. Based on previous work, they believe the r for their experiment could be between .2 and .6, their funders want a *Type II error* probability of no more than 0.4, and they will use a conventional alpha of .05. Calculate what the minimum and the maximum number of subjects they need to run for their experiment to be adequately powered, given their funder's constraints.  

(minsubs <- pwr.r.test(power = 0.6, r=0.6, sig.level = 0.05)) 
#need 12.55059
(maxsubs <- pwr.r.test(power = 0.6, r=0.2, sig.level = 0.05)) 
#need 121.3345

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
con = c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
pcor = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
ntrials = 20
simdata <- datasim(contrasts = con, pcorrect = pcor, ntrials = ntrials)

df_summary <- simdata %>% group_by(contrast,correct) %>% tally() %>%  ungroup() %>% complete(contrast, correct, fill = list(n = 1))

df_summary <- subset(df_summary, correct == 1)
df_summary$incorrect <- ntrials - df_summary$n
df_summary$correct <- df_summary$n
library(ggplot2)

psymet_fit <- glm(formula = cbind(correct, incorrect) ~ log(contrast), family = binomial,data = df_summary)

xseq <- seq(0.01, 0.1, len = 100) 
psymet_fit.pred <- predict(psymet_fit, newdata = data.frame(contrast = xseq), type = "response", se.fit = TRUE)  # obtain predicted values 
psymet_pred_df <- data.frame(xseq, psymet_fit.pred$fit)
colnames(psymet_pred_df) <- c('contrast','fit')

psymet_plot <- ggplot(data = df_summary, aes(x = contrast, y = correct / ntrials)) + geom_point(size = 4) + geom_line(data = psymet_pred_df, aes(x = contrast, y = fit), size = 2) + ylab('proportion correct') + theme(text = element_text(size=16), axis.text.x = element_text(size = 16), axis.text.y = element_text(size=16))
psymet_plot

# 0.5 is the threshold level we chose
Thresh <- exp(qnorm(p = 0.5, mean = -coef(psymet_fit)[1]/coef(psymet_fit)[2], sd = 1/coef(psymet_fit)[2]))
Thresh
