#### Assignment 4 ####

# 1. [4 Points] The BayesFactor package contains a function regressionBF. Use regressionBF on the dTransform data -- use the threshold values (S, M, L, and so on) to predict RE (refractive error). Look at the Bayes Factor output, what does it seem to tell you? The help shows an example of how the BF for each model can be tested against the full model (all thresholds). Put this line in the script with a comment that notes the best model. How does this model compare to what we saw with the stepwise model selection we did in previous lectures? 

regressionBF(RE~ A + L + M + Sneg + Spos + S, data = dTransform, whichModels = "all")

output = regressionBF(RE ~ A + L + M + Sneg + Spos +S, data = dTransform, progress=FALSE)
head(output)

#Comparison of all the factors to the full model and rearranging to show the best model.
head(output / output[63])

#Best model would be Spos and S against RE since it has the highest Bayes factor. This process is a lot easier than the stepwise. 

#2. [3 Points] A colleague is going to run an experiment where they are going to compute a correlation test. Beforehand, they ask you how many subjects they need to run. Based on previous work, they believe the r for their experiment could be between .2 and .6, their funders want a *Type II error* probability of no more than 0.4, and they will use a conventional alpha of .05. Calculate what the minimum and the maximum number of subjects they need to run for their experiment to be adequately powered, given their funder's constraints.   


library(pwr)
pwr.r.test(r= 0.2, sig.level = 0.05, power = 1-0.4)
pwr.r.test(r= 0.6, sig.level = 0.05, power = 1-0.4)


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

# Create vectors 
contrast <- seq(0.01, 0.1, length.out = 10)
pcorrect <- seq(0.0, 1.0,length.out = 10)

# Create data
dat.contast <- datasim(contrast, pcorrect, ntrials=20)

# Summarize data
dc_summary <- dat.contast %>% group_by(contrast,correct) %>% tally() %>%  ungroup() %>% complete(contrast, correct, fill = list(n = 1))

dc_summary <- subset(dc_summary, correct == 1)
dc_summary$incorrect <- 20 - dc_summary$n
dc_summary$correct <- dc_summary$n

# Take the absolute value of negative contrast values
dc_summary$contrast <- abs(dc_summary$contrast)

# Fit the psychometric function
psymet_fit <- glm(formula = cbind(correct, incorrect) ~ log(contrast), family = binomial, data = dc_summary)

xseq <- seq(0.01, 0.1, len = 100) 
psymet_fit.pred <- predict(psymet_fit, newdata = data.frame(contrast = xseq), type = "response", se.fit = TRUE)  # obtain predicted values 
psymet_pred_df <- data.frame(xseq, psymet_fit.pred$fit)
colnames(psymet_pred_df) <- c('contrast','fit')

psymet_plot <- ggplot(data = dc_summary, aes(x = contrast, y = correct / 20)) + geom_point(size = 4) + geom_line(data = psymet_pred_df, aes(x = contrast, y = fit), size = 2) + ylab('proportion correct') + theme(text = element_text(size=16), axis.text.x = element_text(size = 16), axis.text.y = element_text(size=16))

psymet_plot
# The 50% threshold is around 0.050 based on observation of the graph (but this changes slightly based on the simulated data)