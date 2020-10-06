# load in the bootstrap library -- you may have to install
library(boot)
library(tidyverse)
library(tidyr) # will probably need to install this

# read in data
d <- read_csv('cvi.csv')

d <- d %>% gather(d) #gather data via tidyr
  
# compute median
sample_median <- median(d$value)

b.median <- function(d,i) {
  median_boot <- median(d[i])
  return(median_boot)
}

boot_My_sample <- boot(data = d$value, statistic = b.median, R = 999)
boot_My_sample
plot(boot_My_sample)

#median(sample1)

#sample_median <- median(sample1)
# 
# my_sample <- c(47,49,60,49,60,51,45,47,54,51,49,47,45,54)
# sample_median <- median(my_sample)
# 
# library(boot)
# b.median <- function(d,i) {
#   median_boot <- median(d[i])
#   return(median_boot)
# }
# boot_my_sample <- boot(data = My_sample, statistic = b.median, R = 999)