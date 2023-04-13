#Bethany Arabic 

# use the tidyverse
library(tidyverse)

# keep the seed constant to draww the same samples each time the script is run
set.seed(11235813)

# create constants for generating the data.frame ####

# number of subjects
n_subjects <- 30

# create ID for subjects
ID <- 1:n_subjects
ID_all <- rep(ID,2)

# create a vector with letters representing different apps
condition <- c('NEAR','FAR')
condition_list <- c(rep(condition[1], n_subjects), rep(condition[2], n_subjects))

# set plausible min/max ages for each participant
age_min <- 21
age_max <- 25

# generate a list of random ages from a uniform distribution then using ceiling rounding (always round up to the nearest integer)
ages <- ceiling(runif(length(ID), age_min, age_max))

# What is the rating scale that we are going to sample for each of the # likability and usage scores? Draw them randomly for all subjects  *but* with weightings.

# set the plausible responses from 1 through 5.
response_scale <- 1:5

# weight the selection of the ratings for eye tiredness 
rating_weights <- c(0.1, 0.2, 0.4, 0.2, 0.1)
rating_list <- sample(response_scale, length(ID_all), replace = TRUE, prob = rating_weights)

# weight the selection of the rating for vision clarity
usage_weights <- c(0.1, 0.1, 0.25, 0.35, 0.2)
usage_list <- sample(response_scale, length(ID_all), replace = TRUE, prob = usage_weights)

# put it all together into a data.frame and save as CSV file
dd <- data.frame(ID_all, ages, condition_list, rating_list, usage_list)
# change the column names 
colnames(dd) <- c('ID','age', 'distance', 'eyes_tired', 'vision_clarity')
  
write_csv(dd, 'manshul_data.csv')