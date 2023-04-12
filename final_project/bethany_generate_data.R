#Bethany Arabic 

# use the tidyverse
library(tidyverse)

# keep the seed constant to draww the same samples each time the script is run
set.seed(11235813)

# create constants for generating the data.frame ####

# number of subjects
n_subjects <- 100

# create ID for subjects
ID <- 1:n_subjects
ID_all <- rep(ID,3)

# create a vector with letters representing different apps
App_list <- c('A','B','C')
App_list_all <- rep(App_list, n_subjects)

# set plausible min/max ages for each participant
age_min <- 55
age_max <- 90

# generate a list of random ages from a uniform distribution then using ceiling rounding (always round up to the nearest integer)
ages <- ceiling(runif(length(ID), age_min, age_max))

# What is the rating scale that we are going to sample for each of the # likability and usage scores? Draw them randomly for all subjects  *but* with weightings.

# set the plausible responses from 1 through 5.
response_scale <- 1:5

# weight the selection of the ratings for likability 
rating_weights <- c(0.1, 0.2, 0.4, 0.2, 0.1)
rating_list <- sample(response_scale, length(ID_all), replace = TRUE, prob = rating_weights)

# weight the selection of the rating for useability
usage_weights <- c(0.1, 0.1, 0.25, 0.35, 0.2)
usage_list <- sample(response_scale, length(ID_all), replace = TRUE, prob = usage_weights)

# Did the participant continue using the app or not?
continue_use <- c(0,1)

# make a list of whether the observer will continue or not
continue_weights <- c(0.3, 0.7)
continue_list <- sample(continue_use, length(ID_all), replace = TRUE, prob = continue_weights)

# put it all together into a data.frame and save as CSV file
dd <- data.frame(ID_all, ages, App_list_all, rating_list, usage_list, continue_list)
# change the column names 
colnames(dd) <- c('ID','age', 'app', 'like_rating', 'use_rating', 'continue_device_use')
  
write_csv(dd, 'bethany_data.csv')