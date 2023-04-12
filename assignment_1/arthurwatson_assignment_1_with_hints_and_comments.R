# A1.1 [2 points] ####
# Locate a data set -- nice
library(tidyverse)
datasets::ToothGrowth


# A1.2 [2 points] ####
# Load in the data set as shown above and assign it to a variable (e.g., d) -- nice
d <- datasets::ToothGrowth

# A1.3 [2 points] #### 
# In a comment in your script describe the information that the summary() function provides you 

summary(d) #This summary gives the minimum, maximum, 1st, 2nd, 3rd quartiles, the mean for the variables of length, dose. It also gives the n for each supply group (Vitamin C or Orange Juice)

# A1.4 [1 point] #### 
# 5. Create a two-element string vector using c() called trick_type. The two strings should be 'trick1' and 'trick2' -- maybe nice?
trick1 <- d[,1]
trick2 <- d[,3]
trick_type <- c(trick1, trick2)

# A1.5 [3 points] ####
# Create an if / else if / else statement that runs either trick1 or trick2 depending on another variable you set, for example if (do_trick == 'trick1') { [CODE] }. Outside the if statement set a variable called 'user_guess' that contains the guess for the user. The else statement should print "That is not a valid trick" if your do_trick variable is neither "trick1" or "trick2". -- I dont understand the user_guess section

# CPT Hints: 

#1. The variable do_trick is undefined. You could use:

do_trick <- 0.5 # not interactive

#or

do_trick <- readline('Please input a number between 0 and 1: ') # interactive

#2. Note R has a quirk with && versus & see:
utils::browseURL('https://stackoverflow.com/questions/7953833/what-is-the-difference-between-short-and-long-forms-of-and-or-lo')

#3. view() is a little bit tricky to use here try print()

if (do_trick >= 0 && do_trick < 0.25) {
  view(trick1)
} else if (do_trick >= 0.25 && do_trick <= 0.75) {
  view(trick_type)
} else if (do_trick > 0.75 && do_trick <= 1) {
  view(trick2)
} else {
  print('That is not a valid trick; set do_trick to any number equal to x where 0 <= x <= 1')
}

# A1.6 [6 points] ####
# Create a function called trimmed_mean. This function will remove variables from our input data that are greater than and less than a user-defined number of standard deviations above AND below the mean.

set.seed(416)
# generate some fake data
number_of_samples <- 1000
data_to_trim <- rnorm(number_of_samples) 
# compute the standard deviation of data to trim and assign it to xsd
xsd <- sd(data_to_trim)
# compute the mean
data_to_trim_mean <- mean(data_to_trim)


# CPT Hits - see below:

# function code:
trimmed_mean <- function(data_to_trim, standard_devation_cutoff) {
  
  # You need to assign and output a variable for a function
  
  # You had:
  data_to_trim[data_to_trim > (data_to_trim_mean - (standard_devation_cutoff*sd(data_to_trim))) & data_to_trim < (data_to_trim_mean + (standard_devation_cutoff*sd(data_to_trim)))]

  # now a variable is assigned
  out <- data_to_trim[data_to_trim > (data_to_trim_mean - (standard_devation_cutoff*sd(data_to_trim))) & data_to_trim < (data_to_trim_mean + (standard_devation_cutoff*sd(data_to_trim)))]
  
  # now we output the variable back outside the function
  return(out)
}


# CPT Hints: ####
#
# Your issus is that x is not defined here. Try this:

x <- data_to_trim
standard_deviation_cutoff <- 2
y <- standard_deviation_cutoff

# CPT Q: Can the above be simplified?
  
# CPT: A trimmed mean is a single number, note that tm_result is a list of 954 numbers. Q: What do you need to turn these numbers in to a single number, that is a mean that includes sd() trimming?

# using the function
tm_result <- trimmed_mean(data_to_trim = x, standard_devation_cutoff = y)

# A1.7 [2 points] ####
# Create a loop to call your trimmed mean function three times. Print out the trimmed mean when you trim values 1, 2, and 3 standard deviations above AND below the mean. -- im not sure about this

for (standard_deviation_cutoff in 1:3) {
#print(trimmed_mean)  

  # CPT Hint: 
  # You need to call your trimmed_mean() function. What does this mean. Here is an example:
  #
  # tm <- mean(trimmed_mean(data_to_trim, sd_cutoff)
  # print(tm)
}