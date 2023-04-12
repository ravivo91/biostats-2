#### Assignment #1 ####
setwd("~/Biostatistics")
library(ggplot2)
library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
# A1.1 [2 points] ####
# Locate a data set. You may use data that you yourself have (include those data with your submission) or use the data set at this link: https://files.ontario.ca/pssd/en-2018-pssd-20190614.csv 
#I found a common freely available R dataset known as Remissions and will be using this dataset for the assignment.


# A1.2 [2 points] ####
# Load in the data set as shown above and assign it to a variable (e.g., d).

d=read.csv("remission.csv")

#Citation: used NicoleAdam_assignment3.R to refresh memory on read.csv command

# A1.3 [2 points] #### 
# Use the summary() on the data. In a comment in your script describe the information that the summary() function provides you in two/three sentences. 
summary(d)
##        X              LI              m           r         
##  Min.   : 1.0   Min.   :0.400   Min.   :1   Min.   :0.0000  
##  1st Qu.: 7.5   1st Qu.:0.650   1st Qu.:1   1st Qu.:0.0000  
##  Median :14.0   Median :0.900   Median :1   Median :0.0000  
##  Mean   :14.0   Mean   :1.004   Mean   :1   Mean   :0.3333  
##  3rd Qu.:20.5   3rd Qu.:1.250   3rd Qu.:1   3rd Qu.:1.0000  
##  Max.   :27.0   Max.   :1.900   Max.   :1   Max.   :1.0000
#L1 is the variable the data set uses for cell activity. The mean cell activity was 1.004 according to the summary.
#m is the number of patients in each group. We can see from the summary output that all of the groups had one patient in them.
#r was the number of patients out of m who went into remission. We can see that the mean remission was 0.33, suggesting that less than half of the cancer patients went into remission. 
#x is just the patient number, so that output is not meaningful in this situation, since the study just numbered the patients from 1 to 27.

# A1.4 [1 point] #### 
# 5. Create a two-element string vector using c() called trick_type. The two strings should be 'trick1' and 'trick2'
trick_type<-c('trick1','trick2')

# A1.5 [3 points] ####
# Create an if / else if / else statement that runs either trick1 or trick2 depending on another variable you set, for example if (do_trick == 'trick1') { [CODE] }. Outside the if statement set a variable called 'user_guess' that contains the guess for the user. The else statement should print "That is not a valid trick" if your do_trick variable is neither "trick1" or "trick2".
#Citation: used this resource to refresh my memory on if/else statements: 
#https://www.datamentor.io/r-programming/if-else-statement/

do_trick="trick1"
user_guess="guess"
if(do_trick=="trick1")
{
  user_guess<-"trick1_guess"
}else if(do_trick=="trick2")
{
  user_guess<-"trick2_guess"
}else {
  print("That is not a valid trick")
}


# A1.6 [6 points] ####
# Your task is to create a function called trimmed_mean. This function will remove variables from our input data that are greater than and less than a user-defined number of standard deviations above AND below the mean. Here is some code to get you started:

# CPT: I set the seed to have a repoducible result:
set.seed(416)

# generate some fake data
number_of_samples <- 1000
x <- rnorm(number_of_samples) 
# compute the standard deviation of x and assign it to xsd
xsd <- sd(x)
# we can use the square bracket syntax with a logical statement, for example if we want to trim variables that are 2 standard deviations above the mean: 
standard_deviation_cutoff <- 1
x_up_trim <- x[x < standard_deviation_cutoff*xsd]

# looked at the trimmed mean
upper_trimmed_mean <- mean(x_up_trim)
# Look at the original mean - how do they differ? Food for thought: when would you want to compute a trimmed mean?
untrimmed_mean <- mean(x)

# here is function code for you to fill in:

trimmed_mean <- function(data_to_trim, standard_deviation_cutoff) {
  #Determining mean and standard deviation of data
  data_mean<-mean(data_to_trim)
  data_sd<-sd(data_to_trim)
  upper_bound<-data_mean+standard_deviation_cutoff*data_sd
  lower_bound<-data_mean-standard_deviation_cutoff*data_sd
  #we need to remove the numbers which are outside these upper and lower bounds
  new_trimmed_data<-data_to_trim[data_to_trim>=lower_bound&data_to_trim<=upper_bound]
  new_trimmed_mean<-mean(new_trimmed_data) 
  return(new_trimmed_mean)
}

# CPT: I'm testing out your trimmed mean  function with 1:10 sd in a for loop. This might be usefule for Q A1.7...

for (tmp in 1:10) {
  tm <- trimmed_mean(x, tmp)
  print(tm)
}

#Citation: Articles with examples read to review how to find the trimmed mean
#https://www.statology.org/trimmed-mean-in-r/
#https://search.r-project.org/CRAN/refmans/disprose/html/trim_DF.html
#########

# A1.7 [2 points] ####
# Create a loop to call your trimmed mean function three times. Print out the trimmed mean when you trim values 1, 2, and 3 standard deviations above AND below the mean.
