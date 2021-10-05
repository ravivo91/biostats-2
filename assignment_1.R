#### Assignment #1 ####

# Submission file naming ####
# Create a blank script file that is called [yourname]_assignment_1.R where yourname is your fullname without spaces. 

# A1.1 [2 points] ####
# Locate a data set. You may use data that you yourself have (include those data with your submission) or use the data set at this link: https://files.ontario.ca/pssd/en-2018-pssd-20190614.csv 

# A1.2 [2 points] ####
# Load in the data set as shown above and assign it to a variable (e.g., d).

# A1.3 [2 points] #### 
# Use the summary() on the data. In a comment in your script describe the information that the summary() function provides you in two/three sentences. 

# A1.4 [1 point] #### 
# 5. Create a two-element string vector using c() called trick_type. The two strings should be 'trick1' and 'trick2'

# A1.5 [3 points] ####
# Create an if / else if / else statement that runs either trick1 or trick2 depending on another variable you set, for example if (do_trick == 'trick1') { [CODE] }. Outside the if statment set a variable called 'user_guess' that contains the guess for the user. The else statement should print "That is not a valid trick" if your do_trick variable is neither "trick1" or "trick2".

# A1.6 [6 points] ####
# Your task is to create a function called trimmed_mean. This function will remove variables from our input data that are greater than and less than a user-defined number of standard deviations above AND below the mean. Here is some code to get you started:

# generate some fake data
number_of_samples <- 1000
x <- rnorm(number_of_samples) 
# compute the standard deviation of x and assign it to xsd
xsd <- sd(x)
# we can use the square bracket syntax with a logical statement, for example if we want to trim variables that are 2 standard deviations above the mean: 
standard_deviation_cuttoff <- 1
x_up_trim <- x[x < standard_deviation_cuttoff*xsd]

# looked at the trimmed mean
upper_trimmed_mean <- mean(x_up_trim)
# Look at the original mean - how do they differ? Food for thought: when would you want to compute a trimmed mean?
untrimmed_mean <- mean(x)

# here is function code for you to fill in:

trimmed_mean <- function(data_to_trim, standard_devation_cutoff) {
        # YOUR TRIMMING CODE GOES HERE
        tm <- 1 
        return(tm)
}
