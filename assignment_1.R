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