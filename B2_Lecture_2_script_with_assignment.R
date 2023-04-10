#### BIOSTATS 2 - LECTURE SCRIPT ####

### WEEK 1 ####
rm(list = ls()) 

setwd("C:/Users/taylorpc/Downloads")


#### R as a calculator ####

# basic operations
21 + 49

v <- 21 + 49
v1.1 <- 7*3 + 7*7 

# Silly trick: pick a number between 1 and 10 and assign it to original -- but it shows us how variables get over-written.
original <- 2
x <- original
# double it
x <- x*2
# multiply the new number by 5
x <- x*5
# divide by the original number
x <- x / original
# subtract 7 from the number
x <- x - 7

# print to the console - answer will always be 3! 
print(x)
x

# concatinate and case sensitivity
x <- c(1,2,3)
X <- c(1:10)
v <- 'a'
v1 <- 'b'
v2 <- 'c'
vVector <- c(v, v1, v2)

# function examples round()
v2 <- round(7^1.565)+ 7^2
vRoot <- sqrt(v2) 
log(v2)
log10(vRoot)
log(vRoot, base = 10)
# modulo
13 %% 4

# %>%

# getting help
?log

# See also the help tab in the Files, Plots, Packages, Help, and Viewer tab.
# Also many of the basic recipies for much of what you'll want to do can be found at the site http://www.cookbook-r.com

#### packages ####

# installing packages
install.packages('ggplot2') # only need to do this once

# after it is in your environment you call
library(ggplot2)

# install the tidyverse
#install.packages('tidyverse')

# Want to update you packages to the latest versions?
update.packages()

# More on packages and libraries later, but R, like all programming languages relies on extensions to the base language. In python you import, matlab you'll have external toolboxes, in C/C++ you'll have header files. This type of programming is often referred to as modular programming. The downside? Sometimes more than half the battle is finding the right library, package, extension, software development kit, or application programmer's interface, et cetera to do the job you need. 

# Back to R. Before you run it read the code below and ask yourself the question what is the value of y after the third line?
x <- 50
y <- x * 2
x <- 80

# Beginning data frames! Dataframes are the most useful data structure (a general term for how data are arranged) in R. 

# Here is how to create a sample data frame. You'll likely never see code like this again because you'll be reading data in from files.
data <- read.table(header=T, text='
 subject sex threshold
       1   M    0.013
       2   F    0.021
       3   F    0.011
       4   M    0.011
 ')

# values in a data frame can be accessed via square brackets []. Let's get the threshold for the third subject.
data[3,3]

# You can also get the entire row, in this case a single subject, subject 3.
data[3,]

# or all the thresholds that are stored in column 3.
data[,3]

# but that's not the most useful way to do it, because you'd have to remember the column in which you stored the data. We can use the $ notation to access variables within in a dataframe.

data$threshold

# you can also pop out columns of a dataframe easily...

# popping out
new_data_vector <- data$threshold

# putting together
d<-data.frame(rbind(vVector,c(1,2,3)))

#### WEEK 2 ####

#### Reading in Data ####
# read in .csv -- easiest
example_data <- read.csv('Example-Data-1.csv')

# read in an excel xlsx file
#install.packages('readxl')
library(readxl)

# read in excel -- must supply the sheet name or index
example_excel <- read_xlsx('Example-Data-1.xlsx',sheet = 1)

# read in a matlab .mat file
# install.packages('R.matlab')
library('R.matlab')

example_matlab <- readMat('CPT_1_16_blurLevelDiscrim.mat')
head(example_matlab)
example_data$OD
OD <- example_data$OD

# or if you really want to...
example_data_gui <- read.csv(file.choose(), header = TRUE )


#### fundamental programming concepts ####

# We'll usually need to clean-up and reformat data (here I mean data in the general comp sci sense, not data from an experiment) via code. This is not unique to R. Other languages have more elaborate and specific techniques for changing one type of data to another. R is (trust me) on the user friendly end of the spectrum of computer lanagues. To do our clean-up and other tasks where we need to tell the computer to make a decision, we'll need statements that control the flow of a program. What is flow? In short, it is what code gets executed at a given step. Let's dive in...

# The if statement. 
# This is a fundamental statement most programing languages. An if statement works linke a logical statement "If X then Y". Below is an if statement in action.

test_variable <- TRUE  # NB naming variables is an art, not too long, not too short, you don't want spaces and they can't start with a number.
var_to_print <- 3
if (test_variable == TRUE) {
        print(data$threshold[var_to_print])
}

# If we break down this 'if' we have two main parts. The first is the statment enclosed in () that follows our keyword 'if'*. This is a logical statement, if it evaluates to TRUE then the part of the code in curly braces {}, the code block will be executed. Try changing the value of 'test_variable' to FALSE and re-run this code.

### *NB R has certain keywords that you can use for variable names or if you choose a variable name that is that of a function, weird things can happen..)

# With if statements we can happy test conditions and use code blocks. Often you'll see if statements chained together in this way, an 'if' statement, one or more 'else if' statements, then 'else'. Here is an example.

blue_jays_winning_percentage <- 0 #.601

if (blue_jays_winning_percentage > .600) {
        print('The Blue Jays are better than OK!')
} else if (blue_jays_winning_percentage <= .599 && blue_jays_winning_percentage >= .500) {
        print('OK Blue Jays!')
} else if (blue_jays_winning_percentage < .500 && blue_jays_winning_percentage > 0 ) {
        print('Not OK Blue Jays.')
} else {
        print('Is it winter? Go Leafs Go.')
}        

# A full list of logical operators can be found at: https://www.statmethods.net/management/operators.html
# Part of the fun and challenge of programming is figuring out tricks of logic that can make your life easier. 

# Another type of program control flow are called loops. In general think of a loop as a command that tell the computer to repeat a block of code a number of times that can be based on a pre-set number (a for loop) or based on a logical condition. Here is an example of a for loop.

for (ind in 1:length(example_data$Condition)) {
        print(example_data$OD[ind]^2)
}

# sometimes it is simple and you know the number of times you want to do things:

for (counter in 1:10) {
        print(counter)
}

# Here is an example of a while loop that does the exact same thing.

ctr <- 1
while (ctr <= length(example_data$Condition)) {
        print(example_data$OD[ctr]^2)
        ctr <- ctr + 1
}

# QUESTION -- when would we need to use a for loop versus a while loop?

# User defined functions. These can *really* make your life easier and code cleaner, if you find yourself repeating a lot of code in your script. For example, if you want to reproduce several of the same type of plot with different x/y variables, you can write a function that takes those x/y variables as inputs and reproduce the plot. Here is a contrived example of a user-defined function in R

funny_add <- function(var1,var2) {
        var1 + var2 + var1/2 + var2/2
}

(eye_measures <- funny_add(example_data$OD, example_data$OS)) # fun tip -- enclosing a whole statement in () will print out the variable result

#install.packages('TeachingDemos')
#library(TeachingDemos)

#### Exercise 1 #####

#The "Lucky Number 13" trick. 
#This trick that operates on the unique property of multiples of 9. 
#Choose a number between 1-10. 
#Multiply his number by 9.
# Add the first and second digit of his number together. 
# If it's a single digit number (i.e. 9), add 0.
#Add 4 to his new number.
# Each and every time, the answer will be 13.

# Loop through all the numbers 1-10, be careful of the special case 10
# use the digits() function from the TeachingDemos library # Make a function called lucky13 that take the input of the initial number

#### the normality assumption ####
set.seed(100) # set the random number generator
d_al <- rnorm(n = 60, mean = 24, sd = 1.5)

df_al <- data.frame(d)
colnames(df_al) <- c('axial_length')
pal <- ggplot(data = df_al, aes(x = axial_length)) + geom_histogram()
pal

d <- rnorm(n =1000, mean = 0, sd = 1)

d_dev <- (d - mean(d))
x_axis <- c(1:length(d))
dev_df <- data.frame(cbind(x_axis,d_dev))

# This part of the code is where I make the figures for my paper ####
# initialize the plot, ask for the point geom, ask for a line of intercept/slope
dev_plot <- ggplot(data = dev_df, aes(x = x_axis, y = d_dev)) + geom_point(size = 4) + geom_abline(slope = 0, intercept = mean(dev_df$d_dev), color = 'red', size = 2) 

print(dev_plot)

# set the plot labels
dev_plot <- dev_plot + xlab('sample') + ylab('deviation from the mean') 

dev_plot <- dev_plot + theme_classic()
dev_plot

# change the them to have bigger text
dev_plot <- dev_plot + theme(text = element_text(size=20), axis.text.x = element_text(size = 16), axis.text.y = element_text(size=16))

# put the plot in the plot window tab
dev_plot

# show the deviations as a histogram
hist_plot <- ggplot(data = dev_df, aes(d_dev)) + geom_histogram(binwidth = 0.05) + xlab('deviation from the mean') + theme(text = element_text(size=20), axis.text.x = element_text(size = 16), axis.text.y = element_text(size=16))

hist_plot

#### Assignment #1 ####

# Submission file naming ####
# Create a blank script file that is called [yourname]_assignment_1.R where yourname is your fullname without spaces. 

# A1.1 [2 points] ####
# Locate a data set. (Hint: McFarland & Yates is a good resource). You may use data that you yourself have (include a reference to those data with your submission) or use the data set at this link: https://files.ontario.ca/pssd/en-2018-pssd-20190614.csv 

# A1.2 [2 points] ####
# Load in the data set as shown above and assign it to a variable (e.g., d).

# A1.3 [2 points] #### 
# Use the summary() on the data. In a comment in your script describe the information that the summary() function provides you in two/three sentences. 

# A1.4 [1 point] #### 
# 5. Create a two-element string vector using c() called trick_type. The two strings should be 'trick1' and 'trick2'

type_type <- c('trick1','trick2')

user_guess <- readline('Which trick is it? ')

 if (user_guess == 1 ) {
  print('Yay!')
  } 
 else if (user_guess == 2) {
  print('Boo!')
} else {
  print('that is not a valid trick')
}
  
# A1.5 [3 points] ####
# Create an if / else if / else statement that runs either trick1 or trick2 depending on another variable you set, for example if (do_trick == 'trick1') { [CODE] }. Outside the if statement set a variable called 'user_guess' that contains the guess for the user. The else statement should print "That is not a valid trick" if your do_trick variable is neither "trick1" or "trick2".

# A1.6 [6 points] ####
# Your task is to create a function called trimmed_mean. This function will remove variables from our input data that are greater than and less than a user-defined number of standard deviations above AND below the mean. Here is some code to get you started:

set.seed(416) #NB put this in your assignment script this will allow me to check your code easily.

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

# how to call your trimmed_mean function
tm_result <- trimmed_mean(x) # the result is wrong because it is always 1, but will be correct when you add your code

# A1.7 [2 points] ####
# Create a loop to call your trimmed mean function three times. Print out the trimmed mean when you trim values 1, 2, and 3 standard deviations above AND below the mean.