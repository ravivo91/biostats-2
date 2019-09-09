#### BIOSTATS 2 - LECTURE SCRIPT 1 ####
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

# Reading 

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

install.packages('TeachingDemos')
library(TeachingDemos)

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

