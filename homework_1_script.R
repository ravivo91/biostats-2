#Biostats 2 - Homework file

#### Homework-1 ####
# run this line of code
browseURL("http://www.cookbook-r.com/Basics/")
# 
# read through each of the 6 sections
#
# Do the following tasks:
#
# 1. create a variable and take the log

# 2. create a vector of numbers using the concatinate - c(1,2,3) and compute the mean
# hint: use the function length() for the denominator

# run the following code

#
set.seed(100)

#
x <- rnorm(20)
#
y <- rt(20, df=50)
#
random_data <- data.frame(x,y)
#
t.test(random_data$x, random_data$y, var.equal = FALSE)

# 3. In the blank comments above each line write a short description of what each command does.
# HINT: it will be useful to use the help. For example ?rnorm ?rt ?t.test

# 4. t.test takes several options (as you'll see using ?t.test) change the var.equal to TRUE and enter the command below. How does this change the t.test that R is calculating?

# 5. 