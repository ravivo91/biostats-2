#Biostats - Homework #2

# 1.It takes some effort but you can read in excel files from over the web. The code below provides an example:

# library(readxl)
# library(httr)
# url1<-'https://evs.nci.nih.gov/ftp1/CDISC/SDTM/SDTM%20Terminology.xls'
# GET(url1, write_disk(tf <- tempfile(fileext = ".xls")))
# df <- read_excel(tf, 2L)

# Uncomment the code above (use the Code menu) and modify the code to read in the example data file from our github site

# 2. Create a for loop to print out the numbers evenly divisible by 13 that are less than 500

limit <- 500

for (counter in 1:limit) {
  # hint the line below will need to use the modulo operator here in this if statement 
  if (counter) 
  {
  print(counter)
  }  
}

# 3. modify the for loop above to create a variable that contains the values instead of printing them to the console

# hints: create an empty vector and second counter
X <- c()
second_counter <- 0

for (counter in 1:limit) {
  # hint the line below will need to use the modulo operator here in this if statement 
  if (counter %% 13 == 0) 
  {
    second_counter <- second_counter + 1
    # modify the line below to save the values hint use square brackets [] to 'index' using the values of second_counter
    
    X <- 0
  }  
}

# 4. Make a plot using ggplot's qplot function of these numbers

# First create a vector Y that is the same length as your values that are divisible by using seq(1,length(X))

# load in the ggplot library

# use qplot to quickly plot Y versus X
