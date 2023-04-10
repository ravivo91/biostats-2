rm(list = ls())

library(tidyverse)
library(readxl)
library(patchwork)

# note: nm refers to spectral bandwidth which increases the power at each frequency because it is an integral at each frequency
#  The y axis shows the spectral irradiance 

# set working directory in script
setwd("C:/Users/chris/Dropbox/Teaching/Teaching_2023/biostats-2-future")

# constants to read in from the excel file
read_start_xlsx <- 16
read_end_xlsx <- 96

# set consistent x-axis for graphs
x_limts <- c(300, 800)

# read SPD from excel helper function ####
# NOTE: cond variable name changed to condition
load_spd <- function(file_name, condition_name) {
  
  # magic numbers for excel sheets from LEDMOTIVE report
  read_start_xlsx <- 16
  read_end_xlsx <- 96
  
  d <- read_xlsx(file_name, sheet = 'SPD raw data')
  d <- d[read_start_xlsx:read_end_xlsx,1:2]
  colnames(d) <- c('wavelength','spd') #(W/nm/m^2)
  d <- d %>% arrange(wavelength)
  # convert to numeric data type 
  d$wavelength <- as.numeric(d$wavelength)
  d$spd <- as.numeric(d$spd)
  
  # add a condition col
  cond_name <- rep(condition_name, length(d$wavelength))
  d$condition <- as.factor(cond_name)
  
  return(d)
}

# load cold, warm, and melanopsin max/min ####
d_cold_white <- load_spd('spectrum-report-cold-white.xlsx','high')
d_warm_white <- load_spd('spectrum-report-warm-white.xlsx', 'low')
d_equal <- load_spd('spectrum-report-equal.xlsx','equal')
d_control <- load_spd('spectrum-report-rgb-control.xlsx', 'control')

# Plot new light spectra - using our function to load ####
d_melanopsin_max <- load_spd('spectrum-report-ledmotive-melanopsin-max.xlsx', 'melanopsin max')
d_melanopsin_min <- load_spd('spectrum-report-ledmotive-melanopsin-min.xlsx', 'melanopsin min')
d_flat <- load_spd('spectrum-report-ledmotive-flatwhite.xlsx', 'white flat')
d_yellow <- load_spd('spectrum-report-ledmotive-yellow.xlsx', 'yellow')

d_new_light <- rbind(d_flat, d_melanopsin_max, d_melanopsin_min)

p2_new_light <- ggplot(data = d_new_light, aes(x = wavelength, y = spd, color = condition)) + geom_line(size = 1.5) + xlab('wavelength (nm)') + ylab('spectral power density') + ggtitle('') + xlim(x_limts[1], x_limts[2]) + scale_color_manual(values=c("blue", "red", "black", "green"))
p2_new_light

# Chicken Cones ####
d_leds <- read_csv('ledmotive_max_outputs.csv')

d_leds_longer <- d_leds %>%
  pivot_longer(
    cols = L1:L7,
    names_to = c("LED"), values_to = "Power")

d_chick <- read_csv('l_m_s_u_dbl.csv')
wavelength <- d_leds$wavelength
d_chick <- cbind(wavelength, d_chick)

d_chick_longer <- d_chick %>%
  pivot_longer(
    cols = L:D,
    names_to = c("cone"), values_to = "response")

p_chick_cones <- ggplot(d_chick_longer, aes(x=wavelength, y=response, color = cone)) + geom_line(size = 1.2) + ylab('response') + scale_color_manual(values = c("U"="purple","L"="red","M"= "darkgreen", "S"="blue", "D"="black")) 
p_chick_cones

# What might we want to change in the graph?
# Hint: Note the order of the legend. It is in the order of the data file, not an order that would be sensible from left to right.


# Lesson ends here ####
