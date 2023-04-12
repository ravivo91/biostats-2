library(tidyverse)
library(moments) # for skewness
library(car) # for levene's test of normality

# clean
rm(list = ls())

# set the random number generator seed
set.seed(416)

# read in previous axial length data 
dd <- read_csv('neco_axial_length_2019_2022.csv')

# get axial lengths for all right eyes
dd$axial_length <- (dd$OD) 

# compute mean, standard deviation, and skew
mean(dd$axial_length)
sd(dd$axial_length)
skewness(dd$axial_length)

# number to sample
n_obs <- 60

# get empirical counts from histogram
hist_vec <- hist(dd$axial_length, plot = FALSE, breaks = 25)
probs <- hist_vec$counts / length(dd$axial_length)

# sample from empirical distribution and add
sim_al <- sample(hist_vec$mids, n_obs, replace = TRUE, prob = probs) + rnorm(n_obs, mean = 0, sd = 0.25)

# now we have the axial lengths let's simulate the other data

# 1. Srini -- CS with retinal bleaching measurement, control must be no bleaching ####

# mean AUC normal human = 2345.4; sd = 775.6
# fake it with monkey data from Ridder et al. 2013
# mean = 996.7, sd = 247.19 from:
# 
# Ridder, W. H., Zhang, K. M., Karsolia, A., Engles, M., & Burke, J. (2019). Comparison of contrast sensitivity in macaque monkeys and humans. Visual Neuroscience, 36, E008.

# make some data
unbleached <- rnorm(length(sim_al), mean = 2345.4, sd = 776.6)
bleached <- rnorm(length(sim_al), mean = 996.7, sd = 247.19)

# organize data frame
d_srini <- data.frame(cbind(rep(sim_al, 2), c(rep('bleached', length(sim_al)), rep('unbleached', length(sim_al))), c(bleached, unbleached)))
colnames(d_srini) <- c('axial_length', 'condition', 'CSF_AUC')
# cast the data types to num
d_srini$axial_length <- as.numeric(d_srini$axial_length)
d_srini$CSF_AUC <- as.numeric(d_srini$CSF_AUC)

# write to a csv file
write_csv(d_srini,'srini_data.csv')

# 2. Raviv
#
# Load thresholds for on/off at two size from Hogue. 

# load in previous data and subset by condition and one contrast
d_raviv <- read_csv('on_off_full_psychophysical_data.csv')
d_raviv <- subset(d_raviv, stim == 'dog')
d_raviv_off  <- subset(d_raviv, cond == 'OFF') 
d_raviv_off <- subset(d_raviv_off, contrast == d_raviv[3,6]$contrast)
d_raviv_on <- subset(d_raviv, cond == 'ON') 
d_raviv_on <- subset(d_raviv_on, contrast == (-1*d_raviv[3,6]$contrast))

# Do the same sampling routine as above but for dog ON
hist_vec_ON <- hist(d_raviv_on$dprime, plot = FALSE, breaks = 25)
probs_ON <- hist_vec_ON$counts / length(d_raviv_on)
# sample from empirical distribution and add
sim_raviv_ON <- sample(hist_vec_ON$mids, n_obs, replace = TRUE, prob = probs_ON) + rnorm(n_obs, mean = 0, sd = 0.05)

# Do the same sampling routine as above but for dog OFF
hist_vec_OFF <- hist(d_raviv_off$dprime, plot = FALSE, breaks = 25)
probs_OFF <- hist_vec_OFF$counts / length(d_raviv_off)
# sample from empirical distribution and add
sim_raviv_OFF <- sample(hist_vec_OFF$mids, n_obs, replace = TRUE, prob = probs_OFF) + rnorm(n_obs, mean = 0, sd = 0.05)

# Make Raviv's data
d_fp_raviv <- data.frame(cbind(rep(sim_al,2), c(rep('ON', n_obs), rep('OFF', n_obs)), as.numeric(c(sim_raviv_ON, sim_raviv_OFF))))
colnames(d_fp_raviv) <- c('axial_length','condition','d_prime_thresh')
# make sure the columns are the correct data type
d_fp_raviv$axial_length <- as.numeric(d_fp_raviv$axial_length)
d_fp_raviv$d_prime_thresh <- as.numeric(d_fp_raviv$d_prime_thresh)

write_csv(d_fp_raviv,'raviv_data.csv')

# 3. Rachel ####
#
# Contrast Sensitivity Function in the Fovea and Periphery
#
# Data from:Rovamo J, Leinonen L, Laurinen P, Virsu V. Temporal integration and contrast sensitivity in foveal and peripheral vision. Perception. 1984 Dec;13(6):665-74.
#
# roll-off in CS_peak - CS_low_cut -- fovea
# mean = 40, 30 -- express as a percentage
# roll-of in CS_peak - CS_low_cut -- periphery
# mean = 10, 5 -- express as a percentage

peak_cs_fovea <- runif(length(sim_al), min = 35, max = 55) + rnorm(sim_al, mean = 0, sd = 0.5)
low_cut_cs_fovea <- runif(length(sim_al), min = 25, max = 30) + rnorm(length(sim_al), mean = 0, sd = 0.5)

cs_roll_off_fovea <- low_cut_cs_fovea / peak_cs_fovea 

peak_cs_periphery <- runif(sim_al, min = 15, max = 25) + rnorm(length(sim_al), mean = 0, sd = 0.5)
low_cut_cs_periphery <- runif(sim_al, min = 8, max = 12) + rnorm(length(sim_al), mean = 0, sd = 0.5)

cs_roll_off_periphery <- low_cut_cs_periphery / peak_cs_periphery 

d_rachel <- data.frame(cbind(rep(sim_al,2), c(rep('FOVEA', n_obs), rep('PERIPHERY', n_obs)), as.numeric(c(cs_roll_off_fovea, cs_roll_off_periphery))))
colnames(d_rachel) <- c('axial_length','eccentricity','CS_rolloff_ratio')
d_rachel$axial_length <- as.numeric(d_rachel$axial_length)
d_rachel$CS_rolloff_ratio <- as.numeric(d_rachel$CS_rolloff_ratio)

write_csv(d_rachel,'rachel_data.csv')

# HINT 1 ####
#
# if you want a wide data frame from eccentricity in d_rachel
d_rachel_wider <- pivot_wider(d_rachel, names_from = eccentricity, values_from = CS_rolloff_ratio)

# 4. Simon ####
#
# Data from : Kim YJ, Reynaud A, Hess RF, Mullen KT. A normative data set for the clinical assessment of achromatic and chromatic contrast sensitivity using a qCSF approach. Investigative ophthalmology & visual science. 2017 Jul 1;58(9):3628-36.
# 
#  peak sf
# A - mean = 1.63, sd = 0.39
A_peak <- rnorm(length(sim_al), mean = 1.63, sd = 0.39)
# BY - mean = 0.49, sd = 0.12
BY_peak <- rnorm(length(sim_al), mean = 0.49, sd = 0.12)

# high frequency cutoff - data

# A - mean = 27.42, sd = 8.41
A_cutoff <- rnorm(length(sim_al), mean = 27.42, sd = 8.41)
# BY - mean = 6.26, sd = 1.72
BY_cutoff <- rnorm(length(sim_al), mean = 6.26, sd = 1.72)

ID <- 1:length(sim_al)
d_simon <- data.frame(ID, sim_al, A_peak, BY_peak, A_cutoff, BY_cutoff)

write_csv(d_simon,'simon_data.csv')

# HINT 2 ####
#
# if you want a longer data frame 
d_simon_longer <- pivot_longer(d_simon, cols = A_peak:BY_cutoff, names_to = "measures", values_to = "spatial_frequency")