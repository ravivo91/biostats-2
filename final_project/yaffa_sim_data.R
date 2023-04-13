library(tidyverse)

# clean-up
rm(list = ls())

# set the random number generation seeed
set.seed(416)

# how many subjects?
n_obs <- 16

# minimum ages for observers
min_age <- 65
max_guess <- 17

# generate ages
ages_young <- runif(n_obs, min = 19, max = 27)
ages_old <- rep(min_age, n_obs) + runif(n_obs, min = 0, max = max_guess)

# stimulus sizes and contrasts
sz <- c(1.3, 5.0)
m_contrast <- c(4.2, 22)
condition_list <- expand.grid(sz=sz, m_contrast=m_contrast)
n_conditions <- 4

# duration mean thresholds and S.E.M. - young and old
young_mean <- c(50, 60, 65, 110) # size 1, 2; contrast 1, 2
young_sd <- c(2, 5, 7, 10) * sqrt(8) # turn S.E.M to sd, 8 obs

# duration mean thresholds and S.E.M. - old
old_mean <- c(65, 68, 65, 80) # size 1, 2; contrast 1, 2
old_sd <- c(3, 7, 7, 10) * sqrt(8) # turn S.E.M to sd, 8 obs 

dt_sim <- function(n_conditions, n_obs, dt_mean, dt_sd) {
  # simulate young duration threshold data
  dt <- c()
  dt_tmp <- c()
  for (tmp in 1:n_conditions) {
      dt_tmp <- rnorm(n_obs, mean = dt_mean[tmp], sd = dt_sd[tmp])
      dt <- c(dt, dt_tmp)
  }
  return(dt)
}

d_young <- dt_sim(n_conditions, n_obs, young_mean, young_sd)
d_old <- dt_sim(n_conditions, n_obs, old_mean, old_sd)

# time to put together a long data frame

# make some subject IDs
ID <- 1:n_obs
ID_var <- rep(ID, n_conditions*2)

# a numeric variable for ages
age_var <- c(rep(ages_young, n_conditions), rep(ages_old, n_conditions))
# a categorical variable for age
age_str_var <- c(rep('young', n_conditions*n_obs), rep('old', n_conditions*n_obs))
# build a list of conditions 
sz_var <- c(rep(sz[1], n_obs), rep(sz[2], n_obs))
sz_var <- rep(sz_var, 2) 
m_contrast_var <- c(rep(m_contrast[1], n_obs), rep(m_contrast[1], n_obs), rep(m_contrast[2], n_obs), rep(m_contrast[2], n_obs))

# now the duration threshold in ms
dt_data_var <- c(d_young, d_old)

d_yaffa <- data.frame(ID_var, age_var, age_str_var, sz_var, m_contrast_var, dt_data_var)
colnames(d_yaffa) <- c('ID', 'age', 'group', 'size', 'contrast', 'duration_threshold')

# compute the conditional means with aggregate to check the data
aggregate(data = d_yaffa, duration_threshold ~ size + contrast + group, mean)

write_csv(d_yaffa, 'data_yaffa.csv')