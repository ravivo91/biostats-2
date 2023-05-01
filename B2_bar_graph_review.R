library(ggplot2)
library(tidyverse)

re_dat <- read.csv('rbg_noise_ir_data.csv')

# The bar graph - we need to summarize ####

re_dat_long <- pivot_longer(data = re_dat, cols = preD:postD)
re_agg_mean <- aggregate(value ~ name + condition, data = re_dat_long, mean)

re_agg_sd <- aggregate(value ~ name + condition, data = re_dat_long, sd)
re_agg_sd$value <- re_agg_mean$value + re_agg_sd$value

pbar <- ggplot(data = re_agg_mean, aes(x = name, y = value, fill=condition, color = condition)) +  geom_bar(stat = 'identity', position = position_dodge()) + geom_errorbar(stat = 'identity', width = 0.15, position = position_dodge(width = 0.9), aes(ymin = value, ymax = re_agg_sd$value)) + labs(x = "Timing", y = "Refractive Error (D)", title = "Pre vs Post Rx") + theme_bw() + scale_fill_manual(values = c('gray65','gray35')) + scale_color_manual(values = c('gray65','gray35'))

print(pbar) 

