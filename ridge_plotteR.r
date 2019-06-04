# Author: Jeffrey Grover
# Purpose: Create ridge plots using the ggridges package
# Created: 2019-04-19
# Usage: Ridge plots are a nice visualization for data that allows you to not
# see the same data a violin or box plot would show, but also the shape of the
# data. It doesn't suffer from the artificially inflated median-look of the
# violin plot, and turning it horizontal by default focuses the eye on the fact
# that the density is highest near the median, rather than on the extremes.

library(dplyr)
library(ggplot2)
library(ggridges)

# ggridges is not a part of the tudyverse libraries, it is a separate package
# that must be installed and loaded. While, dplyr and ggplot2 will work by import
# of library(tidyverse).

# In this example, "data" is a data frame or tibble in long format with variables
# (columns) "sample" and "expression"

ggplot(data, aes(y = sample, x = expression, fill = sample)) +
    geom_density_ridges2(scale = 0.95, 
                          jittered_points = TRUE,
                          position = position_points_jitter(width = 0.05, height = 0),
                          point_shape = '|', 
                          point_size = 3, 
                          point_alpha = 0.5, 
                          alpha = 1) +
    stat_density_ridges(quantile_lines = TRUE, scale = 0.95) +
    theme_bw(base_size = 24) +
    scale_x_continuous(limits = c(0, 14)) +
    scale_y_discrete(expand = expand_scale(add = c(0.2, 0.8))) +
    scale_fill_manual(values = rev(c('#E69F00', '#56B4E9', '#CC79A7', '#D55E00', '#0072B2', '#009E73'))) +
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_text(color = 'black'),
          axis.text.x = element_text(color = 'black'),
          legend.title = element_blank(),
          legend.position = 'none') +
    xlab('Expression') +
    ggsave(str_c(out_dir, 'out_filename.svg'), width = 10, height = 8)
