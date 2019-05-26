# Author: Jeffrey Grover
# Purpose: A collection of useful settings for ggplot2
# Created: 2019-04-19
# Usage: Load the libraries specified and add to ggplot2 plots

library(ggplot2)
library(RColorBrewer)

# The accent palette is pretty nice with a little reordering to taste

color_brewer_palette <- rev(brewer.pal(6, 'Accent'))[c(2, 6, 3:5, 1)]

# This palette is good, I like it better in reverse though
# https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
# https://jfly.uni-koeln.de/color/

colorblind_palette<- rev(c('#999999', '#E69F00', '#56B4E9', '#009E73', '#F0E442',
                           '#0072B2', '#D55E00', '#CC79A7'))

# Put the following in a jupyter notebook to change the plot size there

options(repr.plot.width= 8, repr.plot.height = 8)

# Add the following options to your ggplot2 command match the size with above

  theme_bw(base_size = 24) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        axis.text = element_text(color = 'black'),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(-10,-10,-10,-10),
        legend.spacing.x = unit(0.2, 'cm')) +
  ggsave('filename.svg', width = 8, height = 8)
