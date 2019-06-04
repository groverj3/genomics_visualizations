# Author: Jeffrey Grover
# Purpose: MetaplotteR creates metaplots of data over genomic features
# Created: 2019-05-21
# Usage: Load the data per bin into a data frame. This data can be in the form of
# coverage, percent methylation, etc. The input data frame should have columns
# for bin, value, and sample_id and there should be 60 bins with a value to plot

library(dplyr)
library(ggplot2)

# Load this function and the above libraries

metaplot <- function(data, window_size, x_labels, y_axis_label, out_prefix, format) {

  # For now the plot labels at 16 discrete intervals, so the x_labels vector must
  # also be 16 items in length. Position 6 corresponds to the start of the
  # feature, and 11 corresponds to the end of the feature.
 
  ggplot(data, aes(x = bin, y = value, group = genotype, color = genotype)) +
    geom_smooth(span = 0.2, se = FALSE) +
    scale_x_continuous(breaks = c(0,4,8,12,16,20,24,28,32,36,40,44,48,52,56,60),
                       labels = x_labels) +
    geom_vline(xintercept = c(20, 40), linetype = 'dotted') +
    ylab(y_axis_label) +
    xlab('Position') +
    theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggsave(str_c(out_prefix, '_metaplot_', window_size, 'nt_window.', format))
}
