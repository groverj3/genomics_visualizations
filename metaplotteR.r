#!/usr/bin/env Rscript

# Author: Jeffrey Grover
# Purpose: MetaplotteR creates metaplots of data over genomic features
# Created: 2019-06-26
# Usage: Reads the table from deeptools' metaplot and makes a prettier one

library(ggplot2)
library(tidyr)


# Define functions

read_deeptools_table <- function(file) {

  n <- max(count.fields(file, sep = '\t'), na.rm = TRUE)
  x <- readLines(file)

  .splitvar <- function(x, sep, n) {
    var <- unlist(strsplit(x, split = sep))
    length(var) <- n
    return(var)
  }

  x <- do.call(cbind, lapply(x, .splitvar, sep = '\t', n = n))
  x <- apply(x, 1, paste, collapse = '\t')
  plot_table <- na.omit(read.csv(text = x, sep = '\t')[-1, ])  # Remove first row

  return(plot_table)
}


table_to_long <- function(plot_table) {

  long_table <- gather(plot_table, 'sample', 'score', bin.labels, bins)
  return(long_table)
 }


metaplot <- function(long_table, start_label, end_label, y_axis_label, span,
                     smooth, line, aspect, colors) {

  start_bin <- subset(long_table, bin.labels == start_label)$bins
  end_bin <- subset(long_table, bin.labels == end_label)$bins

  plot <- ggplot(long_table, aes(x = bins, y = as.numeric(score), color = sample))
  if (smooth == TRUE) plot <- plot + geom_smooth(method = 'loess',
                                                 span = span,
                                                 se = FALSE)
  if (line == TRUE) plot <- plot + geom_line()
  if (colors != 'ggplot') plot <- plot + scale_color_manual(values = unlist(strsplit(colors, ',')))
  plot <- plot +
    scale_x_continuous(breaks = long_table$bins,
                       labels = long_table$bin.labels) +
    geom_vline(xintercept = c(start_bin, end_bin), linetype = 'dotted') +
    ylab(y_axis_label) +
    xlab('Position') +
    theme_bw(base_size = 22) +
    theme(legend.title = element_blank(),
          legend.position = 'bottom',
          legend.direction = 'horizontal',
          legend.margin = margin(0,0,0,0),
          legend.box.margin = margin(-10,-10,-10,-10),
          axis.text = element_text(color = 'black'),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          aspect.ratio = aspect)
}