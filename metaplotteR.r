#!/usr/bin/env Rscript

# Author: Jeffrey Grover
# Purpose: MetaplotteR creates metaplots of data over genomic features
# Created: 2019-06-26
# Usage: Reads the table from deeptools' metaplot and makes a prettier one

library(argparser, quietly=TRUE)
suppressMessages(library(ggplot2))
suppressMessages(library(tidyr))


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
  plot_table <- na.omit(read.csv(text = x, sep = '\t')[-1,])  # Remove first row

  return(plot_table)
}


table_to_long <- function(plot_table) {

  long_table <- gather(plot_table, 'sample', 'score', -bin.labels, -bins)
  return(long_table)
}


metaplot <- function(long_table, start_label, end_label, y_axis_label, span,
                     out_prefix, format, smooth, line, aspect, width, height,
                     colors) {

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
          aspect.ratio = aspect) +
    ggsave(paste0(out_prefix, '.', format),
           width = width,
           height = height)
}


# Parse arguments

get_args <- function() {

  parser <- arg_parser(
    paste0('metaplotteR will read a table output from deeptools\' plotProfile ',
           'command, load it into R, and make a new metaplot with ggplot!')
  )
  parser <- add_argument(parser, '--output_prefix',
                         default='metaplot',
                         help='Prefix to add to output plot')
  parser <- add_argument(parser, '--start_label',
                         default='start',
                         help='Label for the start of the features, must match the table.')
  parser <- add_argument(parser, '--end_label',
                         default='end',
                         help='Label for the end of the features, must match the table.')
  parser <- add_argument(parser, '--y_axis',
                         default='Coverage',
                         help='Label (units) for the y-axis')
  parser <- add_argument(parser, '--line',
                         flag=TRUE,
                         help='Generate a line plot with no smoothing.')
  parser <- add_argument(parser, '--smooth',
                         flag=TRUE,
                         help='Smooth the metaplot with geom_smooth')
  parser <- add_argument(parser, '--span',
                         type='numeric',
                         default=0.2,
                         help='Value for span in geom_smooth, smaller values are wigglier.')
  parser <- add_argument(parser, '--format',
                         default='png',
                         help='Image format. [png|svg|jpeg|pdf|bmp|tiff|eps|ps]')
  parser <- add_argument(parser, '--aspect',
                         type='numeric',
                         default=0.5,
                         help='Aspect ratio for the output plot')
  parser <- add_argument(parser, '--width',
                         default=7.0,
                         help='Width for the output plot')
  parser <- add_argument(parser, '--height',
                         default=5.0,
                         help='Height for the output plot.')
  parser <- add_argument(parser, '--colors',
                         default='ggplot',
                         help='Comma separated list of colors for samples on plot.')
  parser <- add_argument(parser, 'input_file',
                         help='Deeptools plotProfile table')
  return(parse_args(parser))
}


# Main function entry point


main <- function() {

  args <- get_args()

  deeptools_table <- read_deeptools_table(args$input_file)

  deeptools_table <- table_to_long(deeptools_table)

  metaplot(deeptools_table, args$start_label, args$end_label, args$y_axis,
           args$span, args$output_prefix, args$format, args$smooth, args$line,
           args$aspect, args$width, args$height, args$colors)
}

main()
