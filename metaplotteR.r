#!/usr/bin/env Rscript 

# Author: Jeffrey Grover
# Purpose: MetaplotteR creates metaplots of data over genomic features
# Created: 2019-06-24
# Usage: Reads the table from deeptools' metaplot and makes a prettier one

library(argparser, quietly=TRUE)
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))


# Subroutine functions

read_deeptools_table <- function(file, header=TRUE, sep='\t', ...) {

  n <- max(count.fields(file, sep = sep), na.rm = TRUE)
  x <- readLines(file)

  .splitvar <- function(x, sep, n) {
    var <- unlist(strsplit(x, split = sep))
    length(var) <- n
    return(var)
  }

  x <- do.call(cbind, lapply(x, .splitvar, sep = sep, n = n))
  x <- apply(x, 1, paste, collapse = sep) 
  plot_table <- read.csv(text = x, sep = sep, header = header, ...)
  plot_table <- plot_table[-1,]  # Remove first row

  return(plot_table)
}


table_to_long <- function(plot_table) {

  long_table <- gather(plot_table, 'sample', 'score', -bin.labels, -bins)
  return(long_table)
}


metaplot <- function(plot_table, start_label, end_label, y_axis_label, span
                     out_prefix, format) {

  start_bin <- filter(plot_table, bin.labels == start_label)$bins
  end_bin <- filter(plot_table, bin.labels == end_label)$bins

  ggplot(data) +
    aes(x = bins, y = score, group = sample, color = sample) +
    geom_smooth(span = span, se = FALSE) +
    scale_x_continuous(labels = bin.labels) +
    geom_vline(xintercept = c(start_bin, end_bin), linetype = 'dotted') +
    ylab(y_axis_label) +
    xlab('Position') +
    theme_bw(base_size = '24') +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggsave(paste0(out_prefix, '_metaplot.', format))
}


# Parse arguments

get_args <- function() {

  parser <- arg_parser(
    paste0('metaplotteR will read a table output from deeptools\' plotProfile ',
           'command, load it into R, and make a new metaplot with ggplot!')
  )
  parser <- add_argument(parser, c('-o', '--output_prefix'),
                       type='integer',
                       default='plot',
                       help='Prefix to add to output plot',
                       metavar='path/filename')
  parser <- add_argument(parser, c('-s', '--start_label'),
                       default='start',
                       help='Label for the start of the features, must match the table.')
  parser <- add_argument(parser, c('-e', '--end_label'),
                       default='end',
                       help='Label for the end of the features, must match the table.')
  parser <- add_argument(parser, c('-y', '--y_axis'),
                       default='coverage',
                       help='Label (units) for the y-axis')
  parser <- add_argument(parser, c('-n', '--span'),
                       type='numeric',
                       default=0.2,
                       help='Value for span in geom_smooth, smaller values are wigglier.')
  parser <- add_argument(parser, c('-f', '--format'),
                       default='png',
                       help='Image format. [png|svg|jpeg|pdf|bmp|tiff|eps|ps]')
  parser <- add_argument(parser, 'input_file',
                         help='Deeptools plotProfile table',
                         metavar='File')
  return(parse_args(parser))
}


# Main function entry point


main <- function(args=get_args()) {

  deeptools_table <- read_deeptools_table(args$input_file)

  deeptools_table <- table_to_long(deeptools_table)

  metaplot(deeptools_table, args$start_label, args$end_label, args$y_axis,
           args$span, args$out_prefix, args$format)
}

main()
