#!/usr/bin/env Rscript

# Author: Jeffrey Grover
# Purpose: Volcano PlotteR is an R script to generate volcano plots
# Created: 2019-04-19
# Usage: Load the results output from DESeq2

library(dplyr)
library(ggplot2)
library(argparse)

# Define the function here

volcplot <- function(data, sig, fc, prefix, format) {

  # Set the fold-change thresholds

  neg_log2fc <- -log2(fc)
  pos_log2fc <- log2(fc)

  # Make a dataset for plotting, add the status as a new column

  plot_ready_data <- na.omit(data) %>% mutate(log2fc_threshold = ifelse((log2FoldChange >= pos_log2fc) & (padj <= sig), 'up', ifelse((log2FoldChange <= neg_log2fc) & (padj <= sig), 'down', 'unchanged')))

  # Get the number of up, down, and unchanged genes

  up_genes <- plot_ready_data %>% filter(log2fc_threshold == 'up') %>% nrow()
  down_genes <- plot_ready_data %>% filter(log2fc_threshold == 'down') %>% nrow()
  unchanged_genes <- plot_ready_data %>% filter(log2fc_threshold == 'unchanged') %>% nrow()

  # Make the plot, this is a WIP to generalize

  ggplot(plot_ready_data) +
    geom_point(alpha = 0.4, size = 1.5) +
    aes(x=log2FoldChange, y = -log10(padj), color = log2fc_threshold) +
    geom_vline(xintercept = c(neg_log2fc, pos_log2fc), linetype = 'dotted') +
    geom_hline(yintercept = -log10(sig), linetype = 'dotted') +
    scale_x_continuous('log2(FC)', limits = c(-12, 12), breaks = seq(from = -12, to = 12, by = 4)) +
    scale_color_manual(values = c('down' = 'dodgerblue1', 'unchanged' = 'gray', 'up' = 'firebrick1'), labels = c(str_c('Down: ', down_genes), str_c('Unchanged: ', unchanged_genes), str_c('Up: ', up_genes))) +
    labs(color = str_c(fc, '-fold, padj ≤', sig)) +
    theme(aspect.ratio = 1) +
    ggsave(str_c(prefix, '_volcano_fc_', fc, '_padj_', sig, '.', format))
}

# Attempt to write a command line parser in R

parser <- ArgumentParser(description='Make a volcano plot from DESeq2 results. In other words, a table with gene IDs, log2FoldChange, and padj columns.')
parser$add_argument('-s', '--sig',
                    type='double',
                    default=0.05,
                    help='The BH-adjusted p-value to use for a significance threshold',
                    metavar='padj')
format_group <- parser$add_mutually_exclusive_group()
format_group$add_argument('--png', action='store_true')
format_group$add_argument('--svg', action='store_true')

args <- parser$parse_args()

# Run the function, make the plot

volcplot(file, sig, fc, prefix, format)
