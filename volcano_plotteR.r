# Author: Jeffrey Grover
# Purpose: Volcano PlotteR is an R script to generate volcano plots
# Created: 2019-04-19
# Usage: Load the results output from DESeq2 as a data frame, the
# input data must have columns for log2FoldChange and padj

library(dplyr)
library(ggplot2)

# Load this function and the above libraries

volcplot <- function(data, sig, fc, out_prefix, format) {

  # Set the fold-change thresholds

  neg_log2fc <- -log2(fc)
  pos_log2fc <- log2(fc)

  # Make a dataset for plotting, add the status as a new column

  plot_ready_data <- na.omit(data) %>%
    mutate(log2fc_threshold = ifelse((log2FoldChange >= pos_log2fc) & (padj <= sig), 'up',
                              ifelse((log2FoldChange <= neg_log2fc) & (padj <= sig), 'down',
                                     'unchanged')
                                    )
          )

  # Get the number of up, down, and unchanged genes

  up_genes <- plot_ready_data %>% filter(log2fc_threshold == 'up') %>% nrow()
  down_genes <- plot_ready_data %>% filter(log2fc_threshold == 'down') %>% nrow()
  unchanged_genes <- plot_ready_data %>% filter(log2fc_threshold == 'unchanged') %>% nrow()

  # Make the labels for the legend

  legend_labels <- c(str_c('Down: ', down_genes), str_c('Unchanged: ', unchanged_genes),
                     str_c('Up: ', up_genes)
                    )

  # Set the x axis limits, rounded to the next even number

  x_axis_limits <- RoundTo(
    log2(max(log2FoldChange)), 2, ceiling
  )
  x_axis_limits <- c(-x_axis_limits, x_axis_limits)

  # Set the plot colors

  plot_colors <- c('down' = 'dodgerblue1', 'unchanged' = 'gray', 'up' = 'firebrick1')

  # Make the plot, these options are a reasonable strting point

  ggplot(plot_ready_data) +
    geom_point(alpha = 0.4, size = 1.5) +
    aes(x=log2FoldChange, y = -log10(padj), color = log2fc_threshold) +
    geom_vline(xintercept = c(neg_log2fc, pos_log2fc), linetype = 'dotted') +
    geom_hline(yintercept = -log10(sig), linetype = 'dotted') +
    scale_x_continuous('log2(FC)', limits = x_axis_limits) +
    scale_color_manual(values = plot_colors, labels = legend_labels) +
    labs(color = str_c(fc, '-fold, padj ≤', sig)) +
    theme(aspect.ratio = 1) +
    ggsave(str_c(out_prefix, '_volcano_fc_', fc, '_padj_', sig, '.', format))
}
