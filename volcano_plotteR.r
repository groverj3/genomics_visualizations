# Author: Jeffrey Grover
# Purpose: Volcano PlotteR is an R script to generate volcano plots
# Created: 2019-04-19
# Usage: Load the results output from DESeq2 as a data frame, the input data
# must have columns for log2FoldChange and padj

library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggrepel)  # For labeling
library(DescTools)  # For rounding
library(rlang)  # For injecting the label column programmatically

volcplot <- function(
  data,
  padj_threshold = 0.05,
  foldchange_threshold = 1,
  plot_title = "Volcano Plot",
  plot_subtitle = NULL,
  label_column = "hgnc_symbol",
  genelist_vector = NULL,
  genelist_filter = FALSE,
  genelist_label = FALSE
) {

  # Set the fold-change thresholds
  neg_log2fc <- -log2(fc)
  pos_log2fc <- log2(fc)

  # Make a dataset for plotting, add the status as a new column
  plot_ready_data <- data %>%
    mutate_at('padj', ~replace(.x, is.na(.x), 1)) %>%
    mutate_at('log2FoldChange', ~replace(.x, is.na(.x), 0)) %>%
    mutate(
      log2fc_threshold = ifelse(log2FoldChange >= pos_log2fc & padj <= padj_threshold, 'up',
                         ifelse(log2FoldChange <= neg_log2fc & padj <= padj_threshold, 'down', 'ns')
        )
    ) %>%
    mutate(label_column = replace_na(!!sym(label_column), "none"))

  if (genelist_filter) {
    plot_ready_data <- plot_ready_data %>% filter(hgnc_symbol %in% genelist_vector)
  }

  if(!is.null(genelist_vector)) {
    plot_ready_data <- plot_ready_data |>
      mutate(
        !!sym(label_column) := (
          ifelse(
            !!sym(label_column) %in% genelist_vector & padj < padj_threshold & log2fc_threshold != "ns",
            !!sym(label_column),
            ""
          )
        )
      )
  }

  # Get the number of up, down, and unchanged genes
  up_genes <- plot_ready_data %>% filter(log2fc_threshold == 'up') %>% NROW()()
  down_genes <- plot_ready_data %>% filter(log2fc_threshold == 'down') %>% NROW()()
  unchanged_genes <- plot_ready_data %>% filter(log2fc_threshold == 'ns') %>% NROW()

  # Make the labels for the legend
  legend_labels <- c(
      str_c('Up: ', up_genes),
      str_c('NS: ', unchanged_genes),
      str_c('Down: ', down_genes)
  )

  # Set the x axis limits, rounded to the next even number
  x_axis_limits <- DescTools::RoundTo(
    max(abs(plot_ready_data$log2FoldChange)),
    2,
    ceiling
  )

  # Set the plot colors
  plot_colors <- c(
      'up' = 'firebrick1',
      'ns' = 'gray',
      'down' = 'dodgerblue1'
  )


  # Make the plot, these options are a reasonable starting point
  plot <- ggplot(plot_ready_data) +
    geom_point(
      alpha = 0.25,
      size = 1.5
    ) +
    aes(
      x = log2FoldChange,
      y = -log10(padj),
      color = log2fc_threshold,
      label = !!sym(label_column)
    ) +
    geom_vline(
      xintercept = c(neg_log2fc, pos_log2fc),
      linetype = 'dashed'
    ) +
    geom_hline(
      yintercept = -log10(padj_threshold),
      linetype = 'dashed'
    ) +
    scale_x_continuous(
      'log2(FC)',
      limits = c(-x_axis_limits, x_axis_limits)
    ) +
    scale_color_manual(
      values = plot_colors,
      labels = legend_labels
      ) +
    labs(
      color = str_c(fc, '-fold, padj ≤', padj_threshold),
      title = plot_title,
      subtitle = plot_subtitle
    ) +
    theme_bw(base_size = 24) +
    theme(
      aspect.ratio = 1,
      axis.text = element_text(color = 'black'),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),  # Reduces dead area around legend
      legend.spacing.x = unit(0.2, 'cm')
    )

  # Add gene labels if needed
  if (genelist_label && !is.null(genelist_vector)) {
    plot <- plot +
      geom_label_repel(
        size = 6,
        force = 0.1,
        max.overlaps = 100000,
        max.iter = 1000000,
        nudge_x = 1,
        segment.color = "black",
        min.segment.length = 0,
        show.legend = FALSE
      )
  }
    plot
}