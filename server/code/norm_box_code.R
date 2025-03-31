### NORMALIZED BOX & WHISKER CODE ####

observeEvent(input$toggle_boxplot_code_norm, {
  output$boxplot_code_norm <- renderUI({
    if (input$toggle_boxplot_code_norm %% 2 == 1) {
      aceEditor(
        outputId = "boxplot_code_norm",
        mode = "r",
        theme = "solarized_light",
        readOnly = TRUE,
        height = "250px",
        value = "## Box-and-Whisker Plot Logic

  # Create DESeq2 dataset
  dds <- DESeqDataSetFromMatrix(
    countData = round(as.matrix(raw_data)),
    colData = meta_data,
    design = ~ Condition
  )

  # Filter low-count genes
  dds <- dds[rowSums(counts(dds)) > 10, ]

  # Normalize counts
  dds <- estimateSizeFactors(dds)
  norm_counts <- counts(dds, normalized = TRUE)
  pseudoCount <- log2(norm_counts + 1)

  # Convert data to long format for ggplot
  df <- reshape2::melt(pseudoCount)

  # Merge metadata for visualization
  merged_df <- merge(
    df,
    meta_data,
    by.x = 'Var2',
    by.y = 'row.names',
    all.x = TRUE
  )
 
  # Generate Boxplot
  ggplot(merged_df, aes(x = Var2, y = value, fill = Condition)) +
    geom_boxplot(outlier.color = 'red', outlier.shape = 16, outlier.size = 2) +
    xlab('Samples') +
    ylab(expression(log[2](Normalized~Counts~+~1))) +
    ggtitle('Normalized Counts Across Samples') +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = 'bottom'
    )"
      )
    }
  })
})
