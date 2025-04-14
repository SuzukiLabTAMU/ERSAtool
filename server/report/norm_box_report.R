#### NORMALIZED BOX PLOT (REPORT VERSION) ####

norm_box_report <- function(raw_data, meta_data, design_columns) {
  
  tryCatch({
    raw_data <- as.data.frame(raw_data)
    meta_data <- as.data.frame(meta_data)
    
    # Validate design columns
    if (length(design_columns) == 0 || !all(design_columns %in% colnames(meta_data))) {
      stop("Please provide valid design column(s) for grouping.")
    }
    
    # Create combined condition column
    meta_data$Combined_Condition <- apply(meta_data[, design_columns, drop = FALSE], 1, paste, collapse = "_")
    meta_data$Combined_Condition <- as.factor(meta_data$Combined_Condition)
    meta_data$Label <- meta_data$Combined_Condition  # for x-axis
    
    # Check sample name consistency
    if (!all(colnames(raw_data) %in% rownames(meta_data))) {
      stop("Column names of raw counts and row names of metadata do not match!")
    }
    
    # Create DESeq2 object
    dds <- DESeqDataSetFromMatrix(
      countData = round(as.matrix(raw_data)),
      colData = meta_data,
      design = ~ Combined_Condition
    )
    
    dds <- dds[rowSums(counts(dds)) > 10, ]
    dds <- estimateSizeFactors(dds)
    norm_counts <- counts(dds, normalized = TRUE)
    pseudoCount <- log2(norm_counts + 1)
    
    # Melt normalized count matrix
    df <- reshape2::melt(pseudoCount, varnames = c("Gene", "Sample"), value.name = "Log2_Norm_Counts")
    
    # Merge metadata
    merged_df <- merge(df, meta_data, by.x = "Sample", by.y = "row.names", all.x = TRUE)
    merged_df$Condition <- merged_df$Combined_Condition
    merged_df$Label <- merged_df$Combined_Condition
    
    unique_groups <- unique(merged_df$Condition)
    num_groups <- length(unique_groups)
    color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(num_groups)
    
    ggplot(merged_df, aes(x = Sample, y = Log2_Norm_Counts, fill = Condition)) +
      geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
      scale_fill_manual(values = color_palette) +
      guides(fill = guide_legend(override.aes = list(shape = 22, size = 5))) +
      xlab("Samples") +
      ylab(expression(log[2](Normalized~Counts~+~1))) +
      ggtitle("Normalized Box-and-Whisker Plot\nNormalized Counts Across Groups") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(size = 13),
        axis.title = element_text(size = 14),
        legend.position = "bottom"
      )
    
  }, error = function(e) {
    showNotification(paste("Error generating normalized boxplot:", e$message), type = "error")
    plot.new()
    text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
  })
}
