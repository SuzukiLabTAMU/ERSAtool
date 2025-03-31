#### NORMALIZED BOX PLOT ####

norm_box_report <- function(raw_data, meta_data) {

  tryCatch({
    raw_data <- as.data.frame(raw_data)
    meta_data <- as.data.frame(meta_data)
    
    # Detect all condition columns dynamically
    condition_cols <- grep("^Condition(_[0-9]+)?$", colnames(meta_data), value = TRUE)
    
    if (length(condition_cols) == 0) {
      stop("Metadata is missing 'Condition' columns. Please check your file.")
    }
    
    # Create a combined condition column
    meta_data$Combined_Condition <- apply(meta_data[, condition_cols, drop = FALSE], 1, paste, collapse = "_")
    
    # Convert to factor
    meta_data$Combined_Condition <- as.factor(meta_data$Combined_Condition)
    
    # Ensure raw counts and metadata sample names match
    if (!all(colnames(raw_data) %in% rownames(meta_data))) {
      stop("Column names of raw counts and row names of metadata do not match!")
    }
    
    # Create DESeq2 dataset using Combined_Condition
    dds <- DESeqDataSetFromMatrix(
      countData = round(as.matrix(raw_data)),
      colData = meta_data,
      design = ~ Combined_Condition
    )
    
    # Filter out low-expressed genes
    dds <- dds[rowSums(counts(dds)) > 10, ]
    
    # Perform DESeq2 normalization
    dds <- estimateSizeFactors(dds)
    norm_counts <- counts(dds, normalized = TRUE)
    pseudoCount <- log2(norm_counts + 1)
    
    # Reshape normalized count data
    df <- reshape2::melt(pseudoCount)
    
    # Merge metadata
    merged_df <- merge(df, meta_data, by.x = "Var2", by.y = "row.names", all.x = TRUE)
    
    # Assign the new combined condition column
    merged_df$Condition <- merged_df$Combined_Condition
    
    unique_groups <- unique(merged_df$Condition)
    num_groups <- length(unique_groups)
    color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(num_groups)
    
    ggplot(merged_df, aes(x = Var2, y = value, fill = Condition)) +
      geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
      scale_fill_manual(values = color_palette) +
      xlab("Samples") +
      ylab(expression(log[2](Normalized~Counts~+~1))) +
      ggtitle("Normalized Counts Across Samples (Grouped by Combined Condition)") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
    
  }, error = function(e) {
    showNotification(paste("Error generating boxplot:", e$message), type = "error")
  })
}
