#### PRE-NORMALIZED BOX PLOT (REPORT VERSION) ####

pre_norm_box_report <- function(raw_data, meta_data, design_columns) {
  
  tryCatch({
    raw_data <- as.data.frame(raw_data)
    meta_data <- as.data.frame(meta_data)
    
    # Use design columns passed in (like input$design_columns)
    if (length(design_columns) == 0 || !all(design_columns %in% colnames(meta_data))) {
      stop("Please provide valid design column(s) for grouping.")
    }
    
    # Create Combined_Condition
    meta_data$Combined_Condition <- apply(meta_data[, design_columns, drop = FALSE], 1, paste, collapse = "_")
    
    # Match sample names
    if (!all(colnames(raw_data) %in% rownames(meta_data))) {
      stop("Column names of raw counts and row names of metadata do not match!")
    }
    
    # Reshape + log transform
    df <- reshape2::melt(raw_data, variable.name = "Sample", value.name = "Raw_Counts")
    df$Log2_Transformed_Counts <- log2(df$Raw_Counts + 1)
    
    merged_df <- merge(df, meta_data, by.x = "Sample", by.y = "row.names", all.x = TRUE)
    merged_df$Condition <- merged_df$Combined_Condition
    
    unique_groups <- unique(merged_df$Condition)
    num_groups <- length(unique_groups)
    color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(num_groups)
    
    # Grouped by Condition (like your example screenshot)
    ggplot(merged_df, aes(x = Condition, y = Log2_Transformed_Counts, fill = Condition)) +
      geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
      scale_fill_manual(values = color_palette) +
      xlab("Combined Condition") +
      ylab("log2(Raw_Counts + 1)") +
      ggtitle("Box-and-Whisker Plot\nLog2 Transformed Counts by Group") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 13),
        axis.title = element_text(size = 14),
        legend.position = "none"
      )
    
  }, error = function(e) {
    showNotification(paste("Error generating boxplot:", e$message), type = "error")
    plot.new()
    text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
  })
}
