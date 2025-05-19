### DOWNLOAD NORMALIZED BOX & WHISKER PLOT ####

output$download_boxplot_norm <- downloadHandler(
  filename = function() {
    paste("normalized_boxplot_", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    png(file, width = 1200, height = 800, res = 150)
    
    tryCatch({
      raw_data <- as.data.frame(raw_counts())
      meta_data <- as.data.frame(metadata())
      design_cols <- input$design_columns
      
      # Validate selected design columns
      if (length(design_cols) == 0 || !all(design_cols %in% colnames(meta_data))) {
        stop("Please select valid column(s) for DESeq2 design.")
      }
      
      # Create Combined Condition
      meta_data$Combined_Condition <- apply(meta_data[, design_cols, drop = FALSE], 1, paste, collapse = "_")
      
      if (!all(colnames(raw_data) %in% rownames(meta_data))) {
        stop("Column names of raw counts and row names of metadata do not match!")
      }
      
      # Get normalized counts
      dds <- dds_data()
      norm_counts <- counts(dds, normalized = TRUE)
      pseudoCount <- log2(norm_counts + 1)
      
      # Melt + merge
      df <- reshape2::melt(pseudoCount, varnames = c("Gene", "Sample"), value.name = "Log2_Norm_Counts")
      merged_df <- merge(df, meta_data, by.x = "Sample", by.y = "row.names", all.x = TRUE)
      merged_df$Condition <- merged_df$Combined_Condition
      
      # Colors
      unique_groups <- unique(merged_df$Condition)
      num_groups <- length(unique_groups)
      color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(num_groups)
      
      # Plot
      p <- ggplot(merged_df, aes(x = Sample, y = Log2_Norm_Counts, fill = Condition)) +
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
      
      print(p)
      
    }, error = function(e) {
      showNotification(paste("Error generating boxplot:", e$message), type = "error")
    })
    
    dev.off()
  }
)