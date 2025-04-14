### DOWNLOAD PCA PLOT ####

output$download_pca_plot <- downloadHandler(
  filename = function() {
    paste("PCA_plot_", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    png(file, width = 1200, height = 800, res = 150)
    
    tryCatch({
      raw_data <- as.data.frame(raw_counts())
      meta_data <- as.data.frame(metadata())
      design_cols <- input$design_columns
      
      # Validate design columns
      if (length(design_cols) == 0 || !all(design_cols %in% colnames(meta_data))) {
        stop("Please select valid column(s) from design formula for PCA.")
      }
      
      meta_data$Combined_Condition <- apply(meta_data[, design_cols, drop = FALSE], 1, paste, collapse = "_")
      meta_data$Combined_Condition <- as.factor(meta_data$Combined_Condition)
      
      if (!all(colnames(raw_data) %in% rownames(meta_data))) {
        stop("Column names of raw counts and row names of metadata do not match!")
      }
      
      dds <- dds_data()
      rlog_data <- rlog(dds, blind = TRUE)
      
      pc <- prcomp(t(assay(rlog_data)))
      pc_data <- as.data.frame(pc$x[, 1:2])
      colnames(pc_data) <- c("PC1", "PC2")
      pc_data$Sample <- rownames(pc_data)
      
      meta_data$Sample <- rownames(meta_data)
      merged_df <- merge(pc_data, meta_data, by = "Sample", all.x = TRUE)
      merged_df$Condition <- merged_df$Combined_Condition
      
      unique_groups <- unique(merged_df$Condition)
      num_groups <- length(unique_groups)
      color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(num_groups)
      
      p <- ggplot(merged_df, aes(x = PC1, y = PC2, color = Condition)) +
        geom_point(size = 5) +
        scale_color_manual(values = color_palette) +
        labs(title = "PCA Plot (Grouped by Combined Condition)", x = "PC1", y = "PC2") +
        theme_minimal()
      
      print(p)
      
    }, error = function(e) {
      showNotification(paste("Error in PCA Plot:", e$message), type = "error")
    })
    
    dev.off()
  }
)
