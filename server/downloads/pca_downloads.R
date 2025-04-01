### DOWNLOAD PCA PLOT ####

output$download_pca_plot <- downloadHandler(
  filename = function() {
    paste("PCA_plot_", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    
    tryCatch({
      meta_data <- metadata()
      
      condition_cols <- grep("^Condition(_[0-9]+)?$", colnames(meta_data), value = TRUE)
      
      if (length(condition_cols) == 0) {
        stop("Metadata is missing 'Condition' columns. Please check your file.")
      }
      
      meta_data$Combined_Condition <- apply(meta_data[, condition_cols, drop = FALSE], 1, paste, collapse = "_")
      
      raw_data <- raw_counts()
      if (!all(colnames(raw_data) %in% rownames(meta_data))) {
        stop("Column names of raw counts and row names of metadata do not match!")
      }
      
      dds <- dds_data()
      rlog_data <- rlog(dds, blind = TRUE)
      
      pc <- prcomp(t(assay(rlog_data)))
      pc_data <- as.data.frame(pc$x[, 1:2])
      colnames(pc_data) <- c("PC1", "PC2")
      
      pc_data$Condition <- meta_data$Combined_Condition
      unique_groups <- unique(pc_data$Condition)
      num_groups <- length(unique_groups)
      color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(num_groups)
      
      p <- ggplot(pc_data, aes(x = PC1, y = PC2, color = Condition)) +
        geom_point(size = 5) +
        scale_color_manual(values = color_palette) +
        labs(title = "PCA Plot (Grouped by Combined Condition)", x = "PC1", y = "PC2") +
        theme_minimal()
      
      ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
      
    }, error = function(e) {
      showNotification(paste("Error in PCA Plot:", e$message), type = "error")
    })
    
    dev.off()
  }
)
