output$download_boxplot <- downloadHandler(
  filename = function() {
    paste0("pre_normalized_boxplot_", Sys.Date(), ".", input$pre_norm_plot_format)
  },
  content = function(file) {
    tryCatch({
      raw_data <- as.data.frame(raw_counts())
      meta_data <- as.data.frame(metadata())
      design_cols <- input$design_columns
      
      if (length(design_cols) == 0 || !all(design_cols %in% colnames(meta_data))) {
        stop("Please select valid column(s) from design formula for plotting.")
      }
      
      meta_data$Combined_Condition <- apply(meta_data[, design_cols, drop = FALSE], 1, paste, collapse = "_")
      
      if (!all(colnames(raw_data) %in% rownames(meta_data))) {
        stop("Column names of raw counts and row names of metadata do not match!")
      }
      
      df <- reshape2::melt(raw_data, variable.name = "Sample", value.name = "Raw_Counts")
      df$Log2_Transformed_Counts <- log2(df$Raw_Counts + 1)
      
      meta_data$Sample <- rownames(meta_data)
      merged_df <- merge(df, meta_data, by = "Sample", all.x = TRUE)
      merged_df$Condition <- merged_df$Combined_Condition
      
      unique_groups <- unique(merged_df$Condition)
      num_groups <- length(unique_groups)
      color_palette <- viridis::viridis(num_groups)
      
      sample_count <- length(unique(merged_df$Sample))
      plot_width <- max(8, sample_count * 0.3)  # Auto-scale width
      plot_height <- 6  # Good visual height
      
      p <- ggplot(merged_df, aes(x = Sample, y = Log2_Transformed_Counts, fill = Condition)) +
        geom_boxplot(outlier.color = "black", outlier.size = 0.8, outlier.shape = 21) +
        scale_fill_manual(values = color_palette) +
        xlab("Samples") +
        ylab(expression(log[2]*"(Raw Counts + 1)")) +
        ggtitle("Log2 Transformed Counts per Sample") +
        theme_pubready() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.title = element_text(size = 10, face = "bold"),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
        ) +
        guides(fill = guide_legend(title = "Condition", title.position = "top", title.hjust = 0.5))
      
      ggsave(
        filename = file,
        plot = p,
        device = input$pre_norm_plot_format,
        dpi = 300,
        width = plot_width,
        height = plot_height,
        units = "in",
        bg = "white"
      )
      
    }, error = function(e) {
      showNotification(paste("Error generating boxplot:", e$message), type = "error")
    })
  }
)

# Reusable publication theme
theme_pubready <- function(base_size = 12, base_family = "Helvetica") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      legend.key = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(fill = NA, color = "black", size = 0.5),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}
