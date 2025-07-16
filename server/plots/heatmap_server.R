## HEATMAP ####

output$heatmap_plot <- renderPlot({
  heatmap_start_time <- Sys.time()
  req(raw_counts(), metadata(), dds_data())
  tryCatch({
    raw_counts_matrix <- round(as.matrix(raw_counts()))
    
    if (any(raw_counts_matrix < 0)) {
      stop("Raw counts contain negative values. Please check your input file.")
    }
    if (!all(raw_counts_matrix == floor(raw_counts_matrix))) {
      stop("Raw counts contain non-integer values. Please check your input file.")
    }
    
    if (!all(colnames(raw_counts_matrix) %in% rownames(metadata()))) {
      stop("Column names of raw counts and row names of metadata do not match!")
    }
    
    dds <- dds_data()
    rlog_data <- rlog(dds, blind = TRUE)
    
    dist_matrix <- dist(t(assay(rlog_data)))
    mat <- as.matrix(dist_matrix)
    rownames(mat) <- colnames(mat) <- colnames(raw_counts_matrix)
    
    ComplexHeatmap::Heatmap(
      mat,
      name = "Distance",
      col = colorRampPalette(rev(brewer.pal(9, "Reds")))(255),
      column_title = "Sample Distance Heatmap",
      cluster_rows = TRUE,
      cluster_columns = TRUE
    )
  }, error = function(e) {
    showNotification(paste("Error in Heatmap:", e$message), type = "error")
  }, finally = {
    heatmap_end_time <- Sys.time()
    duration <- round(difftime(heatmap_end_time, heatmap_start_time, units = "secs"), 2)
    message(paste("Heatmap Plot:", duration, "seconds"))
    # Optional: log to file
    # cat(Sys.time(), "- Heatmap Plot Duration:", duration, "seconds\n", file = "plot_timings.log", append = TRUE)
  })
})