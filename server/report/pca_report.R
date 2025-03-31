#### PCA PLOT ####

pca_report <- function(raw_data, meta_data){
  
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
    rlog_data <- rlog(dds, blind = TRUE)
    
    # Perform PCA analysis
    pc <- prcomp(t(assay(rlog_data)))
    pc_data <- as.data.frame(pc$x[, 1:2])
    colnames(pc_data) <- c("PC1", "PC2")
    
    # Assign the new combined condition column
    pc_data$Condition <- meta_data$Combined_Condition
    
    unique_groups <- unique(pc_data$Condition)
    num_groups <- length(unique_groups)
    color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(num_groups)
    
    ggplot(pc_data, aes(x = PC1, y = PC2, color = Condition)) +
      geom_point(size = 5) +
      scale_color_manual(values = color_palette) +
      labs(title = "PCA Plot (Grouped by Combined Condition)", x = "PC1", y = "PC2") +
      theme_minimal()
    
  }, error = function(e) {
    stop("Error generating PCA plot: ", e$message)
  })
}
