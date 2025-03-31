## NORMALIZATION ####

dds_data <- reactiveVal(NULL)

observeEvent(list(raw_counts(), metadata()), {
  req(raw_counts(), metadata())
  
  tryCatch({
    meta_data <- metadata()
    
    condition_cols <- grep("^Condition(_[0-9]+)?$", colnames(meta_data), value = TRUE)
    
    if (length(condition_cols) == 0) {
      showNotification("No condition columns found in metadata!", type = "error")
      stop("No condition columns detected in metadata.")
    }
    
    for (col in condition_cols) {
      meta_data[[col]] <- as.factor(meta_data[[col]])
    }
    
    design_formula <- as.formula("~ Combined_Condition")
    count_matrix <- as.matrix(raw_counts())
    
    if (any(is.na(count_matrix))) {
      stop("Raw counts contain NA values!")
    }
    
    dds <- DESeqDataSetFromMatrix(
      countData = round(count_matrix),
      colData = meta_data,
      design = design_formula
    )
    
    dds <- dds[rowSums(counts(dds)) > 10, ]
    dds <- DESeq(dds)  
    dds_data(dds)  
    
    showNotification("Normalization completed successfully!", type = "message")
    
  }, error = function(e) {
    showNotification(paste("Error in DESeq2 processing:", e$message), type = "error")
  })
})