#### ENHANCED VOLCANO PLOT ####

volcano_report <- function(raw_data, meta_data, comparison , logfc_cutoff, adjp_cutoff, species){
  raw_data <- as.data.frame(raw_data)
  meta_data <- as.data.frame(meta_data)
  
  tryCatch({
    comparison_split <- unlist(strsplit(comparison, " vs "))
    
    if (length(comparison_split) != 2) {
      stop("Invalid comparison format. Please select two different conditions.")
    }
    
    condition_1 <- comparison_split[1]
    condition_2 <- comparison_split[2]
    
    condition_cols <- grep("^Condition(_[0-9]+)?$", colnames(meta_data), value = TRUE)
    
    if (length(condition_cols) == 0) {
      stop("Metadata is missing 'Condition' columns. Please check your file.")
    }
    
    meta_data$Combined_Condition <- apply(meta_data[, condition_cols, drop = FALSE], 1, paste, collapse = "_")
    
    meta_data$Combined_Condition <- as.factor(meta_data$Combined_Condition)
    
    if (!(condition_1 %in% meta_data$Combined_Condition) || !(condition_2 %in% meta_data$Combined_Condition)) {
      stop("Selected conditions are not found in metadata. Please check your input.")
    }
    
    if (!all(colnames(raw_data) %in% rownames(meta_data))) {
      stop("Column names of raw counts and row names of metadata do not match!")
    }
    
    dds <- DESeqDataSetFromMatrix(
      countData = round(as.matrix(raw_data)),
      colData = meta_data,
      design = ~ Combined_Condition
    )
    
    dds <- dds[rowSums(counts(dds)) > 10, ]
    
    dds <- DESeq(dds)
    
    res <- results(dds, contrast = c("Combined_Condition", condition_1, condition_2), alpha = 0.05) %>%
      as.data.frame() %>%
      na.omit()
    
    if (nrow(res) == 0) {
      stop("No significant DEG data available for the selected comparison.")
    }
    
    selected_db <- if (species == "org.Mm.eg.db") org.Mm.eg.db else org.Hs.eg.db
    
    has_ensembl_ids <- any(grepl("^ENSMUSG|^ENSG", rownames(res)))  # Mouse (ENSMUSG) or Human (ENSG)
    
    if (has_ensembl_ids) {
      rownames(res) <- gsub("\\..*", "", rownames(res))  # Removes ".x" from ENSG00000123456.2
      
      res$Symbol <- mapIds(
        selected_db,
        keys = rownames(res),
        column = "SYMBOL",
        keytype = "ENSEMBL",
        multiVals = "first"
      )
      
      res$Symbol[is.na(res$Symbol)] <- rownames(res)
      res$Symbol <- make.unique(res$Symbol, sep = "_dup")
    } else {
      res$Symbol <- rownames(res)
    }
    
    required_cols <- c("log2FoldChange", "padj", "Symbol")
    if (!all(required_cols %in% colnames(res))) {
      stop("Required columns (log2FoldChange, padj, Symbol) are missing from the data.")
    }
    
    EnhancedVolcano(
      res,
      lab = res$Symbol,  
      x = 'log2FoldChange',
      y = 'padj',
      pCutoff = adjp_cutoff,
      FCcutoff = logfc_cutoff,
      title = paste("Enhanced Volcano Plot:", comparison),
      subtitle = "Differential Expression",
      caption = paste('FC cutoff:', logfc_cutoff, '; adj p-value cutoff:', adjp_cutoff),
      legendPosition = "right",
      legendLabSize = 14,
      col = c('grey30', 'forestgreen', 'royalblue', 'red2')
    )
    
  }, error = function(e) {
    stop("Error generating Enhanced Volcano Plot: ", e$message)
  })
}
