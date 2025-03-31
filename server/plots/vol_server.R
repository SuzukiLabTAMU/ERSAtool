## ENHANCED VOLCANO PLOT ####

observe({
  req(metadata())
  
  condition_cols <- grep("^Condition(_[0-9]+)?$", colnames(metadata()), value = TRUE)
  
  if (length(condition_cols) == 0) {
    showNotification("No valid 'Condition' columns found!", type = "error")
    return()
  }
  
  meta_data <- metadata()
  meta_data$Combined_Condition <- apply(meta_data[, condition_cols, drop = FALSE], 1, paste, collapse = "_")
  
  metadata(meta_data)
  
  conditions <- unique(meta_data$Combined_Condition)
  
  if (length(conditions) >= 2) {
    updateSelectInput(session, "condition_1", choices = conditions, selected = conditions[1])
    updateSelectInput(session, "condition_2", choices = conditions, selected = conditions[2])
  }
})

reactiveVolcanoData <- reactiveValues(plots = list(), selected_genes = NULL)

observeEvent(input$generate_volcano, {  
  req(dds_data(), metadata(), input$species)
  
  showNotification("Running Enhanced Volcano Plot... Please wait.", type = "message", duration = NULL, id = "EV_msg")
  
  if (is.null(input$condition_1) || is.null(input$condition_2) || input$condition_1 == input$condition_2) {
    showNotification("Please select two different conditions.", type = "error")
    return()
  }
  
  dds <- dds_data()
  design(dds) <- as.formula("~ Combined_Condition")  
  dds <- DESeq(dds)  
  
  comparisons <- list(c(input$condition_1, input$condition_2))
  volcano_plots <- list()
  significant_genes <- list()
  
  for (pair in comparisons) {
    res <- tryCatch({
      as.data.frame(results(dds, contrast = c("Combined_Condition", pair[1], pair[2]), alpha = 0.05)) %>%
        na.omit()
    }, error = function(e) { 
      showNotification(paste("Error in DESeq results:", e$message), type = "error")
      NULL 
    })
    
    # Only clean if rownames look like ENSEMBL IDs (e.g., ENSMUSG00000012345.1)
    if (any(grepl("^ENS.*\\..+", rownames(res)))) {
      clean_ids <- gsub("\\..*", "", rownames(res))
      rownames(res) <- make.unique(clean_ids)  # Ensure uniqueness
    } 
    
    selected_db <- get(input$species, envir = .GlobalEnv)
    
    is_ensg <- if (input$species == "org.Mm.eg.db") {
      any(grepl("^ENSMUSG", rownames(res)))  
    } else {
      any(grepl("^ENSG", rownames(res)))  
    }
    
    if (is_ensg) {
      res$Symbol <- mapIds(
        selected_db,
        keys = rownames(res),
        column = "SYMBOL",
        keytype = "ENSEMBL",
        multiVals = "first"
      )
    } else {
      res$Symbol <- rownames(res)
    }
    
    res$Symbol[is.na(res$Symbol)] <- rownames(res)
    res$Symbol <- make.unique(res$Symbol, sep = "_dup")
    res_df <- as.data.frame(res)
    
    # Function to remove duplicate gene entries, keeping the most significant one
    remove_duplicates <- function(df) {
      if ("Symbol" %in% colnames(df)) {
        df <- df[order(df$padj, decreasing = FALSE), ]  # Sort by smallest padj (most significant)
        df <- df[!duplicated(df$Symbol), ]  # Remove duplicates
      }
      return(df)
    }
    
    # Apply function to remove duplicate gene symbols before plotting
    res_df <- remove_duplicates(as.data.frame(res))
    
    significant <- res_df %>% filter(padj < input$adjp_cutoff, abs(log2FoldChange) > input$logfc_cutoff)
    significant_genes[[paste(pair[1], "vs", pair[2])]] <- significant
    
    volcano_plots[[paste(pair[1], "vs", pair[2])]] <- EnhancedVolcano(
      res_df,
      lab = res_df$Symbol,
      x = 'log2FoldChange',
      y = 'padj',
      pCutoff = input$adjp_cutoff,  
      FCcutoff = input$logfc_cutoff,  
      title = paste(pair[1], "vs", pair[2]),
      legendPosition = "right"
    )
  }
  
  reactiveVolcanoData$plots <- volcano_plots
  reactiveVolcanoData$selected_genes <- significant_genes
  updateSelectInput(session, "comparison_selector", choices = names(volcano_plots), selected = names(volcano_plots)[1])
  
  removeNotification("EV_msg")
})

output$volcano_plot <- renderPlot({
  req(reactiveVolcanoData$plots, input$comparison_selector)
  
  if (length(reactiveVolcanoData$plots) == 0) {
    plot.new()
  } else {
    reactiveVolcanoData$plots[[input$comparison_selector]]
  }
})
