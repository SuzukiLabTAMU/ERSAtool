### PCA CODE ####

observeEvent(input$toggle_pca_code, {
  output$pca_code <- renderUI({
    if (input$toggle_pca_code %% 2 == 1) {
      aceEditor(
        outputId = "pca_code",
        mode = "r",
        theme = "solarized_light",
        readOnly = TRUE,
        height = "250px",
        value = "## PCA Plot Logic
  
  # Create a DESeq2 dataset as done in Normalized Box & Whisker Plot 
 
  # Normalize data
  dds <- estimateSizeFactors(dds)
  rlog_data <- rlog(dds, blind = TRUE)

  # Perform Principal Component Analysis (PCA)
  pc <- prcomp(t(assay(rlog_data)))
  pc_data <- as.data.frame(pc$x[, 1:2])
  colnames(pc_data) <- c('PC1', 'PC2')

  # Merge metadata for coloring
  pc_data$Condition <- metadata()$Condition

  # Generate PCA Plot
  ggplot(pc_data, aes(x = PC1, y = PC2, color = Condition)) +
    geom_point(size = 5) +
    labs(title = 'PCA Plot', x = 'PC1', y = 'PC2') +
    theme_minimal()"
      )
    }
  })
})
