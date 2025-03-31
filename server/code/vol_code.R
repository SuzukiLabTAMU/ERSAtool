### ENHANCED VOLCANO CODE ####

observeEvent(input$toggle_volcano_code, {
  output$volcano_code <- renderUI({
    if (input$toggle_volcano_code %% 2 == 1) {
      aceEditor(
        outputId = "volcano_code",
        mode = "r",
        theme = "solarized_light",
        readOnly = TRUE,
        height = "250px",
        value = "## Enhanced Volcano Plot Logic
          
  # Convert DEG results to data frame

  # Generate Enhanced Volcano Plot
  EnhancedVolcano(
    res,
    lab = res$SYMBOL,              
    x = 'log2FoldChange',          
    y = 'padj',                    
    pCutoff = input$adjp_cutoff,                
    FCcutoff = input$logfc_cutoff,
    title = 'Liver [Exercise (vs) Sedentary]',  
    subtitle = 'Differential expression',        
    caption = paste('FC cutoff:', input$logfc_cutoff, '; adj p-value cutoff:', input$adjp_cutoff),
    legendPosition = 'right',    
    legendLabSize = 14,          
    col = c('grey30', 'forestgreen', 'royalblue', 'red2')
  )"
      )
    }
  })
})
