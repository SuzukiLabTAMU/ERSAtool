## DEG ANALYSIS ####

output$deg_results <- renderDataTable({
  req(reactiveVolcanoData$selected_genes, input$comparison_selector)
  
  selected_genes <- reactiveVolcanoData$selected_genes[[input$comparison_selector]]
  if (is.null(selected_genes) || nrow(selected_genes) == 0) {
    return(NULL)
  }
  
  datatable(selected_genes, options = list(pageLength = 10, autoWidth = TRUE))
})
