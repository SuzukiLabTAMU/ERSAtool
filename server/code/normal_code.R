### Normalization SHOW CODE ####

observeEvent(input$toggle_normal_code, {
  output$normal_code <- renderUI({
    if (input$toggle_normal_code %% 2 == 1) {
      aceEditor(
        outputId = "normal_code",
        mode = "r",
        theme = "solarized_light",
        readOnly = TRUE,
        height = "400px",
        value = "## Normalization with DESeq2

meta_data <- metadata()
colnames(meta_data) <- make.names(colnames(meta_data))
selected_cols <- make.names(input$design_columns)

if (is.null(selected_cols) || length(selected_cols) == 0) {
  stop('No design columns selected.')
}

# Ensure all selected columns are factors
for (col in selected_cols) {
  if (!is.factor(meta_data[[col]])) {
    meta_data[[col]] <- as.factor(meta_data[[col]])
  }
}

design_formula <- as.formula(paste('~', paste(selected_cols, collapse = ' + ')))

count_matrix <- as.matrix(raw_counts())

if (any(is.na(count_matrix))) {
  stop('Raw counts contain NA values!')
}

if (any(count_matrix != floor(count_matrix), na.rm = TRUE)) {
  stop('Count matrix includes non-integer values.')
}

dds <- DESeqDataSetFromMatrix(
  countData = count_matrix,
  colData = meta_data,
  design = design_formula
)

# Filter low-count genes
num_samples <- ncol(counts(dds))
dds <- dds[rowSums(counts(dds)) > num_samples, ]

# Run normalization
dds <- DESeq(dds)
dds_data(dds)"
      )
    }
  })
})

