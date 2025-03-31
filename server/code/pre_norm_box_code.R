### PRE-NORMALIZED BOX & WHISKER CODE ####

observeEvent(input$toggle_boxplot_code, {
  output$boxplot_code <- renderUI({
    if (input$toggle_boxplot_code %% 2 == 1) {
      aceEditor(
        outputId = "boxplot_code",
        mode = "r",
        theme = "solarized_light",
        readOnly = TRUE,
        height = "250px",
        value = "## Box-and-Whisker Plot Logic (Raw Counts)

    # Convert raw data to long format for ggplot
    df <- reshape2::melt(raw_data, variable.name = 'Sample', value.name = 'Raw_Counts')

    # Merge metadata with counts
    merged_df <- merge(df, meta_data, by.x = 'Sample', by.y = 'row.names', all.x = TRUE)

    # Automatically detect groups and assign colors
    unique_groups <- unique(merged_df$Condition)
    num_groups <- length(unique_groups)
    color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, 'Dark2'))(num_groups)

    # Generate Boxplot
    ggplot(merged_df, aes(x = Sample, y = Raw_Counts, fill = Condition)) +
      geom_boxplot(outlier.color = 'red', outlier.shape = 16, outlier.size = 2) +
      scale_fill_manual(values = color_palette) +
      xlab('Samples') +
      ylab('Raw Counts') +
      ggtitle('Raw Counts Across Samples') +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'bottom'
      )"
      )
    }
  })
})
