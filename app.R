source("global.R")
ui <- source("ui/dashboard.R", local = TRUE)$value
server <- source("server/main.R", local = TRUE)$value

shinyApp(ui, server)