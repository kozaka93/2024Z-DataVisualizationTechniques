library(shiny)
library(shinyjs)

### DEFINE UI AND SERVER ###
ui <- source(file.path("app", "ui.R"))$value
server <- source(file.path("app", "server.R"))$value


### RUN APP ###
shinyApp(ui = ui, server = server)