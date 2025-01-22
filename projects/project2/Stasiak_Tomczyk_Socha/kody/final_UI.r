library(shiny)
library(bslib)

# UI
ui <- fillPage(
    tags$style(
        HTML("
      iframe.shiny-frame {
        height: calc(100vh - 100px) !important; /* Zwiększa wysokość iframe do pełnej wysokości okna */
      }
    ")
    ),
    navbarPage(
        "Świat jest w-fem",
        
        tabPanel(
          "Progres",
          source("proj2TWD.R", local = TRUE)$value
        ),
        tabPanel(
            "Samopoczucie i sen",
            source("samopoczucie_sen_UI.R", local = TRUE)$value
        ),
        tabPanel(
            "Efekt muzyki",
            source("music-effectsUI.R", local = TRUE)$value
        ),
        tabPanel(
            "DODAJ TRENING",
            source("DODAJTRENING.R", local = TRUE)$value
        )
    )
)

# Server
server <- function(input, output, session) {
    # Serwery każdej aplikacji będą działały razem
    source("proj2TWD.R", local = TRUE)
    source("samopoczucie_sen_UI.R", local = TRUE)
    source("music-effectsUI.R", local = TRUE)
    source("DODAJTRENING.R", local = TRUE)
}

# Run the app
shinyApp(ui, server)