library(shiny)
library(igraph)
library(dplyr)

set.seed(123)

# wczytanie grafu i skrócenie go do 1000
# wierzchołków ze względu na ograniczenia
# natury technicznej

g <- induced.subgraph(read_graph("netscience.gml",
                                 format = "gml"),
                      vids = 1:1000)

choices <- as.list(setNames(1:1000, V(g)$label))

ui <- fluidPage(
  titlePanel("Sieć współpracy naukowców"),

  sidebarLayout(

    sidebarPanel(
      selectInput("ids", "Wybierz naukowców",
                  choices = choices, multiple = TRUE,
                  selected = c(56, 1)),

      numericInput("c",
                   label = "Stała c, tż. rozmiar wierzchołka v = c * deg(v)",
                   min = 0,
                   value = 3),

      numericInput("dist",
                   label = "Odległość nazwisk od wierzchołków",
                   value = 3),

      numericInput("size",
                   label = "Rozmiar nazwisk",
                   min = 0.1,
                   step = 0.1,
                   value = 1)
    ),


    mainPanel(
      tabsetPanel(
        tabPanel("Opis aplikacji", textOutput("tutorial")),
        tabPanel("Wykres", plotOutput("graph"))
      )
    )
  )
)

server <- function(input, output) {

  ids <- reactive(unique(unlist(neighborhood(g,
                                             order = 1,
                                             nodes = input$ids))))

  graph <- reactive(induced.subgraph(g, vids = ids()))

  layout <- reactive(layout_with_fr(graph()))

  quant <- reactive(quantile(degree(graph()), probs = c(0.25, 0.5, 0.75)))

  colours <- reactive({
    case_when(
      degree(graph()) <= quant()[1] ~ "blue",
      degree(graph()) <= quant()[2] ~ "green",
      degree(graph()) <= quant()[3] ~ "yellow",
      TRUE ~ "red"
    )
  })

  output$graph <- renderPlot({
    plot(graph(), layout = layout(),
         vertex.size = degree(graph()) * input$c,
         vertex.color = colours(),
         vertex.label.dist = input$dist,
         vertex.label.cex = input$size,)

    legend("topright", legend = c("degV <= Q1",
                                  "Q1 < degV <= Q2",
                                  "Q2 < degV <= Q3",
                                  "Q3 < degV "),
           col = c("blue", "green", "yellow", "red"),
           title = "Qi - i-ty kwartyl\nzbioru stopni\nwierzchołków",
           pch = 16, bty = "n")
  })

  output$tutorial <- renderText({
    "Aplikacja służy do badania współprac i interakcji między naukowcami.
    Wierzchołkami grafu są wszyscy powyżej wybrani naukowcy {N1, N2, ..., Nk}
    oraz naukowcy, którzy kiedyś wydali wspólną pracę naukową z pewnym
    naukowcem Ni, i = 1, ..., k. Jeśli dwaj naukowcy wydali kiedyś wspólną
    pracę, to wierzchołki ich reprezentujące są połączone krawędzią.
    Takie ograniczenie (niewyświetlanie wszystkich na raz) 
    zostało wprowadzone ze względu na dużą ilość naukowców -
    a mianowicie 1000. Stopnie wierzchołków wyznaczane są na podstawie
    wyświetlonych wierzchołków."
  })

}

shinyApp(ui = ui, server = server)
