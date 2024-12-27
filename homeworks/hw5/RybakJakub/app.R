library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  tags$style(HTML("
    #plot-container {
      width: 500px;   /* Szerokość div'a */
      height: 400px;  /* Wysokość div'a */
      overflow: hidden; /* Ukrywa zawartość wykraczającą poza kontener */
    }
  ")),
  fluidRow(
  # Suwak
    sliderInput("slider", "prosze włączyć animacje", min = 0, max = 100, value = 0, step = 0.03, animate =
                  animationOptions(interval = 300, loop = TRUE)),
  ),
  fluidRow(
    div(id = "plot-container", plotOutput("plot"))
    
  )
)

server <- function(input, output) {
  
  galazki_lista <- list()
  
  # Pętla tworząca dane dla 4 grup (4 pętle) z różnymi wartościami początkowymi
  for (j in 0:3) {  # 4 grupy
    for (i in 1:(ifelse(j == 3, 7, 8))) {  # W ostatniej grupie mamy 7 elementów, w pozostałych 8
      galazki_lista[[8 * j + i]] <- data.frame(
        x = seq(0.35 + (i - 1) * 0.01 + j * 0.03, 0.65 - (i - 1) * 0.01 - j * 0.03, by = 0.001),
        y = 0.1 + (i - 1) * 0.025 + j * 0.2  # Zmienia się wartość y w zależności od grupy
      )
    }
  }
  
  # Łączenie wszystkich danych w jeden data frame
  galazki <- do.call(rbind, galazki_lista)
  
  
  pien_lista <- list()
  
  # Generujemy dane w jednej pętli
  for (j in 0:4) {
    for (i in 1:6) {
      pien_lista[[6 * j + i]] <- data.frame(
        x = seq(0.45 + (i - 1) * 0.01, 0.55 - (i - 1) * 0.01, by = 0.001),
        y = 0.025 * j  # Wartość y zmienia się w zależności od j
      )
    }
  }
  
  # Łączenie wszystkich elementów listy w jeden data frame
  pien <- do.call(rbind, pien_lista)
  
  ktore_wyswietlic <- function(x,wartosc, margines){
    if(wartosc + margines < 0.865){
      return((x>wartosc) & (x < wartosc + margines))
    }
    return((x > wartosc) | (x<(wartosc + margines) %%0.865))
  }
  
  output$plot <- renderPlot({
    wartosc <- input$slider %% 0.865
    galazki_to_plot <- galazki %>% 
      filter(ktore_wyswietlic(y,wartosc,0.45))
    pien_to_plot <- pien %>% 
      filter(ktore_wyswietlic(y,wartosc,0.45))
    ggplot() + 
      geom_point(data = pien_to_plot, aes(x = x, y = y), color = "saddlebrown", size = 5, shape = 7) +
      geom_point(data = galazki_to_plot, aes(x = x, y = y), color = "darkgreen", size = 5) +
      xlim(0, 0.855) + ylim(0, 0.855) +
      theme_void() +
      theme(legend.position = "none")
      
  })
  
}

shinyApp(ui, server)
