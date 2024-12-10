
library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

inflacja_data <- data.frame(
  rok = 2007:2023,
  inflacja = c(2.5, 4.2, 3.5, 2.6, 4.3, 3.7, 0.9, 0.0, -0.9, -0.6, 2.0, 1.6, 2.3, 3.4, 5.1, 14.4, 11.4)
)

dane <- read_excel("../dane.xlsx")
dane_urodzen <- read_excel("../dane_rodzen.xlsx")

ui <- fluidPage(

  titlePanel("Analiza noworodków pozostawionych w szpitalu oraz urodzeń żywych w kontekście inflacji w Polsce"),
  tags$p("Celem analizy jest zrozumienie zależności pomiędzy liczbą noworodków pozostawionych w szpitalu, liczbą żywych urodzeń, a inflacją w wybranych województwach w Polsce w określonym przedziale lat.Uwzględnienie wpływu inflacji na te zmienne pozwala na porównanie zmian w kontekście sytuacji gospodarczej."),
  
  fluidRow(
    column(12,
           
          selectInput(
            inputId = "wybor",
            label = "Województwo",
            choices = dane$wojewodztwo
          ),
          sliderInput(
            inputId = "rok_przedzial",
            label = "Wybierz przedział lat:",
            min = 2007,
            max = 2023,
            value = c(2007, 2023),
            step = 1,
            animate = TRUE
          )
        )
  ),


  fluidRow(
    column(6, plotOutput("distPlot1")),  # Pierwszy wykres
    column(6, plotOutput("distPlot2"))   # Drugi wykres
  ), 
  tags$h2("Komentarz"),
  tags$p("Z analizy wynika, że w większości województw obserwuje się tendencję, w której wyższa inflacja wpływa na niższą liczbą urodzeń. W niektórych województwach im wyższa inflacja tym więcej noworodków jest pozostawianych w szpitalach. Warto jednak zaznaczyć, że w wielu województwach ta zależność nie występuje, co wskazuje, że nie ma jednoznacznej i bezpośredniej korelacji między inflacją a liczbą noworodków pozostawianych w szpitalach.")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot1 <- renderPlot({
      
      dane_wybrane <- dane %>% 
        filter(wojewodztwo == input$wybor) %>% 
        select(-wojewodztwo) %>% 
        gather(key="rok", value = "wartosc") %>% 
        filter(as.numeric(rok) >= input$rok_przedzial[1], as.numeric(rok) <= input$rok_przedzial[2])
      
      inflacja_data_wybrane <- inflacja_data %>% 
        filter(rok >= input$rok_przedzial[1], rok <= input$rok_przedzial[2])
      
      polaczone_dane <- merge(dane_wybrane, inflacja_data_wybrane, by.x = "rok", by.y = "rok")
      
      polaczone_dane %>% 
        ggplot() + 
        geom_line(aes(x = rok, y = wartosc, group = 1, color = "Noworodki"), linewidth = 3) +
        geom_point(aes(x = rok, y = wartosc, color = "Noworodki"), size = 5.4) + 
        geom_line(aes(x = rok, y = inflacja * 10, group = 1, color = "Inflacja"), linewidth = 1.8) + 
        geom_point(aes(x = rok, y = inflacja*10, color = "Inflacja"), size = 4) + 
        geom_text(aes(x = rok, y = inflacja * 10, label = paste0(inflacja, "%")), color = "#e31a1c", vjust = -1.5, size = 5.5) +
        labs(title = paste("Nowordki pozostawione w szpitalu w województwie ", input$wybor, "\n w latach ", input$rok_przedzial[1], "-", input$rok_przedzial[2], " w porównaniu do inflacji")) +
        xlab("Rok") + 
        ylab("Liczba noworodków") + 
        scale_color_manual(values = c("Noworodki" = "#1f77b4", "Inflacja" = "#e31a1c")) +
        theme(legend.title = element_blank(),  # Ukrywa tytuł legendy
              legend.text = element_text(size = 14),
              axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14, angle = 320), plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
    })
    
    output$distPlot2 <- renderPlot({
      
      dane_wybrane <- dane_urodzen %>% 
        filter(wojewodztwo == input$wybor) %>% 
        select(-wojewodztwo) %>% 
        gather(key="rok", value = "wartosc") %>% 
        filter(as.numeric(rok) >= input$rok_przedzial[1], as.numeric(rok) <= input$rok_przedzial[2])
      
      inflacja_data_wybrane <- inflacja_data %>% 
        filter(rok >= input$rok_przedzial[1], rok <= input$rok_przedzial[2])
      
      polaczone_dane <- merge(dane_wybrane, inflacja_data_wybrane, by.x = "rok", by.y = "rok")
      
      polaczone_dane %>% 
        ggplot() + 
        geom_line(aes(x = rok, y = wartosc, group = 1, color = "Noworodki"), linewidth = 3) +
        geom_point(aes(x = rok, y = wartosc, color = "Noworodki"), size = 5.4) + 
        geom_line(aes(x = rok, y = inflacja * 2000, group = 1, color = "Inflacja"), linewidth = 1.8) + 
        geom_point(aes(x = rok, y = inflacja*2000, color = "Inflacja"), size = 4) + 
        geom_text(aes(x = rok, y = inflacja * 2000, label = paste0(inflacja, "%")), color = "#e31a1c", vjust = -1.5, size = 5.5) +
        labs(title = paste("Urodzenia żywe  w województwie ", input$wybor, " \n w latach ", input$rok_przedzial[1], "-", input$rok_przedzial[2], " w porównaniu do inflacji")) +
        xlab("Rok") + 
        ylab("Liczba urodzeń") + 
        scale_color_manual(values = c("Noworodki" = "#1f77b4", "Inflacja" = "#e31a1c")) +
        theme(legend.title = element_blank(),  # Ukrywa tytuł legendy
              legend.text = element_text(size = 14),
              axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14, angle = 320), plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
