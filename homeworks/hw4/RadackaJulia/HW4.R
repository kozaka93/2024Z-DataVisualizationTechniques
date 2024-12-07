library(shiny)
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(htmltools)



noworodki_pozostawione <- read_excel("/Users/Robert/Documents/Dokumenty Julii/Studia/III Semestr/Techniki Wizualizacji Danych/homeworks/hw4/Noworodki pozostawione w szpitalu 2007-2023.xlsx")
noworodki_pozostawione <- noworodki_pozostawione[-(1:7),]
noworodki_pozostawione <- noworodki_pozostawione[-nrow(noworodki_pozostawione),]
colnames(noworodki_pozostawione) <- c("wojewodztwo","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")
noworodki_pozostawione <- noworodki_pozostawione %>% 
  pivot_longer(cols = -wojewodztwo, names_to = "rok", values_to = "noworodki") %>% 
  filter(rok >= 2014) %>% 
  mutate(rok = as.numeric(rok),
         noworodki = as.numeric(noworodki))


wychowankowie <- read_excel("/Users/Robert/Documents/Dokumenty Julii/Studia/III Semestr/Techniki Wizualizacji Danych/homeworks/hw4/wychowankowie.xlsx")
wychowankowie <- wychowankowie %>%
  rename(wojewodztwo = ...1) %>% 
  pivot_longer(cols = -wojewodztwo, names_to = 'rok', values_to = 'wychowankowie') %>% 
  mutate(rok = as.numeric(rok))
ramka <- wychowankowie %>% 
  left_join(noworodki_pozostawione, by = c('rok', 'wojewodztwo'))

ramka <- head(ramka,-10)



ui <- fluidPage(
  titlePanel(
    div(
    h1("Dane dotyczące noworodków oraz pieczy zastępczej w Polsce"),
    p("Wizualizacja przedstawia liczbę noworodków pozostawionych w szpitalach
      (nie ze względów zdrowotnych) oraz ilość wychowanków w rodzinach
      zastępczych w podziale na województwa i lata z uwzględnieniem daty wprowadzenia
      programu 500+.
      Ramka poniżej przedstawia dokładne liczby.",style = "font-style: italic; color: gray; font-size: 14px;"))),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("wojewodztwo", "Wybierz województwo:", choices = ramka$wojewodztwo,
                  selected = "dolnośląskie"),
      checkboxInput("poczatek500", "Początek 500+", FALSE),
      sliderInput("rok", "Wybierz zakres lat:", 
                  min = min(ramka$rok), 
                  max = max(ramka$rok),
                  value = range(ramka$rok),
                  step = 1,
                  animate = TRUE),
      hr(),
      p("Komentarz:"),
      p("Pierwszy wykres przedstawia liczbę noworodków pozostawionych w szpitalach z powodów niezdrowotnych.
      Dane te mogą być analizowane w kontekście wprowadzenia programu 500+. Jak można się spodziewać w dużej 
      częsci województw, po wprowadzeniu programu 500+ ilość pozostawianych
      noworodków zmalała na przestrzeni lat. Widoczne to jest np. w przypadku województwa małopolskiego,
      dolnośląskiego, śląskiego i zachodniopomorskiego.
      Drugi wykres pokazuje liczbę wychowanków w pieczy zastępczej w wybranym województwie.
      W tym przypadku również widoczne są spadki m.in. w województwie śląskim, warmińsko-mazurskim oraz 
      lubuskim." 
      )
    ),
    
    mainPanel(
    
      plotOutput("plot2"),
      plotOutput("plot1"),
      DT::dataTableOutput("tabela")
    )
  )
)


server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    
    data <- ramka %>% 
      filter(wojewodztwo == input$wojewodztwo & 
               rok >= input$rok[1] & rok <= input$rok[2])
    
    plot1 <- ggplot(data, aes(x = rok, y = wychowankowie)) +
      
      geom_point(aes(color = wychowankowie), size = 3, shape = 16) +  
      
      geom_line(aes(color = wychowankowie), size = 1.2) + 
      labs(title = paste("Liczba wychowanków w pieczy zastępczej w województwie:", input$wojewodztwo),
           x = "Rok", y = "Liczba wychowanków") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none"
      ) 
    if(input$poczatek500){
      plot1 <- plot1 + geom_vline(xintercept = 2016, linetype = "dashed", color = "red", size = 0.5) +
        annotate("text", x = 2016.4, y = max(data$wychowankowie, na.rm = TRUE) * 1.05, 
                 label = "500+", color = "red", angle = 0, size = 5, fontface = "bold", hjust = -0.1)
    }
    
    plot1
  })
  
  output$plot2 <- renderPlot({
    
    data <- ramka %>% 
      filter(wojewodztwo == input$wojewodztwo & 
               rok >= input$rok[1] & rok <= input$rok[2])
    
    plot2 <- ggplot(data, aes(x = rok, y = noworodki)) +
     
      geom_point(aes(color = noworodki), size = 3, shape = 16) +  
     
      geom_line(aes(color = noworodki), size = 1.2) + 
      labs(title = paste("Liczba noworodków pozostawionych w szpitalach (nie ze wględów zdrowotnych)
                         w województwie:", input$wojewodztwo),
           x = "Rok", y = "Liczba noworodków") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none"
      )
    
    if(input$poczatek500){
      plot2 <- plot2 + geom_vline(xintercept = 2016, linetype = "dashed", color = "red", size = 0.5) +
        annotate("text", x = 2016.4, y = max(data$noworodki, na.rm = TRUE) * 1.05, 
                 label = "500+", color = "red", angle = 0, size = 5, fontface = "bold", hjust = -0.1)
    }
    
    plot2
  })
  
  output$tabela <- DT::renderDataTable({
    data <- ramka %>%
      filter(
        wojewodztwo == input$wojewodztwo,
        rok >= input$rok[1],
        rok <= input$rok[2]
      ) %>% 
      rename(Rok = rok, 
             Ilość_wychowanków = wychowankowie,
             Ilość_noworodków = noworodki)
    data[,-1]
      
    
  })
  
  
}

shinyApp(ui = ui, server = server)
