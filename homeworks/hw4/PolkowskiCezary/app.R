# W analizie został wykorzystany podobny format kilku datasetów 
# co pozwoliło na operację na danych beż dużego wysiłku związanego z edytowaniem ich formatu
#
#!!! ABY WIZUALIZACJA DZIAŁAŁA POPRAWNIE WYMAGANE JEST FOLDERU DANE Z PLIKAMI XLSX !!!
# 
# Wizualizacja pokazuje ilość przypadków porzucenia noworodków
# lub ilość wychowanków w pieczy zastępczej na 1000 osob w zależności od wyboru użytkownika
# 
# Po wprowadzeniu 500+ można zauważyć niewielki spadek procentu porzuconych
# noworodków jednak nie widać trwałej zmiany
# 
# W większości województw w ostatnich latach panuje niewielka tendencja wzrostowa 
# wśród procenta osób wychowywanych w pieczy zastępczej
# 

library(shiny)
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)

populacja <- read_xlsx("dane/Liczba osób w wieku 0-24 lata w Polsce, 2014-2023.xlsx")
wychowankowie <- read_xlsx("dane/Wychowankowie (0-24 lata) w pieczy zastępczej 2014-2023.xlsx")
urodzenia <- read_xlsx("dane/Urodzenia żywe w Polsce 2007-2023.xlsx")
pozostawione <- read_xlsx("dane/Noworodki pozostawione w szpitalu 2007-2023.xlsx")

poz_now <- cbind("Województwo" = pozostawione$Województwo ,round(pozostawione[-1]/urodzenia[-1]*1000,2))
pie_zas <- cbind("Województwo" = populacja$Województwo ,round(wychowankowie[-1]/populacja[-1]*1000,2))
pie_zas$Województwo[17] <- "Polska"

ui <- fluidPage(

    titlePanel("Wizualizacja danych do #BI_NGO"),

    sidebarLayout(
        sidebarPanel(
            selectInput("dataset",
                        "Wybierz dane do wizualizacji",
                        c("Pozostawione noworodki",
                          "Piecza zastępcza"),
                        selected = "Pozostawione noworodki" 
                        ),
            selectInput("Województwo",
                        "Wybierz region",
                        choices = pie_zas$Województwo,
                        selected = pie_zas$Województwo[17])
        ),

        mainPanel(
           plotOutput("distPlot")
        )
    )
)

populacja$Województwo
server <- function(input, output) {

    output$distPlot <- renderPlot({
      data <- NA
      if(input$dataset == "Pozostawione noworodki"){
        data <- poz_now %>% 
          filter(Województwo == input$Województwo) %>% 
          select(-Województwo) %>% 
          pivot_longer(cols = everything(), names_to = "rok", values_to = "val")
      } else {
        data <- pie_zas %>% 
          filter(Województwo == input$Województwo) %>% 
          select(-Województwo) %>% 
          pivot_longer(cols = everything(), names_to = "rok", values_to = "val")
      }
      p <- data %>% 
        ggplot(aes(x=rok, y = val, fill = "a")) + 
        geom_col() + 
        labs(x = "Rok",
             subtitle = paste("W regionie:", input$Województwo)) +
        theme(legend.position = "none",
              title = element_text(size = 16),
              subtitle = element_text(size = 12),
              )
      
      if(input$dataset == "Pozostawione noworodki"){
        p <- p + labs(title = "Ilość pozostawionych noworodków na 1000 urodzeń",
                      y = "Ilość pozostawień na 1000 urodzeń") +
          scale_fill_manual(values = "#E5545D") +
          scale_y_continuous(limits = c(0,7), expand = c(0,0))
      } else {
        p <- p + labs(title = "Ilość wychowanków w pieczy zastępczej na 1000 osób w wieku 0-24 lat",
                      y = "Ilość wychowanków na 1000 osób") +
          scale_fill_manual(values = "#576CB0")+
          scale_y_continuous(limits = c(0,11), expand = c(0,0))
      }
      p
      
    })
}
    

shinyApp(ui = ui, server = server)
