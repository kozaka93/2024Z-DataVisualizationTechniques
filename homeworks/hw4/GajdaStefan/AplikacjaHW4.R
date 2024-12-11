#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(dplyr)


dane<-read.csv("dane.csv")

ui <- fluidPage(
  
    # Tytuł aplikacji
    titlePanel("Analiza danych dla fundacji Gajusz"),

    # Sidebar
    sidebarLayout(
        
        sidebarPanel(
          #Slider do wyboru lat 
            sliderInput("zakres_lat",
                        "Wybierz przedział lat",
                        min = 2007,
                        max = 2023,
                        value = c(2007,2023)),
            #Wybór analizowanego województwa
            selectInput(
              "wojewodztwo","Wybierz dla którego województwa chcesz stworzyć wykres",
              choices=dane$Województwa[-length(dane$Województwa)]
            ),
            checkboxInput("czypolska","Czy wyświetlić wykres dla polski"),
            checkboxInput("piecsetplus","Czy zaznaczyć moment dodania 500+")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("calapolska_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        
        
        
        
        opuszczone <-dane  %>% 
          filter(rok>=input$zakres_lat[1]&rok<=input$zakres_lat[2]) %>% 
          filter(Województwa==input$wojewodztwo)
          
        plot<-ggplot(opuszczone,aes(x=rok,y=liczba_opuszczonych/liczba_urodzen*1000) )+
          geom_bar(stat = "identity") + 
          labs(
            title = paste("Liczba opuszczonych noworodków w województwie", input$wojewodztwo),
            x = "Rok",
            y = "Liczba opuszczonych noworodków na 1000 urodzonych"
          ) +
          scale_x_continuous(breaks = seq(2007, 2023, by = 1)) +  # Ustawienie etykiet na osi X co rok
          theme_minimal()
        if (input$piecsetplus) {
          plot <- plot + 
            geom_vline(xintercept = 2016 + 3 / 12, linetype = "dashed", color = "red", size = 1) +  # Linia pionowa w kwietniu 2016
            annotate("text", x = 2016 + 3 / 12, y = min(opuszczone$liczba_opuszczonych / opuszczone$liczba_urodzen * 1000+0.5, na.rm = TRUE), 
                     label = "wprowadzenie 500+", angle = 90, vjust = -0.5, color = "red") 
        }
        plot
        
    })
    output$calapolska_plot <- renderPlot({
      # Rysujemy wykres tylko, jeśli checkbox jest zaznaczony
      if (input$czypolska) {
        opuszczone <- dane  %>% 
          filter(rok>=input$zakres_lat[1]&rok<=input$zakres_lat[2]) %>% 
          filter(Województwa=="Polska")
        
        
        plot1<-ggplot(opuszczone,aes(x=rok,y=liczba_opuszczonych) )+
          geom_bar(stat = "identity") + 
          labs(
            title = paste("Liczba opuszczonych noworodków w Polsce w tych latach"),
            x = "Rok",
            y = "Liczba opuszczonych noworodków w Polsce"
          ) +
          scale_x_continuous(breaks = seq(2007, 2023, by = 1)) +  # Ustawienie etykiet na osi X co rok
          theme_minimal()
        if (input$piecsetplus) {
          plot1 <- plot1 + 
            geom_vline(xintercept = 2016 + 3 / 12, linetype = "dashed", color = "red", size = 1) +  # Linia pionowa w kwietniu 2016
            annotate("text", x = 2016 + 3 / 12, y = min(opuszczone$liczba_opuszczonych, na.rm = TRUE),
                     label = "wprowadzenie 500+", angle = 90, vjust = -0.5, color = "red") 
        }
        plot1
      }
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
