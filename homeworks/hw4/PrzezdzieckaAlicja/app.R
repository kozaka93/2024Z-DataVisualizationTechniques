#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

pozostawione<-read_excel("Noworodki pozostawione w szpitalu 2007-2023.xlsx", skip=2, range = "A9:R25", col_names = FALSE)
urodzenia<- read_excel("Urodzenia żywe w Polsce 2007-2023.xlsx")
nagłówki<- c("województwa", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023")
colnames(pozostawione)<-nagłówki
colnames(urodzenia)<-nagłówki
urodzenia[which(urodzenia$województwa =="POLSKA"), "województwa"]<-"Polska"
pozostawione<-pozostawione %>% pivot_longer(cols = 2:18, names_to = "Rok", values_to = "liczba_pozostawień")
urodzenia<-urodzenia%>% pivot_longer(cols = 2:18, names_to = "Rok", values_to = "liczba_urodzeń")
data<-inner_join(pozostawione, urodzenia, by = c("Rok", "województwa"))%>% mutate(liczba_pozostawień_na_1000 = 1000*liczba_pozostawień / liczba_urodzeń)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Opuszczanie noworodków w szpitalu przez biologicznych rodziców"),

    sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId ="woj1" ,
            label = "Wybierz pierwsze województwo: ",
            choices = unique(data %>% select(województwa) %>% filter(województwa != "Polska")) 
          ),
          selectInput(
            inputId ="woj2" ,
            label = "Wybierz drugie województwo: ",
            choices = unique(data %>% select(województwa) %>% filter(województwa != "Polska"))
          ),
          checkboxInput("polska", "Pokaż wykres podsumowania dla całej Polski", TRUE)
          ),
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("linePlot"),
           tableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$linePlot <- renderPlot({
        wykres<- ggplot() + geom_line(data = (data %>% filter(województwa == input$woj1 | województwa == input$woj2)), aes(x = Rok, y = liczba_pozostawień_na_1000, colour = województwa, group = województwa), size = 1.2)+ scale_color_manual(values = c("#e4007e", "#315ca8"))
        wykres<- wykres + labs(y = "Liczba noworodków pozostawionych na 1000 żywych urodzeń", title = "Wykres liczby pozostawionych noworodków na 1000 żywych urodzeń", colour = "Województwa")
        if(input$woj1 == input$woj2){
          wykres<- wykres + labs(subtitle = paste("w latch 2007-2023 dla województwa: ", input$woj1))
        }
        else{
          wykres<- wykres + labs(subtitle = paste0("w latch 2007-2023 dla województw: ", input$woj1, ", ", input$woj2 ))
        }
        if (input$polska){
          wykres<-wykres + geom_line(data = (data %>% filter(województwa == "Polska")), aes(x = Rok, y =liczba_pozostawień_na_1000, group = województwa),colour = "#b5e0f3", linetype = "longdash", size = 1) + labs(caption = "Przerywaną linią zaznaczono podsumowanie dla całej Polski")
        }
        wykres<- wykres+ theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 13),
                      plot.subtitle = element_text(hjust = 0.5, size = 10),
                      panel.grid.major.x = element_line(colour = 'lightgray'),
                      panel.background = element_rect(fill = 'white'),
                      panel.grid.major.y = element_line(colour = 'lightgray'),
                      plot.caption = element_text(hjust = 1, size = 10))
        wykres<- wykres + scale_y_continuous(limits = c(0, NA), expand = c(0,0), breaks = seq(0, max(data$liczba_pozostawień_na_1000), by = 0.5)) + 
          scale_x_discrete(expand = c(0,0))
        wykres
    })
    
    output$table<- renderTable({
      table_data <- data
      if(input$polska){
        table_data <- data %>% filter(województwa == input$woj1 | województwa == input$woj2 | województwa == "Polska")
      }
      else{
        table_data <- data %>% filter(województwa == input$woj1 | województwa == input$woj2)
      }
      table_data<- table_data %>% group_by(województwa) %>% summarise(max = max(liczba_pozostawień), min = min(liczba_pozostawień), mean = mean(liczba_pozostawień))
      table_data<-table_data %>% rename(
        "Obszar" = województwa,
        "Maksymalna liczba pozostwień" = max,
        "Minimalna liczba pozostawień" = min,
        "Średnia liczba pozostawień" = mean
      )
      table_data
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
