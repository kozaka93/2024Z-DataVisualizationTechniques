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
library(tidyr)

font_add_google("Raleway", "Raleway")
showtext_auto()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("wizualizacja danych w ramach projektu BI_NGO dla fundacji Gajusz"),

    # Sidebar with a select input for choosing the region
    sidebarLayout(
      sidebarPanel(
        selectInput( 
          "wojewodztwo", 
          "Wybierz region:", 
          choices = unique(filter(pozostawione_noworodki, Województwo != "Polska")$Województwo)
        ),
        radioButtons( 
          inputId = "typ_wykresu", 
          label = "Wybierz rodzaj wizualizacji:", 
          choices = list( 
            "Liczba pozostawianych noworodków dla danego województwa" = "A", 
            "Procent pozostawianych noworodków dla danego województwa, w porównaniu do średniej krajowej" = "B"
          ) 
        ),
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("mainPlot")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  pozostawione_noworodki <- read.csv("clean_noworodki_pozostawione_w_szpitalu.csv") %>% 
    pivot_longer(!Województwa, names_to='Rok', values_to = 'Liczba_pozostawionych_noworodków') %>% 
    mutate(Rok = as.numeric(sub('.', '', Rok))) %>% 
    rename(Województwo = Województwa) %>% 
    mutate(Województwo = ifelse(Województwo == "Polska", 
                                Województwo, 
                                paste0("woj. ", Województwo)))
  
  wszystkie_noworodki <- read.csv("urodzenia_zywe_pl_2007-2023.csv") %>% 
    pivot_longer(!Województwo, names_to='Rok', values_to = 'Liczba_urodzeń') %>% 
    mutate(Rok = as.numeric(sub('.', '', Rok))) %>% 
    mutate(Województwo = ifelse(Województwo == "POLSKA", 
                                "Polska", 
                                paste0("woj. ", Województwo)))
  
  full_noworodki_data <- wszystkie_noworodki %>% 
    left_join(pozostawione_noworodki, by=c("Województwo", "Rok")) %>% 
    mutate(Procent_pozostawionych = round(100 * Liczba_pozostawionych_noworodków / Liczba_urodzeń, 2))

  polska_percentage_noworodki_data <- full_noworodki_data %>%
    filter(Województwo == "Polska")

  region_percentage_noworodki_data <- full_noworodki_data %>%
    filter(Województwo != "Polska")

  output$mainPlot <- renderPlot({
    
    max_val <- pozostawione_noworodki %>% 
      filter(Województwo %in% input$wojewodztwo) %>% 
      select("Liczba_pozostawionych_noworodków") %>% 
      max(na.rm = TRUE)
    
    noworodkiPlot <- pozostawione_noworodki %>% 
      filter(Województwo %in% input$wojewodztwo) %>% 
      ggplot(aes(x = Rok, y = Liczba_pozostawionych_noworodków)) +
      geom_line(color = "#e62248", size=1.5) +
      geom_vline(xintercept=2015.33, linetype="dashed", color="#884292", size=1.5) +
      annotate("label", x = 2015.33, y = max_val+20, 
               label = "wprowadzenie programu 500+", 
               fill = "#884292", 
               color = "white", 
               family = "Raleway", 
               label.size = 0, 
               size = 4) +
      labs(title = paste("Procent noworodków pozostawionych w szpitalu nie ze \nwzględów zdrowotnych; ", input$wojewodztwo, ", lata 2006-2023"), 
           subtitle = "Ministerstwo Zdrowia, dane udostępniane na prośbę Fundacji Gajusz", 
           y = "Liczba pozostawionych noworodków") +
      theme_bw() +
      theme(
        axis.title = element_text(family = "Raleway"),
        axis.text = element_text(family = "Raleway"),
        plot.title = element_text(family = "Raleway"), 
        plot.subtitle = element_text(family = "Raleway"))
    
    max_val1 <- full_noworodki_data %>% 
        filter(Województwo %in% input$wojewodztwo | Województwo == "Polska") %>% 
        select("Procent_pozostawionych") %>% 
        max(na.rm = TRUE)
      
    noworodkiPercentagePlot <- region_percentage_noworodki_data %>%
      filter(Województwo %in% input$wojewodztwo) %>%
      ggplot(aes(x = Rok, y = Procent_pozostawionych)) +
      geom_line(color = "#e62248", size=1.5) +
      geom_line(data = polska_percentage_noworodki_data, aes(x = Rok, y = Procent_pozostawionych), linetype = "dotted", color = "#315ca8", size=1.5) +
      geom_vline(xintercept=2015.33, linetype="dashed", color="#884292", size=1.5) +
      annotate("label", x = 2015.33, y = max_val1+0.05,
               label = "wprowadzenie programu 500+",
               fill = "#884292",
               color = "white",
               family = "Raleway",
               label.size = 0,
               size = 4) +
      labs(title = paste("Procent noworodków pozostawionych w szpitalu nie ze \nwzględów zdrowotnych; ", input$wojewodztwo, ", lata 2006-2023"), 
           subtitle = "Ministerstwo Zdrowia, dane udostępniane na prośbę Fundacji Gajusz; \nna niebiesko zaznaczono średnią dla całego kraju",
           y = "Procent pozostawionych noworodków") +
      theme_bw() +
      theme(
        axis.title = element_text(family = "Raleway"),
        axis.text = element_text(family = "Raleway"),
        plot.title = element_text(family = "Raleway"),
        plot.subtitle = element_text(family = "Raleway"))
    
    mainPlot = noworodkiPlot
    
    if(input$typ_wykresu == "B"){
      mainPlot = noworodkiPercentagePlot
    }
    
    mainPlot
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
