


library(dplyr)

library(ggplot2)
library(maps)
library(mapdata)
library(sf)


library(tidyr)

library(shiny)


urodzenia <- read.csv("Urodzenia-zywe-w-Polsce-2007-2023.csv") %>% 
  pivot_longer(cols = X2007:X2023 ,names_to = "rok", values_to = "l_noworodkow") %>% 
  mutate(Rok = as.numeric(substr(rok, 2, 5))) %>% 
  filter(Wojewodztwo != "POLSKA") %>% 
  select(Wojewodztwo, Rok, l_noworodkow) %>% 
  mutate(pom = paste0(as.character(Rok), Wojewodztwo))


noworodki_pozostawione <- read.csv("Noworodki-pozostawione-w-szpitalu-2007-2023.csv") %>% 
  pivot_longer(cols = X2007:X2023, names_to = "rok", values_to = "l_poz_noworodkow") %>% 
  mutate(Rok = as.numeric(substr(rok, 2, 5))) %>% 
  select(Wojewodztwo, Rok, l_poz_noworodkow) %>% 
  mutate(pom = paste0(as.character(Rok), Wojewodztwo)) %>% 
  select(pom, l_poz_noworodkow)


wych_w_pieczy_zastepczej <- read.csv("Wychowankowie-_0-24-lata_-w-pieczy-zastepczej-2014-2023.csv") %>% 
  pivot_longer(cols = X2014:X2023, names_to = "rok", values_to = "l_pod_piecza") %>% 
  mutate(Rok = as.numeric(substr(rok, 2, 5))) %>% 
  select(Wojewodztwo, Rok, l_pod_piecza) %>% 
  mutate(pom = paste0(as.character(Rok), Wojewodztwo)) %>% 
  select(pom, l_pod_piecza)

populacja_0_24 <- read.csv("Liczba-osób-w-wieku-0-24-lata-w-Polsce_-2014-2023.csv") %>% 
  pivot_longer(cols = X2014:X2023, names_to = "rok", values_to = "l_osob") %>% 
  mutate(Rok = as.numeric(substr(rok, 2, 5))) %>% 
  select(Wojewodztwo, Rok, l_osob) %>% 
  mutate(pom = paste0(as.character(Rok), Wojewodztwo))


dane_o_now <- urodzenia %>% 
  left_join(noworodki_pozostawione, by = "pom")%>% 
  select(Wojewodztwo, Rok, l_noworodkow, l_poz_noworodkow) %>% 
  mutate(proporcja = l_poz_noworodkow/l_noworodkow)

dane_o_pieczy <- populacja_0_24 %>% 
  left_join(wych_w_pieczy_zastepczej, by = "pom") %>% 
  select(-pom) %>% 
  mutate(proporcja = l_pod_piecza/l_osob)

wojewodztwa <- st_read("wojewodztwa.shp")

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "rok",            
        label = "Wybierz rok:",      
        min = 2007,                  
        max = 2023,                 
        value = 2014,               
        step = 1,                    
        sep = ""                     
      ),
      
      
    ),
    
    mainPanel(
      textOutput("text1"),
      plotOutput("mapPlot1"),
      textOutput("text2"),
      plotOutput("mapPlot2")
    )
  )
)

server <- function(input, output) {
  
  output$text1 <- renderText({paste0("Wykres poniżej przedstawia jaki ułamek osób w wieku 24 lat i młodszych żyje w pieczy zastępczej w roku ", input$rok)})
  
  output$mapPlot1 <- renderPlot({
    
    wybranyRok <- dane_o_pieczy[dane_o_pieczy$Rok == input$rok, ]
    
    if(input$rok >= 2014){
      ggplot(wojewodztwa) +
        geom_sf(color = "black", aes(fill = wybranyRok$proporcja)) +
        scale_fill_gradient(
          low = "white",
          high = "red",
          name = paste0("Proporcja osób w wieku\n24 lat i młodszych żyjących\nw pieczy zastępczej w roku ", input$rok)
        ) +
        theme_void()  +
        theme(
          legend.position = "right"
        )
    } else {
      ggplot(wojewodztwa) +
        geom_sf(aes(fill = "Brak danych"), color = "black") + 
        scale_fill_manual(
          values = c("Brak danych" = "grey"),
          name = "                                                  ") +              
        theme_void()  +
        theme(
          legend.position = "right"
        )
    }
  })
  
  output$text2 <- renderText({paste0("Wykres poniżej przedstawia jaki ułamek urodzonych dzieci zostaje porzuconych w szpitalu w roku ", input$rok)})
  
  output$mapPlot2 <- renderPlot({
    wybranyRok2 <- dane_o_now[dane_o_now$Rok == input$rok, ]
    ggplot(wojewodztwa) +
      geom_sf(color = "black", aes(fill = wybranyRok2$proporcja)) +
      scale_fill_gradient(
        low = "white",
        high = "#2f2faf",
        name = paste0("Proporcja dzieci porzuconych\n w szpitalu do urodzeń w roku ", input$rok)
      ) +
      theme_void()  +
      theme(
        legend.position = "right"
      )
  })
}


shinyApp(ui = ui, server = server)

