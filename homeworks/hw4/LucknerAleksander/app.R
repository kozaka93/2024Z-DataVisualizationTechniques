
library(shiny)
library(readxl)
library(xlsx)
library(rstudioapi)
library(tidyr)
library(htmlwidgets)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


######## O pd ########
# Praca przedstawia analizę danych o dzieciach będących pod pieczą rodzinną lub
# instytucjonalną. Głównym zamysłem jest zwizualizowanie jak zmienia sie procent
# dzieci pod pieczą w zależności od lat i województwa.

######## Komentarz do wizualizacji ########
# Na przestrzeni lat nie ma znaczących zmian dla województw. Np. w przypadku 
# pieczy rodzinnej najmniejszy procent dzieci pod pieczą jest w województwach
# Podkarpackim lub Małopolskim a największy stale w Zachodniopomorskim.


#Miałem problem z oryginalnymi nazwami plików więc musiałem je zmienić
piecza_rodzinna <- read_excel("./data/Wychowankowie_(0-24_lata)_w_pieczy_zastepczej_2014-2023.xlsx", 1)
colnames(piecza_rodzinna) <- c("wojewodztwo", as.character(2014:2023))
piecza_rodzinna <- piecza_rodzinna[-17,]

piecza_rodzinna <- piecza_rodzinna %>% 
  pivot_longer(
    cols = '2014':'2023',
    names_to = "rok",
    values_to = "dzieci_w_pr"
  )

piecza_inst <- read_excel("./data/Wychowankowie_(0-24_lata)_w_pieczy_zastepczej_2014-2023.xlsx", 2)
colnames(piecza_inst) <- c("wojewodztwo", as.character(2014:2023))
piecza_inst <- piecza_inst[-17,]

piecza_inst <- piecza_inst %>% 
  pivot_longer(
    cols = '2014':'2023',
    names_to = "rok",
    values_to = "dzieci_w_pi"
  )

liczba_osob <- read_excel("./data/Liczba_osób_w_wieku_0-24_lata_w_Polsce,_2014-2023.xlsx", 1)
colnames(liczba_osob) <- c("wojewodztwo", as.character(2014:2023))
liczba_osob <- liczba_osob[-17,]

liczba_osob <- liczba_osob %>% 
  pivot_longer(
    cols = '2014':'2023',
    names_to = "rok",
    values_to = "liczba_osob"
  )

final_df <- liczba_osob %>% 
  left_join(piecza_rodzinna, by = c("wojewodztwo", "rok")) %>% 
  left_join(piecza_inst, by = c("wojewodztwo", "rok")) %>% 
  mutate(rodzinna = 100 * dzieci_w_pr/liczba_osob,
         instytucjonalna = 100* dzieci_w_pi/liczba_osob) %>% 
  select(rok, wojewodztwo, rodzinna, instytucjonalna)
final_df$rok <- as.numeric(as.character(final_df$rok))



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Procent dzieci w wieku 0-24 lat będących pod pieczą rodzinną/instytucjonalną"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("piecza",
                      "Wybierz pieczę:",
                      choices = colnames(final_df)[c(3,4)],
                      selected = colnames(final_df)[3]),
            sliderInput("zakresLat",
                        "Przedział lat:",
                        min = min(final_df$rok),
                        max = max(final_df$rok),
                        value = c(min(final_df$rok), max(final_df$rok)))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({

        piecza <- input$piecza
        
        df <- final_df %>% 
          filter(rok >= input$zakresLat[1] & rok <= input$zakresLat[2]) %>% 
          group_by(wojewodztwo) %>% 
          summarise(avg_piecza = mean(.data[[piecza]])) %>% 
          mutate(
            min_val = avg_piecza == min(avg_piecza),
            max_val = avg_piecza == max(avg_piecza)
          )
        
        
        p <- ggplot(df) +  
          geom_col(aes(x = wojewodztwo, y = avg_piecza, fill = ifelse(min_val, "green", ifelse(max_val, "red", "blue")))) + 
          scale_fill_manual(labels = c("Pozostałe wartości", "Minimum", "Maksimum"), 
                            values = c("blue", "green", "red")) + 
          labs(
            x = "Województwo",
            y = "Średni procent dzieci pod pieczą",
            title = "Procent dzieci w pieczy rodzinnej/instytucjonalnej",
            fill = "Znaczenie kolorów"
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
          )
        
        return(p)
        
    })
    

}



# Run the application 
shinyApp(ui = ui, server = server)
