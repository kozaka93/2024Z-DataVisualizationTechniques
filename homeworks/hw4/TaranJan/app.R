# ustawienie folderu roboczego
# setwd("ścieżka/do/folderu_z_app.R")


library(shiny)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(shinythemes)


ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel(windowTitle = "Analiza danych dzieci opuszczonych",
    title = div(
      icon("chart-line", class = "fa-2x"),
      "Analiza danych dzieci opuszczonych"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("year",
                  "Wybierz rok:",
                  min = 2014,
                  max = 2023,
                  value = 2015),
      
      selectInput(
        inputId = "category_filter",
        label = "Wybierz województwo:",
        choices = NULL
      ),
      
      plotOutput("wojMap")
    ),
    
    mainPanel(
      fluidRow(
        column(6, plotOutput("mapPlot")),
        column(6, plotOutput("mapSecondPlot"))
      ),
      fluidRow(
        column(6, plotOutput("linPlot")),
        column(6, plotOutput("linSecondPlot"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ## Wczytywanie danych
  
  #Piecza zastępcza
  wychowankowieRodzina <- 
    read.xlsx("DANE/Piecza zastępcza/Wychowankowie (0-24 lata) w pieczy zastępczej 2014-2023.xlsx", 
              sheet = "Wychowankowie rodzinnej pieczy")
  
  wychowankowieDomDziecka <- 
    read.xlsx("DANE/Piecza zastępcza/Wychowankowie (0-24 lata) w pieczy zastępczej 2014-2023.xlsx", 
              sheet = "Wychowankowie instytucjonalnej")
  
  dzieciPonizej18 <- 
    read.xlsx("DANE/Piecza zastępcza/Liczba dzieci 0-18 lat w Polsce 2014-2023.xlsx", 
              sheet = "Liczba dzieci 0-18 w Polsce")
  
  dzieciPonizej24 <-
    read.xlsx("DANE/Piecza zastępcza/Liczba osób w wieku 0-24 lata w Polsce, 2014-2023.xlsx", 
              sheet = "Liczba osób 0-24 lata w Polsce")
  
  #dzieci urodzone i te które od razu zostały porzucone
  niemowlecia <- 
    read.xlsx("DANE/Noworodki opuszczone przez rodziców/Urodzenia żywe w Polsce 2007-2023.xlsx", 
              sheet = "Urodzenia żywe 2007-2023")
  
  niemowleciaPorzucone <-
    read.xlsx("DANE/Noworodki opuszczone przez rodziców/Noworodki pozostawione w szpitalu 2007-2023.xlsx", 
              sheet = "Noworodki pozostawione w szpita", startRow = 8, rows = 8:25)
  
  
  wychowankowieDomDzieckaLonger <- wychowankowieDomDziecka %>% 
    pivot_longer(cols = -1, 
                 names_to = "Rok", values_to = "LiczbaWychowankowDomDziecka") %>% 
    mutate(Rok = as.numeric(Rok))
  
  wychowankowieRodzinaLonger <- wychowankowieRodzina %>% 
    pivot_longer(cols = -1, 
                 names_to = "Rok", values_to = "LiczbaWychowankowRodzina") %>% 
    mutate(Rok = as.numeric(Rok))
  
  dzieciPonizej18Longer <- dzieciPonizej18 %>% 
    pivot_longer(cols = -1, 
                 names_to = "Rok", values_to = "LiczbaDzieciPonizej18") %>% 
    mutate(Rok = as.numeric(Rok))
  
  dzieciPonizej24Longer <- dzieciPonizej24 %>% 
    pivot_longer(cols = -1, 
                 names_to = "Rok", values_to = "LiczbaDzieciPonizej24") %>% 
    mutate(Rok = as.numeric(Rok))
  
  niemowleciaLonger <- niemowlecia %>% 
    pivot_longer(cols = -1,
                 names_to = "Rok", values_to = "LiczbaUrodzonychNiemowleci") %>% 
    mutate(Rok = as.numeric(Rok))
  
  niemowleciaPorzuconeLonger <- niemowleciaPorzucone %>% 
    pivot_longer(cols = -1,
                 names_to = "Rok", values_to = "LiczbaPorzuconychNiemowleci") %>% 
    mutate(Rok = as.numeric(Rok))
  
  wychowankowie <- wychowankowieDomDzieckaLonger %>% 
    inner_join(wychowankowieRodzinaLonger) %>% 
    inner_join(dzieciPonizej18Longer) %>% 
    inner_join(dzieciPonizej24Longer) %>% 
    inner_join(niemowleciaLonger) %>% 
    inner_join(niemowleciaPorzuconeLonger)
  
  #wcztywanie granic województw 
  wojewodztwa <- st_read("DANE/wojewodztwa/wojewodztwa.shp")
  
  mapa_dane <- wojewodztwa %>%
    left_join(wychowankowie, by = c("JPT_NAZWA_" = "Województwo"))
    
  # jeden theme używamy na plotach
  theme_custom <- theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )
  
  
  observe({
    updateSelectInput(session, "category_filter",
                      choices = unique(mapa_dane$JPT_NAZWA_),
                      selected = unique(mapa_dane$JPT_NAZWA_)[1])
  })
  
  #mapa województw wizualizująca nazwy i wybrane województwo
  output$wojMap <- renderPlot({
    wojewodztwo <- input$category_filter
    
    wojewodztwa %>%
      ggplot() +
      geom_sf(fill = ifelse(wojewodztwa$JPT_NAZWA_ == wojewodztwo, "lightgreen", "white"),
              color = "black") +
      geom_sf_text(aes(label = JPT_NAZWA_),
                   colour = "black",
                   fontface = ifelse(wojewodztwa$JPT_NAZWA_ == wojewodztwo, "bold", "plain"),
                   size = 4) +
      theme_minimal()
  })
  
  
  #mapa po lewej stronie, przedstawia opuszczone dzieci
  output$mapPlot <- renderPlot({
    year <- input$year
    #Ilość dzieci opuszczonych *100 / ilość dzieci
    mapa_dane %>% 
      filter(Rok == year) %>%
      mutate(Value = (LiczbaWychowankowDomDziecka+LiczbaWychowankowRodzina)*100/
               (LiczbaDzieciPonizej18)) %>% 
      ggplot() +
      geom_sf(aes(fill = Value)) +
      geom_sf_text(aes(label = round(Value, 3)), 
                   colour = "red",
                   size = 4, 
                   fontface = "bold") +
      scale_fill_gradient(low = "white", high = "black") +
      theme_custom +
      labs(title = "Procent opuszczonych\n wśród niepełnoletnich", fill = "Procent")
  })
  
  #mapa po prawej stronie przedstawia porzucone niemowlęcia
  output$mapSecondPlot <- renderPlot({
    year <- input$year
    mapa_dane %>% 
      filter(Rok == year) %>%
      mutate(Value = (LiczbaPorzuconychNiemowleci)*100/
               (LiczbaUrodzonychNiemowleci)) %>% 
      ggplot() +
      geom_sf(aes(fill = Value)) +
      geom_sf_text(aes(label = round(Value, 3)), 
                   colour = "red",
                   size = 4, 
                   fontface = "bold") +
      scale_fill_gradient(low = "white", high = "black") +
      theme_custom +
      labs(title = "Procent porzuconych niemowląt\n zaraz po urodzeniu", fill = "Procent")
  })
  
  # Wykres liniowy odpowiadający lewej mapie
  output$linPlot <- renderPlot({
    wojewodztwo <- input$category_filter
    
    mapa_dane %>%
      filter(JPT_NAZWA_ == wojewodztwo) %>%
      mutate(Value = (LiczbaWychowankowDomDziecka+LiczbaWychowankowRodzina)*100/
               (LiczbaDzieciPonizej18)) %>%
      ggplot(aes(x = Rok, y = Value)) +
      geom_line(color = "blue", linewidth = 2) +
      geom_point(size = 3) +
      xlim(c(2014, 2023)) +
      ylim(c(0, 1.65)) +
      theme_custom +
      labs(title = paste("Procent opuszczonych niepełnoletnich w ciągu lat dla:\n", 
                         wojewodztwo), x = "Rok", y = "Procent")
  })
  
  #Wykres liniowy odpowiadający prawej mapie
  output$linSecondPlot <- renderPlot({
    wojewodztwo <- input$category_filter
    
    mapa_dane %>%
      filter(JPT_NAZWA_ == wojewodztwo) %>%
      mutate(Value = (LiczbaPorzuconychNiemowleci)*100/
                    (LiczbaUrodzonychNiemowleci)) %>%
      ggplot(aes(x = Rok, y = Value)) +
      geom_line(color = "blue", linewidth = 2) +
      geom_point(size = 3) +
      xlim(c(2014, 2023)) +
      ylim(c(0, 1)) +
      theme_custom +
      labs(title = paste("Procent porzuconych niemowląt w ciągu lat dla:\n", 
                         wojewodztwo), x = "Rok", y = "Procent")
  })
}


shinyApp(ui = ui, server = server)
