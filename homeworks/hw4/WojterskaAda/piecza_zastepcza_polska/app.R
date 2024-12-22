# Load required libraries
library(shiny)
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)
library(showtext)

# Ścieżka do pliku czcionki
font_add("MyFont", "TenorSans-Regular.ttf")

# Włącz obsługę showtext
showtext_auto()

#setwd("C:/Users/adawo/OneDrive/Desktop/BI_NGO/piecza_zastepcza_polska")

source("wczytywanie_pieczazastepcza_r.R")
source("wczytywanie_pieczazastepcza_i.R")
source("wczytywanie_dzieci.R")
source("wczytywanie_wiek_i.R")
source("wczytywanie_wiek_r.R")

ui <- fluidPage(
  
  # Dodajemy styl CSS
  tags$head(
      tags$style(HTML("
      
        @font-face {
        font-family: 'MyFont';
        src: url('TenorSans-Regular.ttf') format('truetype');
        }
      
  body {
    background-color: #fbf7f4;
    font-family: Arial, sans-serif;
  }
  .container {
    max-width: 1200px;
    margin: auto;
  }
  .panel {
    background-color: #ffffff;
    border-radius: 10px;
    padding: 20px;
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
    margin-bottom: 30px;
  }
  .plot-container {
    border: 1px solid #d3d3d3;
    border-radius: 10px;
    padding: 20px;
    background-color: #fafafa;
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2); /* Dodano cień */
  }
  .select-input {
    background-color: #ffffff;
    width: 100%;
    padding: 10px;
    font-size: 16px;
    border-radius: 5px;
    border: 1px solid #ccc;
  }
  .header-bar {
    background-color: #994974; /* Kolor paska */
    color: white;
    padding: 15px;
    text-align: center;
    width: 100%; /* Ustalamy szerokość na 100% */
    margin: 0; /* Usuwamy marginesy, by paski sięgały krawędzi */
    border-radius: 10px 10px 10px 10px; /* Zaokrąglone rogi na górze */
    margin-bottom: 0px;
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2); /* Dodano cień */
    font-family: 'MyFont'; /* Czcionka dla header-bar */
  }
  .header-subtitle {
    font-size: 16px;
    font-style: italic;
    margin-top: 5px;
    color: #f0f0f0; /* Kolor tekstu podtytułu */
  }
  .subheader-bar {
    background-color: #484780; /* Kolor paska dla procentowego udziału */
    color: white;
    padding: 15px;
    text-align: center;
    border-radius: 10px 10px 10px 10px; /* Zaokrąglone rogi na górze */
    margin-top: 30px;
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2); /* Dodano cień */
    font-family: 'MyFont';
  }
"))
  ),
  
  # Pasek nad wykresami - tytuł sekcji z podtytułem
  div(class = "header-bar", 
      tags$h3("WYCHOWANKOWIE PIECZY ZASTĘPCZEJ W POLSCE"), 
      div(class = "header-subtitle", "Dane z lat 2014-2023")
  ),
  
  # Pierwszy subheader - przed pierwszym wykresem
  div(class = "subheader-bar", 
      tags$h3("Liczba wychowanków pieczy zastępczej w Polsce")
  ),
  
  # Layout z paskiem bocznym i główną sekcją wykresów
  sidebarLayout(
    sidebarPanel(
      selectInput("wartosc_select", 
                  label = "Wybierz rodzaj pieczy zastępczej:",
                  choices = c("Piecza zastępcza rodzinna" = "WartoscR", 
                              "Piecza zastępcza instytucjonalna" = "WartoscI", 
                              "Razem" = "Suma"),
                  selected = "Suma")  # Domyślny wybór
    ),
    
    mainPanel(
      fluidRow(
        br(),
        column(6,  # Pierwszy wykres - wykres liniowy
               div(class = "plot-container", plotOutput("line_plot"))),
        column(6,  # Drugi wykres - procentowa zmiana
               div(class = "plot-container", plotOutput("percent_change_plot")))
      )
    )
  ),
  
  div(class = "subheader-bar", 
      tags$h3("Procentowy udział pieczy zastępczej i instytucjonalnej w Polsce")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("region_select", 
                  label = "Wybierz region:", 
                  choices = c("Polska" = "POLSKA", 
                              "dolnośląskie", "kujawsko-pomorskie", "lubelskie", "lubuskie", "łódzkie", 
                              "małopolskie", "mazowieckie", "opolskie", "podkarpackie", "podlaskie", 
                              "pomorskie", "śląskie", "świętokrzyskie", "warmińsko-mazurskie", 
                              "wielkopolskie", "zachodniopomorskie"),
                  selected = "Polska"),  # Domyślnie POLSKA
    ),
    
    mainPanel(
      fluidRow(
        br(),
        column(12,
               div(class = "plot-container", plotOutput("share_plot")))
      )
    )
  ),
  
  div(class = "subheader-bar", 
      tags$h3("Wiek wychowanków pieczy zastępczej w Polsce w 2023 roku")
  ),
  
  # Layout z paskiem bocznym i główną sekcją wykresów
  sidebarLayout(
    sidebarPanel(
      selectInput("region_select2", 
                  label = "Wybierz region:", 
                  choices = c("Polska", 
                              "dolnośląskie", "kujawsko-pomorskie", "lubelskie", "lubuskie", "łódzkie", 
                              "małopolskie", "mazowieckie", "opolskie", "podkarpackie", "podlaskie", 
                              "pomorskie", "śląskie", "świętokrzyskie", "warmińsko-mazurskie", 
                              "wielkopolskie", "zachodniopomorskie"),
                  selected = "Polska"),  # Domyślnie POLSKA
    ),
    
    mainPanel(
      fluidRow(
        br(),
        column(12,  # Trzeci wykres - stacked bar plot
               div(class = "plot-container", plotOutput("wiek_plot")))
      ),
      br()
    )
  ),
  
)


server <- function(input, output) {
  
  file_path <- "Wychowankowie (0-24 lata) w pieczy zastępczej 2014-2023.xlsx"
  file_path2 <- "Liczba osób w wieku 0-24 lata w Polsce, 2014-2023.xlsx"
  file_path3 <- "wiek.xlsx"
  
  
  data1 <- reactive({
    wczytywanie_pieczazastepcza_r(file_path)  
  })
  
  data2 <- reactive({
    wczytywanie_pieczazastepcza_i(file_path) 
  })
  
  data3 <- reactive({
    wczytywanie_dzieci(file_path2)  
  })
  
  data4 <- reactive({
    wczytywanie_wiek_i(file_path3)  
  })
  
  data5 <- reactive({
    wczytywanie_wiek_r(file_path3) 
  })
  
  data_combined <- reactive({
    pieczazastepcza_r <- data1()
    pieczazastepcza_i <- data2()
    dzieci <- data3()
    
    # Łączymy oba zbiory danych (Join po 'Rok' i 'Region')
    combined_data <- inner_join(pieczazastepcza_r, pieczazastepcza_i, by = c("Rok", "Region"))
    combined_data <- inner_join(combined_data, dzieci, by = c("Rok", "Region"))
      
    
    combined_data <- combined_data %>%
      mutate(Suma = WartoscR + WartoscI)
    
    combined_data <- combined_data %>%
      mutate(
        WartoscR_10000 = (WartoscR / Dzieci) * 10000, 
        WartoscI_10000 = (WartoscI / Dzieci) * 10000,
        Suma_10000 = (Suma / Dzieci) * 10000
      )
    
    # Zwracamy połączone dane
    return(combined_data)
  })
  
  
  output$line_plot <- renderPlot({
    pieczazastepcza <- data_combined()
    
    polska_pieczazastepcza <- pieczazastepcza %>%
      filter(Region == "POLSKA")
    
    polska_pieczazastepcza$Rok <- as.numeric(polska_pieczazastepcza$Rok)
    
    wartosc_col <- input$wartosc_select
    
    # Zmieniamy na odpowiednią kolumnę po przeliczeniu na 10000 dzieci
    wartosc_col_10000 <- paste0(wartosc_col, "_10000")
    polska_pieczazastepcza[[wartosc_col_10000]] <- as.numeric(polska_pieczazastepcza[[wartosc_col_10000]])
    
    ggplot(polska_pieczazastepcza, aes_string(x = "Rok", y = wartosc_col_10000)) + 
      geom_line(color = "#ed6d8f") +  # Kolor linii
      geom_point(color = "#484780") +  # Kolor punktów
      labs(title = paste("Liczba wychowanków pieczy zastępczej \n w Polsce na 10000 dzieci \n"), 
           x = "Rok", y = "Liczba wychowanków na 10000 dzieci") +
      scale_y_continuous(limits = c(0, 100),labels = scales::comma) +  
      theme_minimal()+
      theme(
        plot.background = element_rect(fill = "#fafafa", color = NA),  # Tło wykresu
        panel.background = element_rect(fill = "#fafafa", color = NA),  # Tło panelu wykresu
        panel.grid.major = element_line(color = "#d3d3d3"),  # Kolor linii siatki
        panel.grid.minor = element_line(color = "#e0e0e0"),  # Kolor mniejszych linii siatki
        axis.text = element_text(color = "#333333", size = 18),  # Kolor tekstu na osiach
        axis.title = element_text(color = "#333333", size = 18),  # Kolor tytułów osi
        plot.title = element_text(hjust = 0.5, color = "#333333", size = 22),
        text = element_text(family = "MyFont")
      )
    
    
  })
  
  # Obliczamy procentową zmianę rok do roku
  output$percent_change_plot <- renderPlot({
    pieczazastepcza <- data_combined()
    
    # Filtrujemy dane dla Polski
    polska_pieczazastepcza <- pieczazastepcza %>%
      filter(Region == "POLSKA")
    
    polska_pieczazastepcza$Rok <- as.numeric(polska_pieczazastepcza$Rok)
    
    wartosc_col <- input$wartosc_select
    
    # Zmieniamy na odpowiednią kolumnę po przeliczeniu na 10000 dzieci
    wartosc_col_10000 <- paste0(wartosc_col, "_10000")
    
    # Obliczamy procentową zmianę rok do roku dla przeliczonej wartości
    polska_pieczazastepcza <- polska_pieczazastepcza %>%
      arrange(Rok) %>%
      mutate(PercentChange = (get(wartosc_col_10000) - lag(get(wartosc_col_10000))) / lag(get(wartosc_col_10000)) * 100)
    
    ggplot(polska_pieczazastepcza, aes(x = Rok, y = PercentChange)) + 
      geom_line(color = "#ed6d8f") +  # Kolor linii
      geom_point(color = "#484780") +  # Kolor punktów
      labs(title = paste("Procentowa zmiana \n liczby wychowanków pieczy zastępczej \n w Polsce roku do roku \n"), 
           x = "Rok", y = "Procentowa zmiana liczby wychowanków(%)") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.7) +  # Linia przez 0
      scale_y_continuous(limits = c(-8, 8), labels = scales::comma) +  
      theme_minimal()+
      theme(
        plot.background = element_rect(fill = "#fafafa", color = NA),  # Tło wykresu
        panel.background = element_rect(fill = "#fafafa", color = NA),  # Tło panelu wykresu
        panel.grid.major = element_line(color = "#d3d3d3"),  # Kolor linii siatki
        panel.grid.minor = element_line(color = "#e0e0e0"),  # Kolor mniejszych linii siatki
        axis.text = element_text(color = "#333333", size = 18),  # Kolor tekstu na osiach
        axis.title = element_text(color = "#333333", size = 18),  # Kolor tytułów osi
        plot.title = element_text(hjust = 0.5, color = "#333333", size = 22),
        text = element_text(family = "MyFont")
      )
    
  })
  
  output$share_plot <- renderPlot({
    pieczazastepcza <- data_combined()
    
    region_selected <- input$region_select
    region_data <- pieczazastepcza %>%
      filter(Region == region_selected)
    
    region_data <- region_data %>%
      mutate(ShareR = WartoscR / Suma * 100, 
             ShareI = WartoscI / Suma * 100)  
    
    region_data_long <- region_data %>%
      select(Rok, ShareR, ShareI) %>%
      pivot_longer(cols = c(ShareR, ShareI), 
                   names_to = "Typ_pieczy", 
                   values_to = "Procentowy_udzial") 
    
    ggplot(region_data_long, aes(x = Rok, y = Procentowy_udzial, fill = Typ_pieczy)) +
      geom_bar(stat = "identity", position = "stack") +  
      scale_fill_manual(
        values = c("ShareR" = "#994974", "ShareI" = "#484780"), 
        labels = c("Rodzinna", "Instytucjonalna")  # Update the legend labels here
      ) +
      labs(title = paste("Procentowy udział pieczy rodzinnej i instytucjonalnej w danym regionie"), 
           x = "Rok", y = "Procentowy udział (%)", fill = "Typ pieczy:") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#fafafa", color = NA),
        panel.background = element_rect(fill = "#fafafa", color = NA),
        panel.grid.major = element_line(color = "#d3d3d3"),
        panel.grid.minor = element_line(color = "#e0e0e0"),
        axis.text = element_text(color = "#333333", size = 18),
        axis.title = element_text(color = "#333333", size = 18),
        plot.title = element_text(hjust = 0.5, color = "#333333", size = 22),
        legend.title = element_text(color = "#333333", size = 18),
        legend.text = element_text(color = "#333333", size = 18),
        text = element_text(family = "MyFont")
      )
  })
  
  
  output$wiek_plot <- renderPlot({
    
    wiekI <- data4()  # Dane pieczy instytucjonalnej
    wiekR <- data5()  # Dane pieczy rodzinnej
    
    #Połączenie danych po kolumnach Region i Rok
    combined_data2 <- left_join(wiekI, wiekR, by = c("Region", "Wiek"))
    
    # Upewniamy się, że dane mają odpowiednie nazwy kolumn
    colnames(combined_data2) <- c("Region", "Wiek", "WartoscIns", "WartoscR")
    # Uporządkowanie kolumn "Wiek" w odpowiedniej kolejności
    
    wiek_levels <- c("ogółem","0", "1—3", "4—6", "7—13", "14—17", "18—24")
    
    # Ustawiamy porządek kategorii w kolumnie "Wiek"
    combined_data2$Wiek <- factor(combined_data2$Wiek, levels = wiek_levels)
    
    region_selected2 <- input$region_select2
    
    region_data2 <- combined_data2 %>%
      filter(Region == region_selected2) %>%
      filter(Wiek != "ogółem") 
    
    region_data2_long <- region_data2 %>%
      pivot_longer(cols = c(WartoscIns, WartoscR), 
                   names_to = "Typ_pieczy", 
                   values_to = "Wartosc") %>%
      mutate(Typ_pieczy = recode(Typ_pieczy, 
                                 "WartoscIns" = "Instytucjonalna", 
                                 "WartoscR" = "Rodzinna"))
    
    # Tworzymy wykres z dwoma słupkami obok siebie
    ggplot(region_data2_long, aes(x = Wiek, y = Wartosc, fill = Typ_pieczy)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Słupki obok siebie
      labs(title = paste("Liczba dzieci w pieczy rodzinnej i instytucjonalnej w regionie \n w podziale na grupy wiekowe"),
           x = "Przedział wiekowy", y = "Wartość", fill = "Typ pieczy") +
      scale_fill_manual(values = c("Instytucjonalna" = "#994974", "Rodzinna" = "#484780")) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#fafafa", color = NA),
        panel.background = element_rect(fill = "#fafafa", color = NA),
        panel.grid.major = element_line(color = "#d3d3d3"),
        panel.grid.minor = element_line(color = "#e0e0e0"),
        axis.text = element_text(color = "#333333", size = 18),
        axis.title = element_text(color = "#333333", size = 18),
        plot.title = element_text(hjust = 0.5, color = "#333333", size = 22),
        legend.title = element_text(color = "#333333", size = 18),
        legend.text = element_text(color = "#333333", size = 18),
        text = element_text(family = "MyFont")
      )
    
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
