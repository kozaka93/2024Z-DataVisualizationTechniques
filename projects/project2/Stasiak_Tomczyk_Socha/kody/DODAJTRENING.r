library(shiny)
library(shinyWidgets)
library(shinythemes)
library(readxl)
library(dplyr)
library(writexl)

# Wczytaj dane z pliku Excel
training_data <- read_excel("dane.xlsx")

# UI aplikacji
ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("Dodaj nowy trening"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("sportowiec", label = "Wybierz sportowca", 
                  choices = unique(training_data$Sportowiec), 
                  selected = unique(training_data$Sportowiec)[1]),
      dateInput("data_treningu", label = "Data treningu", value = Sys.Date(), format = "dd/mm/yyyy"),
      timeInput("godzina_treningu", label = "Godzina treningu", value = Sys.time()),
      selectInput("rodzaj_treningu", label = "Rodzaj treningu", choices = c("FBW", "PUSH", "PULL", "LEGS")),
      selectInput("cwiczenie", label = "Wybierz ćwiczenie", 
                  choices = c("Przysiad", "Martwy ciąg", "OHP", "Bench Press", "Wiosłowanie sztangą", "Hantle biceps")),
      numericInput("serie", label = "Serie", value = 1, min = 1),
      numericInput("powtorzenia", label = "Powtórzenia", value = 1, min = 1),
      numericInput("ciezar", label = "Ciężar (kg)", value = 1, min = 1),
      numericInput("dlugosc", label = "Długość treningu (min)", value = 1, min = 1),
      numericInput("samopoczucie", label = "Samopoczucie przed treningiem (1-10)", value = NA, min = 1, max = 10),
      numericInput("zmeczenie", label = "Poziom zmęczenia przed treningiem (1-10)", value = NA, min = 1, max = 10),
      numericInput("ilosc_snu", label = "Ilość snu poprzedniej nocy (h)", value = NA, min = 0, max = 24),
      switchInput("kawa", label = "Kawa?", value = FALSE, onLabel = "Tak", offLabel = "Nie"),
      numericInput("kroki", label = "Kroki tego dnia", value = NA),
      numericInput("waga", label = "Waga sportowca (kg)", value = NA),
      numericInput("posilki", label = "Liczba posiłków tego dnia", value = NA),
      switchInput("muzyka", label = "Muzyka podczas treningu?", value = FALSE, onLabel = "Tak", offLabel = "Nie"),
      selectInput("rodzaj_muzyki", label = "Rodzaj muzyki", choices = unique(training_data$`Rodzaj muzyki`)),
      actionButton("dodaj_trening", "Dodaj trening", class = "btn-success")
    ),
    mainPanel(
      h4("Podgląd dodawanego treningu:"),
      tableOutput("preview1"),
      tableOutput("preview2"),
      tableOutput("preview3")
    )
  )
)

# Serwer aplikacji
server <- function(input, output, session) {
  
  # Oblicz dynamiczne wartości domyślne na podstawie wybranego sportowca
  observeEvent(input$sportowiec, {
    sportowiec_data <- training_data %>% 
      filter(Sportowiec == input$sportowiec) 
    
    avg_series <- round(mean(as.numeric(sportowiec_data$Serie), na.rm = TRUE), 0)
    avg_reps <- round(mean(as.numeric(sportowiec_data$Powtórzenia), na.rm = TRUE), 0)
    avg_weight <- round(mean(as.numeric(sportowiec_data$Ciężar), na.rm = TRUE), 0)
    avg_duration <- round(mean(as.numeric(sportowiec_data$`Długość treningu (min)`), na.rm = TRUE), 0)
    
    updateNumericInput(session, "serie", value = avg_series)
    updateNumericInput(session, "powtorzenia", value = avg_reps)
    updateNumericInput(session, "ciezar", value = avg_weight)
    updateNumericInput(session, "dlugosc", value = avg_duration)
  })
  
  # Reaktywny podgląd danych
  trening_preview1 <- reactive({
    df<-data.frame(
      Sportowiec = input$sportowiec,
      'Data treningu' = as.character(format(input$data_treningu, "%d/%m/%Y")),
      'Godzina treningu' = as.character(input$godzina_treningu),
      'Rodzaj treningu' = input$rodzaj_treningu,
      'Ćwiczenie' = input$cwiczenie,
      'Serie' = input$serie,
      'Powtórzenia' = input$powtorzenia,
      'Ciężar' = input$ciezar,
      'Długość treningu (min)' = input$dlugosc
      
    )
    colnames(df)[] <- c('Sportowiec',
                        'Data treningu',
                      'Godzina treningu',
                      'Rodzaj treningu',
                      'Ćwiczenie',
                      'Serie',
                      'Powtórzenia',
                      'Ciężar',
                      'Długość treningu (min)')
    df
  })
  # Reaktywny podgląd danych
  trening_preview2 <- reactive({
    df<-data.frame(
      'Samopoczucie przed treningiem (1-10)' = input$samopoczucie,
      'Poziom zmęczenia przed treningiem (1-10)' = input$zmeczenie,
      'Ilość snu poprzedniej nocy (h)' = input$ilosc_snu
    )
    colnames(df) <- c('Samopoczucie przed treningiem (1-10)',
                      'Poziom zmęczenia przed treningiem (1-10)',
                      'Ilość snu poprzedniej nocy (h)')
    df
  })
  trening_preview3 <- reactive({
    df <- data.frame(
      'Czy kawa przed treningiem (tak/nie)' = ifelse(input$kawa, "tak", "nie"),
      'Kroki tego dnia' = input$kroki,
      'Waga sportowca (kg)' = input$waga,
      'Liczba posiłków tego dnia' = input$posilki,
      'Muzyka podczas treningu (0/1)' = ifelse(input$muzyka, 1, 0),
      'Rodzaj muzyki' = input$rodzaj_muzyki
    )
    
    # Zapisz kolumny bez zmiany
    colnames(df) <- c('Czy kawa przed treningiem (tak/nie)', 'Kroki tego dnia', 'Waga sportowca (kg)', 
                      'Liczba posiłków tego dnia', 'Muzyka podczas treningu (0/1)', 'Rodzaj muzyki')
    
    df
  })
  output$preview1 <- renderTable({
    trening_preview1()
  })
  
  output$preview2 <- renderTable({
    trening_preview2()
  })
  output$preview3 <- renderTable({
    trening_preview3()
  })
  
  
  # Dodawanie treningu do danych
  observeEvent(input$dodaj_trening, {
    trening_preview<-cbind(as.data.frame(trening_preview1()),
                           as.data.frame(trening_preview2()),
                           as.data.frame(trening_preview3()))

    
    training_data <<- bind_rows(training_data, trening_preview)
    write_xlsx(training_data, "dane.xlsx")
    showModal(modalDialog(
      title = "Sukces!",
      "Nowy trening został dodany do bazy danych.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })
}

# Uruchomienie aplikacji
shinyApp(ui, server)
