library(readxl)
library(dplyr)
library(shiny)
library(ggplot2)
library(lubridate)

# Wczytanie danych
data <- as.data.frame(read_xlsx("dane.xlsx"))

data <- data %>%
    rename(Data_treningu = `Data treningu`) 

# Modyfikacje danych
data <- data %>%
    mutate(
        Ćwiczenie = tolower(Ćwiczenie),
        Ćwiczenie = case_when(
            Ćwiczenie == "bench press" ~ "wyciskanie sztangą",
            Ćwiczenie == "przysiady na bramie" ~ "przysiad",
            TRUE ~ Ćwiczenie 
        ),
        `Rodzaj treningu` = toupper(`Rodzaj treningu`),  # Poprawna referencja kolumny
        Data_treningu = dmy(Data_treningu)  # Konwersja kolumny Data_treningu do formatu daty
    ) %>%
    select(-`Muzyka podczas treningu (0/1)`)


#View(data)
# Tworzenie ramki danych
repetitions <- 1:20
percent <- c(
    1.00, 0.95, 0.93, 0.90, 0.87, 0.85, 0.83, 0.80, 0.77, 0.75, 
    0.73, 0.70, 0.68, 0.65, 0.63, 0.60, 0.58, 0.55, 0.53, 0.50
)

# Zapisanie do ramki danych
rm_table <- data.frame(repetitions, percent) 

data <- data %>%
    left_join(rm_table,by=c("Powtórzenia"="repetitions")) %>% 
    mutate("rm"=(1/percent)*Ciężar)

View(data)


# Interfejs użytkownika
ui <- fluidPage(
    titlePanel("Maksymalne powtórzenie"),
    sidebarLayout(
        sidebarPanel(
            selectInput("Ćwiczenie", "Wybierz ćwiczenie:", 
                        choices = unique(data$Ćwiczenie))
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Mateusz", 
                         plotOutput("plot_mateusz1"),
                         plotOutput("plot_mateusz2")
                ),
                tabPanel("Karol", 
                         plotOutput("plot_karol1"),
                         plotOutput("plot_karol2")
                ),
                tabPanel("Kacper", 
                         plotOutput("plot_kacper1"),
                         plotOutput("plot_kacper2")
                )
            )
        )
    )
)

# Serwer
server <- function(input, output) {
    # Filtruj dane na podstawie wybranego ćwiczenia
    filtered_data <- reactive({
        data %>% filter(Ćwiczenie == input$Ćwiczenie)
    })
    
    # Wykresy dla Mateusza
    output$plot_mateusz1 <- renderPlot({
        filtered_data() %>%
            filter(Sportowiec == "Mateusz") %>%
            ggplot(aes(x = Ciężar, y = Powtórzenia, color = Data_treningu)) +
            geom_point(size = 8) + # Zwiększona grubość kropek
            scale_color_gradientn(
                colors = c("gold", "orange", "red"), # Kolory gradientu
                name = "Data treningu", 
                breaks = c(
                    as.Date("2024-12-05"), 
                    as.Date("2024-12-26"), # Środkowa data między 5/12/2024 a 7/01/2025
                    as.Date("2025-01-07")
                ), 
                labels = format(
                    c(
                        as.Date("2024-12-05"), 
                        as.Date("2024-12-26"), # Środkowa data
                        as.Date("2025-01-07")
                    ), 
                    "%d/%m/%Y"
                ) # Formatowanie etykiet
            ) +
            ggtitle("Mateusz - Wykres 1") +
            theme_minimal() +
            theme(
                legend.title = element_text(vjust = 5) # Przesunięcie tytułu legendy wyżej
            )
    })
    output$plot_mateusz2 <- renderPlot({
        ggplot(filtered_data() %>% filter(Sportowiec == "Mateusz"), aes(x = Data_treningu, y = rm)) +
            geom_line(color = "blue") +
            geom_point(size = 2, color = "blue") +
            scale_x_date(date_labels = "%d-%b", date_breaks = "1 week") +
            labs(
                title = "1 Rep Max Progression",
                x = "Date",
                y = "1 Rep Max (kg)"
            ) +
            theme_minimal() +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1)
            )
    })
    
    # Wykresy dla Karola
    output$plot_karol1 <- renderPlot({
        filtered_data() %>%
            filter(Sportowiec == "Karol") %>%
            ggplot(aes(x = Ciężar, y = Powtórzenia, color = Data_treningu)) +
            geom_point(size = 8) + # Zwiększona grubość kropek
            scale_color_gradientn(
                colors = c("gold", "orange", "red"), # Kolory gradientu
                name = "Data treningu", 
                breaks = c(
                    as.Date("2024-12-05"), 
                    as.Date("2024-12-26"), # Środkowa data między 5/12/2024 a 7/01/2025
                    as.Date("2025-01-07")
                ), 
                labels = format(
                    c(
                        as.Date("2024-12-05"), 
                        as.Date("2024-12-26"), # Środkowa data
                        as.Date("2025-01-07")
                    ), 
                    "%d/%m/%Y"
                ) # Formatowanie etykiet
            ) +
            ggtitle("Karol - Wykres 1") +
            theme_minimal() +
            theme(
                legend.title = element_text(vjust = 5) # Przesunięcie tytułu legendy wyżej
            )
    })
    output$plot_karol2 <- renderPlot({
        ggplot(filtered_data() %>% filter(Sportowiec == "Karol"), aes(x = Data_treningu, y = rm)) +
            geom_line(color = "blue") +
            geom_point(size = 2, color = "blue") +
            scale_x_date(date_labels = "%d-%b", date_breaks = "1 week") +
            labs(
                title = "1 Rep Max Progression",
                x = "Date",
                y = "1 Rep Max (kg)"
            ) +
            theme_minimal() +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1)
            )
    })
    
    # Wykresy dla Kacpra
    output$plot_kacper1 <- renderPlot({
        filtered_data() %>%
            filter(Sportowiec == "Kacper") %>%
            ggplot(aes(x = Ciężar, y = Powtórzenia, color = Data_treningu)) +
            geom_point(size = 8) + # Zwiększona grubość kropek
            scale_color_gradientn(
                colors = c("gold", "orange", "red"), # Kolory gradientu
                name = "Data treningu", 
                breaks = c(
                    as.Date("2024-12-05"), 
                    as.Date("2024-12-26"), # Środkowa data między 5/12/2024 a 7/01/2025
                    as.Date("2025-01-07")
                ), 
                labels = format(
                    c(
                        as.Date("2024-12-05"), 
                        as.Date("2024-12-26"), # Środkowa data
                        as.Date("2025-01-07")
                    ), 
                    "%d/%m/%Y"
                ) # Formatowanie etykiet
            ) +
            ggtitle("Kacper - Wykres 1") +
            theme_minimal() +
            theme(
                legend.title = element_text(vjust = 5) # Przesunięcie tytułu legendy wyżej
            )
    })
    output$plot_kacper2 <- renderPlot({
        ggplot(filtered_data() %>% filter(Sportowiec == "Kacper"), aes(x = Data_treningu, y = rm)) +
            geom_line(color = "blue") +
            geom_point(size = 2, color = "blue") +
            scale_x_date(date_labels = "%d-%b", date_breaks = "1 week") +
            labs(
                title = "1 Rep Max Progression",
                x = "Date",
                y = "1 Rep Max (kg)"
            ) +
            theme_minimal() +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1)
            )
    })
}

# Uruchom aplikację
shinyApp(ui = ui, server = server)
