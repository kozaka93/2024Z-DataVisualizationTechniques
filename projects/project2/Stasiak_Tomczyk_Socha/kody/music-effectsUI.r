library(readxl)
library(dplyr)
library(shiny)
library(ggplot2)
library(lubridate)

data <- as.data.frame(read_xlsx("dane.xlsx"))

data <- data %>%
    rename(Data_treningu = 'Data treningu') %>% 
    rename(Muzyka = 'Rodzaj muzyki')

# Modyfikacje danych
data <- data %>%
    mutate(
        Ćwiczenie = tolower(Ćwiczenie),
        Ćwiczenie = case_when(
            Ćwiczenie == "bench press" ~ "wyciskanie sztangą",
            Ćwiczenie == "przysiady na bramie" ~ "przysiad",
            TRUE ~ Ćwiczenie 
        ),
        'Rodzaj treningu' = toupper('Rodzaj treningu'),
        Data_treningu = dmy(Data_treningu)
    ) %>%
    select(-`Muzyka podczas treningu (0/1)`)

repetitions <- 1:20
percent <- c(
    1.00, 0.95, 0.93, 0.90, 0.87, 0.85, 0.83, 0.80, 0.77, 0.75, 
    0.73, 0.70, 0.68, 0.65, 0.63, 0.60, 0.58, 0.55, 0.53, 0.50
)

rm_table <- data.frame(repetitions, percent) 

data <- data %>%
    left_join(rm_table, by = c("Powtórzenia" = "repetitions")) %>% 
    mutate("rm" = (1 / percent) * Ciężar)

training_data1 <- data

ui <- fluidPage(
    titlePanel("Analiza treningów sportowców"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "selected_sportowiec",
                "Wybierz sportowca:",
                choices = unique(training_data1$Sportowiec),
                selected = unique(training_data1$Sportowiec)[1]
            ),
            uiOutput("music_selector")
        ),
        mainPanel(
            plotOutput("music_histogram")
        )
    )
)

server <- function(input, output, session) {
    observeEvent(input$selected_sportowiec, {
        sportowiec_data <- training_data1 %>%
            filter(Sportowiec == input$selected_sportowiec)
        
        updateSelectInput(
            session,
            "selected_muzyka",
            choices = unique(sportowiec_data$Muzyka),
            selected = unique(sportowiec_data$Muzyka)[1]
        )
    })
    
    output$music_selector <- renderUI({
        sportowiec_data <- training_data1 %>%
            filter(Sportowiec == input$selected_sportowiec)
        
        selectInput(
            "selected_muzyka",
            "Wybierz rodzaj muzyki:",
            choices = unique(sportowiec_data$Muzyka),
            selected = unique(sportowiec_data$Muzyka)[1]
        )
    })
    
    
    output$music_histogram <- renderPlot({
        filtered_music <- training_data1 %>%
            filter(Sportowiec == input$selected_sportowiec, Muzyka == input$selected_muzyka)
        
        ggplot(filtered_music, aes(x = rm)) +
            geom_histogram(binwidth = 10, fill = "steelblue", color = "black") +
            labs(
                title = paste("Histogram dla sportowca:", input$selected_sportowiec, "i muzyki:", input$selected_muzyka),
                x = "rm",
                y = "Liczba wystąpień"
            ) +
            theme_minimal()
    })
}

shinyApp(ui = ui, server = server)