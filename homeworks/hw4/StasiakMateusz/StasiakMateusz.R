library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
#library('rsconnect')

#rsconnect::setAccountInfo(name='stasiakmateusz', token='', secret='')

data_births <- read_xlsx("Noworodki pozostawione w szpitalu 2007-2023.xlsx")
data_abandoned <- read_excel("Urodzenia żywe w Polsce 2007-2023.xlsx", skip = 3)

data_abandoned <- na.omit(data_abandoned)
colnames(data_abandoned) <- c("Wojewodztwa", as.character(2007:2023))
data_abandoned$`2016` <- as.numeric(data_abandoned$`2016`)

colnames(data_births) <- c("Wojewodztwa", as.character(2007:2023))

births_long <- data_births %>%
    pivot_longer(
        cols = -Wojewodztwa,
        names_to = "Rok",
        values_to = "Zywe_Urodzenia"
    )

abandoned_long <- data_abandoned %>%
    pivot_longer(
        cols = -Wojewodztwa,
        names_to = "Rok",
        values_to = "Noworodki_Pozostawione"
    )

merged_data <- births_long %>%
    inner_join(
        abandoned_long,
        by = c("Wojewodztwa", "Rok")
    )

merged_data <- merged_data %>%
    mutate(Zywe_Urodzenia = as.numeric(Zywe_Urodzenia)) %>% 
    mutate(
        Promil_Pozostawionych = (Zywe_Urodzenia / Noworodki_Pozostawione) * 1000,
        Rok = as.numeric(Rok)
    )
str(merged_data)

ui <- fluidPage(
    titlePanel("Analiza Demograficzna: Pozostawione Noworodki w Polsce"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "wybor_wojewodztwa",
                "Wybierz Wojewodztwo:",
                choices = unique(merged_data$Wojewodztwa),
                selected = unique(merged_data$Wojewodztwa)[1]
            ),
            sliderInput(
                "zakres_lat",
                "Zakres lat:",
                min = min(merged_data$Rok),
                max = max(merged_data$Rok),
                value = c(min(merged_data$Rok), max(merged_data$Rok)),
                step = 1,
                ticks = TRUE,
                sep=""
            )
        ),
        
        mainPanel(
            plotOutput("plot_pozostawione"),
            h3("Opis:"),
            p("W mojej analizie postanowiłem przedstawić wizualizacje tego, jak ewoluowała skala zjawiska pozostawiania noworodków w szpitalu na przestrzeni lat i w różnych województwach. Moja wizualizacja pozwala użytkownikowi dobrze zrozumieć skalę tego problemu, jego historyczne tło oraz zmieniające się tendencje w zależności od regionu Polski. Dzięki temu użytkownik może zaobserwować, że problem ten jest powszechny, a jednocześnie zmienia się w różnym tempie w różnych częściach kraju. Ułatwia to głębszą analizę oraz wyciąganie wniosków na temat skuteczności polityk społecznych w zwalczaniu tego zjawiska."),
            
            h3("Komentarz:"),
            p("Ciekawy fakt widoczny w mojej aplikacji to to, jak wprowadzenie programu 500+ w 2016 roku wpłynęło na tymczasowe zmniejszenie promila noworodków pozostawionych w szpitalu. Widać to szczególnie dobrze w niektórych województwach, takich jak Podlaskie, gdzie zmiana była wyraźniejsza. Analiza ta pokazuje, że polityki społeczne mogą mieć realny wpływ na zachowania rodziców, choć warto zauważyć, że ich skutki mogą być krótkoterminowe i wymagają dalszej obserwacji oraz wsparcia. To także może skłaniać do refleksji nad różnicami międzyregionalnymi oraz nad potrzebą dostosowywania działań do specyficznych uwarunkowań lokalnych.")
            
        )
    )
)

server <- function(input, output) {
    filtrowane_dane <- reactive({
        req(input$wybor_wojewodztwa, input$zakres_lat)
        merged_data %>%
            filter(
                Wojewodztwa == input$wybor_wojewodztwa,
                Rok >= input$zakres_lat[1],
                Rok <= input$zakres_lat[2]
            )
    })
    
    output$plot_pozostawione <- renderPlot({
        ggplot(filtrowane_dane(), aes(x = Rok, y = Promil_Pozostawionych)) +
            geom_area(fill = "skyblue", alpha = 0.5) +
            geom_line(color = "navy", size = 2) +
            labs(
                title = "Część Pozostawionych Noworodków",
                x = "Rok", y = "Promil"
            ) +
            theme_classic() +
            theme(
                plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                axis.title.x = element_text(size = 14, face = "italic"),
                axis.title.y = element_text(size = 14, face = "italic")
            )
    })
}

shinyApp(ui = ui, server = server)


#rsconnect::deployApp(appDir='C:/Users/Mateu/OneDrive/Dokumenty/HW4',
# appName ="StasiakMateusz")
