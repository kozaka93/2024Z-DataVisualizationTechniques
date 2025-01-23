library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)

df <- as.data.frame(read_xlsx("dane.xlsx"))

colnames(df) <- colnames(df) %>%
  tolower() %>%           
  gsub(" ", "_", .) %>% 
  gsub("[()]", "", .) %>% 
  gsub("/", "_", .)

df <- df %>%
  mutate(
    ćwiczenie = tolower(ćwiczenie),
    ćwiczenie = case_when(
      ćwiczenie == "bench press" ~ "wyciskanie sztangą",
      ćwiczenie == "przysiady na bramie" ~ "przysiad",
      TRUE ~ ćwiczenie 
    ),
    rodzaj_treningu = toupper(rodzaj_treningu)
  ) %>% 
  select(-muzyka_podczas_treningu_0_1) %>% 
  mutate(godzina_treningu =  case_when(data_treningu == "05/12/2024" & ćwiczenie == 'ohp' ~ '15:00',
                                       TRUE ~ godzina_treningu)) %>% 
  rename(sen = 'ilość_snu_poprzedniej_nocy_h',
         samopoczucie = 'samopoczucie_przed_treningiem_1-10')


#View(data)
# Tworzenie ramki danych
repetitions <- 1:20
percent <- c(
  1.00, 0.95, 0.93, 0.90, 0.87, 0.85, 0.83, 0.80, 0.77, 0.75, 
  0.73, 0.70, 0.68, 0.65, 0.63, 0.60, 0.58, 0.55, 0.53, 0.50
)

# Zapisanie do ramki danych
rm_table <- data.frame(repetitions, percent) 

df <- df %>%
  left_join(rm_table,by=c("powtórzenia"="repetitions")) %>% 
  mutate("rm"=(1/percent)*ciężar)

# Obliczenia: średnie RM na trening i inne zmienne
processed_data <- df %>%
  group_by(sportowiec, data_treningu) %>%
  summarise(
    srednie_rm = sum(rm) / n(),
  ) %>% 
  right_join(df)

# Tworzenie aplikacji Shiny
ui <- fluidPage(
  titlePanel("Analiza średniego RM na treningu"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "sportowiec",
        "Wybierz sportowca:",
        choices = unique(processed_data$sportowiec),
        selected = unique(processed_data$sportowiec)[1]
      ),
      selectInput(
        "rodzaj_treningu",
        "Wybierz rodzaj treningu:",
        choices = NULL
      )
    ),
    mainPanel(
      plotOutput("plot_sen"),
      plotOutput("plot_samopoczucie")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$sportowiec, {
    updateSelectInput(
      session,
      "rodzaj_treningu",
      choices = unique(processed_data %>% filter(sportowiec == input$sportowiec) %>% pull(rodzaj_treningu)),
      selected = unique(processed_data %>% filter(sportowiec == input$sportowiec) %>% pull(rodzaj_treningu))[1]
    )
  })
  
  filtered_data <- reactive({
    processed_data %>% filter(sportowiec == input$sportowiec, rodzaj_treningu == input$rodzaj_treningu)
  })
  
  output$plot_sen <- renderPlot({
      ggplot(filtered_data(), aes(x = factor(sen), y = srednie_rm)) +
          geom_violin(fill = "blue", alpha = 0.5) +
          labs(
              title = "Zależność ilości snu od średniego RM",
              x = "Sen (godziny)",
              y = "Średnie RM na treningu"
          ) +
          theme_minimal()
  })
  
  output$plot_samopoczucie <- renderPlot({
      ggplot(filtered_data(), aes(x = factor(samopoczucie), y = srednie_rm)) +
          geom_boxplot(fill = "red", color = "black", alpha = 0.5) +
          labs(
              title = "Zależność średniego RM od samopoczucia",
              x = "Samopoczucie",
              y = "Średnie RM na treningu"
          ) +
          theme_minimal()
  })
}

shinyApp(ui = ui, server = server)

