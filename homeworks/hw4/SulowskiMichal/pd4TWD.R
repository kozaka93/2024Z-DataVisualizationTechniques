library(rsconnect)
library(readxl)
library(dplyr)
library(tidyr)
library(shiny)
library(ggplot2)

NoworodkiPozostawione_df <- read_excel("Noworodki pozostawione w szpitalu 2007-2023.xlsx", sheet = 1)
UrodzeniaZywe_df <- read_excel("Urodzenia żywe w Polsce 2007-2023.xlsx", sheet = 1)
NoworodkiPozostawione_df <- NoworodkiPozostawione_df %>% 
  slice(-c(1:7,25))

colnames(NoworodkiPozostawione_df) <- c("wojewodztwa", 2007:2023)
colnames(UrodzeniaZywe_df) <- c("wojewodztwa", 2007:2023)

NoworodkiPozostawione_df <- NoworodkiPozostawione_df %>%
  pivot_longer(cols = -wojewodztwa, names_to = "year", values_to = "count")
UrodzeniaZywe_df <- UrodzeniaZywe_df %>%
  pivot_longer(cols = -wojewodztwa, names_to = "year", values_to = "count")

urodzenia_df <- UrodzeniaZywe_df %>%
  mutate(year = as.numeric(year),
         count = as.numeric(count))

noworodki_df <- NoworodkiPozostawione_df %>%
  mutate(year = as.numeric(year),
         count = as.numeric(count)) %>% 
  left_join(urodzenia_df, by = c("wojewodztwa", "year"), suffix = c("_noworodki", "_urodzenia"))%>%
  mutate(count = (count_noworodki / count_urodzenia) * 10000) %>% 
  select(wojewodztwa,year,count)

ui <- fluidPage(
  titlePanel("Wizualizacja interaktywna"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("wojewodztwa", "Wybierz województwa:",
                  choices = unique(noworodki_df$wojewodztwa),
                  selected = unique(noworodki_df$wojewodztwa)[1],
                  multiple = TRUE),
      helpText("Użyj strzałek oraz klawisza backspace, aby usunąć z wykresu dane województwo."),
    ),
    
    mainPanel(
      plotOutput("noworodki_plot")
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    noworodki_df %>% filter(wojewodztwa %in% input$wojewodztwa)
  })
  observeEvent(input$remove_btn, {
    req(input$wojewodztwa)
    if(length(input$wojewodztwa) > 0) {
      new_wojewodztwa <- setdiff(input$wojewodztwa, input$wojewodztwa[1])
      updateSelectInput(session, "wojewodztwa", selected = new_wojewodztwa)
    }
  })
  
  output$noworodki_plot <- renderPlot({
    data <- filtered_data()
    
    ggplot(data, aes(x = year, y = count, color = wojewodztwa)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      scale_x_continuous(breaks = seq(2007, 2023, 1)) +
      labs(title = paste("Noworodki pozostawione w szpitalu na 10k urodzonych żywych w województwach:", paste(input$wojewodztwa, collapse = ", ")),
           x = "Rok", y = "Liczba noworodków") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)
