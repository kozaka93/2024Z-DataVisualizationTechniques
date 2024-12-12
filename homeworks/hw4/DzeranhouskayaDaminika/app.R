library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(dplyr)
library(tidyr)

births_data <- read_xlsx("D:/downloads/Urodzenia.xlsx")
abandoned_babies_data <- read_excel("D:/downloads/Noworodki.xlsx", skip = 3)

abandoned_babies_data <- na.omit(abandoned_babies_data)

colnames(abandoned_babies_data) <- c("Wojewodztwa", as.character(2007:2023))

abandoned_babies_data$`2016` <- as.numeric(abandoned_babies_data$`2016`)


colnames(births_data) <- c("Wojewodztwa", as.character(2007:2023))

births_long <- births_data %>%
  pivot_longer(
    cols = -Wojewodztwa,
    names_to = "Rok",
    values_to = "Urodzenia_Zywe"
  )

abandoned_babies_long <- abandoned_babies_data %>%
  pivot_longer(
    cols = -Wojewodztwa,
    names_to = "Rok",
    values_to = "Pozostawione_Noworodki"
  ) 


merged_data <- births_long %>%
  inner_join(
    abandoned_babies_long,
    by = c("Wojewodztwa", "Rok")
  )

data <- merged_data %>%
  mutate(
    Procent_Pozostawionych = (Pozostawione_Noworodki / Urodzenia_Zywe) * 100,
    Rok = as.numeric(Rok)
  )


ui <- fluidPage(

  titlePanel("Analiza Urodzen i Pozostawionych Noworodkow w Polsce"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "wojewodztwo",
        "Wybierz Wojewodztwo:",
        choices = unique(data$Wojewodztwa),
        selected = unique(data$Wojewodztwa)[1]
      ),
      sliderInput(
        "rok",
        "Zakres lat:",
        min = min(data$Rok),
        max = max(data$Rok),
        value = c(min(data$Rok), max(data$Rok)),
        step = 1,
        sep = ""
      )
    ),
    
    mainPanel(
      plotOutput("percentagePlot")
    )
  )
)


server <- function(input, output) {
  filteredData <- reactive({
    req(input$wojewodztwo, input$rok)
    data %>%
      filter(
        Wojewodztwa == input$wojewodztwo,
        Rok >= input$rok[1],
        Rok <= input$rok[2]
      )
  })
  
  
  output$percentagePlot <- renderPlot({
    ggplot(filteredData(), aes(x = Rok, y = Procent_Pozostawionych)) +
      geom_line(color = "purple", size = 2) +
      labs(
        title = "Odsetek pozostawionych noworodkow w szpitalach",
        x = "Rok", y = "Procent"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold", colour = "black"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 15.8,  face = "bold")
      ) 
  })
}

shinyApp(ui = ui, server = server)
