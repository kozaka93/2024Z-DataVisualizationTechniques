library(readxl)
library(dplyr)
library(tidyr)
library(shiny)
library(ggplot2)
library(showtext)

font_add_google("Raleway", "raleway") 
showtext_auto()

# Read and clean data 
noworodki <- read_xlsx('Noworodki pozostawione w szpitalu 2007-2023.xlsx', skip = 3, trim_ws = TRUE)
colnames(noworodki) <- c('Region', as.character(2007:2023))
noworodki <- noworodki %>% mutate(across(-Region, as.numeric)) %>% filter(!is.na(Region))

urodzenia <- read_xlsx('Urodzenia żywe w Polsce 2007-2023.xlsx', trim_ws = TRUE)
colnames(urodzenia) <- c('Region', as.character(2007:2023))
urodzenia <- urodzenia %>% mutate(Region = if_else(Region == "POLSKA", "Polska", Region)) %>%
  mutate(across(-Region, as.numeric))

ui <- fluidPage(
  titlePanel("Opuszczanie noworodków w szpitalu przez biologicznych rodziców w latach 2007-2023 w podziale na regiony"),
  
  fluidRow(
    column(6, 
           selectInput("wojewodztwo", "Region:",
                       choices = unique(noworodki$Region),
                       selected = "Polska") 
    ),
    column(6, 
           radioButtons("data_type", "Rodzaj danych:",
                        choices = list("Liczba" = "count", "Procent" = "percent"),
                        selected = "count") 
    )
  ),
  
  fluidRow(
    column(12, 
           plotOutput("histogram")
    )
  )
)

server <- function(input, output, session) {
  output$histogram <- renderPlot({
    noworodki_long <- noworodki %>%
      pivot_longer(cols = -Region, names_to = "Year", values_to = "Count") %>%
      mutate(Year = as.numeric(Year))
    
    urodzenia_long <- urodzenia %>%
      pivot_longer(cols = -Region, names_to = "Year", values_to = "Births") %>%
      mutate(Year = as.numeric(Year))
    
    # Calculate percent
    data_merged <- noworodki_long %>%
      left_join(urodzenia_long, by = c("Region", "Year")) %>%
      mutate(Percentage = (Count / Births) * 100)
    
    # Filter by selected region
    filtered_data <- data_merged %>%
      filter(Region == input$wojewodztwo)
    
    # Set y-axis range for count
    y_max <- if (input$data_type == "count") {
      if (input$wojewodztwo == "Polska") {
        1000  
      } else {
        200  
      }
    } else {
      100   
    }
    
    title <- if (input$data_type == "count") {
      paste("Liczba noworodków pozostawionych w szpitalu -", input$wojewodztwo)
    } else { 
      paste("Procent noworodków pozostawionych w szpitalu -", input$wojewodztwo)
    }
    
    display_data <- if (input$data_type == "count") {
      filtered_data %>% select(Year, Value = Count)
    } else {
      filtered_data %>% select(Year, Value = Percentage)
    }
    
    plot <- ggplot(display_data, aes(x = Year, y = Value, fill = as.factor(Year))) +
      geom_bar(stat = "identity", fill = "#ea4f7f") +
      labs(
        title = title,
        x = "Rok",
        y = if (input$data_type == "count") "Liczba opuszczonych noworodków" else "Procent opuszczonych noworodków"
      ) +
      ylim(0, y_max) + 
      theme_minimal() +
      theme(
        text = element_text(family = "raleway"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 20, hjust = 0.5)
      )
    
    if (input$data_type == "percent") {
      plot <- plot + 
        scale_y_continuous(labels = scales::percent_format(scale = 1), 
                           limits = c(0, 1), 
                           breaks = seq(0, 1, by = 0.1))  
    }
    
    plot
  })
}

shinyApp(ui, server)
