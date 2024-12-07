library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

noworodki <- read_excel("/Users/oliwi/Desktop/WojcickaOliwia/Noworodki pozostawione w szpitalu 2007-2023.xlsx", sheet = "Noworodki pozostawione w szpita", skip = 4)
urodzenia <- read_excel("/Users/oliwi/Desktop/WojcickaOliwia/Urodzenia żywe w Polsce 2007-2023.xlsx", sheet = "Urodzenia żywe 2007-2023")
wynagrodzenia <- read_excel("/Users/oliwi/Desktop/WojcickaOliwia/Zeszyt1.xlsx", sheet = "Arkusz1")

noworodki2 <- noworodki %>%
  pivot_longer(cols = -Województwo, names_to = "Year", values_to = "CountN") %>% 
  mutate(Year = as.numeric(Year),
         CountN = as.numeric(CountN))

urodzenia2 <- urodzenia %>%
  pivot_longer(cols = -Województwo, names_to = "Year", values_to = "CountU") %>% 
  mutate(Year = as.numeric(Year),
         CountU = as.numeric(CountU))

wynagrodzenia2 <- wynagrodzenia %>%
  pivot_longer(cols = -Województwo, names_to = "Year", values_to = "CountW") %>% 
  mutate(Year = as.numeric(Year),
         CountW = as.numeric(CountW))

merged_data <- noworodki2 %>%
  inner_join(urodzenia2, by = c("Województwo", "Year"))

final_data <- merged_data %>%
  inner_join(wynagrodzenia2, by = c("Województwo", "Year")) %>% 
  mutate(Count = CountN/CountU * 10000)    #nie procenty bo male wartosci, a chce miec tylko zaleznosc miedzy nimi
 
base_colors <- c("#303174", "#315ca8", "#b5e0f3", "#884292", "#8c2a64", "#e62248", "#e4007e", "#ea4f7f")
generate_transparent_colors <- function(colors) {
  transparent_colors <- c()
  for (color in colors) {
    transparent_colors <- c(transparent_colors, 
                            scales::alpha(color, 0.8),  
                            scales::alpha(color, 0.6)) 
  }
  last_color <- "#ea4f7f"  
  transparent_colors <- c(transparent_colors,
                          scales::alpha(last_color, 0.8), 
                          scales::alpha(last_color, 0.6)) 
  
  return(transparent_colors)
}
custom_colors <- generate_transparent_colors(base_colors)

ui <- fluidPage(
  titlePanel("Zależność między średnim wynagrodzeniem, a pozostawianiem noworodków w szpitalach"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("wojewodztwo", 
                  "Wybierz województwo:", 
                  choices = unique(final_data$Województwo)),
      
      sliderInput("rok", 
                  "Zakres lat:", 
                  min = min(final_data$Year), 
                  max = max(final_data$Year), 
                  value = range(final_data$Year), 
                  step = 1, 
                  sep = "")
    ),
    
    mainPanel(
      plotOutput("plot"),
      textOutput("description")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    final_data %>%
      filter(Województwo == input$wojewodztwo,
             Year >= input$rok[1],
             Year <= input$rok[2])
  })
  
  output$plot <- renderPlot({
    data <- filtered_data()
    
    ggplot(data, aes(x = CountW, y = Count, color = as.factor(Year))) +
      geom_line(color = "gray", size = 1) + 
      geom_point(size = 3) +  
      scale_color_manual(values = custom_colors) +  
      labs(
        title = paste("Województwo:", input$wojewodztwo),
        x = "Średnie wynagrodzenie roczne",
        y = "Liczba pozostawionych noworodków / 10 000 urodzeń",
        caption = "Źródło: Dane GUS",
        color = "Rok"  
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",  
        plot.title = element_text(size = 16, face = "bold")
      )
  })
}

shinyApp(ui = ui, server = server)