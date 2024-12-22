library(shiny)
library(ggplot2)
library(dplyr)
#install.packages(c('showtext', 'shiny'))
library(showtext)
library(tidyr)

# Wczytanie danych
rodzinna_piecza <- read.csv("csv1.csv", sep = ";", stringsAsFactors = FALSE)
instytucjonalna_piecza <- read.csv("csv2.csv", sep = ";", stringsAsFactors = FALSE)

clean_data <- function(df) {
  colnames(df)[1] <- "Województwo"
  colnames(df)[-1] <- gsub("X", "", colnames(df)[-1])
  df <- df %>%
    filter(!is.na(Województwo) & Województwo != "") %>%
    mutate(across(-Województwo, ~ as.numeric(gsub(" ", "", .))))
  return(df)
}

rodzinna_piecza <- clean_data(rodzinna_piecza)
instytucjonalna_piecza <- clean_data(instytucjonalna_piecza)

font_add("Font1", regular = "fonts/Raleway-VariableFont_wght.ttf")
font_add("Font2", regular = "fonts/Raleway-Bold.ttf")
showtext_auto()

colors <- c("#303174", "#e4007e", "#b5e0f3", "#884292", "#ff6f61", "#8c2a64", 
            "#e62248", "#315ca8", "#ea4f7f", "#ff7f50", "#1abc9c", "#f1c40f", 
            "#3498db", "#e74c3c", "#9b59b6", "#00bcd4", "#2ecc71")


# UI
ui <- fluidPage(
  titlePanel("Wykres liczby wychowanków pieczy zastępczej"),
  sidebarLayout(
    sidebarPanel(
      selectInput("data_type", "Wybierz typ pieczy:", 
                  choices = c("Rodzinna" = "rodzinna", "Instytucjonalna" = "instytucjonalna")),
      uiOutput("region_selector")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Serwer
server <- function(input, output, session) {
  # Typ pieczy
  data_selected <- reactive({
    if (input$data_type == "rodzinna") {
      rodzinna_piecza
    } else {
      instytucjonalna_piecza
    }
  })
  
  # Lista województw
  output$region_selector <- renderUI({
    selectizeInput("regions", "Wybierz województwa:", 
                   choices = unique(data_selected()$Województwo), 
                   selected = c("POLSKA"),  # Domyślnie zaznaczone dane dla Polski
                   multiple = TRUE,
                   options = list(plugins = list("remove_button")))
  })
  
  # Wykres
  output$plot <- renderPlot({
    req(input$regions)
    df <- data_selected() %>% filter(Województwo %in% input$regions)
    
    # Przygotowanie danych
    df_long <- df %>%
      pivot_longer(cols = -Województwo, names_to = "Rok", values_to = "Liczba") %>%
      mutate(Rok = as.numeric(Rok)) 
    
    # Kolory
    wojewodztwa_colors <- setNames(colors[1:length(unique(df_long$Województwo))], unique(df_long$Województwo))
    
    ggplot(df_long, aes(x = Rok, y = Liczba, color = Województwo)) +
      geom_line(size = 1.5) + 
      scale_color_manual(values = wojewodztwa_colors) + 
      labs(title = "Liczba wychowanków w pieczy zastępczej", 
           x = "Rok", y = "Liczba wychowanków") +
      theme_minimal(base_size = 14) + 
      theme(
        plot.title = element_text(family = "Font2", size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(family = "Font1", size = 16),
        axis.text = element_text(family = "Font2", size = 12),
        legend.title = element_text(family = "Font2", size = 14),
        legend.text = element_text(family = "Font1", size = 12)
      )
  })
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)

