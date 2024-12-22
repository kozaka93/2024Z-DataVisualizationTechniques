library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

UŻ<-read_excel("Urodzenia żywe w Polsce 2007-2023.xlsx")
colnames(UŻ)<-c("Województwo", 2007:2023)

UŻ_long <- pivot_longer(
  UŻ,
  cols = -Województwo,  
  names_to = "Rok",     
  values_to = "UŻ" 
)


Now_poz<-read_excel("Noworodki pozostawione w szpitalu 2007-2023.xlsx")
Now_poz<-Now_poz[8:24,]
colnames(Now_poz)<-c("Województwo", 2007:2023)
Now_poz[17,1]<-"POLSKA"
  
Now_poz_long <- pivot_longer(
  Now_poz,
  cols = -Województwo,  
  names_to = "Rok",     
  values_to = "Now_poz" 
)



wych_rodzina<-read_excel("Wychowankowie (0-24 lata) w pieczy zastępczej 2014-2023.xlsx",
                         sheet = 1)
colnames(wych_rodzina)<-c("Województwo", 2014:2023)

wych_rodzina_long <- pivot_longer(
  wych_rodzina,
  cols = -Województwo,  
  names_to = "Rok",     
  values_to = "wych_rodzina" 
)


wych_inst<-read_excel("Wychowankowie (0-24 lata) w pieczy zastępczej 2014-2023.xlsx",
                         sheet = 2)
colnames(wych_inst)<-c("Województwo", 2014:2023)

wych_inst_long <- pivot_longer(
  wych_inst,
  cols = -Województwo,  
  names_to = "Rok",     
  values_to = "wych_inst" 
)


data<-UŻ_long
data<-data %>% 
  left_join(Now_poz_long,by = c("Województwo", "Rok")) %>% 
  left_join(wych_rodzina_long,by = c("Województwo", "Rok")) %>% 
  left_join(wych_inst_long,by = c("Województwo", "Rok"))

colnames(data)<-c("wojewodztwo","Rok",
                  "Urodzenia żywe w 2007-2023",
                  "Noworodki pozostawione w szpitalu 2007-2023",
                  "Wychowankowie (0-24 lata) w rodzinnej pieczy zastępczej 2014-2023",
                  "Wychowankowie (0-24 lata) w  instytucjonalnej pieczy zastępczej 2014-2023"
                  )

data<-data %>%
  mutate(across(c(-wojewodztwo), as.double))


data<-data %>% 
  pivot_longer(cols = c(-wojewodztwo,-Rok),  
               names_to = "typ",     
               values_to = "wartosc")


# Interfejs użytkownika
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Raleway:wght@400;700&display=swap"),
    tags$style(HTML("
      body {
        font-family: 'Raleway', sans-serif;
      }
    "))
  ),
  titlePanel("Analiza urodzeń i noworodków pozostawionych w szpitalach"),
  sidebarLayout(
    sidebarPanel(
      selectInput("wybor_wojewodztwa", "Wybierz województwo:", 
                  choices = unique(data$wojewodztwo),
                  selected = "Polska"),
      checkboxGroupInput("wybor_danych", "Wybierz dane do wyświetlenia:", 
                         choices = unique(data$typ),
                         selected = unique(data$typ))
    ),
    mainPanel(
      plotOutput("wykres")
    )
  )
)

# Logika serwera
server <- function(input, output) {
  # Filtrowanie danych
  filtrowane_dane <- reactive({
    data %>%
      filter(wojewodztwo == input$wybor_wojewodztwa,
             typ %in% input$wybor_danych)
  })
  
  # Wykres
  output$wykres <- renderPlot({
    ggplot(filtrowane_dane(), aes(x = Rok, y = wartosc, color = typ, group = typ)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(title = paste("Dane dla:", input$wybor_wojewodztwa),
           x = "Rok", y = "Liczba") +
      theme_minimal() +
      scale_color_manual(values = c(
        "Urodzenia żywe w 2007-2023" = "#303174",
        "Noworodki pozostawione w szpitalu 2007-2023" = "#b5e0f3",
        "Wychowankowie (0-24 lata) w rodzinnej pieczy zastępczej 2014-2023" = "#8c2a64",
        "Wychowankowie (0-24 lata) w  instytucjonalnej pieczy zastępczej 2014-2023" = "#e4007e"))
  })
  
  
}

# Uruchom aplikację
shinyApp(ui = ui, server = server)



