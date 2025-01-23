library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(shinythemes)


width_image = "100%"

choice_labels <- list(
  "Kalorie [kcal]" = "Kalorie",
  "Aktywność [min]" = "Aktywnosc",
  "Nauka [h]" = "Nauka",
  "Sen [h]" = "Sen",
  "Kroki [1]" = "Kroki",
  "Płyny [L]" = "Plyny",
  "Zadowolenie [1-10]" = "Zadowolenie"
)

choice_labels_reversed <- setNames(names(choice_labels), choice_labels)


labeltoint <- function(label){
  which(unname(choice_labels) == label);
}

przedzialy <- list(
  seq(0, 1250, 250), # kalorie
  c(0, 15, 30, 45, 60, 90, 120, 150, 180), # aktywnosc
  0:8, # nauka
  5:12, # sen
  c(0, 2500, 5000, 7500, 10000, 12500, 20000, 30000), # kroki
  seq(1, 3, 0.25), # plyny
  1:10 # zadowolenie
)

data <- read.csv("data.csv")

data <- data %>%
  mutate(across(4:9, as.numeric))
  colnames(data) <- c("Data", "Imie", "Kalorie", "Aktywnosc", "Nauka", "Sen", "Kroki", "Plyny", "Zadowolenie") #nolint

data <- data %>%
  mutate(Data = as.Date(Data, format = "%Y-%m-%d"))




translate_weekdays <- function(day) {
  
  english_to_polish <- c(
    "Monday" = "poniedziałek",
    "Tuesday" = "wtorek",
    "Wednesday" = "środa",
    "Thursday" = "czwartek",
    "Friday" = "piątek",
    "Saturday" = "sobota",
    "Sunday" = "niedziela"
  )
  
  polish_names <- c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela")
  
  if (day %in% polish_names) {
    return(day)
  }
  
  return(english_to_polish[day])
}

data <- data %>%
  mutate(dzienTygodnia = weekdays(Data),
         dzienTygodnia = sapply(dzienTygodnia, translate_weekdays),
         dzienTygodnia = factor(dzienTygodnia,
                                levels = c("poniedziałek", "wtorek", "środa",
                                           "czwartek", "piątek", "sobota", "niedziela")))


ui1 <- fluidPage(

  sidebarLayout(

    sidebarPanel(

      selectInput("y", "Wybierz wartości", choices = choice_labels),

      sliderInput("date", "Wybierz zakres czasu:",
                  min = min(data$Data), max = max(data$Data),
                  value = c(min(data$Data), max(data$Data)),
                  timeFormat = "%Y-%m-%d"),

      conditionalPanel(
        condition = "input.tabs == 'Wykres punktowy'",
        checkboxInput("trend", "Linia trendu", value = FALSE)
      )
    ),

    mainPanel(

      tabsetPanel(
        id = "tabs",
        tabPanel("Wykres punktowy", plotlyOutput("scatter")),
        tabPanel("Boxplot", plotlyOutput("boxplot")),
        tabPanel("Wykres skrzypcowy", plotlyOutput("violin")),
        tabPanel("Heatmapa", plotOutput("heatmap")),
        tabPanel("Wykres słupkowy", plotlyOutput("col"))
      )
    )


  )
)



ui2 <- fluidPage(
  tags$head(
    tags$style(HTML("
        .fade-in {
          animation: fadeIn 2s ease-in-out;
        }
    
        @keyframes fadeIn {
          from { opacity: 0; }
          to { opacity: 1; }
        }
      "))
  ),
  tags$head(
    tags$style(HTML("
    .highlight:hover {
      color: #FF5733;
      font-weight: bold;
      transition: 0.3s;
    }
  "))
  ),
  tags$head(
    tags$style(HTML("
    .gradient-border {
      border: 4px solid;
      border-image-slice: 1;
      border-width: 4px;
      border-image-source: linear-gradient(to right, #66C7F4, #6C6EA0, #FF1053);
      padding: 15px;
      border-radius: 10px;
    }
  "))
  ),
  tabsetPanel(
    tabPanel("Wniosek: nauka", 
             fluidRow(
               column(6, img(src = "nauka.png", width = width_image)),
               column(6, 
                      div(class = "fade-in gradient-border",
                          p(span("Na wykresie widoczne są następujące trendy:"), class = "highlight"),
                          p(span("a) Antoni uczy się średnio tyle samo niezależnie od okresu;"), class = "highlight"), 
                          p(span("b) Jan najwięcej uczył się przed świętami, a najmniej w czasie świąt;"), class = "highlight"),
                          p(span("c) Kacper podobniej jak Jan, aczkolwiek tu mniej to widać."), class = "highlight") 
                      ))
             )
    ),
    tabPanel("Wniosek: kroki", 
             fluidRow(
               column(6, img(src = "kroki.png", width = width_image)),
               column(6, 
                      div(class = "fade-in gradient-border",
                          p(span("Na przedstawionym wykresie ramka-wąsy widać, że:"), class = "highlight"),
                          p(span("a) minimum kroków Antoniego to mniej więcej 3 kwartyl Jana i Kacpra (~8000);"), class = "highlight"),
                          p(span("b) maksimum Antoniego z kroków to 33500 kroków;"), class = "highlight"),
                          p(span("c) mediana Antoniego (~17500) jest większa niż maksimum Jana i Kacpra."), class = "highlight")
                      )
               )
             )   
    ),
    tabPanel("Wniosek: sen", 
             fluidRow(
               column(6, img(src = "sen.png", width = width_image)),
               column(6,
                      div(class = "fade-in gradient-border",
                          p(span("a) Nawyki senne Antoniego i Jana są bardzo zbliżone do siebie - najczęściej spali 8 godzin."), class = "highlight"),
                          p(span("b) Sen Kacpra ma rozkład zbliżony do rozkładu jednostajnego na przedziale [4, 11]."), class = "highlight")
                      )
               )
             )
    ),
    tabPanel("Wniosek: zadowolenie",
             column(6, fluidRow(p(span("Zadowolenie przed przerwą świąteczną"), class = "highlight"),
                                img(src = "zadowolenie_przed_swietami.png", width = width_image),
                                p(span("Zadowolenie w czasie przerwy świątecznej"), class = "highlight"),
                                img(src = "zadowolenie_swieta.png", width = width_image)
             )),
             column(6, fluidRow(
               div(class = "fade-in gradient-border",
                   p(span("a) Antoni i Kacper byli szczęśliwsi w czasie przerwie przerwy świątecznej niż przed nią."), class = "highlight"),
                   p(span("b) Przeciwną zależność można zauważyć u Janka - był szczęśliwszy przed świętami."), class = "highlight")))
             )
    ),
    tabPanel("Wniosek: aktywność", 
             fluidRow(column(6, img(src = "aktywnosc.png", width = width_image)),
                      column(6,
                             div(class = "fade-in gradient-border",
                                 p(span("a) W ciągu tygodnia akademickiego jesteśmy najbardziej aktywni w środę i piątek, ponieważ w te dni jest mało zajęć."), class = "highlight"),
                                 p(span("b) Antoni preferuje ćwiczyć w piątki, Jan w piątki i soboty, a Kacper w niedziele."), class = "highlight"),
                             )))
    )
  ))





ui3 <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("time_range", "Wybierz zakres czasu",
                  min = min(data$Data), max = max(data$Data),
                  value = c(min(data$Data), max(data$Data)),
                  timeFormat = "%Y-%m-%d"),
      selectInput("x_axis", "Oś X", choices = choice_labels),
      selectInput("y_axis", "Oś Y", choices = choice_labels)
    ),
    mainPanel(
      plotlyOutput("bubble_chart")
    )
  )
)




ui <- navbarPage("TWD Projekt 2",
                 tabPanel("Wykresy interaktywne", ui1),
                 tabPanel("Wykres animowany (WOW!)", ui3),
                 tabPanel("Wnioski", ui2),
                 theme = shinytheme("lumen"))



server <- function(input, output) {

  data_with_date <- reactive(filter(data, Data >= input$date[1],
                                    Data <= input$date[2]))

  no_of_dates <- reactive(as.integer(input$date[2] - input$date[1]))

  przedzial <- reactive(przedzialy[[labeltoint(input$y)]])

  output$scatter <- renderPlotly({

    scatter_plot <- data_with_date() %>%
      ggplot(aes_string(x = "Data", y = input$y, color = "Imie")) +
      geom_point() +
      scale_color_manual(values = c("#66C7F4", "#6C6EA0", "#FF1053")) +
      theme_minimal() +
      labs(y = choice_labels_reversed[[input$y]])


    if (input$trend & no_of_dates() > 1) {
      scatter_plot <- scatter_plot +
        geom_smooth(method = "gam", aes(group = Imie,
                                        fill = Imie),
                    se = FALSE,
                    formula = y ~ s(x, k = 1 + no_of_dates()))
    }

    if (input$trend & no_of_dates() == 1) {
      scatter_plot <- scatter_plot +
        geom_smooth(method = "lm", aes(group = Imie),
                    se = FALSE)
    }

    ggplotly(scatter_plot)
  })

  output$boxplot <- renderPlotly({

    boxplot <- data_with_date() %>%
      ggplot(aes_string(x = "Imie", y = input$y, fill = "Imie")) +
      scale_fill_manual(values = c("#66C7F4", "#6C6EA0", "#FF1053")) +
      geom_boxplot() +
      theme_minimal() +
      labs(y = choice_labels_reversed[[input$y]])

    ggplotly(boxplot)
  })

  output$violin <- renderPlotly({
    violin <- data_with_date() %>%
      ggplot(aes_string(x = "Imie", y = input$y, fill = "Imie")) +
      scale_fill_manual(values = c("#66C7F4", "#6C6EA0", "#FF1053")) +
      geom_violin() +
      theme_minimal() +
      labs(y = choice_labels_reversed[[input$y]])

    ggplotly(violin)
  })

  output$heatmap <- renderPlot({

    data_with_date() %>%
      mutate(quantile_group = sapply(.data[[input$y]],
                                     function(x) przedzial()[which.min(abs(przedzial() - x))])) %>% #nolint
      group_by(Imie, quantile_group) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(Imie) %>%
      mutate(sum = sum(n)) %>%
      ungroup() %>%
      complete(Imie = unique(Imie),
               quantile_group = przedzial(),
               fill = list(n = 0, sum = 1)) %>%
      ggplot(aes(x = Imie, y = as.factor(quantile_group), fill = n / sum)) +
      geom_tile(width = 0.95, height = 0.80) +
      theme_minimal() +
      labs(y = choice_labels_reversed[[input$y]], x = "Imię",
           fill = "Odsetek") +
      scale_fill_gradient(low = "#6C6EA0", high = "#FF1053") +
      theme_minimal()
  })


  output$col <- renderPlotly({
    bar_data <- data_with_date() %>% 
      group_by(dzienTygodnia) %>%
      summarise(val = mean(.data[[input$y]], na.rm = TRUE), .groups = "drop")

    line_data <- data_with_date() %>% 
      group_by(Imie, dzienTygodnia) %>%
      summarise(val = mean(.data[[input$y]], na.rm = TRUE), .groups = "drop")

    colplot <- ggplot() +
      geom_col(data = bar_data,
               aes(x = dzienTygodnia, y = val), fill = "lavender") +
      geom_line(data = line_data,
                aes(x = dzienTygodnia, y = val,
                    group = Imie, color = Imie), size = 0.75) +
      scale_color_manual(values = c("Antoni" = "#66C7F4", "Jan" = "#6C6EA0", "Kacper" = "#FF1053")) + #nolint
      labs(y = "Średnia wartość dla wszystkich",
           x = "Dzień tygodnia", color = "Średnia \ndla osoby",
           title = paste("Średnie wartości: ", choice_labels_reversed[[input$y]])) + #nolint
      scale_y_continuous(expand = c(0,0),limits=c(0,max(line_data$val)*1.05))+
      theme_minimal()

    ggplotly(colplot)
  })
  
  
  
  
  
  
  output$bubble_chart <- renderPlotly({
    filtered_data <- data %>%
      filter(Data >= input$time_range[1], Data <= input$time_range[2])
    
    correlations <- filtered_data %>%
      group_by(Imie) %>%
      summarise(correlation = abs(cor(get(input$x_axis), get(input$y_axis), use = "complete.obs", method = "spearman")) ) %>%
      ungroup()
    
    filtered_data <- filtered_data %>%
      left_join(correlations, by = "Imie")
    
    bubble_plot <- filtered_data %>%
      ggplot(aes_string(
        x = input$x_axis,
        y = input$y_axis,
        size = "correlation",
        color = "Imie",
        frame = "Data"
      )) +
      geom_point(alpha = 0.7) +
      scale_size(range = c(3, 15)) +
      scale_color_manual(values = c("#66C7F4", "#6C6EA0", "#FF1053")) +
      theme_minimal() +
      labs(title = "Rozmiar bąbelków to korelacja Spearmana x oraz y", color = "Osoba", x = input$x_axis, y = input$y_axis) +
      guides(size = "none")
    
    ggplotly(bubble_plot, tooltip = c("x", "y", "size", "color", "frame"))
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)