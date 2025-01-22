library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

path <- "Dane_posiłki_twd2.xlsx"

food_A <- read_excel(path, range = "A1:W130", col_names = TRUE, sheet = 1)
food_P <- read_excel(path, range = "A1:W118", col_names = TRUE, sheet = 2)
food_O <- read_excel(path, range = "A1:W120", col_names = TRUE, sheet = 3)

food_A <- food_A %>% mutate(
  godzina_posilku = substr(godzina_posilku, start = 12, stop = 19),
  data_posilku = as.Date(substr(data_posilku, start = 1, stop = 10), format = "%Y-%m-%d")
)
food_P <- food_P %>% mutate(
  godzina_posilku = substr(godzina_posilku, start = 12, stop = 19),
  data_posilku = as.Date(substr(data_posilku, start = 1, stop = 10), format = "%Y-%m-%d")
)
food_O <- food_O %>% mutate(
  godzina_posilku = substr(godzina_posilku, start = 12, stop = 19),
  data_posilku = as.Date(substr(data_posilku, start = 1, stop = 10), format = "%Y-%m-%d")
)

ui1 <- fluidPage(
  tags$head(
    tags$style(HTML("
    .wrapped-card {
      background: #F6F6F6;
      color: #333;
      padding: 20px;
      border-radius: 10px;
      margin: 15px;
      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      border-left: 5px solid #FFCFA1;
    }
    "))
  ),
  
  titlePanel("Bitewise"),
  
  fluidRow(
    column(
      width = 7,
      div(
        class = "wrapped-card",
        h4("Bitewise to aplikacja bazująca na aplikacji Fitatu, która podsumowuje 
            nawyki żywieniowe z trzech tygodni. Zawiera ona analizy i wizualizacje 
            przedstawione na wykresach (zakładka Statystyki), podsumowania liczbowe 
            uporządkowane tematycznie (zakładka Wrapped) oraz zbiór ulubionych 
            składników widoczny obok.")
      )
    ),
    column(
      width = 5,
      div(
        tags$iframe(
          src = "wordcloud2.html",
          style = "transform: scale(0.8); width: 100%; height: 500px; border: none; margin: auto;"
        )
      )
    )
  )
)

ui2 <- fluidPage(
  tags$head(
    tags$style(HTML("
    #global_date_range .input-daterange span {
      margin: 0;
      padding: 0 5px;
    }
    #date_range1 .input-daterange span {
      margin: 0;
      padding: 0 5px;
    }#date_range2 .input-daterange span {
      margin: 0;
      padding: 0 5px;
    }
    #date_range4 .input-daterange span {
      margin: 0;
      padding: 0 5px;
    }
    .wrapped-card {
      background: #F6F6F6;
      color: #333;
      padding: 20px;
      border-radius: 10px;
      margin: 15px;
      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      border-left: 5px solid #FFCFA1;
    }
    "))
  ),
  
  titlePanel("Bitewise"),
  
  fluidRow(
    column(4,
           div(class = "wrapped-card",
               dateRangeInput("global_date_range", 
                              "Wybierz domyślny zakres dat:", 
                              start = "2024-12-09", 
                              end = "2024-12-29",
                              min = "2024-12-09", 
                              max = "2024-12-29",
                              separator = " do ")
           )
    )
  ),
  
  hr(),
  
  fluidRow(
    column(12, h4("Wykres 1: Jak proporcje makroskładników zmieniają się w zależności od typu posiłku")),
    column(3,
           tags$div(
             class = "wrapped-card",
             selectInput("imie1", "Wybierz osobę:", 
                         choices = c("Alicja", "Paula", "Ola"))
           ),
           checkboxInput("porownanie1", "Pokaż słupek referencyjny", FALSE),
           checkboxInput("change_date_1", "Zmień zakres dat", FALSE),
           conditionalPanel(
             condition = "input.change_date_1 == true",
             tags$div(
               class = "wrapped-card",
               dateRangeInput(
                 "date_range1", 
                 "Wybierz zakres dat:", 
                 start = "2024-12-09", 
                 end = "2024-12-29",
                 min = "2024-12-09", 
                 max = "2024-12-29",
                 separator = " do "
               )
             )
           )
    ),
    column(1),
    column(7, plotOutput("PlotBarA"))
  ),
  
  hr(),
  
  fluidRow(
    column(12, h4("Wykres 2: Wartość makroskładników w czasie")),
    column(3,
           div(
             class = "wrapped-card",
             selectInput("imie2", "Wybierz osobę:", choices = c("Alicja", "Paula", "Ola"))
           ),
           div(
             class = "wrapped-card",
             selectInput("makro2", "Wybierz wartość:", 
                         choices = c("kalorie" = colnames(food_A)[6],
                                     "białko" = colnames(food_A)[7],
                                     "tłuszcz" = colnames(food_A)[8],
                                     "węglowodany" = colnames(food_A)[9]))
           ),
           checkboxInput("change_date_2", "Zmień zakres dat", FALSE),
           conditionalPanel(
             condition = "input.change_date_2 == true",
             tags$div(
               class = "wrapped-card",
               dateRangeInput(
                 "date_range2", 
                 "Wybierz zakres dat:", 
                 start = "2024-12-09", 
                 end = "2024-12-29",
                 min = "2024-12-09", 
                 max = "2024-12-29",
                 separator = " do "
               )
             )
           )
    ),
    column(1),
    column(7, plotOutput("PlotLineB"))
  ),
  
  hr(),
  
  fluidRow(
    column(12, h4("Wykres 3: Częstość spożycia danych grup spożywczych")),
    column(3,
           div(
             class = "wrapped-card",
             selectInput("imie3", "Wybierz osobę:", choices = c("Alicja", "Paula", "Ola"))
           ),
           div(
             class = "wrapped-card",
             selectInput("kategoria3", "Wybierz kategorię:", 
                         choices = c("Typ posiłku" = colnames(food_A)[21],
                                     "Miejsce spożycia" = colnames(food_A)[22],
                                     "Sposób przygotowania" = colnames(food_A)[23],
                                     "Rodziaj posiłku" = colnames(food_A)[4]
                         ))
           )
    ),
    column(1),
    column(7, plotOutput("PlotMapD"))
  ),
  
  hr(),
  
  fluidRow(
    column(12, h4("Wykres 4: Ilości wystąpień kategorii jakościowych")),
    column(3,
           div(
             class = "wrapped-card",
             selectInput("imie4", "Wybierz osobę:", choices = c("Alicja", "Paula", "Ola"))
           ),
           div(
             class = "wrapped-card",
             selectInput("kategoria4", "Wybierz kategorię:", 
                         choices = c("Typ posiłku" = colnames(food_A)[21],
                                     "Miejsce spożycia" = colnames(food_A)[22],
                                     "Sposób przygotowania" = colnames(food_A)[23]))
           ),
           checkboxInput("change_date_4", "Zmień zakres dat", FALSE),
           conditionalPanel(
             condition = "input.change_date_4 == true",
             tags$div(
               class = "wrapped-card",
               dateRangeInput(
                 "date_range4", 
                 "Wybierz zakres dat:", 
                 start = "2024-12-09", 
                 end = "2024-12-29",
                 min = "2024-12-09", 
                 max = "2024-12-29",
                 separator = " do "
               )
             )
           )
    ),
    column(1),
    column(7, plotOutput("PlotColC"))
  ),
  hr()
)

ui3 <- fluidPage(
  tags$head(
    tags$style(HTML("
      .wrapped-card {
        background: #F6F6F6;
        color: #333;
        padding: 20px;
        border-radius: 10px;
        margin: 15px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        border-left: 5px solid #FFCFA1;
        transition: none; /* Prevent color change on button click */
      }
      .nav-tabs > li > a:not(.active) {
        color: black; /* Set color for not chosen tabs */
        font-weight: bold;
        font-size: 18px;
        background-color: #C3EBA0; /* Set background color for not chosen tabs */
      }
      .nav-tabs > li > a {
        color: #FFCFA1;
        background-color: #F6F6F6; /* Set background color on hover */
      }
      .nav-tabs > li.active > a {
        color: black;
        background-color: #F6F6F6;
        border-bottom: 4px solid #FFCFA1;
      }
      .nav-tabs > li > a:hover {
        color: black;
        background-color: #F6F6F6; 
      }
      .btn-primary {
        background-color: #C3EBA0;
        border-radius: 15px;
        border-color: #C3EBA0;
        color: black;
        font-weight: bold;
      }
      .btn-primary:hover {
        background-color: #C3EBA0;
        border-color: #C3EBA0;
      }
      .stat-highlight {
        font-size: 20px;
        color: black;
        font-weight: bold;
      }
    "))
  ),
  tabPanel("Wrapped",
           sidebarLayout(
             sidebarPanel(
               selectInput("wrapped_person", "Wybierz osobę:", choices = c("Alicja", "Paula", "Ola")),
               actionButton("generate_wrapped", "Generuj Wrapped", 
                            class = "btn-primary btn-lg btn-block")
             ),
             mainPanel(
               tabsetPanel(id = "wrapped_tabs",
                           tabPanel("Podsumowanie", 
                                    uiOutput("wrapped_summary_basic")
                           ),
                           tabPanel("Kalorie", 
                                    uiOutput("wrapped_calories")
                           ),
                           tabPanel("Makroskładniki", 
                                    uiOutput("wrapped_macros")
                           ),
                           tabPanel("Czas posiłków", 
                                    uiOutput("wrapped_timing")
                           )
               )
             )
           )
  )
)

server <- function(input, output, session) {
  
  # Funkcja do wyboru danych na podstawie osoby
  selected_food <- function(imie) {
    switch(imie,
           "Alicja" = food_A,
           "Paula" = food_P,
           "Ola" = food_O)
  }
  
  # Dynamiczne ograniczenia daty
  observe({
    updateDateRangeInput(
      session, 
      "global_date_range",
      start = input$global_date_range[1],
      end = max(input$global_date_range[1], input$global_date_range[2]),
      min = "2024-12-09",
      max = "2024-12-29"
    )
  })
  
  observe({
    updateDateRangeInput(
      session, 
      "date_range1",
      start = input$date_range1[1],
      end = max(input$date_range1[1], input$date_range1[2]),
      min = "2024-12-09",
      max = "2024-12-29"
    )
  })
  
  observe({
    updateDateRangeInput(
      session, 
      "date_range2",
      start = input$date_range2[1],
      end = max(input$date_range2[1], input$date_range2[2]),
      min = "2024-12-09",
      max = "2024-12-29"
    )
  })
  
  observe({
    updateDateRangeInput(
      session, 
      "date_range4",
      start = input$date_range4[1],
      end = max(input$date_range4[1], input$date_range4[2]),
      min = "2024-12-09",
      max = "2024-12-29"
    )
  })
  
  # Wykres 1
  output$PlotBarA <- renderPlot({
    if(input$change_date_1){
      filtered_food <- selected_food(input$imie1) %>% 
        filter(data_posilku >= input$date_range1[1] & data_posilku <= input$date_range1[2])
    } else{
      filtered_food <- selected_food(input$imie1) %>% 
        filter(data_posilku >= input$global_date_range[1] & data_posilku <= input$global_date_range[2])
    }
    
    food <- filtered_food %>%  group_by(typ_posilku) %>% 
      summarise(
        suma_makro_bialko = sum(bialko_g, na.rm = TRUE),
        suma_makro_tluszcz = sum(tluszcz_g, na.rm = TRUE),
        suma_makro_wegl = sum(weglowodany_g, na.rm = TRUE)
      ) %>% 
      pivot_longer(
        cols = c(suma_makro_bialko, suma_makro_tluszcz, suma_makro_wegl),
        names_to = "makroskladnik",
        values_to = "wartosc"
      ) %>% 
      group_by(typ_posilku) %>% 
      mutate(procent = wartosc / sum(wartosc) * 100) %>% 
      select(c("typ_posilku", "makroskladnik", "procent"))
    
    food$makroskladnik <- recode(food$makroskladnik, 
                                 "suma_makro_bialko" = "białko", 
                                 "suma_makro_tluszcz" = "tłuszcz", 
                                 "suma_makro_wegl" = "węglowodany")
    food$makroskladnik <- factor(food$makroskladnik, levels = c("białko", "tłuszcz", "węglowodany"))
  
    plot <- ggplot(food, aes(x = typ_posilku, y = procent, fill = makroskladnik)) + 
      geom_bar(stat = "identity") + 
      scale_y_continuous(expand = c(0, 0)) +
      labs(
        title = "Procentowy udział makroskładników w posiłkach",
        x = "Rodzaj posiłku",
        y = "Procent",
        fill = "Makroskładnik:"
      ) +
      scale_fill_manual(
        values = c("białko" = "#92dffd", "tłuszcz" = "#e4d592", "węglowodany" = "#ab9be2")
      ) +
      scale_x_discrete(
        limits = c("śniadanie", "obiad", "kolacja", "napój", "inny")
      )
    
    if (input$porownanie1) {
      reference_data <- data.frame(
        typ_posilku = c("wartości referencyjne"),
        makroskladnik = c("białko", "tłuszcz", "węglowodany"),
        procent = c(15, 30, 55)
      )
      food <- rbind(reference_data, food)
      plot <- ggplot(food, aes(x = typ_posilku, y = procent, fill = makroskladnik)) + 
        geom_bar(stat = "identity") + 
        scale_y_continuous(expand = c(0, 0)) +
        labs(
          title = "Procentowy udział makroskładników w posiłkach",
          x = "Rodzaj posiłku",
          y = "Procent",
          fill = "Makroskładnik:"
        ) + 
        scale_fill_manual(
          values = c("białko" = "#92dffd", "tłuszcz" = "#e4d592", "węglowodany" = "#ab9be2")
        )+
        scale_x_discrete(
          limits = c("śniadanie", "obiad", "kolacja", "napój", "inny", "", "wartości referencyjne")
        )
       
    }
    plot + theme_minimal() + theme(axis.text = element_text(size = 14),
                                   axis.title = element_text(size = 16), 
                                   axis.title.x = element_text(vjust = -0.6),
                                   legend.title = element_text(size = 16),
                                   legend.text = element_text(size = 14),
                                   plot.title = element_text(size = 16, hjust = 0.07))
  })
  
  # Wykres 2
  output$PlotLineB <- renderPlot({
    kategoria_labels <- c(
      "kalorie" = "Kalorie [kcal]",
      "bialko_g" = "Białko [g]",
      "tluszcz_g" = "Tłuszcz [g]",
      "weglowodany_g" = "Węglowodany [g]"
    )
    kategoria_title <- c(
      "kalorie" = "kalorie",
      "bialko_g" = "białka",
      "tluszcz_g" = "tłuszcze",
      "weglowodany_g" = "węglowodany"
    )
    
    if(input$change_date_2){
      filtered_food <- selected_food(input$imie2) %>% 
        filter(data_posilku >= input$date_range2[1] & data_posilku <= input$date_range2[2])
    } else{
      filtered_food <- selected_food(input$imie2) %>% 
        filter(data_posilku >= input$global_date_range[1] & data_posilku <= input$global_date_range[2])
    }
    
    food <- filtered_food %>% 
      group_by(data_posilku) %>% 
      summarise(sum_makro = sum(!!sym(input$makro2), na.rm = TRUE))
    

    # Wartości referencyjne
    reference_values <- tibble(
      osoba = rep(c("Alicja", "Paula", "Ola"), times = 4),
      kategoria = rep(c("kalorie", "bialko_g", "tluszcz_g", "weglowodany_g"), each = 3),
      dolna_granica = c(1800, 1800, 1800, 59, 62, 57, 54, 57, 53, 221, 232, 214),
      gorna_granica = c(2200, 2200, 2200, 98, 103, 95, 65, 69, 63, 343, 360, 333)
    )
    
    ref <- reference_values %>% 
      filter(osoba == input$imie2, kategoria == input$makro2)
    color_ref <-data.frame(cbind(c("kalorie","bialko_g", "tluszcz_g", "weglowodany_g"), c("#ea91ed","#92dffd","#e4d592","#ab9be2")))
    colnames(color_ref) <- c("X1", "X2")
    
    color_used <- color_ref %>% filter(X1 == input$makro2) %>% pull(X2)
    
    y_limits <- list(
      "kalorie" = c(900, 2600),
      "bialko_g" = c(0, 120),
      "tluszcz_g" = c(0, 120),
      "weglowodany_g" = c(0, 450)
    )
    y_breaks <- list(
      "kalorie" = seq(1000, 2600, by = 500),
      "bialko_g" = seq(0, 120, by = 25),
      "tluszcz_g" = seq(0, 120, by = 25),
      "weglowodany_g" = seq(0, 450, by = 100)
    )
    
    ggplot(food, aes(x = data_posilku, y = sum_makro, group = 1)) + 
      geom_point() + 
      geom_line() +
      geom_ribbon(
        aes(ymin = ref$dolna_granica, ymax = ref$gorna_granica, fill = "wartość referencyjna")
        , alpha = 0.5
      ) +
      labs(
        title = paste("Spożyte", kategoria_title[input$makro2], "w czasie"),
        x = "Data posiłku",
        y = kategoria_labels[input$makro2],
        fill = ""
      ) +
      scale_fill_manual(
        values = c("wartość referencyjna" = color_used)
      ) +
      scale_x_date(
        breaks = "1 day",
        labels = scales::date_format("%Y-%m-%d"),
        limits = if (input$change_date_2) c(input$date_range2[1], input$date_range2[2]) else c(input$global_date_range[1], input$global_date_range[2]),
        expand = c(0.01, 0.01)
      ) +
      scale_y_continuous(
        limits = y_limits[[input$makro2]],
        breaks = y_breaks[[input$makro2]],
        expand = c(0,0)
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.minor.x = element_blank()) + theme(axis.text = element_text(size = 14),
                                                       axis.title = element_text(size = 16), 
                                                       axis.title.x = element_text(vjust = -0.6),
                                                       legend.text = element_text(size = 14),
                                                       plot.title = element_text(size = 16))
      
  })
  
  
  #Wykres 3
  output$PlotMapD <- renderPlot({
    kategoria_labels <- c(
      "sposob_przygotowania" = "Sposób przygotowania",
      "miejsce_spozycia" = "Miejsce spożycia",
      "kategoria_posilku" = "Typ posiłku",
      "typ_posilku" = "Rodzaj posiłku"
    )
    
    heatmap_data <- selected_food(input$imie3) %>%
      select(colnames(food_A)[11:20], !!sym(input$kategoria3)) %>%
      group_by(!!sym(input$kategoria3)) %>%
      summarise_all(sum) %>%
      pivot_longer(cols = -!!sym(input$kategoria3), names_to = "produkt", values_to = "ilosc") %>%
      mutate(!!sym(input$kategoria3) := if (input$kategoria3 == "typ_posilku") {
        factor(!!sym(input$kategoria3), levels = c("śniadanie", "obiad", "kolacja", "napój", "inny"))
      } else {
        !!sym(input$kategoria3)
    })
    ggplot(heatmap_data, aes(x = produkt, y = !!sym(input$kategoria3), fill = ilosc)) +
      geom_tile() +
      scale_fill_gradient(low = "#f6f6f6",high = "#f24032") +
      scale_x_discrete(labels = c("alkohol" = "alkohol", "mieso" = "mięso", "nabial" = "nabiał", "orzechy" = "orzechy", "owoce" = "owoce", "produkty_roslinne" = "produkty roślinne", "produkty_zbozowe" = "produkty zbożowe", "przekaski" = "przekąski", "ryby" = "ryby", "warzywa" = "warzywa")) + 
      theme_minimal() +
      labs(
        title = "Spożycie grup produktów",
        x = "Grupy produktów",
        y = kategoria_labels[input$kategoria3],
        fill = "Liczba wystąpień"
      ) + theme(axis.text = element_text(size = 14),
                axis.text.x = element_text(angle = 45, hjust = 1),
               axis.title = element_text(size = 16), 
               axis.title.x = element_text(vjust = -0.6),
               legend.title = element_text(size = 16),
               legend.text = element_text(size = 14),
               plot.title = element_text(size = 16))
  })
  
  
  # Wykres 4
  output$PlotColC <- renderPlot({
    kategoria_labels <- c(
      "sposob_przygotowania" = "Sposób przygotowania",
      "miejsce_spozycia" = "Miejsce spożycia",
      "kategoria_posilku" = "Typ posiłku"
    )
    
    color_palette <- c(
      "#92dffd", "#c3eba0", "#ab9be2", "#f24032", "#ea91ed", "#ffcfa1" , "#e4d592"
    )
    
    if(input$change_date_4){
      filtered_food <- selected_food(input$imie4) %>% 
        filter(data_posilku >= input$date_range4[1] & data_posilku <= input$date_range4[2])
    } else{
      filtered_food <- selected_food(input$imie4) %>% 
        filter(data_posilku >= input$global_date_range[1] & data_posilku <= input$global_date_range[2])
    }
    
    food <- filtered_food %>% group_by(!!sym(input$kategoria4)) %>% count(name = "liczba")
    
    category_color <- color_palette[which(names(kategoria_labels) == input$kategoria4)]
    
    ggplot(food, aes(x = !!sym(input$kategoria4), y = liczba)) + 
      geom_col(width = 0.5, fill = category_color) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(
        title = "Liczba spożytych posiłków w zależności od kategorii",
        x = kategoria_labels[input$kategoria4],
        y = "Liczba posiłków"
      ) + 
      theme_minimal() + 
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 16), 
            axis.title.x = element_text(vjust = -0.6),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            plot.title = element_text(size = 16, hjust = 0.07))
  })
  
  
  # WRAPPED SECTION
  observeEvent(input$generate_wrapped, {
   
    # Get the selected person
    selected_person <- input$wrapped_person
    
    # Fetch the data for the selected person
    user_data <- switch(selected_person,
                        "Alicja" = food_A,
                        "Paula" = food_P,
                        "Ola" = food_O)
    
    # Total number of eaten meals, sum of calories, carbs, protein, and fats
    total_meals <- nrow(user_data)
    total_calories <- sum(user_data$kalorie, na.rm = TRUE)
    total_protein <- sum(user_data$bialko_g, na.rm = TRUE)
    total_fat <- sum(user_data$tluszcz_g, na.rm = TRUE)
    total_carbs <- sum(user_data$weglowodany_g, na.rm = TRUE)

    # Total number of eaten salty and sweet meals
    total_salty_meals <- sum(user_data$kategoria_posilku == "słony")
    total_sweet_meals <- sum(user_data$kategoria_posilku == "słodki")
    
    # Average values per day
    unique_days <- n_distinct(user_data$data_posilku)
    avg_calories_per_day <- total_calories / unique_days
    avg_protein_per_day <- total_protein / unique_days
    avg_fat_per_day <- total_fat / unique_days
    avg_carbs_per_day <- total_carbs / unique_days
    
    # Day with most calories eaten
    max_cal_day <- user_data %>%
      group_by(data_posilku) %>%
      summarise(total_calories = sum(kalorie, na.rm = TRUE)) %>%
      arrange(desc(total_calories)) %>%
      slice(1)  # Get the day with the most calories
    
    # Day with least calories eaten
    min_cal_day <- user_data %>%
      group_by(data_posilku) %>%
      summarise(total_calories = sum(kalorie, na.rm = TRUE)) %>%
      arrange(total_calories) %>%
      slice(1)  # Get the day with the least calories
    
    # Average time for each meal type
    user_data$hour <- as.numeric(substr(user_data$godzina_posilku, 1, 2))  # Extract hour
    user_data$minute <- as.numeric(substr(user_data$godzina_posilku, 4, 5))  # Extract minute
    avg_time_per_meal <- user_data %>%
      group_by(typ_posilku) %>%
      summarise(avg_hour = mean(hour + minute / 60, na.rm = TRUE)) %>%
      mutate(avg_hour = sprintf("%02d:%02d", floor(avg_hour), round((avg_hour %% 1) * 60)))
    
    # Summary Tab
    output$wrapped_summary_basic <- renderUI({
      tagList(
        div(class = "wrapped-card",
          h2(paste("Podsumowanie dla:", selected_person)),
          p(HTML(paste("Liczba dni: ", 
            span(class = "stat-highlight", unique_days)))),
          p(HTML(paste("Łączna liczba posiłków: ", 
            span(class = "stat-highlight", total_meals)))),
          p(HTML(paste("Łączna liczba posiłków słonych: ", 
            span(class = "stat-highlight", total_salty_meals)))),
          p(HTML(paste("Łączna liczba posiłków słodkich: ", 
            span(class = "stat-highlight", total_sweet_meals)))),    
          p(HTML(paste("Średnia liczba posiłków dziennie: ", 
            span(class = "stat-highlight", round(total_meals/unique_days, 1))))),
        )
      )
    })
    
    # Calories Tab
    output$wrapped_calories <- renderUI({
      tagList(
        div(class = "wrapped-card",

         h3("Informacje kaloryczne:"),
          p(HTML(paste("Średnie dzienne spożycie kalorii: ", 
            span(class = "stat-highlight", round(avg_calories_per_day, 1))))),
          p(HTML(paste("Całkowite spożycie kalorii: ", 
            span(class = "stat-highlight", round(total_calories))))),
          
          h3("Twoje rekordy:"),
          p(HTML(paste("Maksymalne dzienne spożycie kalorii: ", 
            span(class = "stat-highlight", round(max_cal_day$total_calories, 1)), 
            " (dzień: ", max_cal_day$data_posilku, ")"))),
          p(HTML(paste("Minimalne dzienne spożycie kalorii: ", 
            span(class = "stat-highlight", round(min_cal_day$total_calories, 1)), 
            " (dzień: ", min_cal_day$data_posilku, ")"))),
        )
      )
    })
    
    # Macros Tab
    output$wrapped_macros <- renderUI({
      tagList(
        div(class = "wrapped-card",
          h3("Przegląd składników odżywczych:"),
          p(HTML(paste("Średnie dzienne spożycie białka: ", 
            span(class = "stat-highlight", round(avg_protein_per_day, 1)), " g"))),
          p(HTML(paste("Średnie dzienne spożycie tłuszczów: ", 
            span(class = "stat-highlight", round(avg_fat_per_day, 1)), " g"))),
          p(HTML(paste("Średnie dzienne spożycie węglowodanów: ", 
            span(class = "stat-highlight", round(avg_carbs_per_day, 1)), " g")))
        )
        
      )
    })
    
    # Timing Tab
    output$wrapped_timing <- renderUI({
      tagList(
        div(class = "wrapped-card",
          h3("Twoje pory posiłków"),
          tableOutput("meal_times_table")
        )
      )
    })
    
    # Render meal times table
    output$meal_times_table <- renderTable({
      avg_time_per_meal %>%
        filter(typ_posilku %in% c("śniadanie", "obiad", "kolacja")) %>%
        arrange(factor(typ_posilku, levels = c("śniadanie", "obiad", "kolacja"))) %>%
        select(typ_posilku, avg_hour) %>%
        rename("Typ posiłku" = typ_posilku,
               "Średnia godzina" = avg_hour)
    })
  })
}

uiall <- navbarPage(
  "",
  header = tags$head(
    # Import Albert Sans font
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Albert+Sans:wght@300;400;500;600&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
      * {
          font-size: 18px;
          font-family: 'Albert Sans', sans-serif !important;
      }
      .input-group .form-control {
          font-size: 18px;  /* Adjust this value to your preferred size */
      }
    "))
  ),
  tabPanel("Bitewise", ui1),
  tabPanel("Statystyki", ui2),
  tabPanel("Wrapped", ui3)
  # theme = bs_theme(bootswatch = "morph")
)


shinyApp(ui = uiall, server = server)

