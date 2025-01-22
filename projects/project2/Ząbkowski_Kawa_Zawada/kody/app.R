# --------------------------------------------------------
# Plik: app.R  (lub inna nazwa np. "appShiny.R")
# --------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(plotly)
library(grid)
library(tidyr)
library(lubridate)

# Wczytanie danych dotyczących odblokowań ----------------
data_igor <- read.csv("IZodblokowania.csv")
data_wiktoria <- read.csv("WKodblokowania.csv")
data_bartosz <- read.csv("BZodblokowania.csv")

# Wczytanie danych dotyczących użycia aplikacji ----------
apps_igor <- read.csv("IZappdata.csv")
apps_wiktoria <- read.csv("WKappdata.csv")
apps_bartosz <- read.csv("BZappdata.csv")

# Łączenie w jedną ramkę danych dot. odblokowań ----------
all_data <- bind_rows(
  data_igor %>%
    mutate(
      date = as.Date(date),
      user = "Igor Zawada",
      unlocks = as.numeric(unlocks)
    ),
  data_wiktoria %>%
    mutate(
      date = as.Date(date),
      user = "Wiktoria Kawa",
      unlocks = as.numeric(unlocks)
    ),
  data_bartosz %>%
    mutate(
      date = as.Date(date),
      user = "Bartosz Ząbkowski",
      unlocks = as.numeric(unlocks)
    )
)

# --------------------------------------------------------
# Ramki pomocnicze do minigry (przykład)
# --------------------------------------------------------

# Funkcja pomocnicza do obliczania średniego czasu korzystania z aplikacji
calculate_avg_time <- function(app_data) {
  time_to_seconds <- function(time_str) {
    times <- as.POSIXct(time_str, format="%H:%M:%S", tz="UTC")
    as.numeric(difftime(times, as.POSIXct("1970-01-01", tz="UTC"), units="secs"))
  }
  
  # Zmieniamy na sekundy, żeby policzyć średni czas
  app_data_time <- app_data
  app_data_time[,-1] <- sapply(app_data[,-1], time_to_seconds)
  
  # Liczymy średni czas (sekundy)
  average_time_seconds <- colMeans(app_data_time[,-1], na.rm = TRUE)
  
  # Ramka danych: nazwa aplikacji + średni czas
  apps <- colnames(app_data)[-1]  
  result <- data.frame(
    Apps = apps,
    Time = average_time_seconds
  )
  
  return(result)
}

# Funkcja do zmiany czasu (sekundy) na format HH:MM:SS
convert_to_hms <- function(seconds) {
  format(as.POSIXct(seconds, origin="1970-01-01", tz="UTC"), "%H:%M:%S")
}


# Ramki ze średnimi czasami dla każdego użytkownika -------
avg_igor <- calculate_avg_time(apps_igor)
avg_wiktoria <- calculate_avg_time(apps_wiktoria)
avg_bartosz <- calculate_avg_time(apps_bartosz)

# Zamiana wyniku na format HH:MM:SS
avg_igor$Time <- sapply(avg_igor$Time, convert_to_hms)
avg_wiktoria$Time <- sapply(avg_wiktoria$Time, convert_to_hms)
avg_bartosz$Time <- sapply(avg_bartosz$Time, convert_to_hms)

# Lista aplikacji (dowolna, np. do losowania w minigrze)
all_apps <- c("Messenger", "Instagram", "Facebook", "Snapchat", "Youtube",
              "Spotify", "TikTok", "Reddit", "Tinder", "2048", "Chrome",
              "Fitatu", "Geometry Dash", "ChatGPT", "Glovo")

# Ramka zbiorcza do minigry
avg_times <- rbind(
  data.frame(user = "Igor Zawada", app = avg_igor$Apps, time = avg_igor$Time),
  data.frame(user = "Wiktoria Kawa", app = avg_wiktoria$Apps, time = avg_wiktoria$Time),
  data.frame(user = "Bartosz Ząbkowski", app = avg_bartosz$Apps, time = avg_bartosz$Time)
)

# Poprawki nazewnictwa (jeśli są rozbieżności)
avg_times <- avg_times %>%
  mutate(app = ifelse(app == "Geometry.Dash", "Geometry Dash", app)) %>%
  mutate(app = ifelse(app == "X2048", "2048", app))



# --------------------------------------------------------
# bslib: ciemny motyw
# --------------------------------------------------------
theme_darkly <- bs_theme(
  bootswatch = "darkly",
  primary    = "#1abc9c",
  secondary  = "#e74c3c",
  base_font  = font_google("Roboto", local = FALSE),
  bg         = "#121212",
  fg         = "#ffffff"
)

# --------------------------------------------------------
# UI
# --------------------------------------------------------
ui <- fluidPage(
  theme = theme_darkly,
  
  tags$style(HTML(".container-fluid { max-width: 95%; margin: 0 auto; }")),
  
  tags$style(HTML("
    #averageUnlocksAll, #averageUnlocksIgor, #averageUnlocksWiktoria, #averageUnlocksBartus {
      width: 100% !important;
    }
  ")),
  
  titlePanel(
    div("Projekt 2 Techniki Wizualizacji Danych", 
        style = "text-align: center; font-size: 24px; font-weight: bold; color: #fff; padding: 20px 0;")
  ),
  
  navlistPanel(
    id = "mainMenu",
    widths = c(3, 9), 
    
    # ---------------- HOME ----------------
    tabPanel("Home",
             tags$div(
               style = "margin-left: 20px;",
               h3("Witamy w naszej aplikacji."),
               p("Przejdź do wybranej zakładki by poznać statystyki"),
               br(),
               p("Autorzy:"),
               p("Igor Zawada"),
               p("Wiktoria Kawa"),
               p("Bartosz Ząbkowski")
             )
    ),
    
    "Użytkownicy",
    
    # ---------------- Odblokowania ----------------
    tabPanel("Ilość odblokowań telefonu",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(
                   "users",
                   "Wybierz użytkowników:",
                   choices = list(
                     "Igor Zawada"    = "Igor Zawada",
                     "Wiktoria Kawa" = "Wiktoria Kawa",
                     "Bartosz Ząbkowski" = "Bartosz Ząbkowski"
                   ),
                   selected = NULL
                 ),
                 dateRangeInput(
                   "dateRange",
                   "Wybierz zakres dat:",
                   start  = min(all_data$date),
                   end    = max(all_data$date),
                   format = "yyyy-mm-dd"
                 ),
                 p("Średnia Odblokowań"),
                 textOutput("averageUnlocksAll"),
                 br(),
                 p("Średnia Odblokowań: Igor"),
                 textOutput("averageUnlocksIgor"),
                 br(),
                 p("Średnia Odblokowań: Wiktoria"),
                 textOutput("averageUnlocksWiktoria"),
                 br(),
                 p("Średnia Odblokowań: Bartosz"),
                 textOutput("averageUnlocksBartus"),
                 br()
               ),
               mainPanel(
                 plotlyOutput("unlockPlot", height = "600px")
               )
             )
    ),
    
    # ---------------- Aplikacje: Heatmapa ----------------
    tabPanel("Aplikacje",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(
                   "users_apps", 
                   "Wybierz użytkowników:",
                   choices = list(
                     "Igor Zawada" = "Igor Zawada",
                     "Wiktoria Kawa" = "Wiktoria Kawa",
                     "Bartosz Ząbkowski" = "Bartosz Ząbkowski"
                   ),
                   selected = NULL
                 ),
                 dateRangeInput(
                   "dateRangeApps",
                   "Wybierz zakres dat:",
                   start  = as.Date("2024-12-15"),
                   end    = as.Date("2025-01-07"),
                   format = "yyyy-mm-dd"
                 ),
                 br(),
                 p("Łączny średni czas (wszyscy wybrani):"),
                 textOutput("avgTotalTimeAll"),
                 br(),
                 p("Łączny średni czas: Igor"),
                 textOutput("avgTotalTimeIgor"),
                 br(),
                 p("Łączny średni czas: Wiktoria"),
                 textOutput("avgTotalTimeWiktoria"),
                 br(),
                 p("Łączny średni czas: Bartosz"),
                 textOutput("avgTotalTimeBartosz")
               ),
               mainPanel(
                 plotlyOutput("appsHeatmap", height = "600px")
               )
             )
    ),
    
    "Aplikacje",
    
    tabPanel("Czas spędzony w poszczególne dni tygodnia",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(
                   "users_bar", 
                   "Wybierz użytkowników:",
                   choices = list(
                     "Igor Zawada" = "cumulated_time_igor",
                     "Wiktoria Kawa" = "cumulated_time_wiktoria",
                     "Bartosz Ząbkowski" = "cumulated_time_bartosz"
                   ),
                   selected = NULL
                 ),
                 br(),
                 radioButtons(
                   "weekChoice", 
                   "Wybierz zakres:",
                   choices = c(
                     "2024-12-16 - 2024-12-22"="1",
                     "2024-12-23 - 2024-12-29"="2",
                     "2024-12-30 - 2025-01-05"="3"
                   ),
                   selected = NULL
                 )
               ),
               mainPanel(
                 plotlyOutput("weeklyUsageBar", height = "600px")
               )
             )
    ),
    
    
    # ---------------- Minigra ----------------
    tabPanel("Minigra - Kto najdłużej korzystał z aplikacji?",
             uiOutput("game_ui")
    )
  )
)

# --------------------------------------------------------
# SERVER
# --------------------------------------------------------
server <- function(input, output, session) {
  
  # ======================================================
  # 1) Statystyki liczby odblokowań
  #    (zakładka "Ilość odblokowań telefonu")
  # ======================================================
  
  output$averageUnlocksAll <- renderText({
    filtered_data <- all_data %>%
      filter(
        user %in% input$users,
        date >= input$dateRange[1],
        date <= input$dateRange[2]
      )
    if(nrow(filtered_data) == 0) {
      return("0")
    } else {
      total_days    <- as.integer(input$dateRange[2] - input$dateRange[1]) + 1
      total_unlocks <- sum(filtered_data$unlocks, na.rm = TRUE)
      avg_unlocks   <- total_unlocks / total_days
      return(round(avg_unlocks, 2))
    }
  })
  
  output$averageUnlocksIgor <- renderText({
    filtered_data <- all_data %>%
      filter(
        user == "Igor Zawada",
        user %in% input$users,
        date >= input$dateRange[1],
        date <= input$dateRange[2]
      )
    if(nrow(filtered_data) == 0) {
      return("0")
    } else {
      total_days    <- as.integer(input$dateRange[2] - input$dateRange[1]) + 1
      total_unlocks <- sum(filtered_data$unlocks, na.rm = TRUE)
      avg_unlocks   <- total_unlocks / total_days
      return(round(avg_unlocks, 2))
    }
  })
  
  output$averageUnlocksWiktoria <- renderText({
    filtered_data <- all_data %>%
      filter(
        user == "Wiktoria Kawa",
        user %in% input$users,
        date >= input$dateRange[1],
        date <= input$dateRange[2]
      )
    if(nrow(filtered_data) == 0) {
      return("0")
    } else {
      total_days    <- as.integer(input$dateRange[2] - input$dateRange[1]) + 1
      total_unlocks <- sum(filtered_data$unlocks, na.rm = TRUE)
      avg_unlocks   <- total_unlocks / total_days
      return(round(avg_unlocks, 2))
    }
  })
  
  output$averageUnlocksBartus <- renderText({
    filtered_data <- all_data %>%
      filter(
        user == "Bartosz Ząbkowski",
        user %in% input$users,
        date >= input$dateRange[1],
        date <= input$dateRange[2]
      )
    if(nrow(filtered_data) == 0) {
      return("0")
    } else {
      total_days    <- as.integer(input$dateRange[2] - input$dateRange[1]) + 1
      total_unlocks <- sum(filtered_data$unlocks, na.rm = TRUE)
      avg_unlocks   <- total_unlocks / total_days
      return(round(avg_unlocks, 2))
    }
  })
  
  # Wykres odblokowań telefonu w czasie
  output$unlockPlot <- renderPlotly({
    min_x <- min(all_data$date)
    max_x <- max(all_data$date)
    max_y <- max(all_data$unlocks, na.rm = TRUE)
    
    filtered_data <- all_data %>%
      filter(
        user %in% input$users,
        date >= input$dateRange[1],
        date <= input$dateRange[2]
      )
    
    if (nrow(filtered_data) == 0) {
      # Gdy brak danych
      dummy_data <- data.frame(
        date   = as.Date(character()),
        unlocks= numeric(),
        user   = factor(character(), levels = c("Igor Zawada", "Wiktoria Kawa", "Bartosz Ząbkowski"))
      )
      
      p <- ggplot(dummy_data, aes(x = date, y = unlocks, color = user, group = user)) +
        geom_blank() +
        scale_color_manual(
          values = c("Igor Zawada"="#16a085", 
                     "Wiktoria Kawa"="#c0392b", 
                     "Bartosz Ząbkowski"="#8e44ad"),
          breaks = c("Igor Zawada","Wiktoria Kawa","Bartosz Ząbkowski"),
          labels = c("Igor","Wiktoria","Bartosz")
        ) +
        scale_x_date(limits = c(min_x, max_x), date_breaks = "2 days", date_labels = "%d-%m") +
        scale_y_continuous(limits = c(0, max_y)) +
        labs(
          title = "Liczba odblokowań telefonu w czasie",
          x = "Data",
          y = "Liczba odblokowań",
          color = "Użytkownik"
        ) +
        theme_minimal(base_family = "Roboto") +
        theme(
          plot.background   = element_rect(fill = "#121212", color = NA),
          panel.background  = element_rect(fill = "#121212", color = NA),
          text              = element_text(color = "#ecf0f1"),
          axis.text         = element_text(color = "#ecf0f1"),
          axis.title        = element_text(color = "#ecf0f1"),
          panel.grid.major  = element_line(color = "#ecf0f1", size = 0.5),
          panel.grid.minor  = element_line(color = "#ecf0f1", size = 0.3),
          legend.position   = "right",
          legend.key.width  = unit(1, "cm"),
          legend.key.height = unit(0.8, "cm"),
          legend.spacing    = unit(0.5, "cm")
        )
      
    } else {
      
      p <- ggplot(filtered_data, aes(x = date, y = unlocks, color = user, group = user)) +
        geom_line(size = 1) +
        geom_point(
          aes(text = paste0("Użytkownik: ", user,
                            "<br>Data: ", date,
                            "<br>Odblokowania: ", unlocks)),
          size = 4
        ) +
        scale_color_manual(
          values = c("Igor Zawada"="#16a085", 
                     "Wiktoria Kawa"="#c0392b", 
                     "Bartosz Ząbkowski"="#8e44ad"),
          breaks = c("Igor Zawada","Wiktoria Kawa","Bartosz Ząbkowski"),
          labels = c("Igor","Wiktoria","Bartosz")
        ) +
        scale_x_date(limits = c(min_x, max_x), date_breaks = "2 days", date_labels = "%d-%m") +
        scale_y_continuous(limits = c(0, max_y)) +
        labs(
          title = "Liczba odblokowań telefonu w czasie",
          x = "Data",
          y = "Liczba odblokowań",
          color = "Użytkownik"
        ) +
        theme_minimal(base_family = "Roboto") +
        theme(
          plot.background   = element_rect(fill = "#121212", color = NA),
          panel.background  = element_rect(fill = "#121212", color = NA),
          text              = element_text(color = "#ecf0f1"),
          axis.text         = element_text(color = "#ecf0f1"),
          axis.title        = element_text(color = "#ecf0f1"),
          panel.grid.major  = element_line(color = "#ecf0f1", size = 0.5),
          panel.grid.minor  = element_line(color = "#ecf0f1", size = 0.3),
          legend.position   = "right",
          legend.key.width  = unit(1, "cm"),
          legend.key.height = unit(0.8, "cm"),
          legend.spacing    = unit(0.5, "cm")
        )
    }
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      layout(autosize = TRUE, margin = list(l=50, r=50, b=50, t=50))
  })
  
  
  # ======================================================
  # 2) Minigra - Kto najdłużej korzystał z aplikacji?
  # ======================================================
  
 game_state <- reactiveVal("start")
  random_app <- reactiveVal(NULL)
  correct_user <- reactiveVal(NULL)
  
  observeEvent(input$next_question, {
    game_state("question")
    random_app(sample(all_apps, 1))
    
    app_times <- avg_times %>%
      filter(app == random_app()) %>%
      mutate(time_sec = as.numeric(as.POSIXct(time, format="%H:%M:%S") - 
                                     as.POSIXct("1970-01-01", tz="UTC"))) %>%
      arrange(desc(time_sec))
    
    if (nrow(app_times) > 0) {
      correct_user(app_times$user[1])
    } else {
      correct_user(NULL)
    }
  })
  
  observeEvent(input$submit, {
    game_state("result")
  })
  
  output$game_ui <- renderUI({
    if (game_state() == "start") {
      fluidRow(
        column(12, align="center", "Kliknij przycisk Start, aby rozpocząć grę!"),
        column(12, align="center",
               actionButton("next_question", "Start", class="btn-primary btn-lg"))
      )
    } else if (game_state() == "question") {
      tagList(
        h3(paste("Zgadnij, kto najwięcej korzystał z aplikacji:", random_app())),
        radioButtons("user_choice", "Wybierz użytkownika:",
                     choices=c("Igor Zawada","Wiktoria Kawa","Bartosz Ząbkowski")),
        actionButton("submit", "Sprawdź odpowiedź", class="btn-success btn-lg")
      )
    } else if (game_state() == "result") {
      selected_user <- input$user_choice
      result_message <- if (selected_user == correct_user()) {
        "Gratulacje! To poprawna odpowiedź!"
      } else {
        paste("Niestety, poprawna odpowiedź to:", correct_user())
      }
      
      tagList(
        h3(result_message),
        tableOutput("average_times_table"),
        plotlyOutput("time_trend_plot"),
        actionButton("next_question", "Dalej", class="btn-primary btn-lg"),
      )
    }
  })
  
  output$average_times_table <- renderTable({
    if (game_state() == "result") {
      avg_times %>%
        filter(app == random_app()) %>%
        arrange(desc(time))
    } else {
      NULL
    }
  })
  
  
  output$time_trend_plot <- renderPlotly({
    
    apps_igor2 <- apps_igor
    apps_wiktoria2 <- apps_wiktoria
    
    apps_igor2 <- apps_igor2 %>%
      rename(
        "Geometry Dash" = "Geometry.Dash"
      )
    
    apps_wiktoria2 <- apps_wiktoria2 %>%
      rename(
        "2048" = "X2048"
      )
    
    
    if (game_state() == "result") {
      # filtruemy dane na podstawie wybranej aplikacji
      selected_app <- random_app()
      app_usage_over_time <- bind_rows(
        apps_igor2 %>% mutate(user = "Igor Zawada"),
        apps_wiktoria2 %>% mutate(user = "Wiktoria Kawa"),
        apps_bartosz %>% mutate(user = "Bartosz Ząbkowski")
      ) %>%
        select(Date, all_of(selected_app), user) %>%
        rename(time = all_of(selected_app)) %>%
        mutate(
          # Konwertujemy czas z formatu HH:MM:SS na minuty
          time_min = sapply(time, format_time_to_minutes)
        ) 

      format_time_to_minutes <- function(time_str) {
        # time_str nie jest pusty lub NA
        if (is.na(time_str) || time_str == "") {
          return(NA)  # time_str jest pusty lub NA, zwracamy NA
        }
        
        # rozdzielamy czas na godziny, minuty i sekundy
        hms <- unlist(strsplit(time_str, ":"))
        
        # nie udało się rozdzielić to zwrócimy NA
        if (length(hms) != 3) {
          return(NA)
        }

        hours <- as.numeric(hms[1])
        minutes <- as.numeric(hms[2])
        seconds <- as.numeric(hms[3])
        
        # całkowity czas w minutach
        total_minutes <- hours * 60 + minutes + seconds / 60
        
        return(total_minutes)
      }
      
      
      
      # maksymalny czas dla danej aplikacji
      # dodajemy 15 zebt wydluzyc os y
      max_time <- max(app_usage_over_time$time_min, na.rm = TRUE)
      # Dodajemy margines 10% do maksymalnej wartości
      max_time_with_margin <- max_time * 1.1
      
      
      # formatowanie minut na HH:MM
      format_hours_minutes <- function(x) {
        hours <- floor(x / 60)
        minutes <- round(x %% 60)
        sprintf("%02d:%02d", hours, minutes)
      }
      
      # wykres ggplot
      p <- ggplot(app_usage_over_time, aes(x = Date, y = time_min, color = user, group = user)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        labs(
          title = paste("Trend korzystania z aplikacji:", selected_app),
          x = "Data",
          y = "Czas korzystania",
          color = "Użytkownik"
        ) +
        scale_y_continuous(
          labels = function(x) format_hours_minutes(x),  # Używamy funkcji do konwersji minut na format HH:MM
          limits = c(0, max_time_with_margin),  # Skalowanie osi Y na podstawie maksymalnej wartości dla tej aplikacji
          expand = c(0, 0)  # Brak marginesu na końcu osi Y
        ) +  
        theme_minimal(base_family = "Roboto") +
        theme(
          plot.background  = element_rect(fill = "#121212", color = NA),
          panel.background = element_rect(fill = "#121212", color = NA),
          text             = element_text(color = "#ecf0f1"),
          axis.text.x      = element_text(angle = 45, hjust = 1, color = "#ecf0f1"),  # Obrót etykiet osi X
          axis.text.y      = element_text(color = "#ecf0f1"),  # Białe etykiety osi Y
          axis.title.x     = element_text(color = "#ecf0f1"),  # Białe tytuły osi X
          axis.title.y     = element_text(color = "#ecf0f1")   # Białe tytuły osi Y
        )
      
      # konwersja do ggplotly z dynamicznym dopasowaniem
      ggplotly(p) %>%
        config(displayModeBar = FALSE) %>%
        layout(autosize = TRUE, margin = list(l = 50, r = 50, b = 50, t = 50))
    } else {
      NULL
    }
  })
  
  
  
  # ======================================================
  # 3) Aplikacje - Heatmapa ze średnim czasem korzystania
  # ======================================================
  
  # 3a) Funkcja pomocnicza: parsowanie "HH:MM:SS" -> minuty
  parse_time_to_minutes <- function(time_str) {
    if (is.na(time_str) || time_str == "") return(NA_real_)
    hms <- unlist(strsplit(time_str, ":"))
    hours   <- as.numeric(hms[1])
    minutes <- as.numeric(hms[2])
    seconds <- as.numeric(hms[3])
    total_minutes <- hours*60 + minutes + seconds/60
    return(total_minutes)
  }
  
  # 3b) Funkcja tworząca ramkę danych ze średnim czasem
  #     w podanym zakresie dat
  get_avg_usage <- function(app_data, start_date, end_date, user_name) {
    # Upewnij się, że kolumna daty to "Date"
    app_data$Date <- as.Date(app_data$Date)
    
    # Filtrowanie po dacie
    filtered <- app_data %>%
      filter(Date >= start_date & Date <= end_date)
    
    # Które kolumny to aplikacje? Wszystkie poza "Date"
    app_cols <- setdiff(names(filtered), "Date")
    
    # Zamiana "HH:MM:SS" na minuty
    for (col in app_cols) {
      filtered[[col]] <- sapply(filtered[[col]], parse_time_to_minutes)
    }
    
    # Obliczamy średnią (po wszystkich dniach) dla każdej aplikacji
    avg_vals <- sapply(filtered[app_cols], mean, na.rm = TRUE)
    
    data.frame(
      user        = user_name,
      app         = names(avg_vals),
      avg_minutes = as.numeric(avg_vals),
      stringsAsFactors = FALSE
    )
  }
  
  # 3c) Reaktywnie budujemy 1 dużą ramkę w oparciu o checkboxy
  all_users_apps_df <- reactive({
    req(input$users_apps)
    
    results_list <- list()
    
    if ("Igor Zawada" %in% input$users_apps) {
      results_list[["Igor"]] <- get_avg_usage(
        apps_igor,
        start_date = input$dateRangeApps[1],
        end_date   = input$dateRangeApps[2],
        user_name  = "Igor Zawada"
      )
    }
    
    if ("Wiktoria Kawa" %in% input$users_apps) {
      results_list[["Wiktoria"]] <- get_avg_usage(
        apps_wiktoria,
        start_date = input$dateRangeApps[1],
        end_date   = input$dateRangeApps[2],
        user_name  = "Wiktoria Kawa"
      )
    }
    
    if ("Bartosz Ząbkowski" %in% input$users_apps) {
      results_list[["Bartosz"]] <- get_avg_usage(
        apps_bartosz,
        start_date = input$dateRangeApps[1],
        end_date   = input$dateRangeApps[2],
        user_name  = "Bartosz Ząbkowski"
      )
    }
    
    final_df <- bind_rows(results_list)
    return(final_df)
  })
  
  # 3d) Renderowanie heatmapy
  output$appsHeatmap <- renderPlotly({
    
    df <- all_users_apps_df()
    
    # Gdy brak danych
    if (is.null(df) || nrow(df) == 0) {
      p <- ggplot() +
        ggtitle("Brak danych (nie wybrano użytkowników lub brak danych w tym zakresie dat)") +
        theme_minimal(base_family = "Roboto") +
        theme(
          plot.background  = element_rect(fill = "#121212", color = NA),
          panel.background = element_rect(fill = "#121212", color = NA),
          text             = element_text(color = "#ecf0f1")
        )
      return(ggplotly(p))
    }
    
    p <- ggplot(df, aes(x = app, y = user, fill = avg_minutes)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "red", na.value = "grey90") +
      labs(
        title = "Średni czas korzystania z aplikacji (w minutach)",
        x = "Aplikacja",
        y = "Użytkownik",
        fill = "Minuty"
      ) +
      theme_minimal(base_family = "Roboto") +
      theme(
        plot.background   = element_rect(fill = "#121212", color = NA),
        panel.background  = element_rect(fill = "#121212", color = NA),
        text              = element_text(color = "#ecf0f1"),
        axis.text.x       = element_text(color = "#ecf0f1", angle = 45, hjust = 1),
        axis.text.y       = element_text(color = "#ecf0f1"),
        axis.title        = element_text(color = "#ecf0f1"),
        panel.grid.major  = element_line(color = "#ecf0f1", size = 0.2),
        panel.grid.minor  = element_blank(),
        legend.position   = "right"
      )
    
    ggplotly(p) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        autosize = TRUE,
        margin = list(l = 50, r = 50, b = 80, t = 50)
      )
  })
  
  # 3e) TextOutput: Łączny średni czas (średnia ze średnich)
  output$avgTotalTimeAll <- renderText({
    df <- all_users_apps_df()
    if (is.null(df) || nrow(df) == 0) return("0")
    avg_all <- mean(df$avg_minutes, na.rm = TRUE)
    round(avg_all, 1)
  })
  
  output$avgTotalTimeIgor <- renderText({
    df <- all_users_apps_df()
    if (is.null(df) || nrow(df) == 0) return("0")
    df_igor <- df %>% filter(user == "Igor Zawada")
    if(nrow(df_igor) == 0) return("0")
    round(mean(df_igor$avg_minutes, na.rm = TRUE), 1)
  })
  
  output$avgTotalTimeWiktoria <- renderText({
    df <- all_users_apps_df()
    if (is.null(df) || nrow(df) == 0) return("0")
    df_wika <- df %>% filter(user == "Wiktoria Kawa")
    if(nrow(df_wika) == 0) return("0")
    round(mean(df_wika$avg_minutes, na.rm = TRUE), 1)
  })
  
  output$avgTotalTimeBartosz <- renderText({
    df <- all_users_apps_df()
    if (is.null(df) || nrow(df) == 0) return("0")
    df_bart <- df %>% filter(user == "Bartosz Ząbkowski")
    if(nrow(df_bart) == 0) return("0")
    round(mean(df_bart$avg_minutes, na.rm = TRUE), 1)
  })
  
  # --------------------------------------------------------
  # używane Aplikacje
  # --------------------------------------------------------
  
  
  
  
  output$weeklyUsageBar <- renderPlotly({
    
    req(input$users_bar, input$weekChoice) 
    
    process_user_data <- function(data, user_name,week_choice) {
      
      if (!"Date" %in% names(data)) {
        stop("Data frame musi zawierać kolumnę 'Date'.")
      }
      
      
      if (week_choice == "1") {
        selected_dates <- seq(as.Date("2024-12-16"), as.Date("2024-12-22"), by = "day")
      } else if (week_choice == "2") {
        selected_dates <- seq(as.Date("2024-12-23"), as.Date("2024-12-29"), by = "day")
      } else if (week_choice == "3") {
        selected_dates <- seq(as.Date("2024-12-30"), as.Date("2025-01-05"), by = "day")
      } else {
        stop("Nieprawidłowa wartość dla week_choice.")
      }
      # Filtrowanie danych po zakresie dat
      data <- data %>% filter(as.Date(Date) %in% selected_dates)
      
      
      
      numeric_cols <- setdiff(names(data), "Date")
      
      
      data[numeric_cols] <- lapply(data[numeric_cols], function(col) {
        as.numeric(lubridate::hms(col)) / 60
      })
      
      
      data$row_sum <- rowSums(data[, numeric_cols], na.rm = TRUE)
      
      
      data <- data %>%
        mutate(User = user_name,row_sum = floor(row_sum)) %>%
        select(Date, User, row_sum)
      
      return(data)
    }
    
    cumulated_time_igor <- process_user_data(apps_igor, "Igor Zawada",input$weekChoice)
    cumulated_time_wiktoria <- process_user_data(apps_wiktoria, "Wiktoria Kawa",input$weekChoice)
    cumulated_time_bartosz <- process_user_data(apps_bartosz, "Bartosz Ząbkowski",input$weekChoice)
    
    
    
    stacked_data <- bind_rows(
      if ("cumulated_time_bartosz" %in% input$users_bar) cumulated_time_bartosz else NULL,
      if ("cumulated_time_wiktoria" %in% input$users_bar) cumulated_time_wiktoria else NULL,
      if ("cumulated_time_igor" %in% input$users_bar) cumulated_time_igor else NULL
    )
    stacked_data <- stacked_data %>%mutate(Dzien = rep(c("Pon","Wt","Sr","Czw","Pt","Sob","Ndz"), length.out = n()))%>%
      select(-c(Date))
    
    if ("cumulated_time_igor" %in% input$users_bar && input$weekChoice == "1") {
      stacked_data <- stacked_data %>%
        add_row(User = "Igor Zawada", row_sum = 0, Dzien = "Ndz")
    }
    
    days_order <- c("Pon", "Wt", "Sr", "Czw", "Pt", "Sob", "Ndz")
    stacked_data$Dzien <- factor(stacked_data$Dzien, levels = days_order)
    
    user_colors <- c(
      "Igor Zawada" = "#BC3A3A", 
      "Bartosz Ząbkowski" = "#4B683D",
      "Wiktoria Kawa" = "#BC953A"
    )
    
    ggplot(stacked_data, aes(x = Dzien, y = row_sum, fill = User)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = user_colors) +
      scale_y_continuous(
        limits = c(0, 500)
      )+
      labs(
        title = "Aktywność użytkowników w dniach tygodnia",
        x = "Dzień tygodnia",
        y = "Czas spędzony na telefonie w minutach",
      ) +
      theme_minimal(base_family = "Roboto") +
      theme(
        plot.background   = element_rect(fill = "#121212", color = NA),
        panel.background  = element_rect(fill = "#121212", color = NA),
        text              = element_text(color = "#ecf0f1"),
        axis.text.x       = element_text(color = "#ecf0f1", angle = 45, hjust = 1),
        axis.text.y       = element_text(color = "#ecf0f1"),
        axis.title        = element_text(color = "#ecf0f1"),
        panel.grid.major  = element_line(color = "#ecf0f1", size = 0.2),
        panel.grid.minor  = element_blank(),
        legend.position   = "right"
      )
    
    
    
  })
  
  
}





# --------------------------------------------------------
# Uruchomienie aplikacji
# --------------------------------------------------------
shinyApp(ui = ui, server = server)