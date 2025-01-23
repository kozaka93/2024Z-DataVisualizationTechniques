library(shiny)
library(ggplot2)
library(plotly)

# Funkcje ekstrakcji danych
extract_white_players <- function(pgn_lines) {
  white_lines <- grep("^\\[White \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  sub('^\\[White "(.*?)"\\]$', '\\1', white_lines)
}
extract_black_players <- function(pgn_lines) {
  black_lines <- grep("^\\[Black \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  sub('^\\[Black "(.*?)"\\]$', '\\1', black_lines)
}
extract_results <- function(pgn_lines) {
  results <- grep("^\\[Result \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  sub('^\\[Result "(.*?)"\\]$', '\\1', results)
}
extract_dates <- function(pgn_lines) {
  dates <- grep("^\\[UTCDate \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  sub('^\\[UTCDate "(.*?)"\\]$', '\\1', dates)
}
extract_white_elo <- function(pgn_lines) {
  white_elo <- grep("^\\[WhiteElo \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  sub('^\\[WhiteElo "(.*?)"\\]$', '\\1', white_elo)
}
extract_black_elo <- function(pgn_lines) {
  black_elo <- grep("^\\[BlackElo \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  sub('^\\[BlackElo "(.*?)"\\]$', '\\1', black_elo)
}
extract_eco <- function(pgn_lines) {
  eco <- grep("^\\[ECO \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  sub('^\\[ECO "(.*?)"\\]$', '\\1', eco)
}
extract_opening <- function(pgn_lines) {
  opening <- grep("^\\[Opening \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  sub('^\\[Opening "(.*?)"\\]$', '\\1', opening)
}
extract_moves <- function(pgn_lines) {
  moves <- pgn_lines[substring(pgn_lines, 1, 1) == "1" | substring(pgn_lines, 1, 1) == " "]
  moves
}


# Wczytaj dane graczy
load_data <- function(filepath, player_name) {
  pgn_text <- readLines(filepath)
  pgn_text <- pgn_text[nzchar(pgn_text)]
  
  white <- extract_white_players(pgn_text)
  black <- extract_black_players(pgn_text)
  result <- extract_results(pgn_text)
  date <- extract_dates(pgn_text)
  white_elo <- extract_white_elo(pgn_text)
  black_elo <- extract_black_elo(pgn_text)
  eco <- extract_eco(pgn_text)
  opening_variation <- extract_opening(pgn_text)
  opening <- sub("\\:.*", "", opening_variation)
  moves <- extract_moves(pgn_text)
  
  data <- data.frame(white, black, result, date, white_elo, 
                     black_elo, opening, opening_variation, eco, moves)
  data <- data[data$white == player_name | data$black == player_name, ]
  data$date <- as.Date(data$date, format = "%Y.%m.%d")
  data$elo <- as.numeric(ifelse(data$white == player_name, 
                                data$white_elo, data$black_elo))
  
  # Dodajemy ID jako unikalny identyfikator
  data$id <- nrow(data):1 # Przypisujemy unikalny numer ID
  
  opening_counts <- table(data$opening)
  valid_openings <- names(opening_counts[opening_counts >= 5])
  data <- data[data$opening %in% valid_openings & data$opening != "?", ]
  
  return(data)
}

# Załaduj dane graczy
wittchen_data <- load_data("www/lichess_wittchen_2024-12-18.pgn", "wittchen")
tadziolul_data <- load_data("www/lichess_tadziolul_2024-12-18.pgn", "tadziolul")

# Funkcja do analizy pola matów
get_mate_square <- function(moves) {
  # Znajduje ruchy zawierające mat (#)
  mate_moves <- grep("\\#", moves)
  if (length(mate_moves) > 0) {
    # Ostatni ruch z matującym ruchem
    mate_move <- moves[max(mate_moves)]
    
    # Ekstrakcja współrzędnych na podstawie pozycji znaku #
    square <- substr(mate_move, nchar(mate_move) - 6, nchar(mate_move) - 5)
    return(square)
  }
  return(NA)
}

# Dodaj kolumnę z polem mata w danych
add_mate_squares <- function(data) {
  data$mate_square <- sapply(data$moves, get_mate_square)
  return(data)
}

get_mating_piece <- function(moves) {
  move_list <- unlist(strsplit(moves, " "))
  mating_move <- grep("#", move_list, value = TRUE)
  if (length(mating_move) == 0) {
    return(NA)
  }
  last_move <- mating_move[length(mating_move)]
  piece <- sub("[a-h1-8x+#=].*", "", last_move)
  if (piece == "") {
    piece <- "P"  # Pionek
  }
  return(piece)
}

add_mating_pieces <- function(data) {
  data$mate_piece <- sapply(data$moves, get_mating_piece)
  return(data)
}

get_moves_number <- function(moves) {
  # Znajdź wszystkie numery ruchów w ciągu partii
  matches <- regmatches(moves, gregexpr("\\d+(?=\\.)", moves, perl = TRUE))
  
  # Wyodrębnij ostatni numer ruchu
  if (length(matches[[1]]) > 0) {
    return(as.numeric(tail(matches[[1]], 1))) # Ostatni element dopasowania
  } else {
    return(NA) # Zwróć NA, jeśli nie znaleziono numerów ruchów
  }
}

add_moves_number <- function(data) {
  data$moves_number <- sapply(data$moves, get_moves_number)
  return(data)
}

# Przetwórz dane
wittchen_data <- add_mate_squares(wittchen_data)
tadziolul_data <- add_mate_squares(tadziolul_data)
wittchen_data <- add_mating_pieces(wittchen_data)
tadziolul_data <- add_mating_pieces(tadziolul_data)
wittchen_data <- add_moves_number(wittchen_data)
tadziolul_data <- add_moves_number(tadziolul_data)


# UI
ui <- navbarPage(
  "Chess Stats",
  id = "player",
  tags$head(
    tags$style(HTML("
       img {
          border: 3px solid #fff;
          border-radius: 8px;
          padding: 5px;
        }
       .navbar-brand {
         font-weight: bold;
       }
       body {
         background-color: #222222;
         color: #bbbbbb;
         font-family: Helvetica, sans-serif
       }
       h2 {
         text-align: center; 
         font-weight: bold; 
       }
     "))
  ),
  tabPanel(
    "Strona główna",
    fluidPage(
      titlePanel("Kim jesteśmy?"),
      fluidRow(
        column(6, htmlOutput("aboutW")),
        column(6, htmlOutput("aboutT"))
      ),
      fluidRow(
        column(12, htmlOutput("aboutA"))
      )
    )
  ),
  tabPanel(
    "Gabriel - statystyki",
    fluidPage(
      titlePanel("Gabriel - statystyki"),
      fluidRow(
        column(12, dateRangeInput("dateRangeW", "Wybierz zakres dat:", 
                                  start = min(wittchen_data$date), 
                                  end = max(wittchen_data$date),
                                  min = min(wittchen_data$date), 
                                  max = max(wittchen_data$date)))
      ),
      plotlyOutput("ratingPlotW", height = "300px"),
      fluidRow(
        style = "padding-top: 50px;",
        column(6, style = "display: flex; justify-content: flex-end;", selectInput("openingW", "Wybierz otwarcie:", 
                                                                                   choices = c("Wszystkie" = "all", unique(wittchen_data$opening)))),
        column(6, class="text-center", uiOutput("gifW"))
      ),
      fluidRow(
        style = "padding-bottom: 50px;",
        column(12, htmlOutput("statsW"))
      ),
      fluidRow(
        column(6, plotlyOutput("mateChessboardW")),
        column(6, plotlyOutput("matePiecesPlotW"))
      ),
      fluidRow(
        style = "padding-bottom: 30px;",
        column(12, plotlyOutput("movesNumberW"))
      ),
      fluidRow(
        column(12, htmlOutput("seriesW"))
      ),
      fluidRow(
        column(12, htmlOutput("topResultsW"))
      )
    )
  ),
  tabPanel(
    "Kuba - statystyki",
    fluidPage(
      titlePanel("Kuba - statystyki"),
      fluidRow(
        column(12, dateRangeInput("dateRangeT", "Wybierz zakres dat:", 
                                  start = min(tadziolul_data$date), 
                                  end = max(tadziolul_data$date),
                                  min = min(tadziolul_data$date), 
                                  max = max(tadziolul_data$date)))
      ),
      plotlyOutput("ratingPlotT", height = "300px"),
      fluidRow(
        style = "padding-top: 50px;",
        column(6, style = "display: flex; justify-content: flex-end;", selectInput("openingT", "Wybierz otwarcie:", 
                                                                                   choices = c("Wszystkie" = "all", unique(tadziolul_data$opening)))),
        column(6, class="text-center", uiOutput("gifT"))
      ),
      fluidRow(
        style = "padding-bottom: 50px;",
        column(12, htmlOutput("statsT"))
      ),
      fluidRow(
        column(6, plotlyOutput("mateChessboardT")),
        column(6, plotlyOutput("matePiecesPlotT"))
      ),
      fluidRow(
        style = "padding-bottom: 30px;",
        column(12, plotlyOutput("movesNumberT"))
      ),
      fluidRow(
        column(12, htmlOutput("seriesT"))
      ),
      fluidRow(
        column(12, htmlOutput("topResultsT"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Funkcja calculate_longest_streaks
  calculate_longest_streaks <- function(data_filtered) {
    # Tworzymy wektor wyników ("W" - wygrana, "L" - porażka, "D" - remis)
    results <- ifelse(
      # Sprawdzamy, czy wygrał "wittchen" lub "tadziolul"
      (data_filtered$result == "1-0" & data_filtered$white %in% c("wittchen", "tadziolul")) |
        (data_filtered$result == "0-1" & data_filtered$black %in% c("wittchen", "tadziolul")), "W",
      ifelse(data_filtered$result == "1/2-1/2", "D", "L")
    )
    
    # Inicjalizacja zmiennych do liczenia najdłuższych serii
    longest_win_streak <- 0
    longest_loss_streak <- 0
    current_win_streak <- 0
    current_loss_streak <- 0
    
    # Iterujemy po wynikach, licząc serie
    for (i in 1:length(results)) {
      result <- results[i]
      
      if (result == "W") {
        current_win_streak <- current_win_streak + 1
        longest_win_streak <- max(longest_win_streak, current_win_streak)
        current_loss_streak <- 0  # resetujemy serię porażek
      } else if (result == "L") {
        current_loss_streak <- current_loss_streak + 1
        longest_loss_streak <- max(longest_loss_streak, current_loss_streak)
        current_win_streak <- 0  # resetujemy serię zwycięstw
      } else {
        # Jeżeli wynik to remis, resetujemy obie serie
        current_win_streak <- 0
        current_loss_streak <- 0
      }
    }
    
    return(list(longest_win_streak = longest_win_streak, longest_loss_streak = longest_loss_streak))
  }
  
  
  
  render_player_data <- function(data, dateRangeInput, openingInput, statsOutput, 
                                 plotOutput, seriesOutput, 
                                 topResultsOutput, gifOutput, mateChessboard, matePiecesPlot, movesNumber) {
    library(dplyr)
    
    # Dane do statystyk
    filteredDataStats <- reactive({
      data_filtered <- data[data$date >= input[[dateRangeInput]][1] &
                              data$date <= input[[dateRangeInput]][2], ]
      if (input[[openingInput]] != "all") {
        data_filtered <- data_filtered[data_filtered$opening == input[[openingInput]], ]
      }
      data_filtered
    })
    
    # Dane do wykresu
    filteredDataPlot <- reactive({
      data_filtered <- filteredDataStats()
      data_filtered <- data_filtered %>%
        group_by(date) %>%
        filter(row_number(desc(date)) == 1) %>%
        ungroup()
      data_filtered
    })
    
    output[[statsOutput]] <- renderUI({
      data_filtered <- filteredDataStats() # Używamy danych do statystyk
      
      # Obliczenia
      won <- sum((data_filtered$result == "1-0" & data_filtered$white == data_filtered$white[1]) |
                   (data_filtered$result == "0-1" & data_filtered$black == data_filtered$black[1]))
      draw <- sum(data_filtered$result == "1/2-1/2")
      lost <- sum((data_filtered$result == "0-1" & data_filtered$white == data_filtered$white[1]) |
                    (data_filtered$result == "1-0" & data_filtered$black == data_filtered$black[1]))
      total <- won + draw + lost
      
      # Procenty
      won_pct <- round((won / total) * 100, 1)
      draw_pct <- round((draw / total) * 100, 1)
      lost_pct <- round((lost / total) * 100, 1)
      
      # Pasek z HTML i CSS
      HTML(paste0(
        "<div style='display: flex; justify-content: center; align-items: center; padding-top: 20px; padding-bottom: 20px;'>",
        "<div style='width: 50%; font-family: Arial, sans-serif;'>",
        # Napisy nad paskiem
        "<div style='display: flex; justify-content: space-between; margin-bottom: 5px;'>",
        "<div style='color: #0cff00; font-weight: bold;'>", won_pct, "%</div>",
        "<div style='color: #bbbbbb; font-weight: bold;'>", draw_pct, "%</div>",
        "<div style='color: #ff0d0d; font-weight: bold;'>", lost_pct, "%</div>",
        "</div>",
        # Pasek procentowy
        "<div style='display: flex; width: 100%; height: 30px;'>",
        "<div style='width:", won_pct, "%; background-color: #0cff00; border-radius: 3px 0 0 3px;'></div>",
        "<div style='width:", draw_pct, "%; background-color: #bbbbbb;'></div>",
        "<div style='width:", lost_pct, "%; background-color: #ff0d0d; border-radius: 0 3px 3px 0;'></div>",
        "</div>",
        # Opisy pod paskiem
        "<div style='display: flex; justify-content: space-between; margin-top: 5px;'>",
        "<span style='color: #0cff00;'>", won, " wygranych</span>",
        "<span style='color: #bbbbbb;'>", draw, " remisów</span>",
        "<span style='color: #ff0d0d;'>", lost, " przegranych</span>",
        "</div>",
        "</div>",
        "</div>"
      ))
    })
    
    
    output[[plotOutput]] <- renderPlotly({
      data_filtered <- data # Używamy danych do wykresu
      
      # Filtrowanie danych na podstawie wybranego zakresu dat
      date_range_start <- input[[dateRangeInput]][1]
      date_range_end <- input[[dateRangeInput]][2]
      
      # Filtrujemy dane do zakresu dat
      data_filtered <- data_filtered[data_filtered$date >= date_range_start & data_filtered$date <= date_range_end, ]
      
      # Interpolacja danych (zwiększenie liczby punktów)
      interpolated_data <- data.frame(
        date = seq(date_range_start, date_range_end, by = "1 day")
      )
      
      # Wypełnianie brakujących dni ostatnim dostępnym rankiem
      interpolated_data$elo <- approx(
        x = as.numeric(data_filtered$date),
        y = data_filtered$elo,
        xout = as.numeric(interpolated_data$date),
        method = "constant", 
        rule = 2  # '2' oznacza, że dla brakujących dni użyj ostatniej znanej wartości
      )$y
      
      # Tworzenie wykresu
      plot <- ggplot() +
        geom_line(data = interpolated_data, aes(x = date, y = elo), 
                  color = "turquoise2", linewidth = 0.4) + # Płynna linia
        geom_point(data = interpolated_data, aes(
          x = date, y = elo, 
          text = paste0(
            "Data: ", date, "<br>",
            "Ranking: ", round(elo, 0)  # Zaokrąglamy ranking do pełnych liczb
          )
        ), 
        color = "turquoise2", size = 0.2) + # Kropki z tooltipami dla każdego dnia
        geom_ribbon(data = interpolated_data, aes(x = date, ymin = min(data_filtered$elo), ymax = elo), 
                    fill = "turquoise2", alpha = 0.2) + # Gradient pod linią
        labs(x = "Data", y = "Ranking") +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#222222", color = NA), # Tło całego wykresu
          panel.background = element_rect(fill = "#222222", color = NA), # Tło panelu
          panel.grid.major = element_line(color = "#444444"), # Linie siatki głównej
          panel.grid.minor = element_line(color = "#444444"), # Linie siatki pomocniczej
          axis.title = element_text(size = 12, color = "#bbbbbb", face = "bold"), # Tytuły osi
          axis.text = element_text(size = 10, color = "#bbbbbb"), # Tekst osi
          legend.text = element_text(color = "#bbbbbb"), # Tekst legendy
          legend.background = element_rect(fill = "#222222", color = NA) # Tło legendy
        ) +
        scale_y_continuous(limits = c(min(data_filtered$elo), NA)) # Zakres osi Y od minimalnego rankingu
      
      # Dodanie interaktywności
      ggplotly(plot, tooltip = c("text")) %>%
        layout(hoverlabel = list(align = "left"))
    })
    
    
    
    
    
    # Najdłuższa seria zwycięstw/przegranych
    output[[seriesOutput]] <- renderUI({
      data_filtered <- data
      
      streaks <- calculate_longest_streaks(data_filtered)
      
      HTML(paste(
        "<div style='text-align: center; font-size: 20px;'>",
        "<p style='font-weight: bold; font-size: 20px;'>Najdłuższa seria zwycięstw:</p>",
        "<p style='font-size: 32px; color: #0cff00; font-weight: 800'>", streaks$longest_win_streak, "</p>",
        "<p style='font-weight: bold; font-size: 20px;'>Najdłuższa seria porażek:</p>",
        "<p style='font-size: 32px; color: #ff0d0d; font-weight: 800'>", streaks$longest_loss_streak, "</p>",
        "</div>"
      ))
    })
    
    output[[topResultsOutput]] <- renderUI({
      data_filtered <- data
      
      # Filtrujemy dane tylko dla graczy "wittchen" i "tadziolul"
      filtered_players <- data_filtered[data_filtered$white %in% c("wittchen", "tadziolul") | data_filtered$black %in% c("wittchen", "tadziolul"), ]
      
      # Zwycięstwa graczy "wittchen" lub "tadziolul" - tylko w przypadku wygranej
      wins <- filtered_players[((filtered_players$result == "1-0" & filtered_players$white %in% c("wittchen", "tadziolul")) |
                                  (filtered_players$result == "0-1" & filtered_players$black %in% c("wittchen", "tadziolul"))), ]
      
      # Porażki graczy "wittchen" lub "tadziolul" - tylko w przypadku przegranej
      losses <- filtered_players[((filtered_players$result == "0-1" & filtered_players$white %in% c("wittchen", "tadziolul")) |
                                    (filtered_players$result == "1-0" & filtered_players$black %in% c("wittchen", "tadziolul"))), ]
      
      # Dla zwycięstw, bierzemy 5 najwyżej notowanych przeciwników
      wins$opponent_elo <- ifelse(wins$white == "wittchen" | wins$white == "tadziolul", as.numeric(wins$black_elo), as.numeric(wins$white_elo))
      top_wins <- wins[order(-wins$opponent_elo), ][1:5, ]
      
      # Dla porażek, bierzemy 5 najniżej notowanych przeciwników
      losses$opponent_elo <- ifelse(losses$white == "wittchen" | losses$white == "tadziolul", as.numeric(losses$black_elo), as.numeric(losses$white_elo))
      top_losses <- losses[order(losses$opponent_elo), ][1:5, ]
      
      # Tworzymy HTML z listami zwycięstw i porażek
      html_output <- tags$div(
        style = "display: flex; justify-content: space-between; padding: 20px;",
        # Lista zwycięstw
        tags$div(
          style = "flex: 1; margin-right: 10px; font-size: 20px;",
          tags$b("Najlepsze zwycięstwa (przeciwnicy i ELO):"),
          tags$ul(
            lapply(1:nrow(top_wins), function(i) {
              opponent <- ifelse(top_wins$white[i] == "wittchen" | top_wins$white[i] == "tadziolul", top_wins$black[i], top_wins$white[i])
              elo <- top_wins$opponent_elo[i]
              tags$li(paste0(opponent, " (", elo, ")"))
            })
          )
        ),
        # Lista porażek
        tags$div(
          style = "flex: 1; margin-left: 10px; font-size: 20px;",
          tags$b("Najgorsze porażki (przeciwnicy i ELO):"),
          tags$ul(
            lapply(1:nrow(top_losses), function(i) {
              opponent <- ifelse(top_losses$white[i] == "wittchen" | top_losses$white[i] == "tadziolul", top_losses$black[i], top_losses$white[i])
              elo <- top_losses$opponent_elo[i]
              tags$li(paste0(opponent, " (", elo, ")"))
            })
          )
        )
      )
      
      # Zwracamy wynik w formie HTML
      html_output
    })
    
    
    
    
    
    output[[gifOutput]] <- renderUI({
      opening <- input[[openingInput]]
      if (opening == "all") {
        opening <- "default"
      }
      
      # Tworzymy div z odpowiednim stylem
      tags$div(
        style = "display: flex; align-items: center;", # Ustawienie GIF-a i tekstu obok siebie
        tags$img(src = paste0(opening, ".gif"), height = "300px", style = "margin-right: 20px;"), # GIF po lewej
        tags$div(
          style = "flex: 1;" # Div na tekst lub inne treści po prawej
        )
      )
    })
    
    output[[mateChessboard]] <- renderPlotly({
      
      # Funkcja przygotowująca dane do wykresu
      mate_squares <- data$mate_square[!is.na(data$mate_square)]
      square_counts <- table(mate_squares)
      chessboard <- expand.grid(
        file = letters[1:8], 
        rank = 1:8
      )
      chessboard$square <- paste0(chessboard$file, chessboard$rank)
      chessboard$count <- ifelse(chessboard$square %in% names(square_counts), 
                                 square_counts[chessboard$square], 0)
      chessboard$percentage <- chessboard$count / sum(chessboard$count) * 100
      
      chessboard$tooltip_text <- paste0(
        "Pole: ", chessboard$square, "<br>",
        "Procent matów: ", round(chessboard$percentage, 4), "%"
      )
      
      # Funkcja do tworzenia wykresu szachownicy
      chessboard_plot <- ggplot(chessboard, aes(x = file, y = rank, fill = percentage, text = tooltip_text)) +
        geom_tile(color = "black", width = 1, height = 1) +  # Kwadratowe pola z czarnymi konturami
        scale_fill_gradient(low = "white", high = "turquoise2", name = "% Matów") +
        scale_y_reverse() +  # Odwróć oś Y, aby szachownica była poprawna
        labs(x = NULL, y = NULL, title = "Na których polach najczęściej dawaliśmy mata?") +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#222222", color = NA), # Tło całego wykresu
          panel.background = element_rect(fill = "#222222", color = NA), # Tło panelu
          panel.grid = element_blank(), # Usuń siatkę
          axis.text.x = element_text(size = 12, margin = margin(t = 5), color = "#bbbbbb"), # Kolor etykiet osi X
          axis.text.y = element_text(size = 12, margin = margin(r = 5), color = "#bbbbbb"), # Kolor etykiet osi Y
          plot.title = element_text(hjust = 0.5, size = 16, color = "#bbbbbb", face = "bold"), # Pogrubiony tytuł
          axis.title = element_blank(), # Brak tytułów osi
          legend.text = element_text(color = "#bbbbbb"), # Kolor tekstu legendy
          legend.title = element_text(color = "#bbbbbb"), # Kolor tytułu legendy
          legend.background = element_rect(fill = "#222222", color = NA), # Tło legendy
          legend.position = "right", # Pozycja legendy
          aspect.ratio = 1 # Ustawienie proporcji kwadratowej
        ) +
        coord_fixed(ratio = 1) +  # Wymuszenie kwadratowego kształtu
        scale_x_discrete(breaks = letters[1:8]) +  # Ustawienie etykiet na osi X (kolumny)
        scale_y_continuous(breaks = 1:8)  # Ustawienie etykiet na osi Y (numery rzędów)
      
      ggplotly(chessboard_plot, tooltip = "text")
    })
    
    
    
    
    output[[matePiecesPlot]] <- renderPlotly({
      
      # Mapowanie figur na Unicode
      piece_unicode <- c(
        "P" = "\u2659", # Pionek
        "R" = "\u2656", # Wieża
        "Q" = "\u2655", # Hetman
        "N" = "\u2658", # Skoczek
        "B" = "\u2657"  # Goniec
      )
      
      # Pełne nazwy figur
      piece_names <- c(
        "P" = "Pionek",
        "R" = "Wieża",
        "Q" = "Hetman",
        "N" = "Skoczek",
        "B" = "Goniec"
      )
      
      # Oblicz liczby matów
      piece_counts <- table(data$mate_piece)
      piece_data <- data.frame(
        piece = names(piece_counts),
        count = as.integer(piece_counts)
      )
      
      # Zastąp nazwy figur Unicode'ami i dodaj pełne nazwy
      piece_data$piece_label <- piece_unicode[piece_data$piece]
      piece_data$piece_name <- piece_names[piece_data$piece]
      piece_data$piece_label <- reorder(piece_data$piece_label, -piece_data$count)
      
      piece_data$tooltip_text <- paste0(
        "Bierka: ", piece_data$piece_name, "<br>",
        "Liczba matów: ", piece_data$count
      )
      
      # Wykres
      gg_plot <- ggplot(piece_data, aes(x = piece_label, y = count, text = tooltip_text)) +
        geom_bar(stat = "identity", fill = "turquoise2", alpha = 0.3, color = "turquoise2") + # Kontury i przezroczystość
        labs(
          title = "Która bierka najczęściej matowała?",
          x = NULL,
          y = "Liczba matów"
        ) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#222222", color = NA), # Tło całego wykresu
          panel.background = element_rect(fill = "#222222", color = NA), # Tło panelu
          plot.title = element_text(hjust = 0.5, size = 16, color = "#bbbbbb", face = "bold"), # Pogrubiony tytuł
          axis.title = element_text(size = 12, color = "#bbbbbb"), # Kolor i rozmiar tytułów osi
          axis.text = element_text(size = 10, color = "#bbbbbb"), # Kolor i rozmiar tekstu osi
          axis.text.x = element_text(hjust = 1, size = 30, color = "#bbbbbb"), # Powiększony i zmieniony kolor tekstu dla ikon
          panel.grid.major = element_line(color = "#444444"), # Linie siatki głównej
          panel.grid.minor = element_line(color = "#444444"), # Linie siatki pomocniczej
          legend.position = "none" # Usuń legendę
        )
      
      # Przekształć na interaktywny wykres z tooltipem
      ggplotly(gg_plot, tooltip = "text")
    })
    
    
    
    
    output[[movesNumber]] <- renderPlotly({
      # Zliczanie wystąpień liczby ruchów
      moves_count <- table(data$moves_number)
      moves_df <- data.frame(
        moves_number = as.numeric(names(moves_count)),
        frequency = as.integer(moves_count)
      )
      moves_df$tooltip_text <- paste0(
        "Ruchów w partii: ", moves_df$moves_number, "<br>",
        "Licznik: ", moves_df$frequency
      )
      
      # Wykres
      gg_plot <- ggplot(moves_df, aes(x = moves_number, y = frequency, text = tooltip_text)) +
        geom_bar(stat = "identity", fill = "turquoise2", alpha = 0.3, color = "turquoise2") + # Słupki
        labs(
          title = "Rozkład liczby ruchów w partiach",
          x = "Liczba ruchów",
          y = "Liczba partii"
        ) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#222222", color = NA), # Tło wykresu
          panel.background = element_rect(fill = "#222222", color = NA), # Tło panelu
          plot.title = element_text(hjust = 0.5, size = 16, color = "#bbbbbb", face = "bold"), # Pogrubiony tytuł
          axis.title = element_text(size = 12, color = "#bbbbbb"), # Tytuły osi
          axis.text = element_text(size = 10, color = "#bbbbbb"), # Tekst osi
          panel.grid.major = element_line(color = "#444444"), # Linie siatki głównej
          panel.grid.minor = element_line(color = "#444444")  # Linie siatki pomocniczej
        )
      
      ggplotly(gg_plot, tooltip = "text")
    })
    
    
  }
  
  
  # Renderowanie danych dla graczy
  render_player_data(wittchen_data, "dateRangeW", "openingW", "statsW", 
                     "ratingPlotW", "seriesW", "topResultsW", "gifW", "mateChessboardW", "matePiecesPlotW", "movesNumberW")
  render_player_data(tadziolul_data, "dateRangeT", "openingT", "statsT", 
                     "ratingPlotT", "seriesT", "topResultsT", "gifT", "mateChessboardT", "matePiecesPlotT", "movesNumberT")
  
  output[["aboutW"]] <- renderUI({
    
    HTML(paste(
      "<div style='margin-left:70px;'>",
      "<h3 style='text-align: center; font-weight: bold;'>Gabriel</h3>",
      "<div class='container' style='display: flex; justify-content: center; width: 50%; padding-bottom: 30px;'>",
      "<img src='grabus.jpg' width=60%>",
      "</div>",
      "W szachy zacząłem grać w 2019 roku, kiedy w liceum zorganizowano turniej szachowy, a ja chcąc tylko i wyłącznie uciec z lekcji",
      "języka polskiego, postanowiłem się na niego zapisać. Nie miałem zbyt dużego pojęcia o tej grze. Myliły mi się nawet czasami",
      "ruchy skoczkiem. Jednakże, na tym turnieju udało mi się zająć wysokie miejsce i to właśnie ono popchnęło mnie do potraktowania",
      "tego na poważnie. W rok udało mi się osiągnąć poziom drugiej kategorii, a od tego czasu grywam sobie bez większych zobowiązań",
      "w internecie.",
      "</div>"
    ))
  })
  output[["aboutT"]] <- renderUI({
    
    HTML(paste(
      "<div style='margin-right:70px;'>",
      "<h3 style='text-align: center; font-weight: bold;'>Kuba</h3>",
      "<div class='container' style='display: flex; justify-content: center; width: 50%; padding-bottom: 30px;'>",
      "<img src='miszczu.jpg' width=60%>",
      "</div>",
      "Moja przygoda z szachami zaczęła się w 2-4 klasie podstawówki, kiedy to grałem regularnie, chodziłem",
      "na zajęcia dodatkowe i jeździłem na turnieje. Później szachy zeszły na dalszy plan i grałem raczej okazjonalnie ",
      "ze znajomymi. Wraz z początkiem pandemii w 2020 roku zacząłem grać online z nudów i ponownie złapałem starą zajawkę. ",
      "Obecnie rozgrywanie przeze mnie partii jest raczej niesystematyczne i zależy od ilości wolnego czasu – czasem gram", 
      "dużo przez kilka tygodni, innym razem robię sobie dłuższe przerwy – szachy znów stały się częścią mojego życia i ",
      "sprawiają mi dużo przyjemności.",
      "</div>"
    ))
  })
  output[["aboutA"]] <- renderUI({
    
    HTML(paste(
      "<div style='margin-left: 70px; margin-right: 70px; padding-top: 40px;'>",
      "<h2 style='text-align: center;'>O aplikacji</h2>",
      "W 2024 roku postanowiliśmy zająć się luźną, a zarazem fascynującą analizą naszych szachowych gier, które", 
      "rozegraliśmy na popularnej platformie lichess.org. Skupiliśmy się na partiach w tempie błyskawicznym, co pozwalało", 
      "nam na szybkie, dynamiczne rozgrywki z losowo dobranymi przeciwnikami z całego świata. W ten sposób nie tylko mogliśmy ",
      "cieszyć się grą, ale również rozwijać swoje umiejętności i eksperymentować z różnymi strategiami. Na poszczególnych ",
      "zakładkach naszej aplikacji znajdziecie szczegółowe statystyki z tego okresu. Można tam śledzić nasze postępy w rankingu, ",
      "analizować najczęściej stosowane przez nas otwarcia, a także zobaczyć, jakie figury najczęściej decydowały o zwycięstwie ",
      "lub porażce. Mamy nadzieję, że ta wizualizacja danych nie tylko dostarczy ciekawych informacji, ale także zachęci ",
      "do szachowych potyczek i docenienia piękna tej gry!",
      "</div>"
    ))
  })
  
}

# Run app
shinyApp(ui = ui, server = server)