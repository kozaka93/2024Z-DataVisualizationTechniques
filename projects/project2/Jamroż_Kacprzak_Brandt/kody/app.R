library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(tidyverse)
library(jsonlite)
library(shinythemes)
library(RCurl)
library(showtext)
font_add_google(name = "Roboto", family = "Roboto")
showtext_auto()
################################################ SOME NECESSERY STAFF   #######################


karol<-read.csv("data_k.csv",sep=",")
mati<-read.csv("data_j.csv",sep=",")
brandt<-read.csv("data_b.csv",sep=",")

input_data<-"
Hog Rider	Rare	4
Little Prince	Champion	3
Lava Hound	Legendary	7
Bomber	Common	3
Fireball	Rare	4
Cannon	Common	3
Mega Knight	Legendary	7
The Log	Legendary	2
Goblin Curse	Epic	2
Mighty Miner	Champion	4
Royal Ghost	Legendary	3
Phoenix	Legendary	4
Monk	Champion	5
Valkyrie	Rare	4
Goblin Machine	Legendary	5
Skeleton King	Champion	4
Ice Spirit	Common	1
Archer Queen	Champion	5
Barbarian Launcher	Common	5
Earthquake	Rare	3
Poison	Epic	4
Tornado	Epic	3
P.E.K.K.A.	Epic	7
Mini P.E.K.K.A	Rare	4
Lumberjack	Legendary	4
Party Hat	Legendary	5
Zap	Common	2
Wizard	Rare	5
Giant Snowball	Common	2
Night Witch	Legendary	4
Party Rocket	Legendary	5
Mega Minion	Rare	3
Balloon	Epic	5
Fire Spirit	Common	2
Mini P.E.K.K.A.	Rare	4
Electro Wizard	Legendary	4
Goblinstein	Champion	2
Executioner	Epic	5
Minion Horde	Common	5
Firecracker	Common	3
Ram Rider	Legendary	5
Bandit	Legendary	3
Magic Archer	Legendary	4
Furnace	Rare	4
Fisherman	Legendary	3
Hunter	Epic	4
Knight	Common	3
Goblin Giant	Epic	6
Ice Golem	Rare	2
Royal Giant	Common	6
Arrows	Common	3
Witch	Epic	5
Goblin Drill	Epic	4
Goblin Barrel	Epic	3
Barbarian Barrel	Epic	2
Sparky	Legendary	4
Skeleton Dragons	Common	4
Skeletons	Common	1
Goblin Gang	Common	3
Dark Prince	Epic	4
Royal Delivery	Common	3
Royal Hogs	Rare	5
Goblin Demolisher	Rare	4
Inferno Tower	Rare	5
Rascals	Common	5
Mortar	Common	4
Rocket	Rare	6
Tesla	Common	4
Lightning	Epic	6
Baby Dragon	Epic	4
Musketeer	Rare	4
Ice Wizard	Legendary	3
Miner	Legendary	3
Skeleton Army	Epic	3
Prince	Epic	5
Electro Dragon	Epic	5
Snowball	Common	2
Xbow	Epic	6
Golem	Epic	8
Goblins	Common	2
Battle Ram	Rare	4
Graveyard	Legendary	5
Mother Witch	Legendary	4
Bomb Tower	Rare	4
Guards	Epic	3
Freeze	Epic	4
Elixir Golem	Rare	3
Royal Hogs	Rare	5
Rage	Epic	2
Canon	Common	3
Canon Cart	Epic	5
Inferno Dragon	Legendary	4
Tombstone	Rare	3
Giant Skeleton	Epic	6
Spear Goblins	Common	2
Minions	Common	3
Bats	Common	2
Clone	Epic	3
Skeleton Barrel	Common	3
Electro Giant	Epic	8
Giant	Rare	5
Bowler	Epic	5
Princess	Legendary	3
Zappies	Rare	4
Goblin Hut	Rare	4
Barbarians	Common	5
Elixir Collector	Rare	6
Barbarian Hut	Rare	7
Royal Recruits	Common	7
Golden Knight	Champion	4
Electro Spirit	Common	1
Three Musketeers	Rare	9
Heal Spirit	Rare	1
Dart Goblin	Rare	3
Mother Witch	Legendary	4
Battle Healer	Rare	4
Goblin Cage	Rare	4
Flying Machine	Rare	4
Elite Barbarians	Common	6
Electro Spirit	Common	1
Electro Giant	Epic	7
Mirror	Epic	0
Archers	Common	2
X-Bow	Epic	7
Wall Breakers	Epic	2"

library(data.table)
nazwy <- fread(text = input_data, header = FALSE)
colnames(nazwy) <- c("deck_cards", "rarity", "cost")

correct_arena_names <- c(
  "Miner" = "Miner's Mine",
  "P.E.K.K.A" = "P.E.K.K.A's Playhouse",
  "Builder" = "Builder's Workshop",
  "Rascal" = "Rascal's Hideout",
  "Executioner" = "Executioner's Kitchen"
)
arena_order <- c(
  "Training Camp",
  "Goblin Stadium",
  "Bone Pit",
  "Barbarian Bowl",
  "Spell Valley",
  "Builder's Workshop",
  "P.E.K.K.A's Playhouse",
  "Royal Arena",
  "Frozen Peak",
  "Jungle Arena",
  "Hog Mountain",
  "Electro Valley",
  "Spooky Town",
  "Rascal's Hideout",
  "Serenity Peak",
  "Miner's Mine",
  "Executioner's Kitchen",
  "Royal Crypt",
  "Silent Sanctuary",
  "Dragon Spa",
  "Boot Camp",
  "Clash Fest",
  "Legendary Arena"
)




extract_arena_name <- function(arena) {
  match <- regexpr("'name':\\s*['\"]([^'\"]+?)['\"]", arena, perl = TRUE)
  if (match > 0) {
    extracted <- regmatches(arena, match)
    extracted <- sub("'name':\\s*['\"]", "", extracted)
    extracted <- sub("['\"]$", "", extracted)
    return(extracted)
  } else {
    return(NA)
  }
}


extract_average_deck_cost <- function(deck_column) {
  fixed_json <- gsub("'", "\"", deck_column)
  deck_list <- tryCatch(
    fromJSON(fixed_json, simplifyVector = FALSE), 
    error = function(e) {
      warning(paste("Błąd parsowania JSON:", fixed_json))
      return(NULL)
    }
  )
  
  if (!is.null(deck_list) && is.list(deck_list)) {
    elixir_costs <- sapply(deck_list, function(card) {
      if (!is.null(card$elixirCost)) {
        card$elixirCost
      } else {
        NA
      }
    })
    return(mean(elixir_costs, na.rm = TRUE))
  } else {
    return(NA)
  }
}


extract_deck <- function(deck_column) {
  fixed_json <- gsub("'", "\"", deck_column)  # Zamiana ' na "
  deck_list <- tryCatch(
    fromJSON(fixed_json, simplifyVector = FALSE), 
    error = function(e) {
      warning(paste("Błąd parsowania JSON:", fixed_json))
      return(NULL)
    }
  )
  
  # Pobranie nazw kart
  if (!is.null(deck_list) && is.list(deck_list)) {
    sapply(deck_list, function(card) {
      if (!is.null(card$name)) {
        card$name
      } else {
        NA
      }
    })
  } else {
    return(NA)
  }
}


process_player_data <- function(dataset) {
  dataset %>%
    mutate(
      average_deck_cost = sapply(currentDeck, extract_average_deck_cost),
      arena_name = sapply(arena, function(arena) {
        arena_extracted <- extract_arena_name(arena)
        if (!is.na(arena_extracted) && arena_extracted %in% names(correct_arena_names)) {
          correct_arena_names[[arena_extracted]]
        } else {
          arena_extracted
        }
      })
    ) %>%
    select(player_name = name, date, arena_name, average_deck_cost) %>%
    filter(!is.na(average_deck_cost))
}


cards_frequency <- function(player_data) {
  player_data %>%
    mutate(deck_cards = lapply(currentDeck, extract_deck)) %>%
    unnest(deck_cards) %>%
    group_by(deck_cards) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(image=paste("cards\\",gsub(" ", "_", deck_cards),".png",sep="")) %>%  #scieżka bezwzględna, trzeba będzie pomyśleć jak zrobić względną
    arrange(desc(count))
}

plot_arena_summary <- function(player_data, player_name) {
  arena_summary <- player_data %>%
    group_by(arena_name) %>%
    summarise(avg_deck_cost = mean(average_deck_cost, na.rm = TRUE)) %>%
    mutate(arena_name = factor(arena_name, levels = arena_order)) %>%
    arrange(arena_name)
  
  ggplot(arena_summary, aes(x = arena_name, y = avg_deck_cost)) +
    geom_bar(stat = "identity", fill = "#ab0eb4") +
    geom_text(aes(label = format(round(avg_deck_cost, 1),1)), 
              nudge_y  = -round(arena_summary$avg_deck_cost, 2)+0.32,
              size = 4,
              fontface = "bold",
              color="white") +  # Pogrubione liczby z lewej
    geom_hline(yintercept = mean(arena_summary$avg_deck_cost, na.rm = TRUE), 
               color = "#760089", linetype = "dashed") +
    coord_flip() +
    labs(
      
      x = "",
      y = ""
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),  # Usuń główną siatkę
      panel.grid.minor = element_blank(),   # Usuń mniejszą siatkę
      plot.title = element_text(size = 16, face = "bold", color="#FFF"),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 10)
      
    )
}


process_data <- function(data) {
  
  
  data_sorted <- data %>%
    arrange(player_tag, date)
  
  # Dodanie informacji o zmianach areny
  data_with_changes <- data_sorted %>%
    group_by(player_tag) %>%
    mutate(
      arena_change = arena != lead(arena, default = last(arena)), 
      next_losses = lead(losses),  
      next_threeCrownWins = lead(threeCrownWins), 
      next_wins = lead(wins),  
      is_last_arena = row_number() == n() 
    ) %>%
    ungroup()
  
  # Przygotowanie tabeli wynikowej, giga istotne
  arena_summary_data <- data_with_changes %>%
    filter(arena_change == TRUE | is_last_arena == TRUE) %>% 
    mutate(
      losses_battles = case_when(  
        is_last_arena ~ losses, 
        TRUE ~ next_losses 
      ),
      threeCrownWins_battles = case_when(  
        is_last_arena ~ threeCrownWins, 
        TRUE ~ next_threeCrownWins 
      ),
      wins_battles = case_when(  
        is_last_arena ~ wins, 
        TRUE ~ next_wins 
      )
    ) %>%
    select(player_tag, arena, losses_battles, threeCrownWins_battles, wins_battles) %>% 
    distinct(player_tag, arena, losses_battles, threeCrownWins_battles, wins_battles) 
  
  
  arena_summary_data <- arena_summary_data %>%
    group_by(player_tag) %>%
    mutate(
      prev_losses = lag(losses_battles, default = 0),  
      prev_threeCrownWins = lag(threeCrownWins_battles, default = 0), 
      prev_wins = lag(wins_battles, default = 0)
    ) %>%
    ungroup()
  
  arena_summary_data <- arena_summary_data %>%
    mutate(
      losses_played = losses_battles - prev_losses, 
      threeCrownWins_played = threeCrownWins_battles - prev_threeCrownWins, 
      wins_played = wins_battles - prev_wins
    ) %>% 
    gather(key = "game_type", value = "games_played", wins_played, losses_played) %>%
    mutate(game_type = factor(game_type, levels = c("wins_played", "losses_played"),
                              labels = c("Wygrane", "Przegrane")))%>%
    mutate(
      arena_name = sapply(arena, function(arena) {
        # Dopasowanie nazwy areny z uwzględnieniem apostrofów
        match <- regexpr("'name':\\s*['\"]([^'\"]+?)['\"]", arena, perl = TRUE)
        if (match > 0) {
          # Wyciągnij dopasowany fragment
          extracted <- regmatches(arena, match)
          # Usuń 'name': i cudzysłowy
          extracted <- sub("'name':\\s*['\"]", "", extracted)
          extracted <- sub("['\"]$", "", extracted)
          return(extracted)
        } else {
          return(NA)
        }
      }),
      # Nadpisz błędne nazwy na podstawie mapy poprawnych nazw
      arena_name = ifelse(arena_name %in% names(correct_arena_names),
                          correct_arena_names[arena_name],
                          arena_name)
    ) %>%
    filter(!is.na(arena_name) & arena_name != "") %>%
    mutate(arena_name = factor(arena_name, levels = arena_order))
  
  return(arena_summary_data)
}





#####################################  UI  ################################################





ui1 <- fluidPage(
  titlePanel("Brandi"),
  sidebarLayout(
    fluidRow(
      sidebarPanel(
        sliderInput("b_battles",
                    "Wybór zakresu:",
                    min = min(brandt$battleCount),
                    max = max(brandt$battleCount),
                    value = c(min(brandt$battleCount),max(brandt$battleCount)))
      )
    ),
    mainPanel = (
      fluidRow(
        column(
          h3("Wygrane vs Przegrane na każdej arenie"),
          plotlyOutput("brandt_win_loss"),
          width=6
        ),
        column(
          h3("Zależność między liczbą bitew, a ilością pucharków"),
          plotlyOutput("brandt_trophies"),
          width=6
        ),
        column(
          h3("Średni koszt postaci w decku na arenach"),
          plotlyOutput("brandt_elixir_cost"),
          width=6
        ),
        column(
          h3("Obecność postaci w używanym decku"),
          plotlyOutput("brandt_deck"),
          width=6
        )
      )
    )
  )
)

ui2 <- fluidPage(
  titlePanel("Mati"),
  sidebarLayout(
    fluidRow(
      sidebarPanel(
        sliderInput("m_battles",
                    "Wybór zakresu:",
                    min = min(mati$battleCount),
                    max = max(mati$battleCount),
                    value = c(min(mati$battleCount),max(mati$battleCount)))
      )
    ),
    mainPanel = (
      fluidRow(
        column(
          h3("Wygrane vs Przegrane na każdej arenie"),
          plotlyOutput("mati_win_loss"),
          width=6
        ),
        column(
          h3("Zależność między liczbą bitew, a ilością pucharków"),
          plotlyOutput("mati_trophies"),
          width=6
        ),
        column(
          h3("Średni koszt postaci w decku na arenach"),
          plotlyOutput("mati_elixir_cost"),
          width=6
        ),
        column(
          h3("Obecność postaci w używanym decku"),
          plotlyOutput("mati_deck"),
          width=6
        )
      )
    )
  )
)

ui3  <- fluidPage(
  titlePanel("Karol"),
  sidebarLayout(
    fluidRow(
      sidebarPanel(
        sliderInput("k_battles",
                    "Wybór zakresu:",
                    min = min(karol$battleCount),
                    max = max(karol$battleCount),
                    value = c(min(karol$battleCount),max(karol$battleCount)))
      )
    ),
    mainPanel = (
      fluidRow(
        column(
          h3("Wygrane vs Przegrane na każdej arenie"),
          plotlyOutput("karol_win_loss"),
          width=6
        ),
        column(
          h3("Zależność między liczbą bitew, a ilością pucharków"),
          plotlyOutput("karol_trophies"),
          width=6
        ),
        column(
          h3("Średni koszt postaci w decku na arenach"),
          plotlyOutput("karol_elixir_cost"),
          width=6
        ),
        column(
          h3("Obecność postaci w używanym decku"),
          plotlyOutput("karol_deck"),
          width=6
        )
      )
    )
  )
)
ui4 <- fluidPage(
  titlePanel("Strona główna"),
  fluidRow(
    # Gracz 1 - Barbarians
    column(4, 
           img(src = "barbarians.png", height = "200px", width = "auto"), 
           h3("Mati"),
           p("Nigdy nie grałam w Clash Royale, więc wszystko jest dla mnie zupełnie nowe. Nie znam ani postaci, ani kart, ani żadnych taktyk, które są używane w grze. Muszę zacząć od podstaw, ucząc się zasad i mechaniki krok po kroku.")
           
    ),
    column(4, 
           img(src = "prince.png", height = "200px", width = "auto"), 
           h3("Karol"),
           p("Kiedyś grałem w Clash Royale bardzo intensywnie, ale od dłuższego czasu miałem przerwę. Pamiętam niektóre postacie i strategie, ale wiele z nich pewnie się zmieniło albo zostało dodanych nowych. Teraz muszę odświeżyć swoje umiejętności i na nowo zapoznać się z mechaniką gry.")),
    column(4, 
           img(src = "wizard.png", height = "200px", width = "auto"), 
           h3("Brandi"),
           p(" Gram w Clash Royale od dłuższego czasu i dobrze znam większość kart oraz strategii. Regularnie grałem w klanie, zdobywałem nagrody i rywalizowałem w różnych trybach gry. Teraz rozpoczynam nową rozgrywkę, ale z doświadczeniem, które zdobyłem przez wszystkie te lata."))
  ),
  fluidRow(
    column(12, 
           h3("Porównanie wyników graczy"),
           plotlyOutput("linePlot")  # Wyświetlanie wykresu
    )
  )
)

####################################################  SERVER   ######################################
server <- function(input, output) {
  b <- reactive({
    brandt %>%
      filter(battleCount >= input$b_battles[1], battleCount <= input$b_battles[2])
  })
  
  m <- reactive({
    mati %>%
      filter(battleCount >= input$m_battles[1], battleCount <= input$m_battles[2])
  })
  
  k <- reactive({
    karol %>%
      filter(battleCount >= input$k_battles[1], battleCount <= input$k_battles[2])
  })
  
  plot1<- function(data){
    ggplotly(
      ggplot(
        process_data(data), aes(x = arena_name, y = games_played, fill = game_type)) +
        geom_bar(stat = "identity", position = "stack", color = "black") +
        labs(
          
          x = "",
          y = "Liczba gier",
          fill = ""
        ) +
        scale_fill_manual(values = c("Wygrane" = "#50a0e5", "Przegrane" = "#e82733")) + 
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid.major = element_blank(),  # Usuń główną siatkę
              panel.grid.minor = element_blank(),   # Usuń mniejszą siatkę
              panel.background = element_rect(fill = "transparent", color = NA),  # Transparent panel background
              plot.background = element_rect(fill = "transparent", color = NA),
              axis.title = element_text(size = 14, color ="#FFFFFF" , family= "Arial Black"),# Transparent plot background
              axis.text = element_text(color="#FFFFFF"),
              
              legend.text = element_text(color = "#FFFFFF")
              
        )
      
    )
  }
  
  output$brandt_win_loss <- renderPlotly({
    plot1(b())  
  })
  
  output$mati_win_loss <- renderPlotly({
    plot1(m())  
  })
  
  output$karol_win_loss <- renderPlotly({
    plot1(k())  
  })
  
  
  plot2<-function(data){
    ggplotly(
      plot_arena_summary(process_player_data(data), "Brandt")+
        theme(
          panel.grid.major = element_blank(),  # Usuń główną siatkę
          panel.grid.minor = element_blank(),   # Usuń mniejszą siatkę
          panel.background = element_rect(fill = "transparent", color = NA),  # Transparent panel background
          plot.background = element_rect(fill = "transparent", color = NA),
          axis.text = element_text(size = 16, color = "#FFFFFF"),# Transparent plot background
          
          
        )
    )
  }  
  
  output$brandt_elixir_cost<-renderPlotly(plot2(b()))
  
  output$mati_elixir_cost<-renderPlotly(plot2(m()))
  
  output$karol_elixir_cost<-renderPlotly(plot2(k()))
  
  
  plot3 <- function(data) {
    encode_image <- function(path) {
      base64enc::dataURI(file = path)
    }
    
    # Przetwarzanie danych
    data <- cards_frequency(data) 
    data$image_encoded <- sapply(data$image, encode_image)
    data <- data[order(data$count), ]
    data <- inner_join(data, nazwy) %>% distinct()
    
    # Zmiana kolejności faktorów, aby zachować porządek po sortowaniu
    data$deck_cards <- factor(data$deck_cards, levels = data$deck_cards)
    
    # Tworzenie wykresu słupkowego (poziomego)
    fig <- plot_ly(
      data,
      x = ~count,  # Liczby na osi x
      y = ~deck_cards,  # Kategorie na osi y
      type = "bar",
      orientation = "h",  # Ustawienie na poziomy wykres słupkowy
      marker = list(color = case_when(
        data$rarity == "Common" ~ "lightblue",
        data$rarity == "Rare" ~ "orange",
        data$rarity == "Epic" ~ "purple",
        TRUE ~ "pink")
      )
    )
    
    # Dodanie obrazków na końcach słupków
    images <- lapply(1:nrow(data), function(i) {
      list(
        source = data$image_encoded[i],
        x = data$count[i] - 1,  # Położenie obrazka na osi x (poza słupkiem)
        y = data$deck_cards[i],  # Położenie obrazka na osi y
        xref = "x",
        yref = "y",
        xanchor = "left",
        yanchor = "middle",
        sizex = 200,  # Rozmiar obrazka na osi x
        sizey = 1.3,  # Rozmiar obrazka na osi y
        layer = "above"
      )
    })
    
    # Dodanie obrazków do układu
    fig <- fig %>% layout(
      images = images,
      
      xaxis = list(
        title = "Liczba wystąpień",
        titlefont = list(
          color = "#FFFFFF", 
          size = 18, 
          family = "Arial Black",
          weight = "bold"  # Zamiast błędnego 'bold = TRUE'
        ),
        tickfont = list(color = "#FFFFFF") # Czarny kolor wartości osi x
      ),
      yaxis = list(
        title = "",
        titlefont = list(
          color = "#FFFFFF", 
          size = 16, 
          family = "Arial",
          weight = "bold"  # Zamiast błędnego 'bold = TRUE'
        ),
        tickfont = list(color = "#FFFFFF") # Czarny kolor wartości osi y
      ),
      plot_bgcolor = "rgba(0,0,0,0)",  # Przezroczyste tło wykresu
      paper_bgcolor = "rgba(0,0,0,0)"  # Przezroczyste tło całego obszaru
    )
    
    # Wyświetlenie wykresu
    fig
  } 
  
  output$brandt_deck<-renderPlotly(plot3(b()))
  
  output$mati_deck<-renderPlotly(plot3(m()))
  
  output$karol_deck<-renderPlotly(plot3(k()))
  
  
  plot4 <- function(data) {
    horizontal_lines <- c(300, 600, 1000, 1300, 1600, 2000, 2300, 2600, 
                          3000, 3400, 3800, 4200, 4600, 5000, 5500, 6000, 6500)
    
    ggplotly(
      ggplot(data = data, aes(x = battleCount, y = trophies)) +
        geom_line(color = "#efa600", size = 1) +
        geom_hline(
          yintercept = horizontal_lines[horizontal_lines <= max(data$trophies + 500) & 
                                          horizontal_lines >= min(data$trophies - 500)], 
          linetype = "dashed", color = "gray"
        ) +
        labs(
          x = "Liczba Bitew",
          y = "Pucharki"
        ) +
        theme_minimal() +
        theme(
          text = element_text(family = "Roboto", color = "#FFFFFF"), # Czarny tekst ogólnie # Główny tytuł
          axis.title = element_text(size = 14, color = "#FFFFFF",family="Arial Black"),
          
          axis.text = element_text(size = 14, color = "#FFFFFF"), # Wartości na osiach
          panel.grid.major = element_blank(),  # Usuń główną siatkę
          panel.grid.minor = element_blank(),   # Usuń mniejszą siatkę
          
          panel.background = element_rect(fill = "transparent", color = NA),  # Transparentne tło panelu
          plot.background = element_rect(fill = "transparent", color = NA)    # Transparentne tło wykresu
        )
    )
  }
  
  
  
  output$brandt_trophies<-renderPlotly(plot4(b()))
  
  output$karol_trophies<-renderPlotly(plot4(k()))
  output$mati_trophies<-renderPlotly(plot4(m()))
  
  output$linePlot <- renderPlotly({
    ggplotly(
      ggplot() +
        geom_line(data = karol, mapping = aes(x = battleCount, y = wins / battleCount, color = "Karol"), size = 1, alpha = 0.7) +
        geom_line(data = mati, mapping = aes(x = battleCount, y = wins / battleCount, color = "Mati"), size = 1, alpha = 0.7) +
        geom_line(data = brandt, mapping = aes(x = battleCount, y = wins / battleCount, color = "Brandi"), size = 1, alpha = 0.7) +
        scale_color_manual(values = c("Karol" = "violet", "Mati" = "black", "Brandi" = "orange")) +
        labs(color = "Gracze", x = "Liczba bitew", y = "Procent wygranych") +
        scale_y_continuous(limits = c(0, 1)) +   # Skala osi Y od 0 do 1
        scale_x_continuous(limits = c(0, NA)) +  # Skala osi X zaczyna się od 0, NA pozwala na automatyczny zakres maksymalny
        theme_minimal() +
        theme(
          text = element_text(family = "Roboto", color = "#FFFFFF"), # Czarny tekst ogólnie # Główny tytuł
          axis.title = element_text(size = 14, color = "#FFFFFF", face = "bold"), # Tytuły osi
          axis.text = element_text(size = 14, color = "#FFFFFF"), # Wartości na osiach
          # Usuń główną siatkę
          panel.grid.minor = element_blank(),   # Usuń mniejszą siatkę
          panel.background = element_rect(fill = "transparent", color = NA),  # Transparentne tło panelu
          plot.background = element_rect(fill = "transparent", color = NA)# Tło wykresu jest przezroczyste
        )
    )
  })
  
}



##################################### Kolorki #################################



ui <- fluidPage(
  theme = shinytheme("cyborg"), 
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Bangers&family=Roboto:wght@300;400;700&display=swap",
      rel = "stylesheet"
    )
  ),
  tags$style(HTML("
    body {
      background-color: #2B4F81; /* Deep royal blue background */
      color: #ffffff; /* White text */
      font-family: 'Roboto', sans-serif; /* Nowoczesna czcionka */
    }
    .navbar-default {
      background-color: #f9a602; /* Golden yellow for navigation bar */
      border-color: #f9a602;
    }
    .navbar-default .navbar-brand, .navbar-default .navbar-nav > li > a {
      color: #ffffff; /* White text for navbar items */
      font-family: 'Bangers', cursive; /* Dynamiczna czcionka dla tytułu */
    }
    .navbar-default .navbar-brand:hover, .navbar-default .navbar-nav > li > a:hover {
      color: #ffd700; /* Bright gold for hover effect */
    }
    .btn-primary {
      background-color: #f9a602; /* Golden yellow for buttons */
      border-color: #ffd700;
      font-family: 'Roboto', sans-serif;
      font-weight: 700; /* Wyróżnione przyciski */
    }
    .btn-primary:hover {
      background-color: #ffd700; /* Brighter gold on hover */
      border-color: #f9a602;
    }
    h1, h2, h3, h4, h5, h6 {
      color: #ffd700; /* Bright gold for headers */
      font-family: 'Bangers', cursive; /* Dynamiczna czcionka dla nagłówków */
    }
    .well {
      background-color: #243763; /* Slightly lighter blue for panels */
      border-color: #f9a602;
      color: #ffffff;
      font-family: 'Roboto', sans-serif;
    }
    p, label {
      color: #dcdcdc; /* Light gray for readability */
      font-family: 'Roboto', sans-serif;
    }
    /* Nowe style dla zakładek */
    .navbar-default .navbar-nav {
      display: flex;
      justify-content: space-evenly; /* Równomierne rozmieszczenie zakładek */
      width: 100%; /* Zakładki na pełną szerokość */
    }
    .navbar-default .navbar-nav > li {
      flex: 1; /* Każda zakładka zajmuje równą ilość miejsca */
      text-align: center; /* Wyśrodkowanie tekstu */
    }
    /* Wyśrodkowanie tytułu w pasku nawigacyjnym */
    .navbar .container-fluid {
      display: flex;
      justify-content: center; /* Wyśrodkowanie wszystkich elementów w navbar */
      align-items: center;
    }
    .navbar-header {
      margin: 0 auto; /* Wyśrodkowanie elementów w navbar-header */
      flex-grow: 1;
      text-align: center;
    }
    .navbar-header .navbar-brand {
      float: none; /* Usuń domyślne wyrównanie */
      text-align: center;
    }
    /* Nowy pasek na tytuł */
    .title-bar {
      background-color: #f9a602; /* Golden yellow background */
      text-align: center; /* Wyśrodkowanie tekstu */
      padding: 10px 0; /* Dodanie przestrzeni w pionie */
      font-family: 'Bangers', cursive;
      font-size: 28px; /* Powiększony tekst */
      color: #ffffff; /* Biały tekst */
    }
  ")),
  # Dodajemy nowy pasek na tytuł
  div(class = "title-bar", "Statystyka gier Clash Royale"),
  navbarPage(
    title = NULL, # Usuwamy tytuł z navbarPage
    tabPanel("Strona główna", ui4),
    tabPanel("Mati", ui2),
    
    tabPanel("Karol", ui3),
    tabPanel("Brandi", ui1)
  )
)

# Run the application 
shinyApp(ui = ui, server = server)




