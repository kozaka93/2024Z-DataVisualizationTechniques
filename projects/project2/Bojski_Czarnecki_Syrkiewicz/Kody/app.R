library(shiny)
library(dplyr)
library(ggplot2)
library(stringi)
library(plotly)
library(heatmaply)
library(tidyr)
library(ggrepel)
library(shinycssloaders)
library(forcats)
library(htmlwidgets)
choose_colour <- function(input) {
  c("#181d24","#f7bb45", "#c89b3c", "#f0e6d2", "#fc6a53", "#be1e37")
  ifelse(input == "Player1", "#005b92", 
         ifelse(input == "Player2", "#be1e37", "#4F6F49")) 
} 
  
choose_colour2 <- function(selected_players) {
  colors <- c(
    Player1 = "#005b92",
    Player2 = "#be1e37",
    Proplayer = "#4F6F49"
  )
  return(colors[selected_players])
}
choose_colour3 <- function(selected_players) {
  colors <- c(
    Player1 = "#003e66",  
    Player2 = "#9d142d",   
    Proplayer = "#3c5639"
  )
  return(colors[selected_players])
}

filter_data <- function(df, date_range, position_filter) {
  if (is.null(position_filter) || length(position_filter) == 0) {
    position_filter <- "All"
  }
  df <- df %>%
    filter(
      Date >= as.POSIXct(date_range[1], format = "%Y-%m-%d", tz = "UTC"),
      Date <= as.POSIXct(date_range[2], format = "%Y-%m-%d", tz = "UTC"),
      if (position_filter != "All") Position == position_filter else TRUE
    )
  
  return(df)
}


add_custom_theme <- function(plot, x_label = NULL, y_label = NULL, plot_title = NULL, 
                             color = "#c89b3c", angle = 45, if_legend = F) {
  plot <- plot + 
    theme_minimal() + 
    theme(
      axis.text.x = element_text(angle = angle, size = 7, colour = color, family = "Spiegel"), 
      axis.title = element_text(size = 14, colour = color, family = "Spiegel"), 
      plot.title = element_text(hjust = 0.5, size = 16, colour = color, family = "Spiegel")) +
    {if (if_legend == T) {theme(legend.title = element_text(size = 10, colour = color, family = "Spiegel"),
      legend.text = element_text(size = 9, colour = color, family = "Spiegel")
      #legend.key.size = unit(1, 'cm')
      )}}
  
  if (!is.null(x_label)) {
    plot <- plot + xlab(x_label)
  }
  if (!is.null(y_label)) {
    plot <- plot + ylab(y_label)
  }
  if (!is.null(plot_title)) {
    plot <- plot + ggtitle(plot_title)
  }
  return(plot)
}

change_plotly_labels <- function(plot, main_color = "#c89b3c", bg_color = "#f0e6d2", alpha = 35){
  temp <- list(tickfont = list(color = bg_color),color = main_color, showgrid = T, 
               gridcolor = paste0(main_color,alpha), zeroline = F, showline = T)
  plot <- plot |> 
    layout(paper_bgcolor = "rgba(0,0,0,0)",
         plot_bgcolor = "rgba(0,0,0,0)",
         xaxis = temp, yaxis = temp,
         font = list(color = main_color, family = 'Spiegel'),
         legend = list(bgcolor = 'rgba(17,1d,24,0)')) |>
    config(displayModeBar = FALSE)
  return(plot)
  
  
}

sliders_select_input <- function(input_number) {
  fluidRow(
    tags$div(
      class = "slider-custom",
      sliderInput(
        inputId = paste0("date_range", input_number),
        label = "Choose date range:",
        min = as.Date("2024-09-01"),
        max = as.Date("2024-12-30"),
        value = c(as.Date("2024-09-01"), as.Date("2024-12-30")),
        timeFormat = "%d-%m-%Y"
      )
    ),
    selectInput(
      paste0("dataset", input_number),
      "Choose player:",
      choices = c("Player1", "Player2", "Proplayer"),
      selected = "Player1"
    ),
    uiOutput(paste0("position_ui", input_number))
  )
}


apply_spinner <- function(plot_name, spinner_type = 1, colour = "#f0e6d2", height = "400px"){
  shinycssloaders::withSpinner(plotlyOutput(plot_name, height = height),
                               image = list(src = "load01.gif"),
                               caption = "PLEASE WAIT",
                               color = getOption("spinner.color", default = colour),
                               size = getOption("spinner.size", default = 0.75)
                               )
}


place_holder <- column(12, tags$div(class = 'placeholder'))

add_text_decorator <- function(text_before = NULL, text_after = NULL, decorator){
  if(decorator == 'large') {
    col <- column(12, align = 'center', 
                  tags$div(class = 'text-img', text_before , tags$img(class = "dec-l", src = "dec_l.png"), text_after))
  } else if (decorator == 'small'){
    col <- column(12, align = 'center', 
                  tags$div(class = 'text-img', text_before , tags$img(class = "dec-s", src = "dec_s.png"), text_after))
  }
  x <- list(
  place_holder, col, place_holder)
}




ui <- navbarPage(
  title = tags$div(class = "app-title", span(img(src = "favicon.png")), 'eague of Stats'),
    tabPanel("Win rate",
             tags$link(rel = "icon", href = "favicon.png"),
             tags$div(class = "slider-custom"),
             tags$div(class = "custom-checkbox"),
             tags$link(type="text/css", rel = "stylesheet", href = "custom_font_family.css"),
             tags$link(type="text/css", rel = "stylesheet", href = "custom_style.css"),
             
             add_text_decorator("The scatterplot shows win rate in relation to the number of pings, which are a communication tool that allows players to quickly and effectively inform their 
                                teammates about different situations in the game, without the need to type in the chat.", decorator = 'large'),

             fluidRow(
               column(3, align = "center",
                      sliders_select_input(1)
               ),
             
               column(9,
                      apply_spinner("ScatterPlotPings") 
               )
             ),
             
             add_text_decorator("Undermentioned barplot depicts the win rate in a given day, showing only days with at least one game played. Next to it, there is a piechart representing the win rate within the certain time range.", decorator = 'small'),
              
             fluidRow(
               column(9,
                      apply_spinner("BarPlotWinRate") 
                      ),
             
               column(3, 
                      apply_spinner("PieChartWinRate") 
                      )
             )
    ),
    
    tabPanel("Number of games",
             add_text_decorator("The barplot shows the number of games played with specific champions, broken down by the roles in which each champion was played. There are about 150 champions in the game, and each can be assigned to a specific role. It's important to analyze which types of champions are most frequently chosen by players.\n To make the plot easier to read, we only show champions that players have played more than 3 games with.", decorator = 'large'),
             
             fluidRow(
               column(3, align = "center",
                      sliders_select_input(2)
               ),
               column(9, apply_spinner("BarPlotChampion")),
             ),

             add_text_decorator("The barplot shows the number of games played each day of the year, highlighting trends in game activity. It provides a clear overview of how many games are played over time, focusing only on the days when we actually played any games. As you can see, we always play League when it's possible.", decorator = 'small'),

             fluidRow(
               column(12, apply_spinner("BarPlotGames"))
             )
    ),
    tabPanel("Detailed stats",
             add_text_decorator("Undermentioned density plot depicts three statistics that show each player's game performance. We have chosen game duration, damage per minute, and gold per minute.", decorator = 'large'),
             
             fluidRow(
               column(6, align = "center",
                      tags$div(
                        class = "custom-checkbox",
                        checkboxGroupInput(
                          inputId = "players",
                          label = "Select Players:",
                          choices = c("Player1", "Player2", "Proplayer"),
                          selected = c("Player1")
                        )
                      )
               ),
               column(6, align = "center",
                      selectInput(
                        inputId = "plotChoice",
                        label = "Choose density plot:",
                        choices = c("Game Duration", "Damage per minute", "Gold per minute"),
                        selected = "DensityDuration"
                      )
               )
             ),
             
             fluidRow(
              column(12,
                    uiOutput("dynamicPlot")
             )
           ),
           
             add_text_decorator("The heatmap shows the number of games played in relation to the week of the year and day of the week.
                                Plot shows data from the beginning of September until the end of the year.", decorator = 'small'),
             
             fluidRow(
              column(10, apply_spinner("Heatmap", height = "600px")
                    ),
              column(2, align = "center",
                    selectInput(
                      "dataset3",
                      "Choose player:",
                      choices = c("Player1", "Player2", "Proplayer"),
                      selected = "Player1"
                    )
                  )
             )
    ),
  tabPanel(
    "Events on map",
    add_text_decorator(
      "This chart shows the number and details of events that took place during the game. It represents the map 
    in League of Legends, with symbols marking the events that occurred, triangles for assists, circles for kills 
      and crosses for deaths respectively.", 
      decorator = 'large'
    ),
    fluidRow(
      column(12,
             fluidRow(
               column(6, align = "center",
                      selectInput(
                        "playerSelect", 
                        "Select Player:", 
                        choices = c("Player1", "Player2", "ProPlayer"),
                        selected = "Player1"
                      )
               ),
               column(6,align="center",
                      tags$div(
                        class = "custom-checkbox",
                        checkboxGroupInput(
                          inputId = "typeFilter",
                          label = "Event types:",
                          choices = c("assist (triangle)", "kill (circle)", "death (cross)"),
                          selected = c("assist (triangle)", "kill (circle)", "death (cross)")
                        )
                      )
               )
             )
      )
    ),
    fluidRow(
      column(4, align="center", offset = 1,
             uiOutput("playerHeader"),  
             uiOutput("playerImage")  
      ),
      column(7, 
             apply_spinner("MapPlot", height = "512px")
      )
    )
  ),
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:26px; color:#f7bb45; font-family:Beaufort;'>
                  About project
                </p>
                <p class='text-center' style='font-size:22px; color:#c89b3c; font-family:Beaufort;'>
                  Authors: MB, RC, MS
                </p>
                <p class='text-left' style='font-size:20px; color:#f0e6d2; font-family:Beaufort; margin:10px;'>
                The project aimed to use the Riot Games API to analyze data from our League of Legends games and compare them to the 
                performance of one of the top players in the world. We wanted to assess our performance, such as how many games we win 
                versus how many we lose, how our in-game behavior affects our win rate, and also identify which champions we perform well 
                with and which ones we need to improve on. The entire project was driven by our curiosity and desire to become better at LoL, 
                and the skills we developed in data analysis enabled us to do so.
                </p>
                <p class='text-center' style='font-size:16px; color:#c89b3c; font-family:Beaufort;'>
                  Source of data:
                  <a class='text-dark' href='https://developer.riotgames.com/'>RiotGames API</a>
                </p>
                </footer>
              "),
)


server <- function(input, output,session) {
### Kod do wyświetlania ograniczonego dla ProPlayer
  observe({
    for (i in 1:2) {
      dataset_input <- paste0("dataset", i)
      position_input <- paste0("position", i)
      if (!is.null(input[[dataset_input]])) {
        choices <- if (input[[dataset_input]] == "Proplayer") {
          c("All", "BOTTOM", "UTILITY")
        } else {
          c("All", "TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY")
        }
        updateSelectInput(
          session,
          inputId = position_input,
          choices = choices,
          selected = "All"
        )
      }
    }
  })
  observe({
    lapply(1:2, function(i) { 
      output[[paste0("position_ui", i)]] <- renderUI({
        selectInput(
          paste0("position", i),
          "Choose position:",
          choices = c("All", "TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"),
          selected = "All"
        )
      })
    })
  })
#### Wyświetlanie 1 z trzech
  output$dynamicPlot <- renderUI({
    selected_plot <- input$plotChoice
    
    if (selected_plot == "Game Duration") {
      apply_spinner("DensityDuration")
    } else if (selected_plot == "Damage per minute") {
      apply_spinner("DensityDamage")
    } else if (selected_plot == "Gold per minute") {
      apply_spinner("DensityGold")
    }
  }) |>
    bindCache(input$plotChoice)
  ###
  BarPlotGamesData <- reactive({
    nazwa_csv <- paste0(input$dataset2,".csv")
    data <- read.csv(nazwa_csv)
    data <- data %>% filter_data(input$date_range2,input$position2) 
    validate(
      need(nrow(data) >  0 , "        There is no data available in chosen date range. Please change it.")
    )
    data <- data %>%
      mutate(
        year = as.numeric(format(as.POSIXct(Date), "%Y")),
        month = as.numeric(format(as.POSIXct(Date), "%m")),
        day = as.numeric(format(as.POSIXct(Date), "%d")),
        rest = format(as.POSIXct(Date), "%H:%M:%S"),
        Date = format(as.POSIXct(Date), "%m-%d")
      ) %>% 
      count(Date)
    return(data)
  })
  
  output$BarPlotGames <- renderPlotly({
    data <- BarPlotGamesData()
    total_games <- sum(data$n) #
    days_with_games <- sum(data$n > 0)
    
    plot <- ggplot(data |> rename(`Number of games` = n), aes(x = Date, y = `Number of games`)) +
      labs(x= "Date (with at least game)")+
      geom_col(fill = choose_colour(input$dataset2)) +
      coord_cartesian(ylim = c(0, 21)) +
      scale_y_continuous(expand = c(0, 0))
    
    plot <- add_custom_theme(
      plot,
      "Date",
      "Number of games",
      paste("Games per day       ", 
            "             Total number of games:", total_games,
            "             Days with games:", days_with_games)
    )
  
    plot <- ggplotly(plot)
    plot <- plot |>
      layout(title = list(
        text = paste0('Games per day',
                      '<br>',
                      '<sup>',
                      'Total number of games: ', total_games,
                      ' | Days with games: ', days_with_games, '</sup>')
      ),
      xaxis = list(title = "Day (with at least one game)"))
    
    change_plotly_labels(plot)
  }) |> 
    bindCache(input$dataset2, input$date_range2, input$position2)
  
  
  BarPlotChampionData <- reactive({
    nazwa_csv <- paste0(input$dataset2, ".csv")
    data <- read.csv(nazwa_csv)
    data <- data %>% 
      filter_data(input$date_range2, input$position2) 
    validate(
      need(nrow(data) >  0 , "        There is no data available in chosen date range. Please change it.")
    )
    data <- data %>%
      count(Champion, Position) %>% 
      filter(n >3)%>%
      filter(!(Champion == "Vladimir") )
    validate(
      need(nrow(data) >  0 , "        There is no champion used more than 3 times in chosen date range. Please change it.")
    )
    return(data)
  })
  
  output$BarPlotChampion <- renderPlotly({
    data <- BarPlotChampionData() %>% 
      select(Champion, Position, n) %>% 
      arrange(n)
    
    position_colors <- c(
      "BOTTOM" = "lightblue", 
      "TOP" = "#f0e6d2", 
      "JUNGLE" = "#2ca02c", 
      "UTILITY" = "#ff7f0e", 
      "MIDDLE" = "#9467bd", 
      "MIXED" = "#8c564b"
    )
    
    data <- data %>% 
      mutate(Position = ifelse(is.na(Position) | Position == "", "MIXED", Position)) %>%
      mutate(Color = ifelse(Position %in% names(position_colors), 
                            position_colors[Position], 
                            position_colors["MIXED"]))
    plot2 <- ggplot(data, aes(x = fct_inorder(Champion), y = n, fill = Position, 
                              text = paste("Champion:", Champion, 
                                           "<br>Number of games:", n, 
                                           "<br>Position:", Position))) +
      geom_col() +
      scale_fill_manual(values = position_colors, name = "Position") +
      scale_x_discrete(expand = c(0, 0)) +
      coord_flip(ylim = c(0, 43), expand = F)
    
    plot2 <- add_custom_theme(
      plot2, 
      "Champions", 
      "Number of games", 
      "Games on champion", 
      angle = 0
    ) +theme(legend.text = element_text(color = "#c89b3c"),legend.title = element_text(color = "#c89b3c")) 
    
    plot <- ggplotly(plot2, tooltip = "text")
    
    
    plot <- ggplotly(plot)
 
    
    change_plotly_labels(plot)
  }) |>
    bindCache(input$dataset2, input$date_range2, input$position2)
  
  
  
  BarPlotWinRateData <- reactive({
    nazwa_csv <- paste0(input$dataset1, ".csv")
    data <- read.csv(nazwa_csv)
    data <- data %>%
      mutate(
        Date = format(as.POSIXct(Date), "%Y-%m-%d"),
        Day = format(as.POSIXct(Date), "%m-%d"),
        win = ifelse(Win == "True", 1, 0)
      ) %>%
      filter_data(input$date_range1, input$position1) 
    validate(
      need(nrow(data) >  0 , "        There is no data available in chosen date range. Please change it.")
    )
    data <- data %>%
      group_by(Day) %>%
      summarise(
        total_matches = n(),
        wins = sum(win, na.rm = TRUE),
        win_ratio = wins / total_matches*100,
        .groups = "drop"
      )
    return(data)
  })
  
  output$BarPlotWinRate <- renderPlotly({
    data <- BarPlotWinRateData()
    plot3 <- ggplot(data, aes(x = Day, y = win_ratio,
    text = paste("Date:", as.character(Day), "<br>Win rate:", round(win_ratio,2),"%")))+
    geom_col(fill = choose_colour(input$dataset1)) +
      coord_cartesian(ylim = c(0, 100))
    plot3<- add_custom_theme(plot3,"Day","Win rate","Win rate in each day")
    plot3 <- plot3 + scale_y_continuous(expand = c(0,0), labels= (\(x) paste(as.character(x),"%") ))
    plot <- ggplotly(plot3,tooltip = 
                       "text")
    plot <- plot |>
      layout( xaxis = list(title = "Day (with at least one game)"))
    change_plotly_labels(plot)
    
  }) |>
    bindCache(input$dataset1, input$date_range1, input$position1)
  
  PieChartWinRateData <- reactive({
    nazwa_csv <- paste0(input$dataset1,".csv")
    data <- read.csv(nazwa_csv)
    data <- data %>%
      mutate(
        win = ifelse(Win == "True", 1, 0),
        Date = as.POSIXct(Date, format = "%Y-%m-%d")
      ) %>% filter_data(input$date_range1,input$position1)
    validate(
      need(nrow(data) >  0 , "        There is no data available in chosen date range. Please change it.")
    )
    data <- data %>%
      reframe(
        total_matches = n(),
        wins = sum(win, na.rm = TRUE),
        winrate = wins / total_matches 
      ) 
    return(data)
  })
  
  output$PieChartWinRate <- renderPlotly({ 
    data <- PieChartWinRateData()
    winrate <- data$winrate + 0.1 
    lose_rate <- 1 - winrate
    
    pie_data <- data.frame(
      status = c("Wins", "Loses"),
      proportion = c(winrate, lose_rate)
    )
    pie_chart <- plot_ly(pie_data, labels = ~status, values = ~proportion, type = 'pie',
                         textinfo = 'label+percent', hoverinfo = 'label+percent', 
                         marker = list(colors = c("#D4AF37", "#D9D9D9"))) %>% 
      layout(
        title = list(
          text = paste("Win rate: ", round(winrate * 100, 2), "%"),
          font = list(size = 16)  
        ),  showlegend = FALSE)
    
    change_plotly_labels(pie_chart)
  }) |>
    bindCache(input$dataset1, input$date_range1, input$position1)
  
  ScatterPlotPingsData <- reactive({
    nazwa_csv <- paste0(input$dataset1, ".csv")
    data <- read.csv(nazwa_csv)
    data <- data %>%
      mutate(
        Date = format(as.POSIXct(Date), "%Y-%m-%d", tz = "UTC"),
        Day = format(as.POSIXct(Date), "%m-%d", tz = "UTC"),
        win = ifelse(Win == "True", 1, 0)
      )%>% filter_data(input$date_range1,input$position1) 
    validate(
      need(nrow(data) >  0 , "        There is no data available in chosen date range. Please change it.")
    )
    data <- data %>%
      mutate(Pings = allInPings + assistMePings + commandPings + enemyMissingPings + 
               enemyVisionPings + getBackPings + needVisionPings + onMyWayPings + pushPings + visionClearedPings) %>%
      mutate(Pings_group = cut(Pings, 
                               breaks = seq(0, max(Pings), by = 2),
                               right = FALSE, 
                               include.lowest = TRUE, 
                               labels = paste0(seq(0, max(Pings) - 2, by = 2), "-", seq(2, max(Pings), by = 2)))) %>%
      
      group_by(Pings_group) %>%
      reframe(
        total_matches = n(),
        wins = sum(win, na.rm = TRUE),
        win_ratio = wins / total_matches*100,
        .groups = "drop"
      )
    return(data)
  })
  
  output$ScatterPlotPings <- renderPlotly({
    data <- ScatterPlotPingsData()
    
    plot5 <- ggplot(data, aes(x = Pings_group, y = win_ratio, size = 3,
                              text=paste("Number of pings:", Pings_group, "<br>Win rate:", round(win_ratio,2),"%"))) +
    geom_point(color = choose_colour(input$dataset1)) +
      scale_y_continuous(labels= (\(x) paste(as.character(x),"%") )) +
      coord_cartesian(ylim = c(0, 100))
    plot5<- add_custom_theme(plot5,"Number of pings","Win rate","Win rate by number of pings")
    
    plot <- ggplotly(plot5,tooltip = "text")
    change_plotly_labels(plot)
  }) |>
    bindCache(input$dataset1, input$date_range1, input$position1)

  HeatmapData <- reactive({
    nazwa_csv <- paste0(input$dataset3, ".csv")
    data <- read.csv(nazwa_csv)
    data <- data %>%
      mutate(
        Date = as.POSIXct(Date, format = "%Y-%m-%d"),
        day = as.numeric(format(Date, "%d")),
        week = as.numeric(format(Date, "%U")),
        month = as.numeric(format(Date, "%m")),
        year = as.numeric(format(Date, "%Y")),
        weekday = weekdays(Date)
      ) %>%
      group_by(year, week, month, weekday, day) %>%
      summarise(game_count = n(), .groups = "drop") %>%
      ungroup()
    
    return(data)
  })
  
  output$Heatmap <- renderPlotly({
    data <- HeatmapData() %>%
      mutate(
        weekday_number = case_when(
          weekday == "poniedziałek" ~ 0,
          weekday == "wtorek" ~ 1,
          weekday == "środa" ~ 2,
          weekday == "czwartek" ~ 3,
          weekday == "piątek" ~ 4,
          weekday == "sobota" ~ 5,
          weekday == "niedziela" ~ 6
        )
      )
    data_aggregated <- data %>%
      group_by(weekday_number, week) %>%
      summarise(game_count = sum(game_count), .groups = "drop") %>%
      complete(
        weekday_number = 0:6,
        week = 35:52, 
        fill = list(game_count = 0)
      )
    data_aggregated <- data_aggregated %>%
      filter(week >= 35 & week <= 52)
    data_matrix <- data_aggregated %>%
      spread(key = week, value = game_count, fill = 0) %>%
      arrange(weekday_number) %>%
      select(-weekday_number) %>% 
      as.matrix()
    num_weeks <- ncol(data_matrix)
    labCol <- paste(35:(34 + num_weeks))
    heat_map <- heatmaply(
      data_matrix,
      limits = c(0, 22),
      xlab = "Week number",
      ylab = "Day of week",
      main = "Number of games heatmap",
      dendrogram = "none",
      colors = list("#e9e6d2", choose_colour3(input$dataset3)),
      showticklabels = c(TRUE, TRUE),
      labRow = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"),
      labCol = labCol, 
      grid_color = "black",
      heatmap_layers = theme(
        legend.background = element_rect(fill = "#181d24"),
        legend.text = element_text(color = "#f0e6d2")
      )
    )
    
    
    plot <- ggplotly(heat_map)
    plot<-change_plotly_labels(plot)
  }) |>
    bindCache(input$dataset3, input$date_range3, input$position3)
  
  
  DensityPlotsData <- reactive({
    selected_players <- input$players
    if (length(selected_players) == 0) {
      return(data.frame())
    }
    
    player_data <- lapply(selected_players, function(player) {
      file_name <- paste0(player, ".csv") 
      data  <- read.csv(file_name)
     data <- data%>%  mutate(riotIdTagline = as.character(riotIdTagline))%>%
        mutate(
          Player = player,
          Date = format(as.POSIXct(Date), "%Y-%m-%d"),
          Day = format(as.POSIXct(Date), "%m-%d"),
          win = ifelse(Win == "True", 1, 0),
          gameLength = gameLength / 60,
          goldPerMinute = ceiling(goldPerMinute)
        )
      return(data)
    })
    
    combined_data <- bind_rows(player_data)
    
    return(combined_data)
  })
  output$DensityDuration <- renderPlotly({
    data <- DensityPlotsData()
    if (nrow(data) == 0) {
      return(NULL)
    }
  
    
    plot9 <- ggplot(data, aes(x = gameLength, fill = Player)) +
      geom_density(alpha = 0.4) +  
      scale_fill_manual(values = choose_colour2(input$players)) +
      coord_cartesian(xlim = c(0, 50), ylim = c(0, 0.07))
    plot9 <- add_custom_theme(plot9, x = "Game Duration", y = "Density", "Game Duration Density",
                              angle = 0, if_legend = T)
    plot9<- plot9 + scale_y_continuous(expand=c(0,0))
    plot <- ggplotly(plot9, tooltip = c("x", "color"))
    change_plotly_labels(plot)
  }) 
  
  output$DensityGold <- renderPlotly({
    data <- DensityPlotsData()
    
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    plot7 <- ggplot(data, aes(x = goldPerMinute, fill = Player)) +
      geom_density(alpha = 0.4) + 
      scale_fill_manual(values = choose_colour2(input$players)) +
      coord_cartesian(xlim = c(0, 1000), ylim = c(0, 0.008))
    plot7 <- add_custom_theme(plot7, x = "Gold Per Minute", y = "Density", "Gold Per Minute Density", 
                              angle = 0, if_legend = T)
    plot7<- plot7 + scale_y_continuous(expand=c(0,0))
    plot <- ggplotly(plot7,tooltip = c("x", "color"))
    change_plotly_labels(plot)
  })
  
  output$DensityDamage <- renderPlotly({
    data <- DensityPlotsData()
    
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    plot8 <- ggplot(data, aes(x = damagePerMinute, fill = Player)) +
      geom_density(alpha = 0.4) + 
      scale_fill_manual(values = choose_colour2(input$players)) +
      coord_cartesian(xlim = c(0, 3000), ylim = c(0, 0.0025))
    plot8 <- add_custom_theme(plot8, x = "Damage Per Minute", y = "Density", "Damage Per Minute Density", 
                              angle = 0, if_legend = T)
    plot8<- plot8 + scale_y_continuous(expand=c(0,0))
    plot <- ggplotly(plot8,tooltip = c("x", "color"))
    change_plotly_labels(plot)
    
  })
  output$MapPlot <- renderPlotly({
    data <- switch(
      input$playerSelect,
      "Player1" = read.csv("Utility_matches.csv"),
      "Player2" = read.csv("Bottom_matches.csv"),
      "ProPlayer" = read.csv("Utility_matches_pro.csv")
    )
    
    player_colors <- ifelse(
      input$playerSelect == "Player1", "#003d66", 
      ifelse(input$playerSelect == "Player2", "#be1e37", "#6a7f45")
    )
    
    filtered_data <- data %>%
      mutate(
        type = case_when(
          type == "assist" ~ "assist (triangle)",  
          type == "kill" ~ "kill (circle)",      
          type == "death" ~ "death (cross)"  
        )
      ) %>%
      filter(type %in% input$typeFilter) %>%
      mutate(
        x = ifelse(x > 13450, 13350, x),  
        y = ifelse(y > 13600, 13150, y),
        shape = case_when(
          type == "assist (triangle)" ~ 6,  
          type == "kill (circle)" ~ 0,   
          type == "death (cross)" ~ 4  
        )
      )
    plot <- plot_ly(
      data = filtered_data,
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "markers",
      marker = list(
        color = player_colors,      
        symbol = ~shape,           
        size = 15,                  
        line = list(
          color = 'black',         
          width = 2                 
        )
      ),
      height = 590,
      width = 590
    ) %>% layout(
      images = list(
        source = base64enc::dataURI(file = "./www/map11.png"),
        x = 0,
        y = 0,
        sizex = 1,
        sizey = 1,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "bottom",
        layer = "below"
      ),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)',
      xaxis = list(
        title = '',
        showgrid = FALSE,
        showticklabels = FALSE,
        range = c(0, 14000),
        tickfont = list(color = 'rgba(0,0,0,0)'),
        linecolor = 'rgba(0,0,0,0)'
      ),
      yaxis = list(
        title = '',
        showgrid = FALSE,
        showticklabels = FALSE,
        range = c(0, 14000),
        tickfont = list(color = 'rgba(0,0,0,0)'),
        linecolor = 'rgba(0,0,0,0)'
      ),
      legend = list(
        font = list(
          color = "#c8aa6e"
        )
      )
    ) %>% config(displayModeBar = FALSE, staticPlot = TRUE)
    
    return(plot)
    
})
  
  player_images <- list(
    Player1 = c("Zilean_0.jpg","Rakan_0.jpg"),
    Player2 = c("Xayah_0.jpg", "Caitlyn_0.jpg"),
    ProPlayer = c("Lulu_0.jpg", "Renata_0.jpg")
  )
  
  first_part = "Games played on Champions "
  player_headers <- list(
    Player1 = paste(first_part, "Rakan and Zilean"),
    Player2 = paste(first_part, "on Xayah and Caitlyn"),
    ProPlayer = paste(first_part, "on Lulu and Renata")
  )
  
  output$playerImage <- renderUI({
    selected_images <- player_images[[input$playerSelect]]
    tagList(
      lapply(selected_images, function(img) {
        tags$img(src = img, width = "auto", height = "500")
      })
    )
  })
  
  output$playerHeader <- renderUI({
    selected_header <- player_headers[[input$playerSelect]] 
    tags$h4(selected_header, style = "color: #FFD700;") 
  })  
  
}



shinyApp(ui = ui, server = server)

