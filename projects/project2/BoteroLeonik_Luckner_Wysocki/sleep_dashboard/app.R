# ------------------------------------------------------------------------------
# Biblioteki
# ------------------------------------------------------------------------------

library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(plotly)
library(ggridges)
library(bslib)
library(fmsb)
library(patchwork)
library(ggalt)
library(jpeg)
library(shinydashboard)
library(thematic)
library(shinycssloaders)

# ------------------------------------------------------------------------------
# Wgranie danych
# ------------------------------------------------------------------------------

SebastianRaw <- read.csv2("../data/sleepdataSebastian.csv")
PiotrRaw <-  read.csv2("../data/sleepDataPiotr (2).csv")
OlekRaw <- read.csv2("../data/sleepdataOlek.csv")
OlekRaw <- OlekRaw %>% slice(3:nrow(OlekRaw))
# ------------------------------------------------------------------------------
# Przetworzenie danych
# ------------------------------------------------------------------------------

parse_percentage <- function(str) {
  as.numeric(substr(str, 1, nchar(str)-1))/100
}

process_raw <- function(dt) {
  dt|>
    mutate(
      Sleep.Quality = parse_percentage(Sleep.Quality),
      Regularity = parse_percentage(Regularity),
      Did.snore = Did.snore == "true",
      Went.to.bed = ymd_hms(Went.to.bed),
      Woke.up = ymd_hms(Woke.up),
      day = as_date(Woke.up),
      Coughing..per.hour. = as.numeric(Coughing..per.hour.),
      Movements.per.hour = as.numeric(Movements.per.hour),
      General.day.asleep = ifelse(hour(ymd_hms(Went.to.bed)) < 15, day(day)-1,day(day)),
      General.month.asleep = ifelse(hour(ymd_hms(Went.to.bed))< 15 & day(day) == 1, 12, month(day)),
      General.day.asleep = ifelse(General.day.asleep == 0, 31, General.day.asleep)
    ) |>
    filter(day %within% (ymd("2024-12-01") %--% now())) |>
    select(-c(
      City,
      Alertness.score,
      Alertness.reaction.time..seconds.,
      Alertness.accuracy
    ))
}

Sebastian <- process_raw(SebastianRaw) |> mutate(sleeper = "Sebastian")
Piotr <- process_raw(PiotrRaw) |> mutate(sleeper = "Piotr")
Olek <- process_raw(OlekRaw) |> mutate(sleeper = "Olek")

Data <- bind_rows(Sebastian, Piotr, Olek)

# ------------------------------------------------------------------------------
# Dodatkowe funkcje
# ------------------------------------------------------------------------------

generate_pom <- function(df){
  df %>% mutate(WeekNumber = week(day - 1)) %>%
    mutate(WeekNumber = if_else(WeekNumber <= 5, 53 + WeekNumber, WeekNumber),
           DayOfWeek = wday(day - 1, week_start = 1, label = TRUE,
                            locale = "en_US")) %>%
    mutate(
      WeekNumber = if_else(
        (DayOfWeek == "Mon" | DayOfWeek == "Tue") & WeekNumber >= 54, 
        WeekNumber + 1, 
        WeekNumber
      )) %>% 
    arrange(desc(WeekNumber))
}

plot <- function(df){
  ggplot(df, aes(x = DayOfWeek,
                 y = WeekNumber,
                 fill = Sleep.Quality)) +
    geom_tile(color = "white", lwd = 1.5) +
    scale_fill_gradientn(colors = hcl.colors(50, "RdYlGn"), limits = c(0.25, 1)) +
    theme_minimal() +
    labs(x = element_blank(), y = element_blank()) +
    scale_y_continuous(breaks = unique(df$WeekNumber),
                       labels = function(x) {
                         ifelse(x >= 54, paste0("2025 W", x - 53), paste0("2024 W", x))
                       }) +
    theme(
      panel.grid = element_blank(),
      text = theme_get()$text,
      axis.text.x = element_text(color = "white", size = 10),
      axis.text.y = element_text(color = "white", size = 10)
    ) 
}

# ------------------------------------------------------------------------------
# Wartości do boxów
# ------------------------------------------------------------------------------

Sleep.Time.Mean <- round(sum(Data$Time.asleep..seconds./3600) / nrow(Data),2)
Sleep.Time.Min <- round(min(Data$Time.asleep..seconds.)/3600, 2)
Sleep.Time.Max <- round(max(Data$Time.asleep..seconds.)/3600, 2)

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

thematic_on()
theme_set(
  theme(
    text = element_text(colour = "white")
  )
)

main_page <- fluidPage(
  tags$style(HTML("
           h1 {
               font-weight: bold;
           }
       ")),
  h1("Welcome to our MiNI REST app!"),
  tags$head(
    tags$style(HTML("
      .custom-box {
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        font-size: 1.5em;
        font-weight: bold;
        color: white;
        text-align: center;
        border-radius: 5px;
        height: 150px; 
        margin: 5px;  
      }
      .navy { background-color: #001f3f; }
      .green { background-color: #2ECC40; }
      .red { background-color: #FF4136; }
      .orange { background-color: #f36f12; }
    "))
  ),
  fluidRow(
    column(2),
    column(2,
           div(class = "custom-box orange", 
               style = "padding: 20px; font-size: 1.2em; width: 100%; text-align: center;",
               icon("chart-simple"),
               div(
                 style = "font-size: 1.5em; margin-bottom: 5px;",
                 "General summary"
               )
           )
    ),
    column(2,
           div(class = "custom-box navy", 
               style = "padding: 20px; font-size: 1.2em; width: 100%; text-align: center;",
               icon("moon"),
               div(
                 style = "font-size: 2em; margin-bottom: 5px;",
                 paste(" ", as.character(Sleep.Time.Mean))
               ),
               div("Average Sleep Time in hours")
           )
    ),
    column(2,
           div(class = "custom-box red", 
               style = "padding: 20px; font-size: 1.2em; width: 100%; text-align: center;",
               icon("clock"),
               div(
                 style = "font-size: 2em; margin-bottom: 5px;",
                 paste(" ", as.character(Sleep.Time.Min))
               ),
               div("Shortest sleep in hours")
           )
    ),
    column(2,
           div(class = "custom-box green", 
               style = "padding: 20px; font-size: 1.2em; width: 100%; text-align: center;",
               icon("mattress-pillow"),
               div(
                 style = "font-size: 2em; margin-bottom: 5px;",
                 paste(" ", as.character(Sleep.Time.Max))
               ),
               div("Longest sleep in hours")
           )
    ),
    column(2)
  ),
  fluidRow(
    
  ),
  fluidRow(
    column(12,
           p("In this project, we embarked on an exciting journey to analyze our sleep patterns during
           the academic year and winter break.
             For over 40 days we have been using the Sleep Cycle app, which tracked our sleep data.
             By making use of shiny, HTML and CSS we took a look into our sleep patterns and factors that affect our night rest.")
    )
  ),
  
  fluidRow(
    column(4,
           uiOutput("img1"),
           h3("Olek Luckner"),
           p("My main leisure activity is studying for college.
             My hobbies are playing soccer and optimizing my
             commute by public transportation.")
    ),
    column(4,
           uiOutput("img2"),
           h3("Piotr Wysocki"),
           p("Student of Data Science at WUT. In free
             time tennis is usually my go-to sport but
             recently I also got into running.")
    ),
    column(4,
           uiOutput("img3"),
           h3("Sebastian Botero Leonik"),
           p("I am a Data Science student and I have no free time for
             I spend it all working on projects for university and doom-scrolling 
             instagram reels.")
    )
  )
)

weekday_opts_names <- c(
  "All",
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday",
  "Friday",
  "Saturday",
  "Sunday"
)

weekday_opts <- 0:7
names(weekday_opts) <- weekday_opts_names

general_data_page <- fluidPage( # pytanie gdzie dac opisy i do jakich wykresow
  # bo do radar plot chyba bez sensu?
  titlePanel("Who has the best sleep?"),
  fluidRow(
    column(6,
           dateRangeInput("dateSelector", "Select a Date:", 
                          format = "yyyy-mm-dd",
                          start = "2024-12-11",
                          end = "2025-1-20",
                          min = "2024-12-11",
                          max = "2025-1-20") 
    )
  ),
  mainPanel(
    width = 12,
    fluidRow(
      column(6,
             h4("Sleep insights", style = "color: #B0B0B0; padding: 10px; border-radius: 5px;"),
             h3("In this page you can compare our features sleep and bed time based on different range factors and also days of the week.
               Comprehensive radar plot present different aspects of our sleep. On our density plot we can guess when our
               alarm clock usually rings, by noticable descends on chart. Lastly, we can delve into density our sleep quality.
               For instance, if we choose Christmas time it is easy to spot that it got higher.")
      ),
      column(6,
             h5("Sleep Overview"),
             plotOutput("radar_plot", height = "600px") |> 
               withSpinner(type = 7, size = 2, color = "#F39C12")
      )
    ),
    fluidRow(
      column(6,
             h5("How often are we asleep at a certain time"),
             plotOutput("sleep_hour_dist_ridgelines") |>
               withSpinner(type = 7, size = 2, color = "#F39C12"),
             selectInput(
               inputId = "weekday_selector",
               label = "Weekdays: ",
               choices = weekday_opts) 
      ),
      column(6,
             h5("Sleep quality distribution across individuals"),
             plotOutput("density_plot") %>% withSpinner(type = 7, size = 2, color = "#F39C12")
      )
    )
  )
)


individual_data_page <- fluidPage(
  tags$style(HTML("
    .sidebar-layout .sidebar {
      border-right: 3px solid white;  /* Grubość 3px, kolor biały */
    }
  ")),
  titlePanel("Let's dive deeper into each of our sleep!"),
  fluidRow(""),
  
  fluidRow(
    column(12,
           selectInput("selectSleeper", 
                       "Select a sleeper (person):",
                       unique(Data$sleeper),
                       selected = "Piotr")
    )),
  sidebarLayout(
    sidebarPanel(
      h2("What happens during our sleep?"),
      h5("Hourly Movements vs. Sleep Quality"),
      plotlyOutput("sleepDistractionScatter", height = "400px") %>% 
        withSpinner(type = 7, size = 2, color = "#F39C12"),
      h2("Is physical activity related to sleep quality?"),
      h5("Distribution of sleep quality in active and inactive days"),
      plotOutput("activityBoxplot", height = "400px") %>% withSpinner(type = 7, size = 2, color = "#F39C12"),
    ),
    mainPanel(
      h2("How was our sleep in specific days?"),
      h5("Sleeptime intervals and quality on a daily basis"),
      plotOutput("sleeptimeCrossbar", height = "400px") %>% 
        withSpinner(type = 7, size = 2, color = "#F39C12"),
      h2("Is there a most stressful day of the week for each of us?"),
      h5("Calendar of sleep quality"),
      plotOutput("heatmap", height = "500px") %>% withSpinner(type = 7, size = 2,
                                                              color = "#F39C12")
    )
  )
)


animation_page <- fluidPage(
  titlePanel("Cumulative sleep time"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("day", "Select a day:", 
                  min = min(Data$day) + 3, 
                  max = max(Data$day), 
                  value = min(Data$day) +3, 
                  step = 1, 
                  animate = animationOptions(interval = 500, loop = FALSE),
                  wellPanel(
                    p("This animated cumulative sleep graph visualizes your sleep patterns over time,
      showing how your nightly rest builds up. Each frame
      reflects your progress, providing a dynamic way to explore your
        sleep trends and understand your journey throught MiNi semester."))
      )),
    mainPanel(
      plotOutput("bar_plot") 
    )
  )
)

# ------------------------------------------------------------------------------
# serwer
# ------------------------------------------------------------------------------

server <- function(input, output) {
  
  ########## main_page - strona główna ##########
  
  output$img1 <- renderUI({
    img(src = "Olek.jpg", width = 150)
  })
  
  output$img2 <- renderUI({
    img(src = "Piotr.jpg", width = 150)
  })
  
  output$img3 <- renderUI({
    img(src = "Seba_morda_2.jpg", width = 150)
  })
  
  ########## general_data_page - ogólne informacje ##########
  
  output$radar_plot <- renderPlot({
    filtered_data <- Data %>% 
      filter(day %within% interval(input$dateSelector[1],
                                   input$dateSelector[2])) %>% 
      mutate(Regularity = replace_na(Regularity, 0)) %>% 
      select(Sleep.Quality,
             Asleep.after..seconds.,
             Regularity, Snore.time..seconds.,
             Coughing..per.hour.,
             Movements.per.hour,
             sleeper)
    
    min_vals <- apply(filtered_data[, -7], 2, min)
    max_vals <- apply(filtered_data[, -7], 2, max)
    data_norm <- as.data.frame(scale(filtered_data[, -7],
                                     center = min_vals,
                                     scale = max_vals - min_vals))
    data_norm$sleeper <- filtered_data$sleeper
    
    data_olek <- colMeans(data_norm[data_norm$sleeper == "Olek", -ncol(data_norm)])
    data_seba <- colMeans(data_norm[data_norm$sleeper == "Sebastian", -ncol(data_norm)])
    data_piotr <- colMeans(data_norm[data_norm$sleeper == "Piotr", -ncol(data_norm)])
    
    data_radar <- as.data.frame(rbind(
      rep(1, ncol(data_norm) - 1),  
      rep(0, ncol(data_norm) - 1),  
      data_olek,  
      data_seba,
      data_piotr))
    
    colnames(data_radar) <- c(
      "sleep quality",
      "time taken to fall asleep",
      "regularity",
      "snore time",
      "coughing per hour",
      "movements per hour"
    )
    
    
    clrs <- palette()[c(1,2,4)]
    # Analogicznie do tego co było ↓
    # clrs_alpha <- c(clrs[1], alpha(clrs[2:3], 0.3125))
    # W ten sposób widać podpisaną oś ↓
    clrs_alpha <- alpha(clrs, 0.3125)
    radarchart(data_radar,
               axistype = 1, 
               # pcol = c("blue", "red", "green"),
               pcol = clrs,
               pfcol = clrs_alpha,
               plty = 1,
               # c("#0000FF50", "#FF000050", "lightgreen"),
               plwd = 2,  
               cglcol = "grey",  
               cglty = 1, 
               axislabcol = theme_get()$text$colour,
               vlcex = 1,
               calcex = 1.2
    )
    
    legend("topright",
           legend = c("Olek", "Piotr", "Sebastian"),
           # col = c("blue", "red", "green"),
           col = clrs,
           lty = 1,
           lwd = 2)
  })
  
  output$sleep_hour_dist_ridgelines <- renderPlot({
    minutes <- Data |>
      filter(day %within% interval(input$dateSelector[1], input$dateSelector[2])) |>
      mutate(minute_went_to_bed = unclass(interval(day, Went.to.bed)) %/% 60) |>
      mutate(minute_woke_up = unclass(interval(day, Woke.up)) %/% 60)
    
    samples <- min(minutes$minute_went_to_bed):max(minutes$minute_woke_up)
    
    minutes <- Data |>
      filter(day %within% interval(input$dateSelector[1], input$dateSelector[2])) |>
      mutate(minute_went_to_bed = unclass(interval(day, Went.to.bed)) %/% 60) |>
      mutate(minute_woke_up = unclass(interval(day, Woke.up)) %/% 60)
    
    clrs <- palette()[c(1,2,4)]
    
    minutes |>
      cross_join(tibble(val = samples)) |>
      filter(input$weekday_selector == 0 |
               input$weekday_selector == wday(day, week_start = 1)) |>
      filter(minute_went_to_bed <= val & val <= minute_woke_up) |>
      ggplot(aes(x = make_datetime(min = val), y = sleeper, fill = factor(sleeper))) +
      stat_density_ridges(alpha = 0.6) +
      scale_fill_manual(values = c("Sebastian" = clrs[3], "Piotr" = clrs[2], "Olek" = clrs[1])) +
      scale_x_datetime(
        limits = c(
          make_datetime(hour = -2),
          make_datetime(hour = 13)
        )
      ) +
      labs(
        x = "Time of a day",
        y = NULL,
        fill = "sleeper"
      ) +
      theme_ridges() +
      theme(
        text = theme_get()$text,
        axis.text = theme_get()$text,
        axis.text.y = element_blank()
      )
  })
  
  output$density_plot <- renderPlot({
    clrs <- palette()[c(1,2,4)]
    filtered_data <- Data %>% 
      filter(between(day, as.Date(input$dateSelector[1]), as.Date(input$dateSelector[2])))
    print(input$dateSelector[2])
    print(filtered_data)
    
    mean_df <- filtered_data %>% 
      group_by(sleeper) %>% 
      summarise(mean_SQ = mean(Sleep.Quality))
    
    
    density_plot <- ggplot(filtered_data, aes(x = Sleep.Quality, fill = sleeper)) + 
      geom_density(alpha = 0.3) +
      geom_vline(data = mean_df,
                 aes(xintercept = mean_SQ, color = sleeper),
                 linetype = "dashed") + 
      scale_fill_manual(values = c("Sebastian" = clrs[3], "Piotr" = clrs[2], "Olek" = clrs[1])) +
      scale_color_manual(values = c("Sebastian" = clrs[3], "Piotr" = clrs[2], "Olek" = clrs[1])) +
      theme_minimal() +
      theme(
        text = theme_get()$text,
        axis.text.x = element_text(
          colour =  theme_get()$text$colour,
          size = 12),
        axis.text.y = element_text(
          colour =  theme_get()$text$colour,
          size = 12)
        
        
      ) + labs(x = "Sleep quality", y = "Density") +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
    
    density_plot
  })
  
  ########## individual_data_page - indywidualne informacje ##########
  
  
  output$sleeptimeCrossbar <- renderPlot({
    personal_colours <- palette()[c(1,2,4)]
    names(personal_colours) <- c("Olek", "Piotr", "Sebastian")
    plot_data <- Data |>
      mutate(woke_up_inter = interval(day, Woke.up),
             went_to_bed_inter = interval(day, Went.to.bed),
             fell_asleep_time = Went.to.bed + dseconds(Asleep.after..seconds.)) |>
      mutate(fell_asleep_inter = interval(day, fell_asleep_time))
    (
      plot_data |>
        filter(sleeper == input$selectSleeper) |>
        ggplot(aes(x = day, y = fell_asleep_inter)) +
        # geom_crossbar(aes(ymin = went_to_bed_inter, ymax = fell_asleep_inter),
        #               fill = "#887711", colour = NA) +
        geom_crossbar(aes(ymax = woke_up_inter, ymin = fell_asleep_inter),
                      fill = personal_colours[input$selectSleeper], colour = NA) +
        scale_y_time(labels = (\(x) format(make_datetime(sec = x), "%H:%M")),
                     breaks =  (\(x) {
                       foo <- make_datetime(sec = floor(x[1]):(x[2]+1));
                       foo <- foo[second(foo)==0 & minute(foo)==0]}),
                     limits = c(
                       min(plot_data$went_to_bed_inter),
                       max(plot_data$woke_up_inter)
                     )
        ) +
        labs(
          y = "time of day"
        ) +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank()
        )
    ) + (
      Data |>
        filter(sleeper == input$selectSleeper) |>
        ggplot(aes(x = day, y = Sleep.Quality)) +
        geom_xspline() +
        # ylim(min(Data$Sleep.Quality), NA) +
        scale_y_continuous(
          labels = (\(x) paste(100*x, "%")),
          limits = c(min(Data$Sleep.Quality) - 0.05, NA),
          breaks = 2:5 /5
        ) +
        labs(
          y = "sleep quality"
        ) +
        scale_x_date(date_breaks = "3 days",
                     date_labels = "%b %e")
    ) + plot_layout(
      # guides = "collect",
      nrow = 2,
      ncol = 1,
      heights = c(0.7, 0.3)
    ) &
      theme(
        panel.background = element_blank(),
        panel.grid.major = element_line(
          colour = alpha(theme_get()$text$colour, 0.5)),
        axis.text = element_text(
          size = 12,
          colour = theme_get()$text$colour
        ),
        axis.title.y = element_text(
          colour = theme_get()$text$colour,
          angle = 90,
          size = 14
        )
      )
  }) |> bindCache(input$selectSleeper)
  
  
  output$sleepDistractionScatter <- renderPlotly({
    personal_colours <- palette()[c(1,2,4)]
    names(personal_colours) <- c("Olek", "Piotr", "Sebastian")
    
    dane <- Data %>% filter(sleeper == input$selectSleeper)
    plot_ly(dane, x = ~Movements.per.hour, y = ~Sleep.Quality,
            text = ~paste("Kaszlnięcia na godzinę: ", Coughing..per.hour.,
                          "<br> Czas chrapania: ", Snore.time..seconds., "s"),
            hoverinfo = "text",
            type = "scatter",
            mode = "markers",
            marker = list(
              color = personal_colours[input$selectSleeper],  
              size = 10,  
              line = list(
                color = "#FFFFFF", 
                width = 1  
              ))) %>% 
      layout(
        paper_bgcolor = "#2C3E50",
        plot_bgcolor = "#2C3E50",  
        font = list(color = "#FFFFFF"),
        xaxis = list(
          title = "Movements per hour",
          color = "#FFFFFF", 
          gridcolor = "#34495E",
          range = c(-5, 150)
        ),
        yaxis = list(
          title = "Sleep quality",
          color = "#FFFFFF", 
          gridcolor = "#34495E",
          range = c(0.3, 1.05)
        ),
        hoverlabel = list(
          bgcolor = "#2C3E50", 
          font = list(color = "#FFFFFF")
        ),
        hoverdistance = 5
      )
  }) |> bindCache(input$selectSleeper)
  
  
  output$activityBoxplot <- renderPlot({
    Piotr <- Piotr %>% 
      mutate(activity = c(FALSE, FALSE, TRUE, FALSE,
                          FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
                          FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE,
                          FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                          TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                          TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE))
    Olek <- Olek %>% 
      mutate(activity = c(TRUE, FALSE, FALSE, FALSE,
                          FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE,
                          FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE,
                          TRUE, FALSE, FALSE, FALSE, TRUE, FALSE,
                          FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE,
                          FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE))
    
    validate(
      need(input$selectSleeper != "Sebastian", "Brak danych dla Sebastiana")
    )
    
    personal_colours <- palette()[c(1,2,4)]
    names(personal_colours) <- c("Olek", "Piotr", "Sebastian")
    
    p <- ggplot(if (input$selectSleeper == "Piotr") Piotr else Olek,
                aes(x = activity, y = Sleep.Quality)) +
      geom_boxplot(fill = personal_colours[input$selectSleeper], color = "white") + 
      theme_minimal() +
      theme(
        text = theme_get()$text,
        panel.background = element_rect(fill = "#2C3E50"),
        plot.background = element_rect(fill = "#2C3E50", color = NA),
        legend.background = element_rect(fill = "#2C3E50"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        axis.text = element_text(color = "white", size = 12), 
        axis.title = element_text(color = "white"),
        panel.grid.major = element_line(color = "grey20"),
        axis.ticks = element_line(color = "white")
      ) +
      ylim(0.3, 1) +
      labs(x = "Physical activity",
           y = "Sleep quality") +
      scale_x_discrete(labels = c("FALSE" = "No", "TRUE" = "Yes"))
    p
    
  }) |> bindCache(input$selectSleeper)
  
  output$heatmap <- renderPlot({
    tmp <- Data %>% 
      filter(sleeper == input$selectSleeper)
    pom1 <- generate_pom(tmp)
    p <- plot(pom1)
    p
  }) |> bindCache(input$selectSleeper)
  
  
  ########## animation_page - aniamacja ##########
  
  
  output$bar_plot <- renderPlot({
    unique_days <- unique(Data$day)
    add_missing_days <- function(df, sleeper, days) {
      missing_days <- setdiff(days, df$day)
      new_data <- data.frame(
        sleeper = sleeper,
        day = as.Date(missing_days),
        Time.asleep..seconds. = rep(NA, length.out = length(missing_days))
      )
      df <- df %>% select(sleeper,day,Time.asleep..seconds.)
      df <- bind_rows(df,new_data)
      df <- df %>%
        mutate(
          meanTime = mean(Time.asleep..seconds., na.rm = TRUE),
          Time.asleep..seconds. = ifelse(is.na(Time.asleep..seconds.), meanTime, Time.asleep..seconds.)
        ) %>% select(-meanTime)
      return(df)
    }
    
    f1 <- add_missing_days(Data %>% filter(sleeper == "Sebastian"), "Sebastian", unique_days)
    f2 <- add_missing_days(Data %>% filter(sleeper == "Olek"), "Olek", unique_days)
    f3 <- Data %>% filter(sleeper == "Piotr") %>% select(sleeper,day, Time.asleep..seconds.)
    filtered_data <- bind_rows(f1,f2,f3)
    
    filtered_data <- filtered_data %>% 
      filter(day >= as.Date('2024-12-15')) %>% 
      filter(day <= input$day) %>%
      group_by(sleeper) %>%
      mutate(CumulativeSleepHours = as.numeric(sum(Time.asleep..seconds.)/ 3600)) %>% 
      summarise(CumulativeSleepHours = max(CumulativeSleepHours)) %>% 
      arrange(desc(CumulativeSleepHours))
    
    ggplot(filtered_data, aes(x = reorder(sleeper, CumulativeSleepHours), y = CumulativeSleepHours, fill = sleeper)) +
      geom_col(alpha = 0.8) +
      labs(
        title = paste("Cumulative sleep time till", input$day),
        x = element_blank(),
        y = "Sleep time (hours)"
      ) +
      theme_minimal() +
      theme(
        text = theme_get()$text,
        axis.text.x = element_text(
          colour =  theme_get()$text$colour,
          size = 12)
      ) +
      coord_flip()
    
  }) |> bindCache(input$day)
}

# ------------------------------------------------------------------------------
# aplikacja
# ------------------------------------------------------------------------------

app_ui <- navbarPage(
  title = div(
    icon("bed", style = "color: darkblue; font-size: 24px;"), 
    "MiNI REST",
    icon("bed", style = "color: darkblue; font-size: 24px;"),
  ),
  tabPanel("Main page", main_page),
  tabPanel("General data", general_data_page),
  tabPanel("Individual data", individual_data_page),
  tabPanel("Animation", animation_page),
  theme = bslib::bs_theme(bootswatch = "darkly", 
                          primary = "#F39C12",
                          secondary = "#3498DB",
                          bg = "#2C3E50",   
                          fg = "#FFFFFF",
                          success = "#AED6F1"),
  header = tags$head(),
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  Link to our 
                  <a class='highlighted-link' href='https://github.com/SebastianBoteroLeonik/
                  TWD-Projekt_2/'
                  style='color: #007bff; font-weight: bold; text-decoration: underline;'
                  > repo</a>
                </p>
                <p class='text-center' style='font-size:12px;'>
                  Used libraries: 
                  <span style='white-space: nowrap;'>
                    • shiny • dplyr • lubridate • ggplot2 • tidyr • plotly • ggridges • 
                    bslib • fmsb • patchwork • ggalt • jpeg • shinydashboard • thematic • shinycssloaders
                  </span>
                </p>
                </footer>
                ")
)

# Run the application 
shinyApp(app_ui, server)


