library(shiny)
library(shinydashboard)
library (shinycssloaders)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(plotly)
library(lubridate)
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)
library(ggforce)
library(stringi)


people <- c("stefan", "pawel", "szymon")


szymon <- read.csv("szymon.csv") %>%
  mutate(ts = as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%SZ")) %>% 
  rename(track = master_metadata_track_name,
         artist = master_metadata_album_artist_name,
         album = master_metadata_album_album_name) %>%
  filter(!is.na(track))
pawel <- read.csv("pawel.csv") %>%
  mutate(ts = as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%SZ")) %>% 
  rename(track = master_metadata_track_name,
         artist = master_metadata_album_artist_name,
         album = master_metadata_album_album_name) %>%
  filter(!is.na(track))
stefan <- read.csv("stefan.csv") %>%
  mutate(ts = as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%SZ")) %>%
  rename(track = master_metadata_track_name,
         artist = master_metadata_album_artist_name,
         album = master_metadata_album_album_name) %>%
  filter(!is.na(track))



# UI
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/2/26/Spotify_logo_with_text.svg", height = "30px"), # Spotify logo
    titleWidth = 300
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Wrapped", tabName = "dashboard", icon = icon("cd", lib = "glyphicon")),
      menuItem("Compare", tabName = "dashboard2", icon = icon("align-left", lib = "glyphicon"))
    ),
    selectInput("osoba", "Choose Person:", choices = c("Stefan", "Szymon", "Pawel"), selected = "Stefan"),
    tags$div(
      h3("Your Summary", style = "color: white; text-align: center;"),
      div(class = "info-box", style = "background-color: #1db954; color: white; text-align: center; padding: 10px; margin-bottom: 10px;",
          h1(shinycssloaders::withSpinner(textOutput("text1"),type = getOption("spinner.type", default = 1),color = getOption("spinner.color", default = "#121212"), size = 0.5,proxy.height = "50px"), style = "font-size: 40px; margin: 0;"),
          p("Minutes listened", style = "margin: 0;margin-bottom: 10px;")
      ),
      div(class = "info-box", style = "background-color: #1db954; color: white; text-align: center; padding: 10px; margin-bottom: 10px;",
          h1(shinycssloaders::withSpinner(textOutput("text2"),type = getOption("spinner.type", default = 1),color = getOption("spinner.color", default = "#121212"), size = 0.5,proxy.height = "50px"), style = "font-size: 40px; margin: 0;"),
          p("Songs played", style = "margin: 0;")
      ),
      div(class = "info-box", style = "background-color: #1db954; color: white; text-align: center; padding: 10px; margin-bottom: 10px;",
          h1(shinycssloaders::withSpinner(textOutput("text3"),type = getOption("spinner.type", default = 1),color = getOption("spinner.color", default = "#121212"), size = 0.5,proxy.height = "50px"), style = "font-size: 40px; margin: 0;"),
          p("Albums", style = "margin: 0;")
      ),
      div(class = "info-box", style = "background-color: #1db954; color: white; text-align: center; padding: 10px; margin-bottom: 10px;",
          h1(shinycssloaders::withSpinner(textOutput("text4"),type = getOption("spinner.type", default = 1),color = getOption("spinner.color", default = "#121212"), size = 0.5,proxy.height = "50px"), style = "font-size: 40px; margin: 0;"),
          p("Artists", style = "margin: 0;")
      )
    )
  ),
  
  dashboardBody(
    tags$link(rel = "stylesheet", type = "text/css", href = "style3.css"),
    tags$head(tags$style(HTML(".main-header { background-color: #191414 !important; }
                              .main-sidebar { background-color: #121212 !important; }
                              .box { background-color: #121212; color: white; margin-bottom: 30px; height: 300px; }
                              .skin-black .main-header .logo { background-color: #191414; color: white; }
                              .skin-black .main-sidebar .sidebar-menu > li.active > a { background-color: #1db954; }
                              .slider-input {margin top: auto !important;}
                              .content-wrapper { background-color: #191414; padding-bottom: 200px}
                              .selectize-input { background-color: #2c2c2c; color: white; border-color: #2c2c2c; }
                              .slider { color: #1db954; margin-top: auto !important;}
                              .navbar-static-top { background-color: #212121 !important; border-color: #212121}
                              .sidebar-toggle {background-color: #212121 !important; border-color: #212121}
                              .box-body {background-color: #121212 !important; color: white !important}
                              .box-header {background-color: #212121 !important; border-color: #212121 !important}
                              .bottom-panel {background-color: #212121 !important; position: fixed; bottom: 0; left: 0; width: 100%; height: 10% !important; z-index: 1000; display: flex; justify-content: center; align-items: center;}
                              .irs-bar {background-color: #1db954 !important; border-color: #1db954 !important; height: 10px !important}
                              .box {border: none !important; background-color: #121212 !important; color: #121212 !important}
                              .tab-pane {border: none !important;}
                              .irs-from {background-color: #1db954  !important;color: white !important;}
                              .irs-to {background-color: #1db954  !important; color: white !important;}
                              .selectize-dropdown-content .active {background-color: #121212 !important;color: #1db954 !important; }
                              .selectize-dropdown-content .selected {background-color: #121212 !important;color: #1db954 !important; }
                              .tab-content {background-color: white !important;}
                              .shiny-plot-output {background-color: #1c1c1c !important; border: none !important}
                              .dashboard2 {padding-bottom: 700px !important;}"))),
    
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 4,
                       box(title = "Select Grouping", status = "primary", solidHeader = TRUE, width = NULL,height = "150px",
                           selectInput("type", "Select Type:", choices = c("track", "album", "artist"),selected = "track")
                       ),
                       box(title = "Select Grouping", status = "primary", solidHeader = TRUE, width = NULL,height = "150px",
                           selectInput("type2", "Select Type:", choices = c("streams", "minutes"),selected = "streams")
                       )
                ),
                column(width = 8,
                       box(title = "", status = "primary", solidHeader = TRUE, width = NULL, height = "25%", shinycssloaders::withSpinner(plotOutput("plot1"),type = getOption("spinner.type", default = 6),color = getOption("spinner.color", default = "#1DB954")))
                )
              ),
              fluidRow(
                column(width = 4,
                       box(title = "Select Grouping", status = "primary", solidHeader = TRUE, width = NULL,height = "150px",
                           selectInput("type3", "Select Period:", choices = c("hour", "weekday", "month"),selected = "hour")
                       )
                ),
                column(width = 8,
                       box(title = "", status = "primary", solidHeader = TRUE, width = NULL, height = "25%",shinycssloaders::withSpinner(plotOutput("plot3"),type = getOption("spinner.type", default = 6),color = getOption("spinner.color", default = "#1DB954")))
                )
              ),
              fluidRow(
                column(width = 6,
                       box(title = "", status = "primary", solidHeader = TRUE, width = NULL, height = "25%",shinycssloaders::withSpinner(plotOutput("plot2"),type = getOption("spinner.type", default = 6),color = getOption("spinner.color", default = "#1DB954")))
                ),
                column(width = 6,
                       box(title = "", status = "primary", solidHeader = TRUE, width = NULL, height = "25%",shinycssloaders::withSpinner(plotlyOutput("plot4"),type = getOption("spinner.type", default = 6),color = getOption("spinner.color", default = "#1DB954")))
                )
              )
      ),
      
      tabItem(tabName = "dashboard2",class = "dashboard2",
              fluidRow(
                column(width = 8,
                       box(title = "", status = "primary", solidHeader = TRUE, width = NULL, height = "27%", shinycssloaders::withSpinner(plotOutput("plot5"),type = getOption("spinner.type", default = 6),color = getOption("spinner.color", default = "#1DB954")))
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  fluidRow(
                    column(
                      width = 6,
                      box(
                        title = "Choose people to compare",
                        status = "primary",
                        solidHeader = TRUE,
                        width = NULL,
                        height = "150px",
                        selectInput("p1", "First Person", people)
                      )
                    ),
                    column(
                      width = 6,
                      box(
                        title = "",
                        status = "primary",
                        solidHeader = TRUE,
                        width = NULL,
                        height = "150px",
                        uiOutput("p2")
                      )
                    )
                  )
                ),
                column(
                  width = 12,
                  box(
                    title = "",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    shinycssloaders::withSpinner(
                      plotOutput("plot6", width = "1150px", height = "875px"),
                      type = getOption("spinner.type", default = 6),
                      color = getOption("spinner.color", default = "#1DB954")
                    )
                  )
                )
              )
              
      )
    ),
    
    div(
      class = "bottom-panel",
      div(
        class = "center-bottom-panel",
        div(
          icon(id = "backward", "backward", style = "margin: auto;",class = "fa-1.5x"),
          icon("pause", style = "margin: auto;",class = "fa-1.5x"),
          icon("forward", id = "forward", style = "margin: auto;", class = "fa-1.5x"),
          class = "icons-box"
        ),
        div(
          class = "slider",
          sliderInput(
            "year",
            "",
            min = 2015,
            max = 2025,
            value = c(2015, 2025),
            timeFormat = "%d.%m"
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  output$text1 <- renderText({
    df <- get(tolower(input$osoba)) # Pobranie danych na podstawie nazwy
    paste(sum(round((
            df %>% filter(between(ts, as.Date(paste0(input$year[1], "-01-01"))
                                  , as.Date(paste0(input$year[2], "-12-31")))))
            $ms_played / 60000)))
  })

   output$text2 <- renderText({
     df <- get(tolower(input$osoba)) # Pobranie danych na podstawie nazwy
     paste(
     nrow(df %>%
            filter(between(ts, as.Date(paste0(input$year[1], "-01-01"))
                           , as.Date(paste0(input$year[2], "-12-31")))) %>% 
            distinct(track)
     ))
   })

   output$text3 <- renderText({
     df <- get(tolower(input$osoba)) # Pobranie danych na podstawie nazwy
     paste(
     nrow(df %>%
            filter(between(ts, as.Date(paste0(input$year[1], "-01-01"))
                           , as.Date(paste0(input$year[2], "-12-31")))) %>% 
            distinct(album)
     ))
   })

   output$text4 <- renderText({
     df <- get(tolower(input$osoba)) # Pobranie danych na podstawie nazwy
     paste(
     nrow(df %>%
            filter(between(ts, as.Date(paste0(input$year[1], "-01-01"))
                           , as.Date(paste0(input$year[2], "-12-31")))) %>% 
            distinct(artist)
     ))
   })
  
  
  output$plot1<-renderPlot({
    wybrana_osoba <- tolower(input$osoba) # Zamiana na małe litery, aby pasowało do nazw obiektów
    df<-get(wybrana_osoba) # Pobranie danych na podstawie nazwy
    
    df %>% filter(between(ts, as.Date(paste0(input$year[1], "-01-01")), as.Date(paste0(input$year[2], "-12-31")))) %>%
      group_by(!!as.symbol(input$type)) %>%
      summarise(count = if (input$type2 == "streams") {
        n()
      } else {
        sum(ms_played / 60000)
      }) %>% 
      top_n(5, count) %>%
      arrange(desc(count)) %>% 
      mutate(data = fct_reorder(!!as.symbol(input$type), count)) %>%
      ggplot(aes(x = data, y = count)) +
      geom_col(fill = "#1db954") +
      coord_flip() +
      # add labs
      labs(title = paste("5 most listened", paste(input$type, "s", sep = ""), "by", input$osoba),
           y = if (input$type2 == "streams") {
             "Number of plays"
           } else {
             "Time spent listening to music [min]"
           }) +
      theme(axis.title.y = element_blank())+
      theme(
        panel.background = element_rect(fill = "#151515", colour = "#000000",size = 2, linetype = "solid"),
        plot.background = element_rect(fill = "#121212",colour = "#000000"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#302f2f"), 
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#302f2f"),
        title = element_text(colour = "white", size = 20),
        axis.text.x = element_text(colour = "white", size = 12),
        axis.text.y = element_text(colour = "white",size = 12),
        legend.background = element_rect(fill = "#121212"),
        legend.text = element_text(face = "bold", color = "white", size = 10),
        panel.border = element_rect(colour = "black", fill = NA, size = 2)
      )
  }, bg="transparent")
  
  
  output$plot2 <- renderPlot({
    wybrana_osoba <- tolower(input$osoba) # Zamiana na małe litery, aby pasowało do nazw obiektów
    df<-get(wybrana_osoba) # Pobranie danych na podstawie nazwy
    df %>% filter(between(ts, as.Date(paste0(input$year[1], "-01-01")), as.Date(paste0(input$year[2], "-12-31")))) %>%
      mutate(ms_played = ms_played / 60000) %>%
      ggplot(aes(x = ms_played)) +
      geom_density(colour = "#1DB954", alpha = 0.7) +
      labs(title = paste("How long did", input$osoba, "listen to a song"),
           x = "Time spent listening to song [min]", y = "Density")+
      theme_minimal()+
      theme(
        panel.background = element_rect(fill = "#151515", colour = "#000000",size = 2, linetype = "solid"),
        plot.background = element_rect(fill = "#121212",colour = "#000000"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#302f2f"), 
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#302f2f"),
        title = element_text(colour = "white", size = 20),
        axis.text.x = element_text(colour = "white", size = 12),
        axis.text.y = element_text(colour = "white",size = 12),
        legend.background = element_rect(fill = "#121212"),
        legend.text = element_text(face = "bold", color = "white", size = 10),
        line = element_line(colour = "#1DB954", size = 2)
      )
  }, bg="transparent")
  
  
  output$plot3 <- renderPlot({
    wybrana_osoba <- tolower(input$osoba) # Zamiana na małe litery, aby pasowało do nazw obiektów
    df <- get(wybrana_osoba) %>% 
      filter(between(ts, as.Date(paste0(input$year[1], "-01-01")), as.Date(paste0(input$year[2], "-12-31"))))
    if (input$type3 == "hour") {
      df %>%
        mutate(date = strftime(ts, format = "%F"), hour = strftime(ts, format = "%H")) %>%
        group_by(date, hour) %>%
        summarise(time = sum(ms_played/60000)) %>%
        pivot_wider(names_from = hour, values_from = time, values_fill = 0) %>%
        pivot_longer(cols = -date, names_to = "hour", values_to = "time") %>%
        group_by(hour) %>%
        summarise(time = mean(time)) %>%
        ggplot(aes(x = hour, y = time)) +
        geom_col(fill = "#1db954") +
        labs(title = "Average time spent listening to music by hour of the day",
             x = "Hour of the day",
             y = "Average time spent listening to music [min]")+
        theme(
          panel.background = element_rect(fill = "#151515", colour = "#000000",size = 2, linetype = "solid"),
          plot.background = element_rect(fill = "#121212",colour = "#000000"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "#302f2f"), 
          panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                          colour = "#302f2f"),
          title = element_text(colour = "white", size = 20),
          axis.text.x = element_text(colour = "white", size = 12),
          axis.text.y = element_text(colour = "white",size = 12),
          legend.background = element_rect(fill = "#121212"),
          legend.text = element_text(face = "bold", color = "white", size = 10),
        )
    } else if (input$type3 == "weekday") {
      df %>%
        mutate(date = strftime(ts, format = "%F")) %>%
        group_by(date) %>%
        summarise(time = sum(ms_played/60000)) %>%
        mutate(weekday = strftime(date, format = "%w")) %>% 
        group_by(weekday) %>% 
        summarise(time = mean(time)) %>%
        ggplot(aes(x = weekday, y = time)) +
        geom_col(fill = "#1db954") +
        labs(title = "Average time spent listening to music by day of the week",
             x = "Day of the week",
             y = "Average time spent listening to music [min]")+
        theme(
          panel.background = element_rect(fill = "#151515", colour = "#000000",size = 2, linetype = "solid"),
          plot.background = element_rect(fill = "#121212",colour = "#000000"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "#302f2f"), 
          panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                          colour = "#302f2f"),
          title = element_text(colour = "white", size = 20),
          axis.text.x = element_text(colour = "white", size = 12),
          axis.text.y = element_text(colour = "white",size = 12),
          legend.background = element_rect(fill = "#121212"),
          legend.text = element_text(face = "bold", color = "white", size = 10),
        )
    } else if (input$type3 == "month") {
      df %>%
        mutate(month = strftime(ts, format = "%m"),
               year = strftime(ts, format = "%Y")) %>% 
        group_by(month, year) %>%
        summarise(time = sum(ms_played/60000)) %>%
        group_by(month) %>% 
        summarise(time = mean(time)) %>%
        ggplot(aes(x = month, y = time)) +
        geom_col(fill = "#1db954") +
        labs(title = "Average time spent listening to music by month",
             x = "Month",
             y = "Average time spent listening to music [min]")+
        theme(
          panel.background = element_rect(fill = "#151515", colour = "#000000",size = 2, linetype = "solid"),
          plot.background = element_rect(fill = "#121212",colour = "#000000"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "#302f2f"), 
          panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                          colour = "#302f2f"),
          title = element_text(colour = "white", size = 20),
          axis.text.x = element_text(colour = "white", size = 12),
          axis.text.y = element_text(colour = "white",size = 12),
          legend.background = element_rect(fill = "#121212"),
          legend.text = element_text(face = "bold", color = "white", size = 10),
        )
    }
  }, bg="transparent")
  
  
  output$plot4<-renderPlotly({
    wybrana_osoba <- tolower(input$osoba) # Zamiana na małe litery, aby pasowało do nazw obiektów
    df<-get(wybrana_osoba) # Pobranie danych na podstawie nazwy
    df <- df %>%
      filter(
        between(ts, as.Date(paste0(input$year[1], "-01-01")), as.Date(paste0(input$year[2], "-12-31"))),
        !is.na(skipped)
      ) %>%
      mutate(
        skipped = if_else(reason_end %in% c("fwdbtn", "backbtn"), TRUE, FALSE),
        ms_played = ms_played / 60000
      ) %>%
      group_by(skipped) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(
        percentage = count / sum(count) * 100
      )
      
      plot_ly(
        data = df,
        labels = ~skipped,
        values = ~count,
        type = "pie",
        textinfo = "percent",
        hoverinfo = "text+label+value+percent",
        marker = list(colors = c("#6b6b6b", "#1db954"))
      ) %>%
      layout(
        title = paste("Proportion of skipped songs by", input$osoba),
        paper_bgcolor = "#1c1c1c",
        plot_bgcolor  = "#1c1c1c",
        # Text and legend styling
        font = list(color = "white"),
        legend = list(
          font = list(color = "white"),
          title = list(text = "Skipped")
        )
      )
    
  })
  
  
  #Wykresy do drugiej zakładki
  output$plot5<-renderPlot({
    szym <- szymon %>% 
      filter(year(ts) >= input$year[1] & year(ts) <= input$year[2]) %>% 
      mutate(date = paste(year(ts), format(ts, "%m"), format(ts, "%d"), sep = "-")) %>% 
      group_by(date) %>%
      summarise(ms_played = sum(ms_played)) %>%
      arrange(date()) %>%
      mutate(minutes_cumulative = cumsum(as.numeric(ms_played)) / 60000,
             username = "szymon")
    stef <- stefan %>% 
      filter(year(ts) >= input$year[1] & year(ts) <= input$year[2]) %>% 
      mutate(date = paste(year(ts), format(ts, "%m"), format(ts, "%d"), sep = "-")) %>% 
      group_by(date) %>%
      summarise(ms_played = sum(ms_played)) %>%
      arrange(date) %>%
      mutate(minutes_cumulative = cumsum(as.numeric(ms_played)) / 60000,
             username = "stefan")
    paw <- pawel %>% 
      filter(year(ts) >= input$year[1] & year(ts) <= input$year[2]) %>% 
      mutate(date = paste(year(ts), format(ts, "%m"), format(ts, "%d"), sep = "-")) %>% 
      group_by(date) %>%
      summarise(ms_played = sum(ms_played)) %>%
      arrange(date) %>%
      mutate(minutes_cumulative = cumsum(as.numeric(ms_played)) / 60000,
             username = "pawel")
    bind_rows(szym, stef, paw) %>% mutate(date = as.POSIXct(date, format = "%Y-%m-%d")) %>% 
      ggplot(aes(x = date, y = minutes_cumulative, color = username, group = username)) +
      geom_line() +
      labs(title = "Cumulative time spent listening to music",
           x = "Date",
           y = "Cumulative time spent listening to music",
           color = "User")+
      theme(
        panel.background = element_rect(fill = "#151515", colour = "#000000", size = 2, linetype = "solid"),
        plot.background = element_rect(fill = "#121212", colour = "#000000"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#302f2f"), 
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "#302f2f"),
        title = element_text(colour = "white", size = 20),
        axis.text.x = element_text(colour = "white", size = 12),
        axis.text.y = element_text(colour = "white", size = 12),
        legend.background = element_rect(fill = "#121212"),
        legend.text = element_text(face = "bold", color = "white", size = 10),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")  
      ) +
      scale_y_continuous(labels = scales::label_number())
    
  }, bg="transparent")
  
  output$p2 <- renderUI({
    selectInput("p2", "Second Person", people[!(people %in% input$p1)])
  })
  
  output$plot6 <- renderPlot({
    df1 <- get(input$p1) %>% 
      filter(year(ts) >= input$year[1] & year(ts) <= input$year[2]) %>% 
      group_by(track) %>% 
      summarise(count1 = n())
    
    df2 <- get(input$p2) %>%
      filter(year(ts) >= input$year[1] & year(ts) <= input$year[2]) %>% 
      group_by(track) %>% 
      summarise(count2 = n())
    
    df_all <- bind_rows(get(input$p1), get(input$p2)) %>% 
      filter(year(ts) >= input$year[1] & year(ts) <= input$year[2]) %>% 
      group_by(track) %>% 
      summarise(count_all = n())
    
    df <- left_join(df_all, df1, by = "track") %>% 
      left_join(df2, by = "track") %>% 
      replace_na(list(count1 = 0, count2 = 0)) %>%
      arrange(desc(count_all)) %>% 
      mutate(track = fct_reorder(track, count_all),
             track_percent = count1 / count_all) %>% 
      top_n(30, count_all) %>% 
      mutate(track = sapply(track, function(x) paste(strwrap(x, width = 15), collapse = "\n")))
    
    
    packing <- circleProgressiveLayout(df$count_all, sizetype = "area")
    df <- data.frame(df, packing)
    
    dat.gg <- circleLayoutVertices(packing, npoints = 200)
    dat.gg <- bind_cols(dat.gg, rep(df$track_percent, each = 201))
    colnames(dat.gg)[4] <- "track_percent"
    ggplot() +
      geom_polygon_interactive(
        data = dat.gg,
        aes(x, y, group = id, data_id = id, fill = track_percent),
        colour = "transparent"
      ) + 
      coord_equal() +
      theme_void() +
      geom_text_interactive(
        data = df,
        aes(
          x = x, 
          y = y,
          label = track,
          data_id = track,
          size = radius/100        
        ),
        colour = "black"
      ) +
      labs(
        title = paste("Most listened tracks by", stringr::str_to_title(input$p1), "and", stringr::str_to_title(input$p2)),
        fill = "Who listened more"
      ) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "#1c1c1c",colour = NA),
        plot.background  = element_rect(fill = "#1c1c1c",colour = NA),
        legend.background = element_rect(fill = "#1c1c1c",colour = NA),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.border = element_blank(),
        axis.line = element_blank(),
        legend.text = element_text(colour = "white"),
        legend.title = element_text(colour = "white"),
        plot.title = element_text(colour = "white", size = 20)
      ) +
      scale_fill_gradient(
        low = "#6A0DAD",    
        high = "#1db954",    
        name = "Who listened more",  
        limits = c(0, max(df$track_percent)),  
        breaks = c(0, max(df$track_percent)),  
        labels = c(stringr::str_to_title(input$p2), stringr::str_to_title(input$p1))        
      ) +
      scale_size(range = c(3, 7), guide = FALSE)
    
  }, bg="transparent")
}

# Run App
shinyApp(ui, server)
