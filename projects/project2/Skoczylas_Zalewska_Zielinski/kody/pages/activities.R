activitiesUI <- function(id) {
  ns <- NS(id)
  
  tags$div(
    class = "container-fluid",
    
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Audiowide&display=swap"),
      tags$style(HTML("
      body {
        background-color: #F2F7F7; 
      }
      .control-label {
        font-weight: bold;
        
      }
      .box-title {
        font-family: 'Audiowide', cursive;
        font-size: 20px;
         font-weight: bold;
         
      }
    "))
    ),
    
    box(
      title = "",
      solidHeader = TRUE,       
      status = "primary",              
      width = 12,                       
      style = "background-color: #158cba	; border: 2px solid #158cba; border-radius: 10px; padding: 10px ;color: white;", 
      fluidRow(
        column(4,
               dateInput(ns("selected_date"), "Choose a date", 
                         value = "2024-12-18", 
                         format = "yyyy-mm-dd",
                         min = "2024-12-08",
                         max = max(as.Date(activities_df$time.ended))))
      )),
    fluidRow(style = "height: 40px;"),
    fluidRow(
      column(3,box(
        title = tags$span(
          "What do we do during the day?", 
          style = "background-color: #ff7f0e; color: white; padding: 10px; border-radius: 10px; width: 100%; display: inline-block; box-sizing: border-box;"
        ),
        solidHeader = TRUE, 
        width = 12,
        style = "background-color: #f9f9f9; border: 2px solid #ff7f0e; border-radius: 10px; padding: 10px;",  # Pomarańczowa obramówka
        tags$div(
          style = "font-size: 16px; color: black; ",
          uiOutput(ns("text1"))
        )
      )),
      column(9,box(
        title = "", 
        solidHeader = TRUE,                     
        status = "primary", 
        width = 12,
        style = "background-color: #f9f9f9; border: 2px solid #ff7f0e; border-radius: 10px; padding: 0px; display: inline-block; vertical-align: top; width: 100%;", 
        tags$div(
          style = "display: flex; justify-content: center; align-items: center; height: 100%;",
          plotlyOutput(ns("timeline"), height = "100%", width = "1100px" )%>%
            shinycssloaders::withSpinner(color = "#158cba", type=6)
        )
      )) 
    ),
    fluidRow(style = "height: 80px;"),
    box(
      title = "",
      solidHeader = TRUE,       
      status = "primary",              
      width = 12,                       
      style = "background-color: #158cba	; border: 2px solid #158cba; border-radius: 10px; padding: 10px ;color: white;", 
      fluidRow(
        selectInput(
          ns("activityChoice"),
          "Choose an activity", 
          choices = unique(activities_df$activity.name), 
          selected = "MINI"
        )
      )),
    fluidRow(style = "height: 40px;"),
    fluidRow(
      column(4,
             value_box(
               title= uiOutput(ns("avg_duration2")),
               value = uiOutput(ns("avg_duration1")),
               showcase = icon("clock"),
               showcase_layout = "top right",
               style = "background-color: #ff7f0e; color: white;")),
      column(4, 
             value_box(
               title=uiOutput(ns("max_duration2")),
               value = uiOutput(ns("max_duration1")),
               showcase = icon("trophy"),
               showcase_layout = "top right",
               style = "background-color: #dc143c; color: white;")
      ),
      column(4, 
             value_box(
               title=uiOutput(ns("min_duration2")),
               value = uiOutput(ns("min_duration1")),
               showcase = icon("face-sad-tear"),
               showcase_layout = "top right",
               style = "background-color: #158cba; color: white;")
      )
    ),
    fluidRow(style = "height: 40px;"),
    fluidRow(
      column(3, box(
        title = tags$span(
          "What do we...?", 
          style = "background-color: #dc143c; color: white; padding: 10px; border-radius: 10px; width: 100%; display: inline-block; box-sizing: border-box;"
        ),
        solidHeader = TRUE, 
        width = 12,
        style = "background-color: #f9f9f9; border: 2px solid #dc143c; border-radius: 10px; padding: 10px;",  # Pomarańczowa obramówka
        tags$div(
          style = "font-size: 16px; color: black; text-align: left;",
          uiOutput(ns("text2"))
        )
      )),
      column(9, box(
        #title = "Radar Charts", 
        solidHeader = TRUE, 
        status = "primary",
        width = 12,
        style = "background-color: #f9f9f9; padding: 10px; border: 2px solid #dc143c; border-radius: 10px;",
        fluidRow(
          column(4, 
                 plotOutput(ns('radarChartKasia')) %>%
                   shinycssloaders::withSpinner(color = "#158cba", type=6),
                 uiOutput(ns('userKasia'))  
          ),
          column(4, 
                 plotOutput(ns('radarChartZuzia')) %>%
                   shinycssloaders::withSpinner(color = "#158cba", type=6),
                 uiOutput(ns('userZuzia'))  
          ),
          column(4, 
                 plotOutput(ns('radarChartMilosz')) %>%
                   shinycssloaders::withSpinner(color = "#158cba", type=6),
                 uiOutput(ns('userMilosz'))  
          )
        )
      ))
    ),
    fluidRow(style = "height: 80px;")
  )
}

# funkcje do generowania radaru
generate_hours <- function(start_hour, end_hour) {
  if (start_hour == end_hour) {
    return(start_hour)
  } else if (start_hour< end_hour){
    return(c(start_hour:end_hour))
  }else {
    return (c(start_hour:24, 1:end_hour))
  }
}
minutes_in_hour <- function(start_hour, start_minute, end_hour, end_minute, hour) {
  if (start_hour == end_hour) {
    if (hour == start_hour) {
      return(end_minute - start_minute)
    } else {
      return(0)
    }
  } else if (hour == start_hour) {
    return(60 - start_minute)
  } else if (hour == end_hour) {
    return(end_minute)
  } else {
    return(60)
  }
}

activitiesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$text1<-renderText("text1")
    
    selected_df <- reactive({
      combined_data <- bind_rows(
        milosz_activities_df %>% mutate(user = "Milosz"),
        kasia_activities_df %>% mutate(user = "Kasia"),
        zuzia_activities_df %>% mutate(user = "Zuzia")
      )
      
      combined_data <- combined_data %>%
        mutate(
          time.started = as.POSIXct(time.started, format = "%Y-%m-%d %H:%M:%S"),
          time.ended = as.POSIXct(time.ended, format = "%Y-%m-%d %H:%M:%S")
        )
      
      filtered_data <- combined_data %>%
        filter(
          as.Date(time.started) <= as.Date(input$selected_date) &
            as.Date(time.ended) >= as.Date(input$selected_date)
        )
      
      # Funkcja podziału aktywności na dwa wiersze (jeśli przekraczają północ)
      split_night_activities <- function(df) {
        df <- df %>%
          rowwise() %>%
          mutate(
            split_needed = as.Date(time.started) != as.Date(time.ended)
          ) %>%
          ungroup()
        
        split_df <- df %>%
          filter(split_needed) %>%
          mutate(
            time.ended = as.POSIXct(paste0(as.Date(time.started), " 23:59:59")),
            duration.minutes = as.numeric(difftime(time.ended, time.started, units = "mins"))
          ) %>%
          bind_rows(
            df %>%
              filter(split_needed) %>%
              mutate(
                time.started = as.POSIXct(paste0(as.Date(time.ended), " 00:00:00")),
                duration.minutes = as.numeric(difftime(time.ended, time.started, units = "mins"))
              )
          )
        
        df <- df %>%
          filter(!split_needed) %>%
          bind_rows(split_df) %>%
          arrange(time.started)
        
        return(df)
      }
      
      updated_data <- split_night_activities(filtered_data)
      
      activity_order <- c(
        "Code", "Relax", "Shopping", "Walk", "Exercise", 
        "Cooking", "MINI", "Commute", "Youtube", "Eat", 
        "Chores", "Sleep", "Learn"
      )
      
      updated_data <- updated_data %>%
        mutate(
          activity.name = factor(activity.name, levels = activity_order)
        ) %>%
        arrange(activity.name, time.started) # Sortowanie według aktywności i czasu rozpoczęcia
      
      return(updated_data)
    })
    
    output$timeline <- renderPlotly({
      df <- selected_df()
      req(df, nrow(df) > 0)
      
      # Unikalne aktywności
      activities <- unique(df$activity.name)
      
      # Mapowanie aktywności na osie Y
      df$y_pos <- match(df$activity.name, activities)
      
      df$user_pos <- as.numeric(factor(df$user))  
      user_colors <- c("Milosz" = '#158cba',
                       "Kasia" = "#ff7f0e",
                       "Zuzia" = "#dc143c")
      plot_ly() %>%
        layout(
          showlegend = TRUE,
          xaxis = list(
            title = "",
            type = "date",
            tickformat = "%H:%M",
            range = list(
              as.POSIXct(paste(input$selected_date, "00:00:00")),
              as.POSIXct(paste(input$selected_date, "23:59:59"))
            ),
            automargin = TRUE  
          ),
          yaxis = list(
            title = "",
            ticktext = activities,
            tickvals = seq_along(activities),
            range = c(0.5, length(activities) + 0.5)
          ),
          paper_bgcolor = 'rgba(0, 0, 0, 0)',
          plot_bgcolor = 'rgba(0, 0, 0, 0)'
        )%>%
        add_segments(
          data = df,
          x = ~time.started,
          xend = ~time.ended,
          y = ~y_pos + 0.2 * (df$user_pos - 1 ),  
          yend = ~y_pos + 0.2 * (df$user_pos - 1 ),  
          color = ~factor(user),  
          colors = user_colors,
          hoverinfo = "text",
          text = ~paste(
            "User:", user,
            "<br>Activity:", activity.name,
            "<br>Start:", format(time.started, "%H:%M"),
            "<br>End:", format(time.ended, "%H:%M"),
            "<br>Duration:", round(duration.minutes), "min"
          ),
          line = list(width = 6)
        ) %>%
        config(displayModeBar = FALSE)
    })

    # Funkcja pomocnicza dla wykresów radarowych
    generate_radar <- function(title, name_choice) {
      activities_df$start.hour = as.numeric(format(activities_df$time.started,"%H"))
      activities_df$start.minute = as.numeric(format(activities_df$time.started, "%M"))
      activities_df$start.hour[activities_df$start.hour == 0] <- 24
      activities_df$end.hour = as.numeric(format(activities_df$time.ended,"%H"))
      activities_df$end.minute = as.numeric(format(activities_df$time.ended, "%M"))
      activities_df$end.hour[activities_df$end.hour == 0] <- 24
      
      filtered_data <- activities_df %>%
        filter(activity.name == input$activityChoice,
               name == name_choice)
      
      hourly_data <- filtered_data %>%
        rowwise() %>%
        mutate(hours = list(generate_hours(start.hour, end.hour))) %>%
        unnest(cols = c(hours)) %>%
        group_by(hours) %>%
        mutate(minutes = mapply(minutes_in_hour, start.hour, start.minute, end.hour, end.minute, hours)) %>%
        group_by(hours) %>%
        summarise(total_duration_minutes = sum(minutes, na.rm = TRUE)) %>%
        rename(start.hour = hours)
      
      hourly_data <- left_join(data.frame(start.hour = 1:24), hourly_data, by = 'start.hour') %>%
        arrange(start.hour) %>%
        mutate(total_duration_minutes = ifelse(is.na(total_duration_minutes), 0, total_duration_minutes))
      
      radar_data <- as.data.frame(t(hourly_data$total_duration_minutes))
      colnames(radar_data) <- paste0(hourly_data$start.hour, ":00")
      
      radar_data <- radar_data[, rev(seq_len(ncol(radar_data)))]
      radar_data <- rbind(rep(max(radar_data, na.rm = TRUE), ncol(radar_data)),
                          rep(0, ncol(radar_data)),
                          radar_data)
      
      labels <- ifelse(as.numeric(sub(":.*", "", colnames(radar_data))) %% 2 == 0,
                       colnames(radar_data), "")
      radarchart(radar_data,
                 pcol = "#dc143c",  # Czerwone obramowanie
                 pfcol = rgb(0.85, 0.05, 0.05, 0.5),  plwd = 2,
                 cglty = 1, axislabcol = "grey",
                 cglcol = 'grey', vlcex = 0.8, title = '',
                 pty = 32, vlabels = labels)
      # text(x = 0, y = - 1.30, labels = name_choice, col = "black", cex = 1.5, font = 1)
    }
    
    # Wykresy radarowe
    output$radarChartKasia <- renderPlot({
      generate_radar(input$activityInput, "Kasia")
    })
    
    output$radarChartZuzia <- renderPlot({
      generate_radar(input$activityInput, "Zuzia")
    })
    
    output$radarChartMilosz <- renderPlot({
      generate_radar(input$activityInput, "Milosz")
    })
    
    # Tekst użytkowników
    output$userKasia <- renderUI({
      tagList(
        div(
          style = "font-size: 22px; font-weight: bold; text-align: center; margin-top: 10px;",
          "Kasia"
        )
      )
    })
    
    output$userZuzia <- renderUI({
      tagList(
        div(
          style = "font-size: 22px; font-weight: bold; text-align: center; margin-top: 10px;",
          "Zuzia"
        )
      )
    })
    
    output$userMilosz <- renderUI({
      tagList(
        div(
          style = "font-size: 22px; font-weight: bold; text-align: center; margin-top: 10px;",
          "Milosz"
        )
      )
    })
    
    #Tekst 1
    output$text1 <- renderUI({
      "This timeline showcases the activities all three of us engaged in throughout the chosen day, offering the opportunity to compare how differently we spent the day or which activities we had in common."
    })   
    
    #Tekst 2
    output$text2 <- renderUI({
      "These three radar charts illustrate the distribution of a selected activity, allowing you to see when each of us engages in it most frequently. They also offer the opportunity to compare our daily routines, providing insight into the differences between the three users."
    })   
    
    output$avg_duration2<-renderText({
      paste("<span style='white-space: nowrap;'>Average Duration </span>")
    })
    output$avg_duration1<-renderText({
      corrected_data <- activities_df %>%
        filter(activity.name == input$activityChoice) %>%
        mutate(
          start_hour = as.numeric(format(time.started, "%H")) + as.numeric(format(time.started, "%M")) / 60,
          end_hour = as.numeric(format(time.ended, "%H")) + as.numeric(format(time.ended, "%M")) / 60,
          end_hour = ifelse(end_hour < start_hour, end_hour + 24, end_hour),
          day = as.Date(time.started)
        ) %>%
        group_by(name, day) %>%
        summarise(total_duration = sum(duration.minutes), .groups = 'drop')
      
      # Obliczamy średnią
      avg_duration <- mean(corrected_data$total_duration, na.rm = TRUE)
      paste0(round(avg_duration, 2), " minutes")
    })
    output$max_duration1<-renderText({
      corrected_data <- activities_df %>%
        filter(activity.name == input$activityChoice) %>%
        mutate(
          start_hour = as.numeric(format(time.started, "%H")) + as.numeric(format(time.started, "%M")) / 60,
          end_hour = as.numeric(format(time.ended, "%H")) + as.numeric(format(time.ended, "%M")) / 60,
          end_hour = ifelse(end_hour < start_hour, end_hour + 24, end_hour),
          day = as.Date(time.started)
        ) %>%
        group_by(name, day) %>%
        summarise(total_duration = sum(duration.minutes), .groups = 'drop')
      
      # Wybieramy wiersz z maksymalnym czasem
      max_row <- corrected_data %>% filter(total_duration == max(total_duration)) %>% slice(1)
      paste0(round(max_row$total_duration, 2), " minutes")
    })
    output$max_duration2<-renderText({
      corrected_data <- activities_df %>%
        filter(activity.name == input$activityChoice) %>%
        mutate(
          start_hour = as.numeric(format(time.started, "%H")) + as.numeric(format(time.started, "%M")) / 60,
          end_hour = as.numeric(format(time.ended, "%H")) + as.numeric(format(time.ended, "%M")) / 60,
          end_hour = ifelse(end_hour < start_hour, end_hour + 24, end_hour),
          day = as.Date(time.started)
        ) %>%
        group_by(name, day) %>%
        summarise(total_duration = sum(duration.minutes), .groups = 'drop')
      
      # Wiersz z maksymalnym czasem
      max_row <- corrected_data %>% filter(total_duration == max(total_duration)) %>% slice(1)
      paste("<span style='white-space: nowrap;'>Longest Duration by <strong>", max_row$name, "</strong></span>")
      
    })
    output$min_duration1<-renderText({
      corrected_data <- activities_df %>%
        filter(activity.name == input$activityChoice) %>%
        mutate(
          start_hour = as.numeric(format(time.started, "%H")) + as.numeric(format(time.started, "%M")) / 60,
          end_hour = as.numeric(format(time.ended, "%H")) + as.numeric(format(time.ended, "%M")) / 60,
          end_hour = ifelse(end_hour < start_hour, end_hour + 24, end_hour),
          day = as.Date(time.started)
        ) %>%
        group_by(name, day) %>%
        summarise(total_duration = sum(duration.minutes), .groups = 'drop')
      
      # Wiersz z minimalnym czasem
      min_row <- corrected_data %>% filter(total_duration == min(total_duration)) %>% slice(1)
      paste0(round(min_row$total_duration, 2), " minutes")
    })
    output$min_duration2<-renderText({
      corrected_data <- activities_df %>%
        filter(activity.name == input$activityChoice) %>%
        mutate(
          start_hour = as.numeric(format(time.started, "%H")) + as.numeric(format(time.started, "%M")) / 60,
          end_hour = as.numeric(format(time.ended, "%H")) + as.numeric(format(time.ended, "%M")) / 60,
          end_hour = ifelse(end_hour < start_hour, end_hour + 24, end_hour),
          day = as.Date(time.started)
        ) %>%
        group_by(name, day) %>%
        summarise(total_duration = sum(duration.minutes), .groups = 'drop')
      
      # Wiersz z minimalnym czasem
      min_row <- corrected_data %>% filter(total_duration == min(total_duration)) %>% slice(1)
      paste("<span style='white-space: nowrap;'>Shortest Duration by <strong>", min_row$name, "</strong></span>")
      
    })
    
  })
  
}