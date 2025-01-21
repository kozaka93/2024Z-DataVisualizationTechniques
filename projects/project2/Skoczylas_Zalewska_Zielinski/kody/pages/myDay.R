myDayUI <- function(id) {
  
  ns <- NS(id)
  
  # Ścieżka do grafiki telefonu
  svg_image4 <- "images/phone.svg"
  svg_data4 <- base64enc::dataURI(file = svg_image4, mime = "image/svg+xml")
  
  tags$div(
    class = "container-fluid",
    
    tags$style(HTML("
      body {
        background-color: #F2F7F7; 
      }
    ")),
    
    box(
      title = "",
      solidHeader = TRUE,       
      width = 12,                       
      style = "background-color: #158cba	; border: 2px solid #158cba; border-radius: 10px; padding: 10px ;color: white;", 
      fluidRow(
        column(3,                       
               selectInput(
                 ns("personChoice"),
                 label = tagList(tags$b("Choose a person")), 
                 choices = c("Kasia", "Zuzia", "Milosz"),
                 selected = "Kasia"
               )
        ),
        column(3,                      
               dateInput(
                 ns("dateChoice"),
                 label = tagList(tags$b("Choose date")),
                 value = min(apps_df$date),
                 min = min(apps_df$date),
                 max = max(apps_df$date),
                 format = "yyyy-mm-dd"
               )
        )
      )),
    
    fluidRow(style = "height: 40px;"),
    fluidRow(
      column(3, 
             box(
               title = tags$span(
                 "How do I spend my day?", 
                 style = "background-color: #ff7f0e; color: white; padding: 10px; border-radius: 10px; width: 100%; display: inline-block; box-sizing: border-box;"
               ),
               solidHeader = TRUE, 
               width = 12,
               style = "background-color: #f9f9f9; border: 2px solid #ff7f0e; border-radius: 10px; padding: 10px;",  # Pomarańczowa obramówka
               tags$div(
                 style = "font-size: 16px; color: black;",
                 uiOutput(ns("text1"))
               )
             )
      ),
      column(9, 
             box(
               title = "", 
               solidHeader = TRUE,                     
               status = "primary", 
               width = 12,
               style = "background-color: #f9f9f9; border: 2px solid #ff7f0e; border-radius: 10px; padding: 0px; display: inline-block; vertical-align: top;", 
               tags$div(
                 style = "display: flex; justify-content: center; align-items: center; height: 100%;",
                 plotlyOutput(ns("barPlot"), height = "100%", width = "1100px")%>%
                   shinycssloaders::withSpinner(color = "#158cba", type=6)
               )
             )
      )
    )
    
    ,
    fluidRow(style = "height: 80px;"),
    fluidRow(      
      column(3, 
             box(
               title = tags$span(
                 "How do I use my phone?", 
                 style = "background-color: #dc143c; color: white; padding: 10px; border-radius: 10px; width: 100%; display: inline-block; box-sizing: border-box;"
               ),
               solidHeader = TRUE, 
               width = 12,
               style = "background-color: #f9f9f9; border: 2px solid #dc143c; border-radius: 10px; padding: 10px;",
               tags$div(
                 style = "font-size: 16px; color: black;",
                 textOutput(ns("text2"))
               )
             )
      ),      column(5, box( 
        title = "", 
        solidHeader = TRUE,                     
        status = "primary",                     
        width = 12,                              
        style = "background-color: #f9f9f9; border: 2px solid #dc143c; border-radius: 10px; padding: 0px 0px 0px 0px;", 
        tags$div(
          plotlyOutput(ns("appsPlotPie"))%>%
            shinycssloaders::withSpinner(color = "#158cba", type=6)
        )) 
        
      ),
      column(4,   tags$div(
        class = "col-2",
        style = "position: relative; width: 100%; max-width: 500px; margin: 0 auto;padding: 0px;", 
        tags$img(
          id = ns("homeImage4"),
          src = svg_data4,
          style = "width: 110%; height: auto;padding: 0px;",
          alt = "Image 4"
        ),
        tags$div(
          uiOutput(ns("stats")),
          style = "position: absolute; top: 50%; left: 85%; transform: translate(-50%, -50%); 
             font-size: 1vw; color: black; width: 90%;padding: 0px;" 
        )
      ))
    ),
    fluidRow(style = "height: 40px;")
  )
}

myDayServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    output$text1 <- renderUI({
      HTML("The plot on the right provides an overview of the activities performed during the selected day by one of us, showing the amount of time dedicated to each task.")
    })
    
    output$text2 <- renderText({
      "This pie chart illustrates apps used on the selected day, along with the time spent on each app. Additionally, on the right there is a summary of the phone usage for the chosen user."
    })    
    
    
    output$stats<-renderUI({
      df_zuzia<-process_daily_data2(zuzia_apps_df1,"Zuzia")
      df_kasia<-process_daily_data2(kasia_apps_df1,"Kasia")
      df_milosz<-process_daily_data2(milosz_apps_df1,"Milosz")
      stats<-rbind(df_milosz,df_kasia,df_zuzia)
      user_data <- stats %>% filter(user == input$personChoice)
      
      avg_time <- user_data %>%
        filter(application == "Total time") %>%
        summarise(avg = mean(value, na.rm = TRUE)) %>%
        pull(avg)
      
      min_time <- user_data %>%
        filter(application == "Total time") %>%
        summarise(min = min(value, na.rm = TRUE)) %>%
        pull(min)
      
      max_time <- user_data %>%
        filter(application == "Total time") %>%
        summarise(max = max(value, na.rm = TRUE)) %>%
        pull(max)
      
      avg_notifications <- user_data %>%
        filter(application == "Notifications") %>%
        summarise(avg_notifications = mean(value, na.rm = TRUE)) %>%
        pull(avg_notifications)
      
      avg_unlocks <- user_data %>%
        filter(application == "Unlocked") %>%
        summarise(avg_unlocks = mean(value, na.rm = TRUE)) %>%
        pull(avg_unlocks)
      stats_text <- paste(
        "<h3 style='font-size: 18px;'>Daily Phone Wrapped","<br>"," for","<b>", input$personChoice, "</b>", "</h3>",
        "<p><b>Average time:</b> ", round(avg_time, 0), " min</p>",
        "<p><b>Minimum time:</b> ", round(min_time, 0), " min</p>",
        "<p><b>Maximum time:</b> ", round(max_time, 0), " min</p>",
        "<p><b>Notifications:</b> ", round(avg_notifications, 0), "</p>",
        "<p><b>Unlocks:</b> ", round(avg_unlocks, 0), "</p>"
      )
      HTML(paste0(
        "<div style='background-color: #158cba; padding: 10px; border-radius: 20px 20px 20px 20px; display: inline-block; color: white; position: relative;'>",
        stats_text,
        "<div style='position: absolute; bottom: 0; left: -7px; width: 0; height: 0; border-left: 15px solid transparent; border-right: 15px solid transparent; border-bottom: 15px solid #158cba;'></div>",  # Trójkątny wyciągnięty róg w lewo z poziomą krawędzią na dole
        "</div>"
      ))
    })
    
    # plotpie
    output$appsPlotPie <- renderPlotly({
      plot_data <- apps_df %>%
        filter(user == input$personChoice, date == input$dateChoice) %>%
        filter(application != "Total time")
      
      total_time <- sum(plot_data$time_spent, na.rm = TRUE)
      
      plot_data <- plot_data %>%
        mutate(percentage = (time_spent / total_time) * 100)
      
      plot_data <- plot_data %>%
        mutate(application = ifelse(percentage < 2, "Other", as.character(application)))
      
      plot_data_aggregated <- plot_data %>%
        group_by(application) %>%
        summarise(time_spent = sum(time_spent)) %>%
        ungroup()
      
      app_colors <- c(
        "Messenger" = "#158cba",  
        "YouTube" = "#dc143c",    
        "WhatsApp" = "#ff7f0e",   
        "Duolingo" = "#FF4545",   
        "X" = "#8E1616",        
        "Google" = "#FFB200",     
        "Mobile USOS PW" = "#FF6347",    
        "Spotify" = "#2E8B57",    
        "Facebook" = "#0B8494",  
        "Teams" = "#640D5F",
        "Mapy" = "#FFD65A",
        "Simple Time Tracker" = "#577D86",
        "Phone" = "#125B9A",
        "Other" = "#D3D3D3",
        "Rest" = "#F9E6CF"
      )
      
      plot_data_aggregated$color <- sapply(plot_data_aggregated$application, function(app) {
        if (app %in% names(app_colors)) {
          return(app_colors[[app]])
        } else {
          return(app_colors[["Rest"]])  
        }
      })
      
      plot_pie <- plot_ly(plot_data_aggregated, labels = ~application, values = ~time_spent, type = 'pie',
                          hovertemplate = paste(
                            "Application: %{label}<br>",
                            "Time spent: %{value} minutes<br>",
                            "Percentage: %{percent}<extra></extra>"  # <extra></extra> usuwa dodatkowe informacje o śladzie
                          ),
                          marker = list(colors = plot_data_aggregated$color)) %>%
        #marker = list(colors = RColorBrewer::brewer.pal(length(unique(plot_data_aggregated$application)), "Set3"))) %>%
        layout(
          showlegend = TRUE,
          paper_bgcolor = "rgba(0, 0, 0, 0)", 
          plot_bgcolor = "rgba(0, 0, 0, 0)") %>% 
        config(displayModeBar=FALSE)
      
      return(plot_pie)
      
    })
    
    # Wykres słupkowy
    output$barPlot <- renderPlotly({
      user_data <- activities_df %>% 
        filter(name == input$personChoice)
      
      split_activity_by_day <- function(data) {
        data %>%
          rowwise() %>%
          do({
            start <- .$time.started
            end <- .$time.ended
            activity <- .$activity.name
            
            split_times <- list()
            
            while (start < end) {
              day_end <- as.POSIXct(paste0(as.Date(start), " 23:59:59"), tz = "UTC")
              next_day_start <- as.POSIXct(paste0(as.Date(start) + 1, " 00:00:00"), tz = "UTC")
              
              if (end <= day_end) {
                split_times[[length(split_times) + 1]] <- data.frame(
                  date = as.Date(start),
                  activity.name = activity,
                  duration.minutes = as.numeric(difftime(end, start, units = "mins"))
                )
                break
              } else {
                split_times[[length(split_times) + 1]] <- data.frame(
                  date = as.Date(start),
                  activity.name = activity,
                  duration.minutes = as.numeric(difftime(day_end, start, units = "mins")) + 1
                )
                start <- next_day_start
              }
            }
            
            bind_rows(split_times)
          }) %>%
          ungroup()
      }
      
      split_data <- split_activity_by_day(user_data)
      
      activity_summary <- split_data %>%
        filter(date == input$dateChoice) %>%
        group_by(activity.name) %>%
        summarise(total_minutes = sum(duration.minutes, na.rm = TRUE)) %>%
        ungroup()
      
      all_activities <- unique(user_data$activity.name)
      activity_summary <- data.frame(activity.name = all_activities) %>%
        left_join(activity_summary, by = "activity.name") %>%
        mutate(total_minutes = ifelse(is.na(total_minutes), 0, total_minutes))
      
      plot_ly(activity_summary,
              x = ~activity.name,
              y = ~round(total_minutes),
              type = 'bar',
              name = 'Czas aktywności',
              marker = list(color = "#ff7f0e"),
              hoverinfo = 'text',
              text = ~paste('Activity: ', activity.name, 
                            '<br>Time : ', round(total_minutes),' min'),
              textposition = 'none') %>%
        layout(xaxis = list(title = 'Activity', tickangle = -45),
               yaxis = list(title = 'Time [min]'),
               bargap = 0.5,  # Mniejszy odstęp między słupkami
               paper_bgcolor = 'rgba(0, 0, 0, 0)',
               plot_bgcolor = 'rgba(0, 0, 0, 0)') %>% 
        config(displayModeBar = FALSE)
    })
    
    
  })
}