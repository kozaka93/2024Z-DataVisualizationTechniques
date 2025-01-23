#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(warn = -1)
options(shiny.suppressAllPackageMessages = TRUE)
suppressMessages({
  library(dplyr)
})
library(shiny)
library(rjson)
library(lubridate)
library(ggplot2)
library(leaflet)
library(tidyr)
library(bslib)
library(shinythemes)
library(shinymaterial)
library(scales)
library(fmsb)
library(rlang)


lok_joz <- fromJSON(file = "../data/Os_czasu_jo.json")
lok_mic <- fromJSON(file = "../data/Os_czasu_mi.json")
lok_kla <- fromJSON(file = "../data/Os_czasu_kl.json")

# read merged df
merged_df <- read.csv("../data/merged_data.csv")
lok_names <- read.csv("../data/lok_names.csv")

Sys.setlocale("LC_TIME", "C")


transport <- function(x){
  col_names <- c("activity", "activity_type","distance", "startTime", "endTime", "weekNum")
  
  
  result <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(result) <- col_names
  
  counter = 1
  for(i in 1:length(x)){
    if(!is.null(x[[i]]$activity)){
      
      result <- rbind(result, data.frame(activity = x[[i]]$activity$topCandidate$type, 
                                         activity_type = "transport",
                                         distance = x[[i]]$activity$distanceMeters,
                                         startTime = x[[i]]$startTime, endTime = x[[i]]$endTime,
                                         weekNum = ceiling(as.numeric(difftime(as.POSIXct(substr(x[[i]]$startTime, 1, 10), format = "%Y-%m-%d"),
                                                                               as.POSIXct("2024-12-08", format = "%Y-%m-%d"), units = "weeks")))))
      counter <- counter + 1
    }
  }
  result
}

miejsca <- function(x){
  
  col_names <- c("place", "activity_type", "startTime", "endTime", "weekNum")
  
  
  result <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(result) <- col_names
  
  counter = 1
  for(i in 1:length(x)){
    if(!is.null(x[[i]]$visit)){
      
      
      day_diff <- ceiling(as.numeric(difftime(as.POSIXct(substr(x[[i]]$endTime, 1, 10), format = "%Y-%m-%d"),
                                              as.POSIXct(substr(x[[i]]$startTime, 1, 10), format = "%Y-%m-%d"), units = "days"))) + 1
      
      
      for(j in 1:day_diff){
        result <- rbind(result, data.frame(place = ifelse(sum(lok_names$placeID == x[[i]]$visit$topCandidate$placeID) == 1,lok_names[lok_names$placeID == x[[i]]$visit$topCandidate$placeID,"name"], "other"),
                                           activity_type = ifelse(sum(lok_names$placeID == x[[i]]$visit$topCandidate$placeID) == 1,lok_names[lok_names$placeID == x[[i]]$visit$topCandidate$placeID,"category"], "other"),
                                           startTime = case_when(j == 1 ~ substr(x[[i]]$startTime, 1, 19),
                                                                 TRUE ~ paste0(as.character(tail(seq(as.POSIXct(substr(x[[i]]$startTime, 1, 19), format = "%Y-%m-%d"), by= "day", length = j), n = 1)), "T00:00:00")), 
                                           endTime = case_when(j == day_diff ~ substr(x[[i]]$endTime, 1, 19),
                                                               j == 1 ~ paste0(as.character(as.POSIXct(substr(x[[i]]$startTime, 1, 19), format = "%Y-%m-%d")), "T23:59:59"),
                                                               TRUE ~ paste0(as.character(tail(seq(as.POSIXct(substr(x[[i]]$startTime, 1, 19), format = "%Y-%m-%d"), by= "day", length = j), n = 1)), "T23:59:59")),
                                           weekNum = ceiling(as.numeric(difftime(as.POSIXct(substr(x[[i]]$startTime, 1, 10), format = "%Y-%m-%d"),
                                                                                 as.POSIXct("2024-12-08", format = "%Y-%m-%d"), units = "weeks")))))
      }
      counter <- counter + 1
    }
  }
  result
}
{
  podroze_joz <- transport(lok_joz) %>% 
    mutate(timeDurSec = as.integer(difftime(as.POSIXct(substr(endTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"),
                                            as.POSIXct(substr(startTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"), #
                                            units = "secs"))) %>% 
    mutate(day = as.POSIXct(substr(endTime, 1, 10), format = "%Y-%m-%d")) %>% 
    mutate(person = "Józef")
  
  wizyty_joz <- miejsca(lok_joz) %>% 
    
    mutate(timeDurSec = as.integer(difftime(as.POSIXct(substr(endTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"),
                                            as.POSIXct(substr(startTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"), #
                                            units = "secs")))%>% 
    group_by(place) %>% 
    mutate(sumTime = sum(timeDurSec)) %>% 
    mutate(day = as.POSIXct(substr(endTime, 1, 10), format = "%Y-%m-%d")) %>% 
    mutate(person = "Józef")
  
  
  
  podroze_mic <- transport(lok_mic) %>% 
    mutate(timeDurSec = as.integer(difftime(as.POSIXct(substr(endTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"),
                                            as.POSIXct(substr(startTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"), #
                                            units = "secs"))) %>% 
    mutate(day = as.POSIXct(substr(endTime, 1, 10), format = "%Y-%m-%d")) %>% 
    mutate(person = "Michał")
  
  wizyty_mic <- miejsca(lok_mic) %>% 
    
    mutate(timeDurSec = as.integer(difftime(as.POSIXct(substr(endTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"),
                                            as.POSIXct(substr(startTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"), #
                                            units = "secs")))%>% 
    group_by(place) %>% 
    mutate(sumTime = sum(timeDurSec)) %>% 
    mutate(day = as.POSIXct(substr(endTime, 1, 10), format = "%Y-%m-%d")) %>% 
    mutate(person = "Michał")
  
  
  
  podroze_kla <- transport(lok_kla) %>% 
    mutate(timeDurSec = as.integer(difftime(as.POSIXct(substr(endTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"),
                                            as.POSIXct(substr(startTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"), #
                                            units = "secs"))) %>% 
    mutate(day = as.POSIXct(substr(endTime, 1, 10), format = "%Y-%m-%d")) %>% 
    mutate(person = "Klaudia")
  
  wizyty_kla <- miejsca(lok_kla) %>% 
    
    mutate(timeDurSec = as.integer(difftime(as.POSIXct(substr(endTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"),
                                            as.POSIXct(substr(startTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"), #
                                            units = "secs")))%>% 
    group_by(place) %>% 
    mutate(sumTime = sum(timeDurSec)) %>% 
    mutate(day = as.POSIXct(substr(endTime, 1, 10), format = "%Y-%m-%d")) %>% 
    mutate(person = "Klaudia")
  
}


######################################################################UI######################################################################

ui <- fluidPage(
  
  theme = bs_theme(
    version = 5,            
    bootswatch = "zephyr",   
    primary = "#4285F4",    
    secondary = "#0f9d58", 
    success = "#FBBC05", 
    warning = "#E94335", 
    base_font = font_google("Product Sans"),  
    heading_font = font_google("Product Sans")  
  ),
  
  tags$style(HTML("
  body {
    background-color: #f5f5f5;
  }
  ")),
  
  # start page
  uiOutput("startPage"),
  
  # app
  uiOutput("mainApp")
)


server <- function(input, output, session) {
  
  values <- reactiveValues(showStartPage = TRUE)
  
  output$startPage <- renderUI({
    if (values$showStartPage) {
      
      tagList(
        leafletOutput("mapBackground", height = "100vh"),
        
        tags$style(HTML("
      
        body {
          background-image: url('http://mapa-google.pl/warszawa/'); 
          background-size: cover;
          background-position: center;
          height: 100vh;
          margin: 0;
          font-family: 'Product Sans', sans-serif;
        }
        
        #person_icon {
        position: absolute;
        top: 10px;
        right: 20px;
        padding: 10px;
        }
        
        ")),
        
        tags$img(id = "person_icon", src = "https://cdn-icons-png.flaticon.com/512/3177/3177440.png", height = "70px", width = "70px"),
        
        div(style = "text-align: center;
        font-size: 100px;
        height: 200px;
        width: 400px;
        position: absolute;
        top: 40%;
        left: 50%;
        transform: translate(-50%, -50%);",
            
            h1(style = "text-align: center; font-size: 100px; font-weight: 1000",
               HTML(
                 paste0(
                   # Google colors to each letter
                   paste0(
                     '<span style="color: #4285F4;">', substr("Map My Moments", 1, 1), '</span>',
                     '<span style="color: #0f9d58;">', substr("Map My Moments", 2, 2), '</span>',
                     '<span style="color: #FBBC05;">', substr("Map My Moments", 3, 3), '</span>',
                     
                     '<span style="color: #EA4335;">', substr("Map My Moments", 4, 4), '</span>',
                     
                     '<span style="color: #4285F4;">', substr("Map My Moments", 5, 5), '</span>',
                     '<span style="color: #0f9d58;">', substr("Map My Moments", 6, 6), '</span>',
                     
                     '<span style="color: #FBBC02;">', substr("Map My Moments", 7, 7), '</span>',
                     
                     '<span style="color: #E94335;">', substr("Map My Moments", 8, 8), '</span>',
                     '<span style="color: #4285F4;">', substr("Map My Moments", 9, 9), '</span>',
                     '<span style="color: #0f9d58;">', substr("Map My Moments", 10, 10), '</span>',
                     '<span style="color: #FBBC05;">', substr("Map My Moments", 11, 11), '</span>',
                     '<span style="color: #E94335;">', substr("Map My Moments", 12, 12), '</span>',
                     '<span style="color: #4285F4;">', substr("Map My Moments", 13, 13), '</span>',
                     '<span style="color: #0f9d58;">', substr("Map My Moments", 14, 14), '</span>'
                   )
                 )
               )
            ),
            
            
            
            actionButton("startBtn", label = NULL, 
                         icon = icon("arrow-right"), 
                         class = "btn-enter",
                         style = "background-color: #4285F4; 
                     color: white; 
                     font-size: 18px; 
                     padding: 15px 30px; 
                     border-radius: 5px; 
                     border: none; 
                     cursor: pointer; 
                     font-weight: 500; 
                     box-shadow: 0 2px 6px rgba(0,0,0,0.2);")
        )
      )
    }
  })
  
  
  output$mainApp <- renderUI({
    if (!values$showStartPage) {
      tagList(
        
        tags$head(
          tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/icon?family=Material+Icons")
        ),
        
        tags$style(HTML("

        #person_icon {
        position: absolute;
        top: 10px;
        right: 20px;
        padding: 10px;
        }
        
        .nav-tabs {
          display: block;
          width: 90px;
          float: left;
          background-color: f1f1f1;
          position: fixed;
          top: 50%;
          transform: translateY(-50%);
        }
        
        .tab-content {
          margin-left: 100px;
          margin-right: 100px
        }
        
        .tab-content-wrapper {
          background-color: #333; 
        }
        
        .fixed-bottom-row {
          position: fixed;
          bottom: 0;
          width: 100%;
          background-color: #f8f9fa;
          padding: 10px 20px;
          box-shadow: 0 -2px 5px rgba(0, 0, 0, 0.1);
          z-index: 1000;
        }
        
        .ggtitle, .xlab, .ylab, .axis.text, .legend.text, .legend.title {
      font-family: 'Arial', sans-serif;

        }

        ")),
        

        tags$img(id = "person_icon", src = "https://cdn-icons-png.flaticon.com/512/3177/3177440.png", height = "70px", width = "70px"),
        
 
        titlePanel(
          
          h1(style = "font-size: 60px; font-weight: 1000; padding: 10px",
             
             HTML(
               
               paste0(
                 
                 
                 paste0(
                   '<span style="color: #4285F4;">', substr("Map My Moments", 1, 1), '</span>',
                   '<span style="color: #0f9d58;">', substr("Map My Moments", 2, 2), '</span>',
                   '<span style="color: #FBBC05;">', substr("Map My Moments", 3, 3), '</span>',
                   
                   '<span style="color: #EA4335;">', substr("Map My Moments", 4, 4), '</span>',
                   
                   '<span style="color: #4285F4;">', substr("Map My Moments", 5, 5), '</span>',
                   '<span style="color: #0f9d58;">', substr("Map My Moments", 6, 6), '</span>',
                   
                   '<span style="color: #FBBC02;">', substr("Map My Moments", 7, 7), '</span>',
                   
                   '<span style="color: #E94335;">', substr("Map My Moments", 8, 8), '</span>',
                   '<span style="color: #4285F4;">', substr("Map My Moments", 9, 9), '</span>',
                   '<span style="color: #0f9d58;">', substr("Map My Moments", 10, 10), '</span>',
                   '<span style="color: #FBBC05;">', substr("Map My Moments", 11, 11), '</span>',
                   '<span style="color: #E94335;">', substr("Map My Moments", 12, 12), '</span>',
                   '<span style="color: #4285F4;">', substr("Map My Moments", 13, 13), '</span>',
                   '<span style="color: #0f9d58;">', substr("Map My Moments", 14, 14), '</span>'
                 )
                 
               )
               
             )
          )
          
        ),
        
        tabsetPanel(
          tags$style(HTML("
          .tab-content {
            width: 100% !important;
          }
          .tab-pane {
            width: 100% !important; 
          }
        ")),
           tags$style(HTML("
            .shiny-input-container {
              position: relative;
            }
            .shiny-input-container .selectize-dropdown {
              position: absolute !important;
              bottom: 100% !important;
              top: auto !important;
              margin-bottom: 5px;
            }
          ")),
          type = "tabs",
          
          tabPanel(
            title = tags$img(src = "https://cdn-icons-png.flaticon.com/128/2773/2773319.png", height = "40px", width = "40px"),
            
            div(
              class = "fixed-bottom-row",
              style = "display: flex; gap: 20px; height: 120px",
              sliderInput("sliderWeek1", "Select weeks for the plot:",
                          min = 1, max = 6, value = c(1, 2), step = 1),
              selectInput("dropdown1", "Choose the person:",
                          choices = c("Józef", "Michał", "Klaudia"))
            ),

            mainPanel(

              style = "flex-grow: 1; width: 90%; padding: 20px; height: 120px;",
              div(
                style = "display: flex; flex-direction: column; gap: 40px; align-items: stretch; width: 100%;",
                div(
                  style = "text-align: center; width: 100%; padding: 50;",
                  textOutput("weeklyActivitiesText1")
                ),
                div(
                  style = "width: 100%; padding: 50;",
                  plotOutput("dailyActivitiesPlot", width = "100%", height = "400px") 
                )
              )
            )
          ),

          tabPanel(
            title = tags$img(src = "https://cdn-icons-png.flaticon.com/128/1034/1034795.png", height = "40px", width = "40px"),
            
            div(
              class = "fixed-bottom-row",
              style = "display: flex; gap: 20px; height: 120px",
              selectInput(
                inputId = "People",
                label = "Select people:",
                choices = c("Józef", "Klaudia", "Michał"),
                selected = c("Klaudia", "Michał"),
                multiple = TRUE
              ),
              sliderInput("sliderWeekTransport", "Select weeks for the plot:",
                          min = 1, max = 6, value = c(1, 2), step = 1)
            ),

            mainPanel(
              style = "flex-grow: 1; width: 90%; padding: 20px; height: 1200px",
              fluidRow(
                
                column(
                  width = 4,
                  plotOutput("spiderPlot")
                ),

                
                column(
                  width = 8,
                  plotOutput("dotPlot"),
                  plotOutput("transportTimePlot"),
                  textOutput("transport_text")
                )
                
                
              )
            )
            
            
            
          ),
          
          
          

          tabPanel(
            title = tags$img(src = "https://cdn-icons-png.flaticon.com/128/7552/7552703.png", height = "40px", width = "40px"),
            
            div(
              class = "fixed-bottom-row",
              style = "display: flex; gap: 20px; height: 120px",
              selectInput(
                inputId = "People2",
                label = "Select people:",
                choices = c("Józef", "Klaudia", "Michał"),
                selected = c("Klaudia", "Michał"),
                multiple = TRUE
              ),
              selectInput(
                inputId = "TransportType",
                label = "Select transport type:",
                choices = c("cycling", "in car", "in subway", "in tram", "walking", "in train"),
                selected = "walking"
              )
            ),
            
            mainPanel(
              style = "flex-grow: 1; width: 90%; padding: 20px; height: 1700px",
              div(
                style = "display: flex; flex-direction: column; align-items: center; gap: 30px; margin-bottom: 50px;",
                div(
                  style = "flex: 0 1 auto; text-align: center; padding-bottom: 20px;",
                  textOutput("transportSpeedText1")
                ),
                div(
                  style = "flex: 1; width: 100%;",
                  plotOutput("transportSpeedBoxPlot")
                )
              ),
              div(
                style = "display: flex; align-items: center; width: 100%; margin-bottom: 50px;",
                div(
                  style = "width: 50%;",
                  plotOutput("transportSpeedBoxPlot2")
                ),
                div(
                  style = "margin-left: 60px;",
                  textOutput("transportSpeedText2")
                )
              ),
              div(
                style = "display: flex; align-items: flex-start; gap: 20px;",
                div(
                  style = "flex: 0 1 200px; display: flex; flex-direction: column; gap: 10px; width: 40%;",
                  selectInput(
                    inputId = "selectedPerson2",
                    label = "Select Person:",
                    choices = c("Józef", "Michał", "Klaudia"),
                    selected = "Michał"
                  ),
                  textOutput("transportSpeedText3")
                ),
                div(
                  style = "flex: 2;",
                  plotOutput("transportSpeedHeatmap", height = "500px")
                )
              )
            )
          ),
          
          tabPanel(
            title = tags$img(
              src = "https://cdn-icons-png.flaticon.com/128/854/854878.png",
              height = "40px",
              width = "40px"
            ),
            value = 'mapTab',
            
            fluidRow(
              column(
                12,
                div(
                  style = "position: relative; display: flex; justify-content: left; align-items: left;", 
                  leafletOutput("map", width = "90%", height = "550px"),
                  actionButton(
                    inputId = "infoButton",
                    label = NULL,
                    icon = icon("info-circle"),
                    style = "position: absolute; bottom: 15px; left: 20px; z-index: 1000; background-color: white;"
                  )
                )
              )
            ),
        
            
            div(
              class = "fixed-bottom-row",
              style = "display: flex; gap: 20px; height: 120px;",
              fluidRow(
                column(4,
                       sliderInput(
                         "sliderWeekMap",
                         "Select weeks for the plot:",
                         min = 1, max = 6, value = c(1, 2), step = 1
                       )
                ),
                column(4,
                       selectInput(
                         inputId = "PeopleMap",
                         label = "Select people:",
                         choices = c("Józef", "Klaudia", "Michał"),
                         selected = c("Józef", "Klaudia", "Michał"),
                         multiple = TRUE
                       )
                ),
                column(4,
                       sliderInput(
                         inputId = "topPlacesCountMap",
                         label = "Select number of top places to display:",
                         min = 1, max = 20, value = 5, step = 1
                       )
                )
              )
            )
          )
          
          
        )

      )
    }
  })
  
  
  
  
  observeEvent(input$startBtn, {
    values$showStartPage <- FALSE
    shinyjs::show("loading")
    
    Sys.sleep(1)
    
    shinyjs::hide("loading")
    
  })
  
  
  
  
  
  ######################### Rodzaj Transportu ##################################
  
  person_colors <- c(
    "Klaudia" = "#1ea362",
    "Michał"  = "#4a89f3",
    "Józef"   = "#dd4b3e"
  )
  
  #########################
  #  Dot Plot 
  #########################
  output$dotPlot <- renderPlot({
    # Laczenie danych
    all_data <- dplyr::bind_rows(
      podroze_joz  %>% mutate(distance = as.numeric(distance)),
      podroze_mic  %>% mutate(distance = as.numeric(distance)),
      podroze_kla  %>% mutate(distance = as.numeric(distance))
    )
    
    # Filtrowanie
    filtered_data <- all_data %>%
      dplyr::filter(person %in% input$People) %>%
      dplyr::filter(weekNum >= input$sliderWeekTransport[1],
                    weekNum <= input$sliderWeekTransport[2])
    
    # Jak brak danych 
    if (nrow(filtered_data) == 0) {
      plot.new()
      text(0.5, 0.5, "No data for the chosen people/weeks.")
      return()
    }
    
    person_data_stacked <- filtered_data %>%
      group_by(activity, person) %>%
      mutate(countIndex = row_number()) %>%
      ungroup()
    
    all_activities <- sort(unique(
      c(podroze_joz$activity, podroze_mic$activity, podroze_kla$activity)
    ))
    
    # Wykres
    ggplot(person_data_stacked, aes(x = activity, y = countIndex, color = person)) +
      geom_point(position = position_dodge(width = 0.6), size = 3, alpha = 0.8) +
      scale_x_discrete(limits = all_activities) +
      scale_color_manual(values = person_colors) +
      theme_minimal(base_family = "sans", base_size = 14) +   # <-- USTAWIENIE CZCIONKI I ROZMIARU
      labs(
        title = paste0("Dot Plot of Types of Used Transport (Count) - Weeks ",
                       paste(input$sliderWeekTransport, collapse = "-")),
        x     = "Transport Type",
        y     = "Count Index"
      ) +
      theme(
        panel.background = element_rect(fill = "#f5f5f5", color = NA), 
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
      )
  })
  
  #########################
  #  Transport Time Plot
  #########################
  output$transportTimePlot <- renderPlot({
    # laczymy dane
    all_data <- dplyr::bind_rows(
      podroze_joz  %>% mutate(distance = as.numeric(distance)),
      podroze_mic  %>% mutate(distance = as.numeric(distance)),
      podroze_kla  %>% mutate(distance = as.numeric(distance))
    )
    
    # Filtrujemy
    filtered_data <- all_data %>%
      filter(person %in% input$People) %>%
      filter(weekNum >= input$sliderWeekTransport[1],
                    weekNum <= input$sliderWeekTransport[2])
    
    # Gdy brak danych
    if (nrow(filtered_data) == 0) {
      plot.new()
      text(0.5, 0.5, "No data for chosen people/weeks.")
      return()
    }
    
    #Lista wszystkich możliwych aktywności
    all_activities <- sort(unique(
      c(podroze_joz$activity, podroze_mic$activity, podroze_kla$activity)
    ))
    
    # Podsumowujemy czas per (activity, person)
    transportTime <- filtered_data %>%
      group_by(activity, person) %>%
      summarise(totalTime = sum(timeDurSec, na.rm = TRUE), .groups = "drop")
    
    # Uzupełniamy brakujące kombinacje (aktywnosc, osoba) zerem
    transportTime <- transportTime %>%
      tidyr::complete(
        activity = all_activities,
        person   = input$People,
        fill = list(totalTime = 0) 
      )
    
    # Wykres
    ggplot(transportTime, aes(x = activity, y = totalTime / 3600, fill = person)) +
      geom_bar(
        stat     = "identity",
        position = position_dodge(width = 0.8),
        width    = 0.7                        
      ) +
      scale_x_discrete(limits = all_activities) +
      scale_fill_manual(values = person_colors) +
      theme_minimal(base_family = "sans", base_size = 14) + 
      labs(
        title = paste0("Time Spent on Transport (Hours) - Weeks ",
                       paste(input$sliderWeekTransport, collapse = "-")),
        x = "Transportation Type",
        y = "Time (Hours)"
      ) +
      theme(
        panel.background = element_rect(fill = "#f5f5f5", color = NA), 
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        axis.text.x      = element_text(angle = 45, hjust = 1),
        legend.title     = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  
  output$transport_text <- renderText({
    "Michał demonstrates the most diverse transport habits, with a notable emphasis on cycling alongside frequent use of subways and passenger vehicles. Klaudia relies predominantly on walking and subways, mirroring Józef’s reliance on walking as a primary mode of transport. Over time, Klaudia’s transport profile aligns more closely with Józef’s, while Michał’s balanced and active transport style remains distinct."
  })
  
  #####################
  #### Spider Plot ####
  #####################
  
  output$spiderPlot <- renderPlot(
    height = function() {
      340 * max(length(input$People), 1)
    },
    {
      
      # laczenie danych
      all_data <- bind_rows(
        podroze_joz %>% mutate(distance = as.numeric(distance)),
        podroze_mic %>% mutate(distance = as.numeric(distance)),
        podroze_kla %>% mutate(distance = as.numeric(distance))
      )
      
      # filtrowanie 
      filtered_data <- all_data %>%
        filter(person %in% input$People) %>%
        filter(
          weekNum >= input$sliderWeekTransport[1],
          weekNum <= input$sliderWeekTransport[2]
        )
      
      # gdy brak danych
      if (nrow(filtered_data) == 0) {
        plot.new()
        text(0.5, 0.5, "No data for the chosen people/weeks.")
        return()
      }
      
      all_activities <- sort(unique(
        c(podroze_joz$activity, podroze_mic$activity, podroze_kla$activity)
      ))
      
      # helper function do wykresu
      create_beautiful_radarchart <- function(data, color = "#4285F4", 
                                              vlabels = colnames(data), vlcex = 0.7,
                                              caxislabels = rep("", 5), title = NULL, ...) {
        radarchart(
          data,
          axistype = 1,
          pcol      = color,
          pfcol     = scales::alpha(color, 0.5),
          plwd      = 2, 
          plty      = 1,
          cglcol    = "grey", 
          cglty     = 1, 
          cglwd     = 0.8,
          axislabcol = "grey",
          vlcex     = vlcex, 
          vlabels   = vlabels,
          caxislabels = caxislabels,
          title     = title,
          ...
        )
      }
      
      # lista wybranych osob
      persons_selected <- unique(filtered_data$person)
      
      # ustawienia ukladu
      # ustawienia ukladu
      par(
        mfrow  = c(length(persons_selected), 1),
        mar    = c(3, 3, 3, 3),
        family = "sans",
        cex    = 1.2,     
        bg     = "#f5f5f5"    
      )
      
      # petla po osobach
      for (p in persons_selected) {
        data_for_person <- filtered_data %>%
          filter(person == p)
        
        transport_counts <- data_for_person %>%
          count(activity) %>%
          complete(activity = all_activities, fill = list(n = 0)) %>%
          arrange(activity)
        
        wide_df <- transport_counts %>%
          pivot_wider(
            names_from  = activity,
            values_from = n,
            values_fill = 0
          )
        
        max_val <- max(wide_df, na.rm = TRUE)
        df_for_radar <- rbind(
          rep(max_val, ncol(wide_df)),  # MAX
          rep(0,       ncol(wide_df)),  # MIN
          wide_df
        )
        
        df_for_radar <- as.data.frame(df_for_radar)
        rownames(df_for_radar) <- c("MAX", "MIN", "DATA")
        
        color_for_person <- person_colors[p] %||% "#999999"
        
        create_beautiful_radarchart(
          data  = df_for_radar,
          color = color_for_person,
          family = "sans",
          cex    = 1.2,
          title = paste0("Transport - ", p, 
                         "\n(Weeks ", paste(input$sliderWeekTransport, collapse = "-"), ")")
        )
      }
    }
  )
  
  
  
  ############################## Aktywności ####################################
  
  output$weeklyActivitiesText1 <- renderText({
    paste0("This stacked barplot shows where ", input$dropdown1, " spends their average day of the week. 
       These places have been grouped into a set of categories: shopping includes all the time spent buying groceries or new clothes; 
       restaurants include coffee shops and bakeries; university refers to all the buildings of the Warsaw University of Technology; 
       home includes vacation homes; entertainment encompasses sports, movie theaters, and similar activities
       and transport is the time spent traveling.")
  })
  
  output$dailyActivitiesPlot <- renderPlot({
    if(input$dropdown1 == "Józef"){
      podroze <- podroze_joz
      wizyty <- wizyty_joz
    } else if(input$dropdown1 == "Klaudia"){
      podroze <- podroze_kla
      wizyty <- wizyty_kla
    } else {
      podroze <- podroze_mic
      wizyty <- wizyty_mic
    }
    
    activity_colors <- c(
      "home" = "#1ea362",
      "university" = "#4a89f3",
      "transport" = "#dd4b3e",
      "entertainment" = "#ED7014",
      "shopping" = "#ffe047",
      "restaurants" = "#aadaff",
      "other" = "#5400CF"
    )
    
    czas_w_transporcie <- podroze %>%
      filter(weekNum >= input$sliderWeek1[1] & weekNum <=  input$sliderWeek1[2]) %>%
      mutate(dayOfWeek = weekdays(as.Date(endTime, format = "%Y-%m-%d")),
             dayOfWeek = factor(dayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
      group_by(dayOfWeek) %>% 
      mutate(meanTimeDur = sum(timeDurSec)/(input$sliderWeek1[2] - input$sliderWeek1[1] + 1)) %>%
      ungroup() %>% 
      group_by() %>% 
      select(dayOfWeek, meanTimeDur, activity_type) %>% 
      group_by(dayOfWeek, meanTimeDur) %>% 
      slice(1)
    
    czas_miejsca <- wizyty %>% 
      filter(weekNum >= input$sliderWeek1[1] & weekNum <=  input$sliderWeek1[2]) %>%
      filter(place != "MiNI") %>% 
      mutate(day = as.POSIXct(substr(endTime, 1, 10), format = "%Y-%m-%d")) %>%
      mutate(dayOfWeek = weekdays(as.Date(endTime, format = "%Y-%m-%d")),
             dayOfWeek = factor(dayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
      select(dayOfWeek, timeDurSec, activity_type) %>% 
      group_by(dayOfWeek, activity_type) %>% 
      mutate(meanTimeDur = sum(timeDurSec)/(input$sliderWeek1[2] - input$sliderWeek1[1] + 1)) %>%
      ungroup() %>% 
      select(dayOfWeek, activity_type, meanTimeDur) %>% 
      group_by(dayOfWeek, activity_type, meanTimeDur) %>% 
      slice(1)
    
    czas <- rbind(czas_miejsca, czas_w_transporcie)
    
    
    
    wyk2 <- ggplot(czas, aes(y = meanTimeDur, x = dayOfWeek, fill = activity_type))+
      geom_bar(stat = "identity", position = "fill") +
      labs(fill = "Activity Type") +                         
      xlab("Day of week") +                                     
      ylab("% of time of day") +                      
      ggtitle(paste0("Average daily time spent in each place by ", input$dropdown1)) +     
      theme_minimal()+
      scale_fill_manual(values = activity_colors) +
      theme(
        panel.background = element_rect(fill = "#f5f5f5", color = NA), 
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.title = element_text(size = 20),      
        axis.title.x = element_text(size = 20),    
        axis.title.y = element_text(size = 20),    
        axis.text = element_text(size = 15),       
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        axis.text.x = element_text(angle = 0, hjust = 0.5)
      ) +
      scale_y_continuous(labels = scales::percent)
    
    wyk2
  })
  
  
  ############################## Predkość ######################################
  
  output$transportSpeedBoxPlot <- renderPlot({
    
    podroze2 <- rbind(podroze_joz, podroze_kla, podroze_mic) %>%
      mutate(Day = weekdays(as.Date(startTime, format = "%Y-%m-%d")),
             Day = factor(Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
      filter(person %in% input$People2, distance >= 1) %>%
      mutate(activity2 = case_when(
        activity == "CYCLING" ~ "cycling",
        activity == "IN_PASSENGER_VEHICLE" ~ "in car",
        activity == "in passenger vehicle" ~ "in car",
        activity == "IN_SUBWAY" ~ "in subway",
        activity == "IN_TRAM" ~ "in tram",
        activity == "WALKING" ~ "walking",
        activity == "IN_TRAIN" ~ "in train",
        TRUE ~ activity
      )) %>%
      filter(activity2 == input$TransportType)
    
    people_colors <- c(
      "Klaudia" = "#1ea362",
      "Michał" = "#4a89f3",
      "Józef" = "#dd4b3e"
    )
    
    
    ggplot(podroze2, aes(x = Day, y = as.numeric(distance) / as.numeric(timeDurSec), fill = person)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste("Speed Distribution by Weekday and Person -", input$TransportType),
           x = "Weekday", y = "Speed (m/s)", fill = "Person") +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
      scale_fill_manual(values = people_colors) +
      theme(
        panel.background = element_rect(fill = "#f5f5f5", color = NA), 
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.title = element_text(size = 20),      
        axis.title.x = element_text(size = 20),    
        axis.title.y = element_text(size = 20),    
        axis.text = element_text(size = 15),       
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20)
      )
  })
  
  
  output$transportSpeedBoxPlot2 <- renderPlot({
    
    podroze2 <- rbind(podroze_joz, podroze_kla, podroze_mic) %>%
      mutate(Day = weekdays(as.Date(startTime, format = "%Y-%m-%d")),
             Day = factor(Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
      filter(person %in% input$People2, distance >= 1) %>%
      mutate(activity2 = case_when(
        activity == "CYCLING" ~ "cycling",
        activity == "IN_PASSENGER_VEHICLE" ~ "in car",
        activity == "in passenger vehicle" ~ "in car",
        activity == "IN_SUBWAY" ~ "in subway",
        activity == "IN_TRAM" ~ "in tram",
        activity == "WALKING" ~ "walking",
        activity == "IN_TRAIN" ~ "in train",
        TRUE ~ activity
      )) %>%
      filter(activity2 == input$TransportType)
    
    people_colors <- c(
      "Klaudia" = "#1ea362",
      "Michał" = "#4a89f3",
      "Józef" = "#dd4b3e"
    )
    
    ggplot(podroze2, aes(x = person, y = as.numeric(distance) / as.numeric(timeDurSec), fill = person)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste("Speed Comparison by Person -", input$TransportType),
           x = "Person", y = "Speed (m/s)", fill = "Person") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
      scale_fill_manual(values = people_colors) +
      theme(
        panel.background = element_rect(fill = "#f5f5f5", color = NA), 
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.title = element_text(size = 20),      
        axis.title.x = element_text(size = 20),    
        axis.title.y = element_text(size = 20),    
        axis.text = element_text(size = 15),       
        legend.position = "none"
      )
  })
  
  
  
  output$transportSpeedText1 <- renderText({
    paste0("This boxplot shows the distribution of the selected people's speed when ", case_when(
      input$TransportType == "in car" ~ "traveling by car",
      input$TransportType == "in tram" ~ "traveling by tram",
      input$TransportType == "in train" ~ "traveling by train",
      input$TransportType == "in subway" ~ "traveling by subway",
      TRUE ~ input$TransportType), " by day of the week.")
  })
  
  output$transportSpeedText2 <- renderText({
    paste0("This boxplot shows the distribution of the selected people's speed when ", case_when(
      input$TransportType == "in car" ~ "traveling by car",
      input$TransportType == "in tram" ~ "traveling by tram",
      input$TransportType == "in train" ~ "traveling by train",
      input$TransportType == "in subway" ~ "traveling by subway",
      TRUE ~ input$TransportType), ", for a more general comparison than the previous plot.")
  })
  
  output$transportSpeedText3 <- renderText({
    paste0("This heatmap shows the distance covered by ", input$selectedPerson2, " when ",
           case_when(
             input$TransportType == "in car" ~ "traveling by car",
             input$TransportType == "in tram" ~ "traveling by tram",
             input$TransportType == "in train" ~ "traveling by train",
             input$TransportType == "in subway" ~ "traveling by subway",
             TRUE ~ input$TransportType), ", for every day in the six weeks of our data gathering.
           When analysing it we must keep in mind that the data only includes time spent with our phones and an internet connection.")
  })
  
  
  
  
  
  output$transportSpeedHeatmap <- renderPlot({
    all_weeks <- 1:6
    all_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    complete_data <- expand.grid(weekNum = all_weeks, Day = all_days)
    
    lighten_color <- function(color, factor) {
      rgb_vals <- col2rgb(color) / 255  
      white <- c(1, 1, 1) 
      lighter_rgb <- rgb_vals * (1 - factor) + white * factor 
      rgb(lighter_rgb[1], lighter_rgb[2], lighter_rgb[3]) 
    }
    
    
    max_people_colors <- list(
      "Klaudia" = c("#1ea362"),
      "Michał" = c("#4a89f3"),
      "Józef" = c("#dd4b3e")
    )
    
    
    people_colors <- lapply(max_people_colors, function(max_color) {
      pastel_color <- lighten_color(max_color, factor = 0.85)  
      c(pastel_color, max_color)
    })
    
    
    names(people_colors) <- names(max_people_colors)
    
    
    
    podroze2 <- rbind(podroze_joz, podroze_kla, podroze_mic) %>%
      mutate(activity = case_when(
        activity == "CYCLING" ~ "cycling",
        activity == "IN_PASSENGER_VEHICLE" ~ "in car",
        activity == "in passenger vehicle" ~ "in car",
        activity == "IN_SUBWAY" ~ "in subway",
        activity == "IN_TRAM" ~ "in tram",
        activity == "WALKING" ~ "walking",
        activity == "IN_TRAIN" ~ "in train",
        TRUE ~ activity
      )) %>%
      filter(activity == input$TransportType) %>% 
      mutate(Day = weekdays(as.Date(startTime, format = "%Y-%m-%d")),
             Day = factor(Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
      mutate(distance = as.numeric(distance)) %>% 
      filter(person == input$selectedPerson2, distance >= 1) %>% 
      group_by(person, Day, weekNum) %>% 
      mutate(distanceSum = sum(distance)) %>% 
      ungroup() %>% 
      select(Day, weekNum, distanceSum) %>% 
      group_by(Day, weekNum, distanceSum) %>% 
      slice(1)
    
    person_color = case_when(
      input$selectedPerson2 == "Józef" ~ people_colors$Józef,
      input$selectedPerson2 == "Klaudia" ~ people_colors$Klaudia,
      input$selectedPerson2 == "Michał" ~ people_colors$Michał
    )
    
    podroze2 <- merge(complete_data, podroze2, by = c("weekNum", "Day"), all.x = TRUE)
    podroze2[is.na(podroze2$distanceSum),"distanceSum"] <- 0
    ggplot(podroze2 , aes(x = Day, y = weekNum, fill = distanceSum)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = person_color[1], high = person_color[2], name = "Distance (m)") +
      labs(title = paste0("Distance ", input$TransportType ," over 6 Weeks by ", input$selectedPerson2), x = "Day of Week", y = "Week") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(
        panel.background = element_rect(fill = "#f5f5f5", color = NA), 
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.title = element_text(size = 20),      
        axis.title.x = element_text(size = 20),    
        axis.title.y = element_text(size = 20),    
        axis.text = element_text(size = 15),       
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20)
      )
    
    
  })
  
  
  ################################# Mapa #####################################
  # filter data
  filtered_data <- reactive({
    person_map <- c("Józef" = "jo", "Michał" = "mi", "Klaudia" = "kl")
    selected_people <- input$PeopleMap
    ppl_map <- person_map[selected_people]
    
    merged_df %>%
      filter(relativeWeekNum >= input$sliderWeekMap[1] & relativeWeekNum <= input$sliderWeekMap[2]) %>%
      filter(person %in% ppl_map) %>% 
      filter(!is.na(placeName))
  })
  
  # map title
  output$dynamicTitleMap <- renderUI({
    h4(paste("Top", input$topPlacesCountMap, "Most Visited Places"))
  })
  
  # map
  output$map <- renderLeaflet({
    data_filtered <- filtered_data()
    
    top_n <- input$topPlacesCountMap
    
    # get top n places
    top_places <- data_filtered %>%
      count(placeName, latitude, longitude, person) %>%
      group_by(person) %>%
      arrange(person, desc(n)) %>%  
      slice_head(n = top_n) %>%    
      ungroup()
    
    # calculate values for setView
    lat_range <- max(top_places$latitude, na.rm = TRUE) - min(top_places$latitude, na.rm = TRUE)
    lng_range <- max(top_places$longitude, na.rm = TRUE) - min(top_places$longitude, na.rm = TRUE)
    
    if (nrow(top_places) > 0) {
      center_lat <- mean(top_places$latitude, na.rm = TRUE)
      center_lng <- mean(top_places$longitude, na.rm = TRUE)
      zoom <- ifelse(lng_range > 10, 5, ifelse(lat_range > 1 || lng_range > 1, 8, 14))
      
    } else {
      # warsaw default
      center_lat <- 52.2298
      center_lng <- 21.0118
      zoom <- 15
    }
    
    # colors
    color_palette <- colorFactor(c("#dd4b3e", "#1ea362", "#4a89f3"), levels = c("jo", "kl", "mi"))
    
    # plot
    leaflet(data = top_places) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = center_lng, lat = center_lat, zoom = zoom) %>%
      addCircleMarkers(
        lat = ~latitude, lng = ~longitude,
        radius = ~sqrt(n) * 3,
        color = ~color_palette(person),
        popup = ~paste(
          "<strong>Place Name:</strong>", placeName, "<br>",
          "<strong>Visits:</strong>", n, "<br>"
        )
      ) %>%
      addControl(
        html = "<h5 style='text-align: center; color: #333;'>Map of Top most visited places</h5>",
        position = "topright"
      )
  })

  observeEvent(input$infoButton, {
    showModal(modalDialog(
      title = "Map Information",
      "This map displays the top n most visited locations within the selected
      time period for the chosen person or group. Use the filters to customize the view and explore data interactively.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  ################################# Mapa tło #####################################
  output$mapBackground <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)  %>%  # 
      setView(lng = 21.0122, lat = 52.2298, zoom = 12) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
