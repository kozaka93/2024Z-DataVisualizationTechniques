library(shiny)
library(stringr)
library(ggplot2)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(fmsb)
library(tibble)
library(plotly)
library(scales)
library(shinyjs)
library(DT)
library(ggnewscale)
library(shinycssloaders)

data <- read.csv("data/training_data.csv")

source("functions/plot_functions/plot_01.R")
source("functions/plot_functions/plot_02.R")
source("functions/plot_functions/plot_03.R")
source("functions/plot_functions/plot_05.R")
source("functions/plot_functions/plot_06.R")
source("functions/plot_functions/plot_07.R")
source("functions/plot_functions/plot_08.R")
source("functions/plot_functions/plot_09.R")
source("functions/plot_functions/plot_10.R")


source("functions/Individual data/summarising.R")

adjust_plotly <- function(plotly_obj) {
  plotly_obj %>%
    layout(
      plot_bgcolor = "#3d3a36",
      paper_bgcolor = "#3d3a36",
      font = list(color = "white"),
      legend = list(
        bgcolor = "#3d3a36",
        font = list(color = "white")
      )
    ) %>%
    config(displayModeBar = FALSE)
}

generate_plot_data <- function() {
  plotly_plots <- list()
  static_plots <- list()
  
  for (i in 1:10) {
    if (!(i %in% c(4,6))) {
      plot_func <- get(paste0("plot_", sprintf("%02d", i)))
      ggplot_obj <- plot_func()
      #plotly_obj <- ggplotly(ggplot_obj)
      #plotly_obj <- adjust_plotly(plotly_obj)
      #plotly_plots[[length(plotly_plots) + 1]] <- plotly_obj
      static_plots[[length(static_plots) + 1]] <- ggplot_obj
    }
  }#
  
  # static plots - main
  return(list(main = static_plots, thumbnails = static_plots))
}

plots <- generate_plot_data()


ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      *{border: none;}
      body { background-color: #33332f; color: white;}
      .navbar { background-color: #3d3a36; color: #FFFFFF; 2px solid #0edfe6; border-radius: 1% }
      .navbar-brand { color: #FFFFFF; font-weight: bold; font-size: 20px; }
      .navbar-nav > li > a { color: #FFFFFF !important; font-size: 18px; }
      .navbar-nav > .active > a { background-color: #c45d08 !important; color: #FFFFFF !important; }
      .navbar-nav > li > a:hover { background-color: #c45d08; color: #FFFFFF !important; }
      .tab-content { padding: 20px; }
      .navbar-default .navbar-brand { color: #FFFFFF;}
      h3 { color: white; }
      .box { background-color: #3d3a36; color: white; border: none; }
      .btn-group .btn { background-color: #0edfe6; color: #FFFFFF; border: none; margin-right: 5px; }
      .btn-group .btn:hover { background-color: #3de60e; color: #FFFFFF; }
      
      .plot-container {
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 0;
        margin: 0 auto 40px auto;
        width: 100%;
        height: auto;
        position: relative;
        border: none;
      }

      .arrow {
        font-size: 32px;
        color: white;
        cursor: pointer;
        z-index: 10;
        background-color: #3d3a36;
        border: none;
        padding: 10px;
        border-radius: 50%;
        box-shadow: 0 2px 5px rgba(0,0,0,0.3);
        margin: 0 10px;
      }

      .arrow:hover {
        color: #c45d08;
        background-color: #1f1e1d;
      }
      
      .thumbnail-wrapper {
        padding: 10px;  
        display: flex;
        justify-content: center;
        flex-direction: column;
        align-items: center;
      }
      
      .thumbnail-wrapper h4 {font-size: 15px;}
      .thumbnail {
        border: 2px solid #ddd;
        border-radius: 4px;
        cursor: pointer;
        transition: border-color 0.3s;
        width: 100%;
        padding-top: 100%;
        position: relative;
        overflow: hidden;
      }
      .thumbnail:hover {
        border-color: #c45d08;
      }
      
      .thumbnail-plot {
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
      }
      
      #text_container {
        font-size: 15px;
        justify-content: center;
        padding: 20px;
        margin: 0 auto 40px auto;
        max-width: 400px; 
        width: 100%;
        box-sizing: border-box;
        text-align: justify; 
        overflow-wrap: break-word; 
      }

      
      .indicator-container {
        text-align: center;
        margin-bottom: 20px;
      }
      .indicator {
        display: inline-block;
        width: 15px;
        height: 15px;
        margin: 0 5px;
        background-color: #ccc;
        border-radius: 50%;
        cursor: pointer;
        transition: background-color 0.3s, transform 0.3s;
      }
      .indicator:hover {
        transform: scale(1.2);
      }
      .indicator.active {
        background-color: #c45d08;
      }
      .styled-plot {
      border: 2px solid #ddd;
      border-radius: 4px;
    }
      
      @media (max-width: 768px) {
        .arrow {
          font-size: 24px;
          left: -30px;
          right: -30px;
        }
        .thumbnail-wrapper {
          padding: 5px;
        }
      }
      
    /* datatables styling */
    table.dataTable {
      background-color: #3d3a36 !important; /* Unified dark gray background */
    }
    table.dataTable thead th {
      background-color: #3d3a36 !important; /* Same background for header */
      color: white !important; /* White text for column names */
      border-bottom: 1px solid #3d3a36 !important; /* Add white border under headers */
    }
    table.dataTable tbody tr {
      color: white !important; /* White text color for rows */
    }
    table.dataTable td {
      border: none !important; /* Remove table cell borders */
    }
    
    /* BODY PART BUTTONS STYLING */
    .layout-container {
      display: flex;  /* Use flexbox for layout */
    }
    .btn-group-body_part {
      display: grid;  /* Use grid layout for buttons */
      grid-template-columns: repeat(3, 1fr); /* 3 buttons per row */
      gap: 10px;  /* Spacing between buttons */
      margin-right: 20px; /* Space between buttons and table */
    }
    .btn-group-body_part .btn {
      display: inline-block;
      background-color: transparent;
      border: none;
      cursor: pointer;
    }
    .btn-group-body_part .btn img {
      width: 50px; 
      height: 50px;
    }
    .table-container {
      flex: 1;  
    }
    .navbar-brand img {
        max-height: 30px; 
        margin-right: 10px;
        vertical-align: middle;
      }
      
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('scrollToTop', function(message) {
        window.scrollTo({ top: 0, behavior: 'smooth' });
      });
    "))
  ),
  
  navbarPage(
    title = tagList(
      "Workout Analysis"
    ),
    tabPanel("General",
             div(
               div(
                 class = "plot-container",
                 actionButton("prev_plot", "←", class = "arrow arrow-left"),
                 div(
                   id = "text_container",
                   textOutput("plot_description")
                 ),
                 plotOutput("main_plot"),
                 actionButton("next_plot", "→", class = "arrow arrow-right")
               ),
               div(
                 class = "indicator-container",
                 uiOutput("plot_indicators")
               ),
               fluidRow(
                 uiOutput("thumbnail_plots")
               )
             )
    ),
    
    tabPanel("Individual",
             div(
               style = "font-size: 18px; display: flex; align-items: center; justify-content: space-between; width: 100%; padding-bottom: 40px;",
               div(
                 div(
                   class = "btn-group",
                   actionButton("btn_ludwik", "Ludwik", class = "btn", style = "background-color: #3de60e; color: #3d3a36; font-weight: bold;"),
                   actionButton("btn_yahor", "Yahor", class = "btn", style = "background-color: #0edfe6; color: #3d3a36; font-weight: bold;"),
                   actionButton("btn_maksim", "Maxim", class = "btn", style = "background-color: #dbf20a; color: #3d3a36; font-weight: bold;")
                 ),
                 div(
                   style = "display: flex; align-items: center;",
                   div(
                     style = "display: flex; align-items: center;",  
                     tags$h4("selected person:", style = "font-size: 20px; margin-right: 10px; margin-top: 20px;"), 
                     div(textOutput("selected_person_display"), style = "font-size: 24px; font-weight: bold; margin-top: 10px;")
                   )
                 )
               ),
               div(
                 style = "margin-left: 20px;",
                 uiOutput("person_image")
               )
             ),
             div(
               style = "padding-bottom: 40px;",
               tagList(
                 tags$h4("Do trainings impact how we drink?", style = "font-size: 24px;"),
                 tags$p("The plot illustrates drink consumption by type over time with dots marking training days, highlighting that water is predominantly consumed on training days.", style = "font-size: 18px;"),
                 withSpinner(plotlyOutput("drinksPlot")) 
               )               
             ),
             div(
               style = "padding-bottom: 40px;",
               tagList(
                 tags$h4("Drinking Statistics", style = "font-size: 24px;"),
                 withSpinner(DTOutput("drinksTable")) 
               )               
             ),
             
             tagList(
               tags$h4("Muscles in Action – Lifting Stats! (in repetitions)", style = "font-size: 24px; padding-bottom: 10px;"),
               tags$p("click the image :)",
                      style = "font-size: 12px; opacity: 0.6; text-align: center;text-align: left; font-weight: bold;"
               ),
               div(class = "layout-container",
                   style = "padding-bottom: 40px; text-align: center;",
                   div(class = "btn-group-body_part",
                       style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 20px; justify-items: center;",
                       tags$button(
                         id = "btn_chest",
                         class = "btn action-button",
                         tags$img(src = "https://img.icons8.com/?size=100&id=WDjwFnyNuTH9&format=png&color=000000",
                                  alt = "Chest")
                       ),
                       tags$button(
                         id = "btn_legs",
                         class = "btn action-button",
                         tags$img(src = "https://img.icons8.com/?size=100&id=vJCvRFKe4IQV&format=png&color=000000",
                                  alt = "Legs")
                       ),
                       tags$button(
                         id = "btn_core",
                         class = "btn action-button",
                         tags$img(src = "https://img.icons8.com/?size=100&id=-jpRPRt70j85&format=png&color=000000",
                                  alt = "Core")
                       ),
                       tags$button(
                         id = "btn_arms",
                         class = "btn action-button",
                         tags$img(src = "https://img.icons8.com/?size=100&id=8CFgbQTEEMV5&format=png&color=000000",
                                  alt = "Arms")
                       ),
                       tags$button(
                         id = "btn_back",
                         class = "btn action-button",
                         tags$img(src = "https://img.icons8.com/?size=100&id=Mgnho7JTNT2l&format=png&color=000000",
                                  alt = "Back")
                       ),
                       tags$button(
                         id = "btn_shoulders",
                         class = "btn action-button",
                         tags$img(src = "https://img.icons8.com/?size=100&id=35837&format=png&color=000000",
                                  alt = "Shoulders")
                       )
                   ),
                   div(class = "table-container",
                       withSpinner(DTOutput("exercisesTable"))
                   )
               )
               
             ),
             div(
               style = "padding-bottom: 40px;",
               tagList(
                 tags$h4("Marathon or Couch? Workout Streaks (in days)", style = "font-size: 24px;"),
                 withSpinner(DTOutput("streak_breakTable")) 
               )               
             ),
             
             div(
               style = "padding-bottom: 40px;",
               tagList(
                 tags$h4("Our Gym Records! (reps * kg per set)", style = "font-size: 24px;"),
                 withSpinner(DTOutput("mean_max_weightTable")) 
               )
             ),
             tagList(
               tags$h4("The Exercise That Wins Hearts (favourite)", style = "font-size: 24px;"),
               withSpinner(DTOutput("favouriteTable")) 
             )
    ),
    
    tabPanel("About",
             
             div(style = "font-size: 18px; text-align: center;",
                 div(
                   style = "padding-bottom: 40px;", 
                   p("Mikrus Gym is the place where the data for this project was collected."),
                   
                   img(src = "images/gym.jpg", alt = "Project Image", style = "width: 100%; max-width: 800px; display: block; margin: 0 auto;")
                 ),
                 
                 div(
                   style = "padding-top: 40px; padding-bottom: 40px;",
                   p("We gathered the data using a workout tracking application 'Strong'."),
                   
                   
                   div(
                     style = "display: flex; justify-content: space-around; gap: 20px;",
                     div(
                       style = "flex: 1; text-align: center;",
                       img(src = "images/screen1.jpg", alt = "Feature 1", style = "width: 100%; max-width: 250px;")
                     ),
                     div(
                       style = "flex: 1; text-align: center;",
                       img(src = "images/screen2.jpg", alt = "Feature 2", style = "width: 100%; max-width: 250px;")
                     ),
                     div(
                       style = "flex: 1; text-align: center;",
                       img(src = "images/screen3.jpg", alt = "Feature 3", style = "width: 100%; max-width: 250px;")
                     )
                   )
                 ), 
                 
                 div(
                   style = "padding-bottom: 40px;", 
                   p("We collected data on the liquids we consumed ourselves, recording and tracking what we drank every day.")
                 )
             )
    )
  )
)

server <- function(input, output, session) {
  
  plots <- generate_plot_data()
  total_plots <- length(plots$main)
  current_plot_index <- reactiveVal(1)
  
  observeEvent(input$prev_plot, {
    new_index <- current_plot_index() - 1
    if (new_index < 1) {
      new_index <- total_plots
    }
    current_plot_index(new_index)
  })
  
  observeEvent(input$next_plot, {
    new_index <- current_plot_index() + 1
    if (new_index > total_plots) {
      new_index <- 1
    }
    current_plot_index(new_index)
  })
  
  observeEvent(input$thumb_click, {
    new_index <- input$thumb_click
    if (new_index >= 1 && new_index <= total_plots) {
      current_plot_index(new_index)
      session$sendCustomMessage("scrollToTop", list())
    }
  })
  
  observeEvent(input$indicator_click, {
    new_index <- input$indicator_click
    if (new_index >= 1 && new_index <= total_plots) {
      current_plot_index(new_index)
      session$sendCustomMessage("scrollToTop", list())
    }
  })
  
  output$main_plot <- renderPlot({
    plots$main[[current_plot_index()]]
  })
  descriptions <- list("The chart shows how often we visited the gym at different hours and days of the week. The color intensity reflects the frequency of visits – lighter areas indicate higher activity, while darker areas indicate lower activity. It can be observed that Ludwik trains late in the evenings, in contrast to Yahor and Maxim.",
                       "The chart shows how long we trained on each day. The values represent the time spent at the gym each day, allowing us to observe differences in the length of workouts. In Maxim's case, it’s easy to see that he prefers \"lightning-fast\" training sessions – his chart looks like a series of warm-ups.",
                       "This chart shows the distribution of workout sets across different muscle groups. For example, Yahor focuses mostly on arms, as does Ludwik, while Maxim trains his back more. It also highlights the least frequently trained muscle groups.",
                       "This chart shows the weight distribution we use during training. Maxim mostly trains with bodyweight exercises, while Yahor and Ludwik primarily use weights.",
                       #"The plot shows drink consumption over time by type, with dots marking training days, where water is the most consumed, and the \"others\" category shows occasional spikes.",
                       "The plot tracks sets performed per minute, showing Maxim with highly variable intensity in his workouts — sometimes pushing at full throttle and other times taking it slower. In contrast, Yahor and Ludwik maintain more steady and consistent paces, with Yahor training at a moderate intensity and Ludwik preferring a more relaxed approach.",
                       "The chart shows the distribution of volume, which represents the total amount of work performed during the workout. Volume is calculated as the product of the number of repetitions and the weight used. This chart allows us to see which muscle groups were most engaged during the training, highlighting differences in the load across different muscle areas.",
                       "The chart shows the distribution of the number of repetitions performed during workouts. It's clear that Maxim has definitely \"accidentally\" focused on his legs, with far more repetitions than for other muscle groups.",
                       "The plot shows the relationship between total repetitions and workout length, with total repetitions generally increasing with duration.")
  output$plot_description <- renderText({
    paste(descriptions[current_plot_index()])
  })
  
  output$plot_indicators <- renderUI({
    lapply(1:total_plots, function(i) {
      div(
        class = ifelse(i == current_plot_index(), "indicator active", "indicator"),
        onclick = sprintf("Shiny.setInputValue('indicator_click', %d, {priority: 'event'});", i)
      )
    })
  })
  
  output$thumbnail_plots <- renderUI({
    plot_indices <- 1:total_plots
    n <- 4
    plot_matrix <- split(plot_indices, ceiling(seq_along(plot_indices)/n))
    
    thumbnail_rows <- lapply(plot_matrix, function(row) {
      columns <- lapply(row, function(i) {
        column(
          width = 3,
          div(
            class = "thumbnail-wrapper",
            h4(
              switch(i,
                     "1" = "Workout Schedule Distribution",
                     "2" = "Workout Duration By Date",
                     "3" = "Number of Sets per Muscle Group",
                     "5" = "Distribution Of Training Weights",
                     "7" = "Workout Intensity",
                     "8" = "Weight Lifted Per Set Vs. Body Part",
                     "9" = "Repetitions Per Set Vs. Body Part",
                     "10" = "Total Reps Vs. Workout Length",
                     paste0("Plot ", i)
              )
            ),
            div(
              class = "thumbnail",
              div(
                class = "thumbnail-plot",
                plotOutput(
                  outputId = paste0("thumb_", i),
                  height = "100%",
                  width = "100%"
                )
              ),
              onclick = sprintf("Shiny.setInputValue('thumb_click', %d, {priority: 'event'});", i)
            )
          )
        )
      })
      fluidRow(columns)
    })
    
    do.call(tagList, thumbnail_rows)
  })
  
  lapply(1:total_plots, function(i) {
    local({
      index <- i
      output[[paste0("thumb_", index)]] <- renderPlot({
        p <- plots$thumbnails[[index]]
        p <- p + theme(
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          strip.text.x = element_blank(),
          strip.text.y = element_blank()
        ) 
        print(p)
      })
    })
  })
  
  
  
  # data generated for individual pages
  
  selected_person <- reactiveVal("Ludwik")
  
  reactive_drinks <- reactive({
    drinks_data <- personal_data_6_7_8(name = selected_person(), categories = c("WODA", "KAWA", "HERBATA", "INNE"))
    
    ## DODAC JAKOS TOOLTIP PO NAJECHANIU MYSZKA NA IKONKE!!!
    drink_image_map <- c(
      "WODA" = "https://img.icons8.com/?size=100&id=dfwEeQVX02pB&format=png&color=000000",  # Water icon
      "KAWA" = "https://img.icons8.com/?size=100&id=dNiyF3EQG6ME&format=png&color=000000",      # Coffee icon
      "HERBATA" = "https://img.icons8.com/fluency/48/000000/tea.png",     # Tea icon
      "INNE" = "https://img.icons8.com/?size=100&id=bcVyoIxvSOFz&format=png&color=000000" # Juice icon
    )
    
    drink_order <- c("WODA", "HERBATA", "KAWA", "INNE")
    polish_to_english_drinks <- c(
      "WODA" = "Water",
      "HERBATA" = "Tea",
      "KAWA" = "Coffee",
      "INNE" = "Others"
    )
    
    drinks_data <- drinks_data %>%
      mutate(
        Image = drink_image_map[Drink],
        Drink_Name = polish_to_english_drinks[Drink],
      ) %>%
      rename(
        min = paste0("min_consumption_", selected_person()),
        mean = paste0("mean_consumption_", selected_person()),
        max = paste0("max_consumption_", selected_person()),
      )
    
    
    
    pic_drinks <- drinks_data %>%
      mutate(
        Drink_Display = paste0(
          "<div style='display: flex; align-items: center;'>",
          "<img src='", Image, "' height='30' style='margin-right: 10px;' title='", polish_to_english_drinks[Drink], "'/>",
          "<span>", polish_to_english_drinks[Drink], "</span>",
          "</div>"
        ),
        Drink = factor(Drink, levels = drink_order)
      ) %>%
      arrange(Drink) %>%
      select(Drink_Display, min, mean, max)
    
    pic_drinks
  })
  
  
  output$drinksTable <- renderDT({
    
    pic_drinks <- reactive_drinks()
    
    datatable(
      pic_drinks,
      colnames = c("drink type","min", "mean", "max"),
      options = list(
        dom = "t",  
        ordering = FALSE,  
        pageLength = nrow(pic_drinks) 
      ),
      escape = FALSE,
      rownames = FALSE,
      selection = "none"
    )
    
  })
  
  ## DRINK PLOTLY PLOTS 
  
  output$drinksPlot <- renderPlotly({
    
    
    myplot <- ggplotly(plot_06(people = selected_person()))
    myplot <- config(myplot, displayModeBar = FALSE)
    
    
    for (i in 1:length(myplot$x$data)){
      if (!is.null(myplot$x$data[[i]]$name)){
        myplot$x$data[[i]]$name =  gsub("\\(","",str_split(myplot$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    
    myplot %>%
      layout(
        plot_bgcolor = "#3d3a36",
        paper_bgcolor = "#3d3a36",
        font = list(color = "white"),
        title = list(font = list(color = "white", size = 20)),
        xaxis = list(
          title = "Date",
          tickformat = "%b %d",
          gridcolor = "#5b5c55",
          color = "white"
        ),
        yaxis = list(
          title = "Quantity (l)",
          gridcolor = "#5b5c55",
          color = "white"
        ),
        legend = list(
          title = list(text = "<b>Drink Type</b>"),
          font = list(color = "white")
        ),
        hoverlabel = list(
          bgcolor = "#5b5c55", # Background color of the tooltip
          bordercolor = "#3d3a36", # Border color of the tooltip
          font = list(
            color = "white", # Font color
            size = 12,       # Font size
            family = "Arial" # Font family
          )
        ),
        annotations = list(
          list(
            text = "The dots represent training days",
            x = 0.5,
            y = 1,  
            xref = "paper",  
            yref = "paper",  
            xanchor = "center",  
            yanchor = "bottom",  
            showarrow = FALSE, 
            font = list(size = 15, color = "white")  
          ))
      ) 
  })
  
  
  ## DATA AND TABLE FOR REPS FOR EVERY BODY PART
  
  selected_body_part <- reactiveVal("Chest")
  
  reactive_exercises <- reactive({
    exercises_data <- personal_data_05(name = selected_person(), body_part = selected_body_part()) |>
      select(-Body.Part)
    exercises_data
  })
  
  output$exercisesTable <- renderDT({
    
    pic_exercises <- reactive_exercises()
    
    datatable(
      pic_exercises,
      colnames = c("min", "mean", "max"),
      options = list(
        dom = "t",
        ordering = FALSE,
        pageLength = nrow(pic_exercises)
      ),
      escape = FALSE,
      rownames = FALSE,
      selection = "none"
    )
  })
  
  
  ## DATA AND TABLE FOR STREAK AND BREAK 
  
  reactive_streak_break <- reactive({
    streak_break_data <- personal_data_03_04(name = selected_person())
    
    
    streak_image_map <- list(
      "Streak" = "https://img.icons8.com/?size=100&id=18515&format=png&color=000000"
    )
    
    streak_break_data$Image <- NA
    streak_break_data$Image[1] <- streak_image_map["Streak"]
    
    pic_streak <- streak_break_data %>%
      mutate(
        picture = paste0(
          "<img src=\"",
          Image,
          "\" height=\"30\" title=\"", "", "\"></img>"
        ),
      ) %>%
      select(picture, longest_streak, longest_break)
    
    pic_streak
    
    
  })
  
  output$streak_breakTable <- renderDT({
    
    pic_streak <- reactive_streak_break()
    
    datatable(
      pic_streak,
      colnames = c("", "longest streak", "longest break"),
      options = list(
        dom = "t",
        ordering = FALSE,
        pageLength = nrow(pic_streak)
      ),
      escape = FALSE,
      rownames = FALSE,
      selection = "none"
    )
  })
  
  
  
  ## DATA AND TABLE FOR MEAN MAX WIEGHT DURING TRAINING
  reactive_mean_max_weight <- reactive({
    mean_max_weight_data <- personal_data_02(name = selected_person()) |>
      select(-Owner)
    
    weight_map <- list(
      "W" = "https://img.icons8.com/?size=100&id=80735&format=png&color=000000"
    )
    
    mean_max_weight_data$Image <- NA
    mean_max_weight_data$Image[1] <- weight_map[["W"]]
    
    pic_weight <- mean_max_weight_data %>%
      mutate(
        picture = paste0(
          "<img src=\"",
          Image,
          "\" height=\"30\" title=\"", "", "\"></img>"
        ),
      ) %>%
      select(picture, mean_weight_lifted_during_training, max_weight_lifted_during_training)
    
    pic_weight
    
    
    
    
  })
  
  output$mean_max_weightTable <- renderDT({
    
    mean_max_weight <- reactive_mean_max_weight()
    
    
    
    datatable(
      mean_max_weight,
      colnames = c("mean", "max"),
      options = list(
        dom = "t",
        ordering = FALSE,
        pageLength = nrow(mean_max_weight)
      ),
      escape = FALSE,
      rownames = FALSE,
      selection = "none"
    )
  })
  
  ## DATA AND TABLE FOR FAVOURITE EXERCISE
  # https://img.icons8.com/?size=100&id=hLvRN9LQ1WfQ&format=png&color=000000
  reactive_favourite <- reactive({
    favourite_data <- personal_data_01(name = selected_person()) |>
      select(-Owner)
    
    
    exercises_map <- c(
      "Pull Up" = "https://img.icons8.com/?size=100&id=lmxIIMslSEXk&format=png&color=000000",
      "Bench Press (Barbell)" = "https://img.icons8.com/?size=100&id=MUcS5TRquPqI&format=png&color=000000",      # Coffee icon
      "Bench Press (Dumbbell)" = "https://img.icons8.com/?size=100&id=JvRnjqoqMC6B&format=png&color=000000",
      "Bicep Curl (Cable)" = "https://cdn-icons-png.flaticon.com/512/11437/11437928.png"
    )
    
    
    favourite_data <- favourite_data %>%
      mutate(
        Image = exercises_map[Exercise.Name]
      )
    
    pic_favourite_data <- favourite_data %>%
      mutate(
        Exercise_Display = paste0(
          "<div style='display: flex;
      flex-direction: column;
    justify-content: center;
      text-align: left;'>",
          "<img src='", Image, "' height='90' width='90' style='margin-bottom: 5px;' title='", Exercise.Name, "'/>",
          "<span>", Exercise.Name, "</span>",
          "</div>"
        )
        
      ) %>%
      select(Exercise_Display, Set_count)
    
    pic_favourite_data
  })
  
  output$favouriteTable <- renderDT({
    
    favourite <- reactive_favourite()
    
    datatable(
      favourite,
      colnames = c("exercise", "attempts at the exercise"),
      options = list(
        dom = "t",
        ordering = FALSE,
        pageLength = nrow(favourite),
        columnDefs = list(
          list(className = 'dt-left', targets = 0)
        )
      ),
      escape = FALSE,
      rownames = FALSE,
      selection = "none"
    )
  })
  
  
  # observe PERSON
  observeEvent(input$btn_ludwik, {
    selected_person("Ludwik")
  })
  
  observeEvent(input$btn_yahor, {
    selected_person("Yahor")
  })
  
  observeEvent(input$btn_maksim, {
    selected_person("Maxim")
  })
  
  # rendering TEXT PERSIN CHOSEN 
  output$selected_person_display <- renderText({selected_person()})
  # rendering IMAGE OF PERSON CHOSEN   
  
  output$person_image <- renderUI({
    selected_person <- selected_person() 
    
    if (selected_person == "Ludwik") {
      img(src = "images/ludwik.jpg", height = "200px")
    } else if (selected_person == "Yahor") {
      img(src = "images/yahor.jpg", height = "200px")
    } else if (selected_person == "Maxim") {
      img(src = "images/maks.jpg", height = "200px")
    } else {
      img(src = "images/maks.jpg", height = "200px")
    }
  })
  
  
  
  # observe BODY PART
  
  observeEvent(input$btn_chest, {
    selected_body_part("Chest")
  })
  
  observeEvent(input$btn_legs, {
    selected_body_part("Legs")
  })
  
  observeEvent(input$btn_core, {
    selected_body_part("Core")
  })
  
  observeEvent(input$btn_arms, {
    selected_body_part("Arms")
  })
  
  observeEvent(input$btn_back, {
    selected_body_part("Back")
  })
  
  observeEvent(input$btn_shoulders, {
    selected_body_part("Shoulders")
  })
  
}


shinyApp(ui = ui, server = server)