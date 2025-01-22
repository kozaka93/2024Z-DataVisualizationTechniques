library(ggplot2)
library(shiny)
library(dplyr)
library(shinycssloaders)
library(plotly)
library(bslib)
library(tidyr)





Fond<-theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  ) 

Fond_pie<- theme(legend.title = element_text(size = 14, face = "bold"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
    legend.text = element_text(size = 12))
 



daily_goals <- data.frame(
  title = c("kcal", "proteins", "fats", "carbo"),
  P = c(2369, 94, 72, 341),
  K = c(2280, 120, 80, 270),
  M = c(2588, 100, 80, 372)
)

color_palette <- c("#3498DB", "#18BC9C", "#F39C12", "#E74C3C")

#Filter daty
maxdate <- as.Date("2025-01-07")
date <- as.Date("2024-12-07")

df_1 <- read.csv("dane_posilkow.csv") %>% select(-X)
df_2 <- read.csv("dane_ogolne.csv") %>% select(-X)

df_1$day <- as.Date(df_1$day)
df_2$day <- as.Date(df_2$day)

Sys.setlocale("LC_ALL","English")

         
times <- seq(
  from = as.POSIXct("2024-12-24 00:00:00"),
  to = as.POSIXct("2024-12-24 23:30:00"),
  by = "30 min"
)

formatted_times <- as.character(format(times, "%H:%M"))


ui <- navbarPage(
  "Food dashboard",
  
  theme = bs_theme(preset = "flatly", bg = "#F1F1F1", fg = "#000000"),
  ### ZAKŁADKA 1 ###
  
  tabPanel(
    "Daily consume of nutritions",
    fluidPage(
      titlePanel("Daily consume of nutritions"),
      
      fluidRow(
        column(3, wellPanel(
          selectInput(
            inputId = "person1_1",
            label = "Choose person:",
            choices = c("Gourmand 1", "Gourmand 2", "Gourmand 3"),
            selected = "Gourmand 1"
          ),
          "Daily needs for calories and macro",
          tableOutput("goals_table")
        ))
        ,
        column(
          8, card(card_header("Consume of chosen macro over month"),
          selectInput(
            "chosen_macro",
            "Choose macro you want to see",
            daily_goals$title[2:4],
            selected = "proteins"
          ),
          plotOutput("macro_plot") %>% withSpinner(image = "food_loading.gif"))
        )
      ),
      fluidRow(column(3), column(
        8,
        card(card_header("Calories intake with and without trash food included"),
             plotOutput("calories_plot") %>% withSpinner(image = "food_loading.gif"))
      )),
      fluidRow(
        column(3),
        column(4,
               card(card_header(textOutput("trashText")),
        plotOutput("trashfood_percent", fill = TRUE) %>% withSpinner(image = "food_loading.gif"))),
        column(4, card(card_header(textOutput("snackText")),
               plotOutput("snackpercent", fill = TRUE) %>% withSpinner(image = "food_loading.gif"))))
    )
  ),
  tabPanel("Let's compete", ### ZAKŁADKA 2  ###
           
           fluidPage(
             titlePanel("Let's compete"),
             
             fluidRow(
               column(
                 3,
                 wellPanel(
                   selectInput(
                     inputId = "person1",
                     label = "Choose the first person:",
                     choices = c("Gourmand 1", "Gourmand 2", "Gourmand 3"),
                     selected = "Gourmand 1"
                   )
                 ),
                 wellPanel(
                   selectInput(
                     inputId = "person2",
                     label = "Choose the second person",
                     choices = c("Gourmand 1", "Gourmand 2", "Gourmand 3","None"),
                     selected = "None"
                   )
                 ),
                 wellPanel(
                   selectInput(
                     inputId = "Yaxis",
                     label = "Choose what you want to compare",
                     choices = c("calories", "liquids", "steps"),
                     selected = "liquids"
                   )
                 ),
                 wellPanel(
                 sliderInput(
                   inputId = "date_range",
                   label = "Select a date range:",
                   min = as.Date("2024-12-07"),
                   max = as.Date("2025-01-07"),
                   value = c(as.Date("2024-12-07"), as.Date("2025-01-07")),
                   timeFormat = "%Y-%m-%d")
                 )
                 
                 
                 
               ),
               column(
                 9, card(card_header(textOutput("compar")),
                 plotOutput("LiquidPlot") %>% withSpinner(image = "food_loading.gif")),
                 card(card_header(textOutput("compar_week")),
                 plotOutput("PlotPerWeek") %>% withSpinner(image = "food_loading.gif")),
                 card(card_header("Summary of the data above"),
                 tableOutput("tableAVG"))
               )
               
             ),
             
             fluidRow(
               column(
                 3,
                 wellPanel(
                   selectInput(
                     inputId = "meal",
                     label = "Choose the meal: ",
                     choices = c("snack", "breakfast", "lunch", "supper"),
                     selected = "lunch"
                   )
                 ),
                 wellPanel(
                   selectInput(
                     inputId = "DayWeek",
                     label = "Choose the day: ",
                     choices = c(
                       "Monday",
                       "Tuesday",
                       "Wednesday",
                       "Thursday",
                       "Friday",
                       "Saturday",
                       "Sunday",
                       "Whole week"
                     ),
                     selected = "Whole week"
                   )
                 ),
                 wellPanel(
                 sliderInput(
                   inputId = "date_range_2",
                   label = "Select a date range:",
                   min = as.Date("2024-12-07"),
                   max = as.Date("2025-01-07"),
                   value = c(as.Date("2024-12-07"), as.Date("2025-01-07")),
                   timeFormat = "%Y-%m-%d"
                 ))
                 
               ),
               column(
                 9, card(card_header("Number of eaten meals by hour"),
                 plotOutput("MealHourPlot") %>% withSpinner(image = "food_loading.gif")
               )
             ))
           )),
  tabPanel(
    "Overall stats",
    ### ZAKŁADKA 3  ###
    fluidPage(
      titlePanel("Overall stats"),
      
      # fluidRow(
      #   column(3, wellPanel(
      #     selectInput(
      #       inputId = "person_bm3",
      #       label = "Choose person:",
      #       choices = c("Gourmand 1", "Gourmand 2", "Gourmand 3"),
      #       selected = "Gourmand 1"
      #     ),
      #     selectInput(
      #       "chosen_macro_bm3",
      #       "Choose calories/macro",
      #       c("kcal", "proteins", "fats", "carbo"),
      #       selected = "kcal"
      #     ),
      #     selectInput(
      #       inputId = "meal_bm3",
      #       label = "Choose meal:",
      #       choices = c("breakfast", "lunch", "snack", "supper"),
      #       selected = "breakfast"
      #     )
      #   )),
      #   
      #   column(
      #     9,
      #     card(card_header(textOutput("plot_bm3title")),
      #     plotOutput("plot_bm3") %>% withSpinner(image = "food_loading.gif")
      #   ))
      # ),
      
      
      fluidRow(
        column(3, wellPanel(
          selectInput(
            inputId = "person_2_bm3",
            label = "Choose person:",
            choices = c("Gourmand 1", "Gourmand 2", "Gourmand 3"),
            selected = "Gourmand 1"
          )
        ), ),
        
        column(
          9,
          card(card_header("Number of eaten meals within specific hours"),
          plotlyOutput("bubble_chart_bm3") %>% withSpinner(image = "food_loading.gif")
        ))
      ),
      
      fluidRow(
        column(3, ),
        
        column(
          9,
          card(card_header("Calories intake from trash food depending on alcohol consumption"),
          plotOutput("boxplot_bm3") %>% withSpinner(image = "food_loading.gif")),
          card(card_header("Skipped meals during a month"),
          tableOutput("table_bm3")),
          card(card_header("Calories each person consumed in a month"),
          plotOutput("stacked_calorie_chart") %>% withSpinner(image = "food_loading.gif"))
        )
      )
      
    )
    
  ))


get_name <- function(person_input) {
  if (person_input == "Gourmand 1") {
    Name <- "P"
  } else if (person_input == "Gourmand 2") {
    Name <- "K"
  } else if  (person_input == "Gourmand 3"){
    Name <- "M"
  }else{
    Name<-"None"
  }
  return(Name)
}

get_name_reverse <- function(person_input) {
  if (person_input == "P") {
    Name <- "Gourmand 1"
  } else if (person_input == "K") {
    Name <- "Gourmand 2"
  } else if (person_input == "M") {
    Name <- "Gourmand 3"
  }else{
    Name<-"None"
  }
  return(Name)
}

server <- function(input, output, session) {
  ### ZAKŁADKA 1 ###
  
 

  
  output$goals_table <- renderTable({
    
    Name<-get_name(input$person1_1)
    
    daily_goals %>%
      select(c(1, Name)) %>%
      filter(title != "Person")
  }, colnames = FALSE)
  
  output$macro_plot <- renderPlot({
    Name<-get_name(input$person1_1)
    
    
     df_1 %>%
      filter(SymbolImienia == Name) %>%
      group_by(day) %>%
      select(-meal, -time, -SymbolImienia) %>%
      summarize_all(list( ~ sum(., na.rm = TRUE))) %>%
      ggplot() +
      geom_line(aes(day, !!sym(input$chosen_macro), color = input$chosen_macro), linewidth = 2) +
      geom_line(data = NULL,
                aes(
                  x = day,
                  y = (
                    daily_goals %>% filter(title == input$chosen_macro) %>%
                      select(Name)
                  )[[1]],
                  color = paste0("Daily needs for ", input$chosen_macro)
                ),
                linewidth = 1.5, linetype = "dashed") +
      theme_minimal() +
      labs(color = element_blank()) +
      scale_color_manual(values = c("purple", color_palette[1])) + 
      ylim(0,NA)+
      Fond +
      theme(
      )
      

      
  })
  
  output$calories_plot <- renderPlot({
    
    Name<-get_name(input$person1_1)
    
    temp <- df_1 %>%
      filter(SymbolImienia ==Name) %>%
      group_by(day) %>%
      summarize(kcal = sum(kcal, na.rm = TRUE)) %>%
      mutate(SymbolImienia =Name) %>%
      left_join(df_2, by = c("day", "SymbolImienia")) %>%
      select(kcal, day, trash_food)
    
      ggplot(temp) +
      geom_line(aes(day, kcal, color = "Calories"), linewidth = 2) +
      geom_line(aes(day, kcal - trash_food, color = "Calories without trash food"),
                linewidth = 2) +
      ylim(0, max(temp$kcal)) +
      geom_line(aes(
        x = day,
        y = (
          daily_goals %>% filter(title == "kcal") %>%
            select(Name)
        )[[1]],
        color = "Daily needs for calories"
      ),
      linewidth = 2, linetype = "dashed") +
      scale_color_manual(values = c("darkred", "darkgreen", "purple")) +
      theme(legend.position = "right", legend.title = element_blank()) +
        labs(color = "Different types of calories")+
        Fond
    
    
  })
  
  output$trashfood_percent <- renderPlot({
    Name<-get_name(input$person1_1)
    
    kcal1 <- df_1 %>%
      group_by(SymbolImienia) %>%
      summarize(kcal = sum(kcal))
    
    df_2 %>%
      select(day, SymbolImienia, trash_food) %>%
      filter(SymbolImienia == Name) %>%
      group_by(SymbolImienia) %>%
      summarise(trash_food = sum(trash_food)) %>%
      left_join(kcal1, by = "SymbolImienia") %>%
      mutate(regular_food = kcal - trash_food) %>%
      select(-kcal) %>%
      tidyr::pivot_longer(cols = c(trash_food, regular_food)) %>%
      ggplot(aes(x = "", y = value, fill = name)) +
      geom_col(width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      scale_fill_manual(
        values = color_palette,
        labels = c(trash_food = "Trash food", regular_food = "Regular food")
      ) +
      labs(fill = element_blank())+
      Fond_pie
    
    
  })
  
  output$snackpercent <- renderPlot({
    input$person4
    
    Name<-get_name(input$person1_1)
    
    kcal1 <- df_1 %>%
      filter(SymbolImienia == "M") %>%
      group_by(SymbolImienia) %>%
      summarize(kcal = sum(kcal))
    
    df_1 %>%
      filter(meal == "snack", SymbolImienia == Name) %>%
      summarize(snack_kcal = sum(kcal)) %>%
      mutate(kcal = kcal1$kcal, meals = kcal - snack_kcal) %>%
      select(-kcal) %>%
      tidyr::pivot_longer(cols = c(snack_kcal, meals)) %>%
      ggplot(aes(x = "", y = value, fill = name)) +
      geom_col(width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      scale_fill_manual(
        values = color_palette[3:4],
        labels = c(snack_kcal = "Snack kcal", meals = "Meals")
      ) +
      labs(fill = element_blank())+
      Fond_pie
      
  })
  
  output$snackText <- renderText({
    paste0("Percentage of calories from snacks in ", input$person1_1, "'s diet")
    })
  
  output$trashText <- renderText({
    
    paste0("Percentage of trash food in ", input$person1_1, "'s diet")
    
  })
  
  
  ### ZAKŁADKA 2 ###
  
  output$compar <- renderText({
    paste("Comparison of", input$Yaxis)
  })
  
  output$compar_week <- renderText({
    paste("Comparison of", input$Yaxis, "by day of the week")
  })
  
  
  observeEvent(input$person1, {
    updateSelectInput(
      session,
      inputId = "person2",
      choices = setdiff(c("Gourmand 1", "Gourmand 2", "Gourmand 3", "None"), input$person1),
      selected = "None"
    )
  })
  
  output$LiquidPlot <- renderPlot({
    Name1<-get_name(input$person1)
    Name2<-get_name(input$person2)
    
    r<-df_2 %>%filter(SymbolImienia ==Name1 |
                      SymbolImienia == Name2) %>%
      filter(day > input$date_range[1] &
               day < input$date_range[2]) %>%
      inner_join(df_1, by = c("SymbolImienia" = "SymbolImienia", "day" =
                                "day")) 
      
      r%>%mutate(SymbolImienia=sapply(r$SymbolImienia,get_name_reverse))%>%
      group_by(day, SymbolImienia) %>%
      mutate(calories = sum(kcal)) %>%
      ungroup() %>%
      ggplot(aes(
        x = day,
        y = !!sym(input$Yaxis),
        color = SymbolImienia
      )) +
      geom_line(linewidth = 2) +
      geom_point(size = 3) +
      labs(x = "days", y = input$Yaxis, color = "Gourmands") +
      scale_color_manual(values = color_palette) +Fond
      
    
    
  })
  
  output$PlotPerWeek <- renderPlot({
    
    Name1<-get_name(input$person1)
    Name2<-get_name(input$person2)
    
    
    
    r<-df_2 %>% filter(SymbolImienia == Name1 |
                      SymbolImienia ==Name2) %>%
      filter(day > input$date_range[1] &
               day < input$date_range[2]) %>%
      inner_join(df_1, by = c("SymbolImienia" = "SymbolImienia", "day" =
                                "day")) 
      
      
      r%>%mutate(SymbolImienia=sapply(r$SymbolImienia,get_name_reverse))%>%
      group_by(day, SymbolImienia) %>%
      mutate(SumKcal = sum(kcal)) %>%
      ungroup() %>%
      group_by(DayOfTheWeek, SymbolImienia) %>%
      mutate(
        calories = mean(SumKcal, na.rm = TRUE),
        liquids = mean(liquids, na.rm = TRUE),
        steps = mean(steps, na.rm = TRUE)
      ) %>%
      ggplot(aes(
        x = as.factor(DayOfTheWeek),
        y = !!sym(input$Yaxis),
        fill = SymbolImienia
      )) +
      geom_col(position = "dodge") +
      scale_x_discrete(
        labels = c(
          "Monday",
          "Tuesday",
          "Wednesday",
          "Thursday",
          "Friday",
          "Saturday",
          "Sunday"
        )
      ) +
      labs(x = "Days of the week", fill = "Gourmands") +
    
      scale_fill_manual(values = color_palette) +
      Fond
    
    
  })
  
  output$MealHourPlot <- renderPlot({
    if (input$meal == "breakfast") {
      hours <- formatted_times[12:25]
    } else if (input$meal == "lunch") {
      hours <- formatted_times[23:37]
    }
    else if (input$meal == "supper") {
      hours <- formatted_times[35:48]
    } else{
      hours <- formatted_times[23:37]
    }
    
    Name1<-get_name(input$person1)
    Name2<-get_name(input$person2)
    
    
    if (input$DayWeek != "Whole week") {
      r<-df_1 %>% filter(SymbolImienia ==  Name1|
                        SymbolImienia ==  Name2) %>%
        inner_join(df_2,
                   by = c("SymbolImienia" = "SymbolImienia", "day" = "day")) %>%
        filter(day > input$date_range[1] &
                 day < input$date_range[2]) %>%
        filter(DayOfTheWeek == input$DayWeek) %>%
        filter(meal == input$meal) 
      
        r%>%mutate(SymbolImienia=sapply(r$SymbolImienia,get_name_reverse))%>%
        ggplot(aes(x = as.factor(time), fill = SymbolImienia)) +
        geom_bar(position = "dodge") +
        labs(x = "Hours", y = "Count", fill = "Gourmands") +
        scale_x_discrete(limits = as.factor(hours)) +
        scale_fill_manual(values = color_palette) +
        Fond
    } else {
      r<-df_1 %>% filter(SymbolImienia ==  Name1 |
                        SymbolImienia ==  Name2) %>%
        inner_join(df_2,
                   by = c("SymbolImienia" = "SymbolImienia", "day" = "day")) %>%
        filter(day > input$date_range_2[1] &
                 day < input$date_range_2[2]) %>%
        filter(meal == input$meal) 
        
        r%>%mutate(SymbolImienia=sapply(r$SymbolImienia,get_name_reverse))%>%
        ggplot(aes(x = as.factor(time), fill = SymbolImienia)) +
        geom_bar(position = "dodge") +
        labs(x = "Hours", y = "Count", fill = "Gourmands") +
        scale_x_discrete(limits = as.factor(hours)) +
        scale_fill_manual(values = color_palette) +
        Fond
       
      
      
    }
    
  })
  output$tableAVG <- renderTable({
    peopleAvg_df1 <- df_1 %>%
      filter(day > input$date_range[1] &
               day < input$date_range[2]) %>%
      group_by(day, SymbolImienia) %>%
      mutate(SumOfKcal = sum(kcal)) %>%
      ungroup() %>%
      group_by(SymbolImienia) %>%
      summarise_at(c("SumOfKcal"), mean, na.rm = TRUE)
    
    
    # Srednia dla df_2
    peopleAvg_df2 <- df_2 %>%
      filter(day > input$date_range[1] &
               day < input$date_range[2]) %>%
  
      group_by(SymbolImienia) %>%
      mutate(
        AlcoCounts = sum(AlcoConsumed),
        CaffeineCounts = sum(caffeineConsumed)
      ) %>%
      summarise_at(c("steps", "liquids", "AlcoCounts", "CaffeineCounts"),
                   mean,
                   na.rm = TRUE)
    
    
    
    table <- merge(peopleAvg_df2, peopleAvg_df1, by = "SymbolImienia")
    table$SymbolImienia[1:3] <- c("First", "Second", "Third")
    table <- table %>%
      rename(
        "Gourmand"="SymbolImienia",
        "Average steps in a day" = "steps",
        "Average liquid intake [L]" = "liquids",
        "Days with alcohol consumption" = "AlcoCounts",
        "Days with caffeine consumption" = "CaffeineCounts",
        "Average kcals per day " = "SumOfKcal"
      )
    
  })
  
  ### ZAKŁADKA 3 ###
  
  output$plot_bm3title <- renderText({
    if (input$meal_bm3 == "lunch") {
      suffix = "es"
    } else {
      suffix = "s"
    }
    paste0("Amount of eaten ",
           input$chosen_macro_bm3,
           " in ",
           input$meal_bm3,
           suffix)
    
  })
  
  # Do dopracowania
  output$plot_bm3 <- renderPlot({
    formatted_times <- data.frame(formatted_times) %>%
      rename(time = formatted_times) %>%
      mutate(time = paste0(substr(time, 1, 2), ":00")) %>%
      distinct(time)
    
    
    df_1 %>% filter(SymbolImienia == get_name(input$person_bm3), meal == input$meal_bm3) %>%
      #group_by(time) %>%
      mutate(time = paste0(substr(time, 1, 2), ":00")) %>%
      group_by(time, meal) %>%
      #summarise(count = n(), .groups = "drop") %>%
      summarize(
        count = n(),
        kcal = mean(kcal, na.rm = TRUE),
        proteins = mean(proteins, na.rm = TRUE),
        fats = mean(fats, na.rm = TRUE),
        carbo = mean(carbo, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      right_join(formatted_times, by = "time") %>%
      mutate(
        kcal = coalesce(kcal, 0),
        proteins = coalesce(proteins, 0),
        fats = coalesce(fats, 0),
        carbo = coalesce(carbo, 0)
      ) %>%
      ggplot(aes(
        x = time,
        y = !!as.symbol(input$chosen_macro_bm3)
      )) +
      geom_segment(aes(
        x = time,
        xend = time,
        y = 0,
        yend = !!as.symbol(input$chosen_macro_bm3)
      )) +
      geom_point() +
      Fond
    
  })
  
  
  output$bubble_chart_bm3 <- renderPlotly({
    df_transformed <- df_1 %>%
      filter(SymbolImienia == get_name(input$person_2_bm3)) %>%
      filter(!is.na(time)) %>%
      mutate(time = paste0(substr(as.character(time), 1, 2), ":00")) %>%
      group_by(time, meal) %>%
      summarise(count = n(), .groups = "drop")
    
    plot_ly(
      data = df_transformed,
      x = ~ time,
      y = ~ meal,
      size = ~ count,
      type = 'scatter',
      mode = 'markers',
      marker = list(sizemode = 'diameter', opacity = 0.6),
      color = ~ meal,
      colors = color_palette
    ) %>% layout(
      xaxis = list(
        title = list(text = "Time", font = list(size = 14, family = "Arial Black")),
        tickfont = list(size = 12),
        tickangle = 45
      ),
      yaxis = list(
        title = list(text = "Type of meal", font = list(size = 14, family = "Arial Black")),
        tickfont = list(size = 12), 
        showticklabels = FALSE
      ),
      legend = list(
        title = list(text = "Meal", font = list(size = 14, family = "Arial Black")),
        font = list(size = 12)
      )
    )
  })
  
  # Violin kalorii z trash foodu w zależności od spożycia alkoholu
  output$boxplot_bm3 <- renderPlot({
    ggplot(df_2, aes(x = AlcoConsumed, y = trash_food)) +
      geom_boxplot(fill = color_palette[1]) +
      labs(x = "Alcohol consumed", y = "Calories from trash food") +
      Fond
    
  })
  
  # Tabela niezjedzonych posiłków w ciągu miesiąca
  output$table_bm3 <- renderTable({
    table<-df_1 %>%
      group_by(SymbolImienia, meal) %>%
      summarise(days_with_zero_kcal = sum(kcal == 0),
                .groups = "drop") %>%
      pivot_wider(
        names_from = meal,
        values_from = days_with_zero_kcal,
        values_fill = 0
      )
    table$SymbolImienia[1:3] <- c("First", "Second", "Third")
    table%>%
    rename("Gourmand"="SymbolImienia")
    
  })
  
  # Wykres stacked area kalorii, do dopracowania
  output$stacked_calorie_chart <- renderPlot({
    df_1 %>%
      mutate(SymbolImienia=sapply(df_1$SymbolImienia,get_name_reverse))%>%
      group_by(day, SymbolImienia) %>%
      summarise(total_kcal = sum(kcal, na.rm = TRUE),
                .groups = "drop") %>%
      ggplot(aes(x = day, y = total_kcal, color = SymbolImienia)) +
      geom_line(position = "identity", size = 2) +
      labs(
        x = "Day",
        y = "Calories",
        color = "Gourmand"
      ) +
      scale_color_manual(values = color_palette) +
      Fond
    
  })

  
}




shinyApp(ui = ui, server = server)
