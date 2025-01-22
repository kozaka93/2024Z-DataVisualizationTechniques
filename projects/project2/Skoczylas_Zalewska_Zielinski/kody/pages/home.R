# Wczytywanie ramek i funkcje pewne do ramek
zuzia_apps_df <- read.csv("data/nasz_dzienZuzia.csv",sep=";")
kasia_apps_df <- read.csv("data/nasz_dzienKasia.csv",sep=";")
milosz_apps_df <- read.csv("data/nasz_dzienMilosz.csv",sep=";")

process_daily_data <- function(data, user) {
  data <- data %>%
    filter(X!="Unlocked" & X!="Notifications"  & X!="Total time")%>% 
    pivot_longer(cols = -1, names_to = "date", values_to = "time_spent") %>%
    rename(application = 1) %>%
    mutate(
      date = as.Date(sub("^X", "", date), format = "%Y.%m.%d"),  
      time_spent = round(as.numeric(as.difftime(time_spent, format="%H:%M:%S", units="mins"))),
      user = user
    )
  return(data)
}
process_daily_data2 <- function(data, user) {
  data <- data %>%
    filter(X == "Unlocked" | X == "Notifications" | X == "Total time") %>%
    pivot_longer(cols = -1, names_to = "date", values_to = "value") %>%
    rename(application = 1) %>%
    mutate(
      date = as.Date(sub("^X", "", date), format = "%Y.%m.%d"),  
      
      value = ifelse(application == "Total time", 
                     round(as.numeric(as.difftime(value, format="%H:%M:%S", units="mins"))), 
                     as.numeric(value)),
      
      value = ifelse(application == "Notifications" | application == "Unlocked", 
                     as.numeric(value), 
                     value),
      
      user = user
    )
  return(data)
}

kasia_apps_df <- process_daily_data(kasia_apps_df, "Kasia")
zuzia_apps_df <- process_daily_data(zuzia_apps_df, "Zuzia")
milosz_apps_df <- process_daily_data(milosz_apps_df, "Milosz")

zuzia_apps_df1 <- read.csv("data/nasz_dzienZuzia.csv",sep=";")
kasia_apps_df1 <- read.csv("data/nasz_dzienKasia.csv",sep=";")
milosz_apps_df1 <- read.csv("data/nasz_dzienMilosz.csv",sep=";")

# Ostateczna ramka z dokładnością do aplikacji
apps_df <- bind_rows(kasia_apps_df, zuzia_apps_df, milosz_apps_df)

# Wczytanie aktywnosci
kasia_activities_df <- read.csv("data/kasia.csv")
zuzia_activities_df <- read.csv("data/zuzia.csv")
milosz_activities_df <- read.csv("data/milosz.csv")

kasia_activities_df$name <- "Kasia"
zuzia_activities_df$name <- "Zuzia"
milosz_activities_df$name <- "Milosz"

activities_df <- bind_rows(kasia_activities_df, zuzia_activities_df, milosz_activities_df)
activities_df <- activities_df %>%
  mutate(
    time.started = as.POSIXct(time.started, format = "%Y-%m-%d %H:%M:%S"),
    time.ended = as.POSIXct(time.ended, format = "%Y-%m-%d %H:%M:%S")
  )


homeUI <- function(id) {
  ns <- NS(id)  
  
  tags$div(
    tags$h3(
      style = "text-align: center; margin-bottom: 14px;",
      "Welcome to our app!"
    ),
    
    tags$div(
      tags$h2(
        style = "text-align: center; margin-top: 10px; font-size: 20px;",
        textOutput(ns("main_text"))
      )
    ),
    fluidRow(style = "height: 20px;"),
    tags$div(
      tags$h4(
        style="text-align: left;",
        "Here’s a brief overview of what each page offers:"
      )
    ),
    fluidRow(style = "height: 15px;"),
    box(
      title = tags$span(
        tagList(
          shiny::icon("mobile"),  # Icon for "Apps"
          "Apps"
        ), 
        style = "background-color: #ff7f0e; color: white; padding: 10px; border-radius: 10px; width: 100%; display: inline-block; box-sizing: border-box;"
      ),
      solidHeader = TRUE, 
      width = 12,
      style = "background-color: #f9f9f9; border: 2px solid #ff7f0e; border-radius: 10px; padding: 10px;", 
      tags$div(
        style = "font-size: 16px; color: black;",
        "Here you can explore detailed data about the most frequently used applications, 
    including usage time and frequency. This page reveals which apps are the most popular for each of us."
      )
    ),
    fluidRow(style = "height: 15px;"),
    box(
      title = tags$span(
        tagList(
          shiny::icon("running"),  # Icon for "Activities"
          "Activities"
        ),
        style = "background-color: #dc143c; color: white; padding: 10px; border-radius: 10px; width: 100%; display: inline-block; box-sizing: border-box;"
      ),
      solidHeader = TRUE, 
      width = 12,
      style = "background-color: #f9f9f9; border: 2px solid #dc143c; border-radius: 10px; padding: 10px;",
      tags$div(
        style = "font-size: 16px; color: black;",
        "Dive into a visual timeline of daily activities. This page shows how each of us spends their day. 
      You can also easily compare the differences between our days on this page."
      )
    ),
    fluidRow(style = "height: 15px;"),
    box(
      title = tags$span(
        tagList(
          shiny::icon("calendar-alt"),  # Icon for "My Day"
          "My Day"
        ),
        style = "background-color: #158cba; color: white; padding: 10px; border-radius: 10px; width: 100%; display: inline-block; box-sizing: border-box;"
      ),
      solidHeader = TRUE, 
      width = 12,
      style = "background-color: #f9f9f9; border: 2px solid #158cba; border-radius: 10px; padding: 10px;",
      tags$div(
        style = "font-size: 16px; color: black;",
        "Access statistics for each user, including our daily activities and app usage. 
      This page provides a deeper understanding of each person’s habits."
      )
    ),
    fluidRow(style = "height: 35px;"),
    tags$div(
      tags$h4(
        style="text-align: center; font-weight: bold;",
        "Authors"
      )
    ),
    tags$div(
      style = "text-align: center;",
      tags$div(
        style = "display: inline-block; margin: 10px 20px 20px 20px;",
        shiny::icon("user", style = "font-size: 60px; color: #158cba;"),
        tags$div("Miłosz", style = "margin-top: 20px; font-weight: bold;")
      ),
      tags$div(
        style = "display: inline-block; margin: 10px 20px 20px 20px;",
        shiny::icon("user", style = "font-size: 60px; color: #ff7f0e;"),
        tags$div("Kasia", style = "margin-top: 20px; font-weight: bold;")
      ),
      tags$div(
        style = "display: inline-block; margin: 10px 20px 20px 20px;",
        shiny::icon("user", style = "font-size: 60px; color: #dc143c;"),
        tags$div("Zuzia", style = "margin-top: 20px; font-weight: bold;")
      )
    ),
    fluidRow(style = "height: 20px;"),
    
  )
}


homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$main_text <- renderText({"
        This project showcases data we collected from 9th December to 12th January. During this time, we carefully tracked our daily activities and phone usage. 
      The app is designed to provide meaningful insights into how time is spent throughout the day, highlighting patterns in app usage and daily routines."
      
    })
    
    output$desc_text <- renderUI({
      HTML("
    <div style='text-align: left;'>
      <h4>Here’s a brief overview of what each page offers:</h4>
      
      
    </div>
  ")
    })
    
  })
}