library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(dplyr)
library(plotly)
library(tidyr)
library(calendR)
library(ggplot2)
library(lubridate)
library(forcats)
library(shinycssloaders)
library(htmlwidgets)

# data setup

df_ada <- read.csv('data/TWD_proj2_dataset_Ada.csv') %>%
  mutate(data = as.Date(data, '%d.%m.%Y'))
df_hania <- read.csv("data/TWD_proj2_dataset_Hania.csv") %>%
  mutate(data = as.Date(data, '%d.%m.%Y'))
df_martyna <- read.csv("data/TWD_proj2_dataset_Martyna.csv") %>%
  mutate(data = as.Date(data, '%d.%m.%Y'))

df_ects <- read.csv('data/TWD_proj2_dataset_ECTS.csv')

#-------------------------------------------------------------------------------
# tworzenie zestawu danych zawierajacego zera w dni gdzie danej aktywnosci nie bylo
df_ada_zeroes <- df_ada %>%
  group_by(data, aktywnosc, przedmiot) %>%
  summarise(sum_time = sum(czas_total_min)) %>%
  ungroup() %>%
  pivot_wider(names_from = data, values_from = sum_time) %>%
  mutate_all(~ replace(., is.na(.), 0)) %>%
  pivot_longer(
    cols = !c(aktywnosc, przedmiot),
    names_to = "data",
    values_to = "sum_time"
  ) %>%
  mutate(data = as.Date(data, '%Y-%m-%d'), osoba = "Ada")

df_hania_zeroes <- df_hania %>%
  group_by(data, aktywnosc, przedmiot) %>%
  summarise(sum_time = sum(czas_total_min)) %>%
  ungroup() %>%
  pivot_wider(names_from = data, values_from = sum_time) %>%
  mutate_all(~ replace(., is.na(.), 0)) %>%
  pivot_longer(
    cols = !c(aktywnosc, przedmiot),
    names_to = "data",
    values_to = "sum_time"
  ) %>%
  mutate(data = as.Date(data, '%Y-%m-%d'), osoba = "Hania")

df_martyna_zeroes <- df_martyna %>%
  group_by(data, aktywnosc, przedmiot) %>%
  summarise(sum_time = sum(czas_total_min)) %>%
  ungroup() %>%
  pivot_wider(names_from = data, values_from = sum_time) %>%
  mutate_all(~ replace(., is.na(.), 0)) %>%
  pivot_longer(
    cols = !c(aktywnosc, przedmiot),
    names_to = "data",
    values_to = "sum_time"
  ) %>%
  mutate(data = as.Date(data, '%Y-%m-%d'), osoba = "Martyna")

#-----------------------------------------------------------------------------
# tworzenie zestawu danych z zerami i ze wszystkimi osobami (do combined charts)
df_everyone_zeroes <- df_ada_zeroes %>% bind_rows(df_hania_zeroes) %>% bind_rows(df_martyna_zeroes)

#-----------------------------------------------------------------------------
# zestaw danych z zerami i pelnym zakresem dat (do konca stycznia)
# Tworzenie pełnego zakresu dat
full_dates <- seq(as.Date("2024-12-01"), as.Date("2025-01-31"), by = "day")

# Tworzenie wszystkich możliwych kombinacji dat, aktywności i przedmiotów
full_combinations <- expand.grid(
  data = full_dates,
  aktywnosc = unique(df_everyone_zeroes$aktywnosc),
  przedmiot = unique(df_everyone_zeroes$przedmiot)
)

# Łączenie z oryginalnymi danymi i wypełnianie brakujących wartości zerami
df_ada_zera_p <- full_combinations %>%
  left_join(df_ada, by = c("data", "aktywnosc", "przedmiot")) %>%
  mutate(czas_total_min = replace_na(czas_total_min, 0)) %>%
  group_by(data, aktywnosc, przedmiot) %>%
  summarise(sum_time = sum(czas_total_min)) %>%
  ungroup()

df_hania_zera_p <- full_combinations %>%
  left_join(df_hania, by = c("data", "aktywnosc", "przedmiot")) %>%
  mutate(czas_total_min = replace_na(czas_total_min, 0)) %>%
  group_by(data, aktywnosc, przedmiot) %>%
  summarise(sum_time = sum(czas_total_min)) %>%
  ungroup()

df_martyna_zera_p <- full_combinations %>%
  left_join(df_martyna, by = c("data", "aktywnosc", "przedmiot")) %>%
  mutate(czas_total_min = replace_na(czas_total_min, 0)) %>%
  group_by(data, aktywnosc, przedmiot) %>%
  summarise(sum_time = sum(czas_total_min)) %>%
  ungroup()

#-------------------------------------------------------------------------------

# UI for Overview page
overview <- fluidPage(
  
  titlePanel("How much of our time is preoccupied by uni?"),
  br(),
  fluidRow(
    shinycssloaders::withSpinner(valueBoxOutput("overview_uniClassesBox", width = 3)),
    shinycssloaders::withSpinner(valueBoxOutput("overview_breaksBox", width = 3)),
    shinycssloaders::withSpinner(valueBoxOutput("overview_commuteBox", width = 3)),
    shinycssloaders::withSpinner(valueBoxOutput("overview_studyBox", width = 3))
  ),
  br(),
  fluidRow(
    column(
      width = 3,
      box(
        status = "warning",
        checkboxInput("overview_includeIndependentStudyTime", "include independent study time", FALSE),
        radioButtons(
          inputId = "overview_chartType",
          label = "Choose plot variant:",
          choices = list(
            "total time spent on uni classes, breaks and commute" = "total_time",
            "proportion of time spent on uni classes, breaks and commute (normalised to 100%)" = "percentages"
          )
        ),
        width = 200
      )
    ),
    column(
      width = 9,
      box(title = "Time spent on uni-related activities on a regular working day", status = "primary", shinycssloaders::withSpinner(plotlyOutput("overview_stackedBarChart")), width = 500)
      #shinycssloaders::withSpinner(plotlyOutput("overview_stackedBarChart")),
    )
  )
  
)

# UI for Studying page
studying <- fluidPage(
  titlePanel("How do we study?"),
  br(),
  fluidRow(
    column(
      width = 3,
      box(
        status = "warning",
        selectInput(
          "przedmiot",
          "Choose subject:",
          c("TWD", "ZPOiF", "Fizyka", "jezyki", "RP", "MN", "ASD", "total"),
          selected = "TWD"
        ),
        width = 200
      )
    ),
    column(
      width = 9,
      box(
        title = "Calendar", status = "primary",
        shinycssloaders::withSpinner(plotOutput("studying_subjectsHeatmap")),
        width = 500
      )
    )
  ),
  
  br(),
  
  fluidRow(
    column(
      width = 3,
      box(
        status = "warning",
        selectInput(
          "suma",
          "Choose data:",
          c("Sum", "Uni", "Home"),
          selected = "Sum"
        ),
        width = 200
      )
    ),
    column(
      width = 9,
      box(title = "Total studying time for subjects", status = "primary", shinycssloaders::withSpinner(plotlyOutput("studying_subjectsTotalBarplot")), width = 500)
    )
  ),
  
  br(),
  
  fluidRow(
    column(
      width = 3,
      box(
        status = "warning",
        dateRangeInput(
          "weekday_date",
          label = 'Select date range:',
          start = "2024-12-05",
          end = "2025-01-19",
          min = "2024-12-05",
          max = "2025-01-19",
          format = "yyyy-mm-dd"
        ),
        width = 200
      )
    ),
    column(
      width = 9,
      box(title = "Distribution of study time on particular days of the week", status = "primary", shinycssloaders::withSpinner(plotlyOutput("studying_weekdayBoxplot")), width = 500)
    )
  ), 
  
  br(),
  
  fluidRow(
    column(
      width = 3,
      box(
        status = "warning",
        radioButtons(
          inputId = "chart_type_ects",
          label = "Choose plot variant:",
          choices = list(
            "Average weekly studying time by person, compared to ECTS points" = "compare_by_person",
            "Time spent on each subject per 1 ECTS point" = "all_subjects_per_1_ects"
          )
        ),
        conditionalPanel(
          condition = "input.chart_type_ects == 'compare_by_person'",
          selectInput(
            "subject_ects",
            "Choose subject:",
            c("TWD", "ZPOiF", "Fizyka", "jezyki", "RP", "MN", "ASD", "total"),
            selected = "total"
          )
        ),
        dateRangeInput(
          "date_range_ects",
          label = 'Select date range:',
          start = "2024-12-05",
          end = "2025-01-19",
          min = "2024-12-05",
          max = "2025-01-19",
          format = "yyyy-mm-dd"
        ),
        width = 200
      )
    ),
    column(
      width = 9,
      box(title = "Average weekly studying time compared to the amount of time \nspecified by ECTS points", status = "primary", shinycssloaders::withSpinner(plotlyOutput("studying_ectsPlot")), width = 500)
    )
  )
)

#UI for Leisure page
leisure <- fluidPage(
  titlePanel("Do we even have time for anything else?"),
  br(),
  fluidRow(
    shinycssloaders::withSpinner(valueBoxOutput("leisure_familyTimeBox")),
    shinycssloaders::withSpinner(valueBoxOutput("leisure_hobbyBox")),
    shinycssloaders::withSpinner(valueBoxOutput("leisure_friendsBox"))
  ),
  br(),
  fluidRow(
    box("Family time", width = 4,
        shinycssloaders::withSpinner(plotlyOutput("leisure_familyTimeAnimation"))),
    box("Hobby", width = 4,
        shinycssloaders::withSpinner(plotlyOutput("leisure_hobbyAnimation"))),
    box("Friends", width = 4,
        shinycssloaders::withSpinner(plotlyOutput("leisure_friendsAnimation")))
  ),
  br(),
  
  fluidRow(
    column(
      width = 3,
      box(
        status = "warning",
        dateRangeInput(
          "leisure_line_date",
          label = 'Select date range:',
          start = "2024-12-05",
          end = "2025-01-19",
          min = "2024-12-05",
          max = "2025-01-19",
          format = "yyyy-mm-dd"
        ),
        width = 200
      )
    ),
    column(
      width = 9,
      box(title = "Time spent on each activity", status = "primary", shinycssloaders::withSpinner(plotlyOutput("leisure_lineChart")), width = 500)
    )
  ),
  br(),
  
  fluidRow(
    column(
      width = 3,
      box(
        status = "warning",
        dateRangeInput(
          "sleeping_date",
          label = 'Select date range:',
          start = "2024-12-05",
          end = "2025-01-19",
          min = "2024-12-05",
          max = "2025-01-19",
          format = "yyyy-mm-dd"
        ),
        width = 200
      )
    ),
    column(
      width = 9,
      box(title = "Time spent sleeping", status = "primary", shinycssloaders::withSpinner(plotlyOutput("leisure_sleepChart")), width = 500)
    )
  )
)

# Dashboard UI
shinyUI <- dashboardPage(
  # dashboardHeader(title = tags$a(tags$img(
  #   src = 'logo.png', width = '100%'
  # ))),
  
  dashboardHeader(title = tags$a(tags$img(
    src = 'logo.png', width = '100%'
  ))#,
  
  # dropdownMenu(type = "messages",
  #              icon = icon("circle-user"),
  #              headerText = "About us",
  #              badgeStatus = NULL,
  #              messageItem(
  #                from = "New User",
  #                message = "From 5th December 2024 until 19th of January 2025
  #                we have been collecting data about how much time we spend 
  #                on various activities during the day. The focus of our work 
  #                is the time we've spent on uni-related activities, be it class time, 
  #                independent studying, commute to uni, or breaks - we've tried to put 
  #                it into perspective by comparing it between subjects, people, 
  #                ECTS-standardized studying time, and showing it in proportion 
  #                to all the other activities in our lives.",
  #                icon = icon("question"),
  #                time = "13:45"
  #              )
  # 
  #)
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = 'tabs',
      menuItem(
        "Overview",
        tabName = 'overview',
        icon = icon("calendar-days")
      ),
      menuItem(
        "Studying",
        tabName = 'studying',
        icon = icon("business-time")
      ),
      menuItem("Leisure", tabName = 'leisure', icon = icon("mug-saucer")),
      uiOutput('sidebar')
    ),
    # Add CSS for fixed sidebar
    tags$style(
      HTML(
        "
      .main-sidebar {
        position: fixed;
        height: 100%;
        top: 0;
        left: 0;
      }
      .content-wrapper, .right-side {
        margin-left: 250px;
      }
 
    "
      )
    )
  ),
  
  dashboardBody(# Add CSS for fixed header
    tags$style(
      HTML(
        "
      .main-header {
        position: fixed;
        width: 100%;
        top: 0;
        z-index: 1000;
      }
      .content-wrapper {
        margin-top: 50px;
      }
    "
      )
    ), tabItems(
      tabItem(tabName = 'overview', overview),
      tabItem(tabName = 'studying', studying),
      tabItem(tabName = 'leisure', leisure)
    )),
  
  # Link do zewnętrznego motywu Lumen
  tags$head(
    tags$link(rel = "stylesheet", href = "https://stackpath.bootstrapcdn.com/bootswatch/4.3.1/readable/bootstrap.min.css")
    #tags$link(rel = "stylesheet", href = "https://stackpath.bootstrapcdn.com/bootswatch/4.3.1/yeti/bootstrap.min.css")
  )
  
)

# Server function
server <- function(input, output) {
  # User selection in sidebar
  output$sidebar <- renderUI({
    selectInput("user",
                "Select user data:",
                c("Ada", "Hania", "Martyna"),
                selected = "Ada")
  })
  
  output$overview_uniClassesBox <- renderValueBox({
    current_df <- df_ada_zeroes
    if (input$user == "Hania") {
      current_df <- df_hania_zeroes
    } else if (input$user == "Martyna") {
      current_df <- df_martyna_zeroes
    }
    
    current_df <- current_df %>%
      filter(aktywnosc == "zajecia_na_uczelni")
    
    uni_classes_time <- round(sum(current_df$sum_time)/60, 2)
    
    valueBox(
      paste0(uni_classes_time, "h"), "Spent in classes", icon = icon("school"),
      color = "purple"
    )
  })
  
  output$overview_breaksBox <- renderValueBox({
    current_df <- df_ada_zeroes
    if (input$user == "Hania") {
      current_df <- df_hania_zeroes
    } else if (input$user == "Martyna") {
      current_df <- df_martyna_zeroes
    }
    
    current_df <- current_df %>%
      filter(aktywnosc == "przerwy_na_uczelni")
    
    breaks_time <- round(sum(current_df$sum_time)/60, 2)
    
    valueBox(
      paste0(breaks_time, "h"), "Spent on breaks", icon = icon("bell", class = "fa-solid"),
      color = "maroon"
    )
  })
  
  output$overview_commuteBox <- renderValueBox({
    current_df <- df_ada_zeroes
    if (input$user == "Hania") {
      current_df <- df_hania_zeroes
    } else if (input$user == "Martyna") {
      current_df <- df_martyna_zeroes
    }
    
    current_df <- current_df %>%
      filter(aktywnosc == "dojazdy_na_uczelnie")
    
    commute_time <- round(sum(current_df$sum_time)/60, 2)
    
    valueBox(
      paste0(commute_time, "h"), "Spent on commute", icon = icon("bus"),
      color = "olive"
    )
  })
  
  output$overview_studyBox <- renderValueBox({
    current_df <- df_ada_zeroes
    if (input$user == "Hania") {
      current_df <- df_hania_zeroes
    } else if (input$user == "Martyna") {
      current_df <- df_martyna_zeroes
    }
    
    current_df <- current_df %>%
      filter(aktywnosc == "nauka")
    
    study_time <- round(sum(current_df$sum_time)/60, 2)
    
    valueBox(
      paste0(study_time, "h"), "Spent on independent studying", icon = icon("book"),
      color = "orange"
    )
  })
  
  output$overview_stackedBarChart <- renderPlotly({
    dni_wolne <- c(
      "2024-12-23",
      "2024-12-24",
      "2024-12-25",
      "2024-12-26",
      "2024-12-27",
      "2024-12-30",
      "2024-12-31",
      "2025-01-01",
      "2025-01-06"
    )
    
    p <- df_everyone_zeroes %>%
      select(-przedmiot)
    
    if(input$overview_includeIndependentStudyTime == TRUE) {
      p <- p %>% filter(
        aktywnosc == "zajecia_na_uczelni" |
          aktywnosc == "dojazdy_na_uczelnie" |
          aktywnosc == "przerwy_na_uczelni" | 
          aktywnosc == "nauka")
    } else{
      p <- p %>% filter(
        aktywnosc == "zajecia_na_uczelni" |
          aktywnosc == "dojazdy_na_uczelnie" |
          aktywnosc == "przerwy_na_uczelni")
    }
    
    p <- p %>%
      mutate(weekday = weekdays(as.Date(data))) %>%
      filter(weekday != "Saturday" & weekday != "Sunday") %>%
      filter(!(data %in% dni_wolne)) %>%
      group_by(osoba, data, aktywnosc) %>%
      summarise(sum_time = sum(sum_time)) %>%
      ungroup() %>%
      group_by(osoba, aktywnosc) %>%
      summarise(avg_time = mean(sum_time)) %>%
      rename(activity = aktywnosc, person = osoba) %>% 
      mutate(activity = case_when(activity == "nauka" ~ "independent studying",
                                  activity == "przerwy_na_uczelni" ~ "breaks at uni",
                                  activity == "zajecia_na_uczelni" ~ "uni classes",
                                  activity == "dojazdy_na_uczelnie" ~ "commute to uni")) %>% 
      mutate(activity = fct_relevel(
        activity,
        "breaks at uni",
        "commute to uni",
        "uni classes",
        "independent studying"
      )) %>% 
      ggplot(aes(fill = activity, y = avg_time, x = person))
    
    
    
    if (input$overview_chartType == "total_time") {
      p <- p +
        geom_bar(position = "stack", stat = "identity") +
        labs(title = "",
             # title = "Average time spent on uni-related activities on a regular working day", 
             x = "", 
             y = "Average daily time (minutes)") +
        scale_y_continuous(expand = expand_scale(mult = c(0, 0.1)))
    } else {
      p <- p + geom_bar(position = "fill", stat = "identity") + 
        geom_text(aes(label = paste0(round(avg_time / ave(avg_time, person, FUN = sum) * 100, 1), "%")),
                  position = position_fill(vjust = 0.5),
                  size = 3,
                  color = "white") +
        labs(title = "", 
             # title = "Average proportion of time spent on uni-related activities on a regular working day", 
             x = "", 
             y = "Fraction of uni-related time") +
        scale_y_continuous(expand = expand_scale(mult = c(0, 0)),
                           labels = scales::percent_format())
      
    }
    
    p <- p +
      scale_fill_manual(values = c("#d81b60", "#3d9970", "#605ca8", "#ff851b")) +
      theme_gray() +
      theme(
        panel.background = element_rect(fill = "#ECEFF3", color = NA),
        plot.title = element_text(hjust = 0.5),
        plot.title.position = "none",
        plot.margin = margin(t = 20)
      )
    
    ggplotly(p) %>% 
      layout(legend = list(itemclick = FALSE, itemdoubleclick = FALSE), margin=list(l=60, t=0)) %>% # disable legend clicks bc they break bar charts; decrease upper margin, increase left margin so axis text doesnt touch ais label
      config(displayModeBar = FALSE)
  })
  
  output$studying_subjectsHeatmap <- renderPlot({
    current_df <- df_ada_zera_p
    if (input$user == "Hania") {
      current_df <- df_hania_zera_p
    } else if (input$user == "Martyna") {
      current_df <- df_martyna_zera_p
    }
    
    # Filtrowanie na podstawie aktywności i wybranego przedmiotu
    if (input$przedmiot == "total"){
      current_df <- current_df %>%
        filter(aktywnosc == "nauka") %>%
        group_by(data) %>%
        summarise(sum_time = sum(sum_time))
    }
    else {
      current_df <- current_df %>%
        filter(aktywnosc == "nauka") %>%
        filter(przedmiot == input$przedmiot)
    }
    
    
    
    # Calendar
    calendR(year = 2024,
            month = 12,
            title = "",
            title.size = 1,
            from = "2024-12-01",
            to = "2025-01-31",
            special.days = current_df$sum_time,
            gradient = TRUE,
            weeknames = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
            monthnames = c("December", "January"),
            start = "M",
            
            low.col = "white",
            special.col = "#d81b60",
            legend.pos = "bottom",
            legend.title = "Minutes spent on studying",
            bg.col = "#ECEFF3"
            #bg.col = "white",
    )
  })
  
  output$studying_subjectsTotalBarplot <- renderPlotly({
    current_df <- df_ada_zera_p
    if (input$user == "Hania") {
      current_df <- df_hania_zera_p
    } else if (input$user == "Martyna") {
      current_df <- df_martyna_zera_p
    }
    
    current_df1 <- current_df %>%
      filter(aktywnosc == "nauka") %>%
      filter(przedmiot != "") %>%
      group_by(przedmiot) %>%
      summarise(total_czas_przedmiot = sum(sum_time, na.rm = TRUE)) %>%
      ungroup()
    
    current_df2 <- current_df %>%
      filter(aktywnosc == "zajecia_na_uczelni") %>%
      filter(przedmiot != "") %>%
      group_by(przedmiot) %>%
      summarise(total_czas_przedmiot = sum(sum_time, na.rm = TRUE)) %>%
      ungroup()
    
    current_df_sum <- current_df %>%
      filter(aktywnosc == c("nauka", "zajecia_na_uczelni")) %>%
      filter(przedmiot != "") %>%
      group_by(przedmiot) %>%
      summarise(total_czas_przedmiot = sum(sum_time, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(
        przedmiot == "ASD" |
          przedmiot == "MN" |
          przedmiot == "RP" |
          przedmiot == "TWD" |
          przedmiot == "ZPOiF" |
          przedmiot == "jezyki" |
          przedmiot == "Fizyka"
      ) %>%
      mutate(przedmiot = fct_relevel(
        przedmiot,
        "ASD",
        "MN",
        "RP",
        "TWD",
        "ZPOiF",
        "jezyki",
        "Fizyka"
      ))
    
    current_df <- current_df_sum
    if (input$suma == "Uni") {
      current_df <- current_df2
    } else if (input$suma == "Home") {
      current_df <- current_df1
    }
    
    # Dodanie kolumny z informacją, które przedmioty są w top 3
    current_df <- current_df %>%
      mutate(top_3 = ifelse(
        total_czas_przedmiot %in% sort(total_czas_przedmiot, decreasing = TRUE)[1:3],
        "Top 3",
        "Other"
      )) %>% 
      rename(subject = przedmiot, total_studying_time = total_czas_przedmiot, is_top_3 = top_3) %>% 
      filter(total_studying_time != 0)
    
    p <- ggplot(current_df,
                aes(x = subject, y = total_studying_time, fill = is_top_3)) +
      geom_bar(stat = "identity") +  # stat = "identity" bo mamy już zgrupowane dane
      scale_fill_manual(
        values = c("Top 3" = "#3d9970", "Other" = "#D9D9DA"),
        breaks = c("Top 3", "Other")
      ) +
      labs(x = "Subject", y = "Total study time (minutes)") +
      guides(fill = guide_legend(title = "Status")) +
      theme_grey() +
      theme(
        panel.background = element_rect(fill = "#ECEFF3", color = NA),
        #plot.title = element_text(hjust = 0.5),
        #plot.title.position = "plot",
        #plot.margin = margin(t = 20)
      ) +
      scale_y_continuous(expand = expand_scale(mult = c(0, 0.1)))
    
    ggplotly(p) %>% 
      layout(margin=list(l=60, t=0)) %>% # decrease upper margin, increase left margin so axis text doesnt touch axis label
      config(displayModeBar = FALSE)
  })
  
  output$studying_weekdayBoxplot <- renderPlotly({
    current_df <- df_ada_zeroes
    if (input$user == "Hania") {
      current_df <- df_hania_zeroes
    } else if (input$user == "Martyna") {
      current_df <- df_martyna_zeroes
    }
    
    if (input$weekday_date[1] <= input$weekday_date[2]) {
      current_df <- current_df %>%
        filter(data >= input$weekday_date[1],
               data <= input$weekday_date[2])
    }
    
    current_df <- current_df %>%
      select(-osoba) %>%
      filter(aktywnosc == "nauka") %>%
      filter(przedmiot != "") %>%
      group_by(data) %>%  # Grupujemy po dniu tygodnia
      summarise(total_czas_dzien = sum(sum_time, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(dzien_tygodnia = wday(data, label = TRUE, abbr = FALSE, locale = Sys.setlocale("LC_ALL","English")))
    
    
    p <- ggplot(current_df, aes(x = dzien_tygodnia, y = total_czas_dzien)) +
      geom_boxplot(color = "#db5502", fill = "#ff851b") +  # Tworzymy boxplot
      labs(x = "Day of the week", y = "Total study time (minutes)") +
      theme_grey() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "#ECEFF3", color = NA),
        # plot.title = element_text(hjust = 0.5),
        # plot.title.position = "plot",
        # plot.margin = margin(t = 20)
      )
    
    ggplotly(p)%>% 
      layout(margin=list(l=60, t=0)) %>% # decrease upper margin, increase left margin so axis text doesnt touch axis label
      config(displayModeBar = FALSE)
  })
  
  output$studying_ectsPlot <- renderPlotly({
    TOTAL_NUM_WEEKS = 18
    TIME_PER_ECTS_MIN = 25 # hrs
    TIME_PER_ECTS_MAX = 30 # hrs
    TOTAL_ECTS = 30
    
    if (input$chart_type_ects == "compare_by_person") {
      current_df <- df_everyone_zeroes # bc it's a combined chart
    } else{
      current_df <- df_ada_zeroes
      if (input$user == "Hania") {
        current_df <- df_hania_zeroes
      } else if (input$user == "Martyna") {
        current_df <- df_martyna_zeroes
      }
    }
    
    if (input$date_range_ects[1] <= input$date_range_ects[2]) {
      current_df <- current_df %>%
        filter(data >= input$date_range_ects[1],
               data <= input$date_range_ects[2])
    }
    
    current_df <- current_df %>%
      filter(aktywnosc == "nauka" |
               aktywnosc == "zajecia_na_uczelni") %>%
      filter(
        przedmiot == "ASD" |
          przedmiot == "MN" |
          przedmiot == "RP" |
          przedmiot == "TWD" |
          przedmiot == "ZPOiF" |
          przedmiot == "jezyki" |
          przedmiot == "Fizyka"
      ) %>%
      mutate(aktywnosc = "actual_studying_time") %>%
      group_by(data, aktywnosc, przedmiot, osoba) %>%
      summarise(total_day_time = sum(sum_time)) %>%
      ungroup() %>%
      group_by(aktywnosc, przedmiot, osoba) %>%
      summarise(avg_week_time = 7 * mean(total_day_time) / 60) %>%
      ungroup() %>%
      select(-aktywnosc) %>%
      left_join(df_ects, by = "przedmiot") %>%
      mutate(min_ects_time = ECTS * TIME_PER_ECTS_MIN / TOTAL_NUM_WEEKS) %>%
      mutate(max_ects_time = ECTS * TIME_PER_ECTS_MAX / TOTAL_NUM_WEEKS) %>%
      mutate(przedmiot = fct_relevel(
        przedmiot,
        "ASD",
        "MN",
        "RP",
        "TWD",
        "ZPOiF",
        "jezyki",
        "Fizyka"
      ))
    
    if (input$chart_type_ects == "compare_by_person") {
      current_ects = TOTAL_ECTS
      
      if (input$subject_ects != "total") {
        current_ects <- df_ects %>%
          filter(przedmiot == input$subject_ects) %>%
          pull(ECTS)
        
        current_df <- current_df %>%
          filter(przedmiot == input$subject_ects) %>%
          select(osoba, avg_week_time)
      } else {
        current_df <- current_df %>%
          mutate(aktywnosc = "nauka") %>%
          group_by(aktywnosc, osoba) %>%
          summarise(avg_week_time = sum(avg_week_time)) %>%
          ungroup() %>%
          select(-aktywnosc)
      }
      
      current_df <- current_df %>%
        add_row(
          osoba = "min_ects_time",
          avg_week_time = current_ects * TIME_PER_ECTS_MIN / TOTAL_NUM_WEEKS
        ) %>%
        add_row(
          osoba = "max_ects_time",
          avg_week_time = current_ects * TIME_PER_ECTS_MAX / TOTAL_NUM_WEEKS
        ) %>%
        rename("actual_studying_time" = "avg_week_time") %>%
        mutate(osoba = fct_relevel(
          osoba,
          "Ada",
          "Hania",
          "Martyna",
          "min_ects_time",
          "max_ects_time"
        )) %>%
        mutate(
          column_color = case_when(
            osoba == "Ada" | osoba == "Hania" | osoba == "Martyna" ~ "basic",
            osoba == "min_ects_time" ~ "min_color",
            osoba == "max_ects_time" ~ "max_color"
          )
        ) %>% 
        rename(person = osoba, total_studying_time = actual_studying_time)
      
      p <- ggplot(current_df,
                  aes(x = person, y = total_studying_time, fill = column_color)) +
        geom_col() +
        labs(x = "", y = "Total studying time (hours)") +
        scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
        scale_x_discrete(
          labels = c(
            "Ada" = "Ada's actual \nstudying time",
            "Hania" = "Hania's actual \nstudying time",
            "Martyna" = "Martyna's actual \nstudying time",
            "min_ects_time" = "minimum time \nas per ECTS",
            "max_ects_time" = "maximum time \nas per ECTS"
          )
        ) +
        scale_fill_manual(
          values = c(
            "basic" = "#605ca8",
            "min_color" = "#d81b60",
            "max_color" = "#3d9970"
          ),
          breaks = c("basic", "min_color", "max_color")
        )
      
    } else if (input$chart_type_ects == "all_subjects_per_1_ects") {
      current_df <- current_df %>%
        select(-osoba) %>%
        mutate(avg_week_time_per_ects = avg_week_time / ECTS) %>%
        select(przedmiot, avg_week_time_per_ects)
      
      min_ects_time = TIME_PER_ECTS_MIN / TOTAL_NUM_WEEKS
      max_ects_time = TIME_PER_ECTS_MAX / TOTAL_NUM_WEEKS
      
      
      p <- ggplot(current_df, aes(x = przedmiot, y = avg_week_time_per_ects)) +
        geom_col(fill = "#605ca8") +
        labs(x = "Subject", y = "Total studying time per each ECTS point (hours)") +
        scale_y_continuous(expand = expand_scale(mult = c(0, 0.1)))
      
      p <- p +
        geom_hline(
          yintercept = min_ects_time,
          linetype = "dashed",
          color = "#d81b60",
          size = 0.5
        ) +
        geom_hline(
          yintercept = max_ects_time,
          linetype = "dashed",
          color = "#3d9970",
          size = 0.5
        ) +
        geom_text(
          aes(6.5, min_ects_time),
          label = "minimum time \nper one ECTS",
          colour = "#d81b60",
          vjust = -1
        ) +
        geom_text(
          aes(6.5, max_ects_time),
          label = "maximum time \nper one ECTS",
          colour = "#3d9970",
          vjust = -1
        )
    }
    
    p <- p +
      theme_grey() +
      theme(
        panel.background = element_rect(fill = "#ECEFF3", color = NA),
        #plot.title = element_text(hjust = 0.5),
        #plot.title.position = "plot",
        #plot.margin = margin(t = 20),
        legend.position = "none"
      )
    
    
    ggplotly(p)%>% 
      layout(margin=list(l=60, t=0)) %>% # decrease upper margin, increase left margin so axis text doesnt touch axis label
      config(displayModeBar = FALSE)
  })
  
  output$leisure_familyTimeBox <- renderValueBox({
    current_df <- df_ada_zeroes
    if (input$user == "Hania") {
      current_df <- df_hania_zeroes
    } else if (input$user == "Martyna") {
      current_df <- df_martyna_zeroes
    }
    
    current_df <- current_df %>%
      filter(aktywnosc == "czas_z_rodzina")
    
    family_time <- round(sum(current_df$sum_time)/60, 2)
    
    valueBox(
      paste0(family_time, "h"), "Spent with family", icon = icon("house"),
      color = "purple"
    )
  })
  
  output$leisure_hobbyBox <- renderValueBox({
    current_df <- df_ada_zeroes
    if (input$user == "Hania") {
      current_df <- df_hania_zeroes
    } else if (input$user == "Martyna") {
      current_df <- df_martyna_zeroes
    }
    
    current_df <- current_df %>%
      filter(aktywnosc == "hobby")
    
    hobby_time <- round(sum(current_df$sum_time)/60, 2)
    
    valueBox(
      paste0(hobby_time, "h"), "Spent on hobby", icon = icon("palette"),
      color = "maroon"
    )
  })
  
  output$leisure_friendsBox <- renderValueBox({
    current_df <- df_ada_zeroes
    if (input$user == "Hania") {
      current_df <- df_hania_zeroes
    } else if (input$user == "Martyna") {
      current_df <- df_martyna_zeroes
    }
    
    current_df <- current_df %>%
      filter(aktywnosc == "spotkania_towarzyskie")
    
    hobby_time <- round(sum(current_df$sum_time)/60, 2)
    
    valueBox(
      paste0(hobby_time, "h"), "Spent with friends", icon = icon("people-group"),
      color = "olive"
    )
  })
  
  output$leisure_familyTimeAnimation <- renderPlotly({
    
    accumulate_by <- function(dat, var) {
      var <- lazyeval::f_eval(var, dat)
      lvls <- plotly:::getLevels(var)
      dats <- lapply(seq_along(lvls), function(x) {
        cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
      })
      dplyr::bind_rows(dats)
    }
    
    current_df <- df_ada_zeroes
    if (input$user == "Hania") {
      current_df <- df_hania_zeroes
    } else if (input$user == "Martyna") {
      current_df <- df_martyna_zeroes
    }
    
    current_df <- current_df %>%
      filter(aktywnosc == "czas_z_rodzina") %>%
      mutate(cumsum_time = round(cumsum(sum_time)/60, 2)) %>%
      accumulate_by(~data) %>%
      mutate(data_numeric = as.numeric(data))
    
    plot_ly() %>%
      add_trace(
        data = current_df,
        x = ~data_numeric,                
        y = ~cumsum_time,
        frame = ~frame,          
        type = 'scatter',
        mode = 'lines',
        fill = 'tozeroy', 
        line = list(color = "#605ca8"),
        fillcolor = "rgba(96, 92, 168, 0.3)",
        hoverinfo = "none"
      ) %>%
      animation_opts(
        frame = 70,
        transition = 0,
        redraw = TRUE) %>%
      layout(
        showlegend = FALSE,
        xaxis = list(
          title = "Date",
          showgrid = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          title = "Hours"
        )
      ) %>%
      animation_button(
        x = 0,
        xanchor = 'left',
        y = 0,
        yanchor = 'bottom'
      ) %>%
      onRender("
        function(el, x) {
          Plotly.animate(el);
        }
      ") %>% 
      config(displayModeBar = FALSE)
    
  })
  
  output$leisure_hobbyAnimation <- renderPlotly({
    
    accumulate_by <- function(dat, var) {
      var <- lazyeval::f_eval(var, dat)
      lvls <- plotly:::getLevels(var)
      dats <- lapply(seq_along(lvls), function(x) {
        cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
      })
      dplyr::bind_rows(dats)
    }
    
    current_df <- df_ada_zeroes
    if (input$user == "Hania") {
      current_df <- df_hania_zeroes
    } else if (input$user == "Martyna") {
      current_df <- df_martyna_zeroes
    }
    
    current_df <- current_df %>%
      filter(aktywnosc == "hobby") %>%
      mutate(cumsum_time = round(cumsum(sum_time)/60, 2)) %>%
      accumulate_by(~data) %>%
      mutate(data_numeric = as.numeric(data))
    
    plot_ly() %>%
      add_trace(
        data = current_df,
        x = ~data_numeric,                
        y = ~cumsum_time,
        frame = ~frame,          
        type = 'scatter',
        mode = 'lines',
        fill = 'tozeroy',
        line = list(color = "#d81b60"),
        fillcolor = "rgba(216, 27, 96, 0.3)",
        hoverinfo='none'
      ) %>%
      animation_opts(
        frame = 70,
        transition = 0,
        redraw = TRUE) %>%
      layout(
        showlegend = FALSE,
        xaxis = list(
          title = "Date",
          showgrid = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          title = "Hours"
        )
      ) %>%
      animation_button(
        x = 0,
        xanchor = 'left',
        y = 0,
        yanchor = 'bottom'
      ) %>%
      onRender("
        function(el, x) {
          Plotly.animate(el);
        }
      ") %>% 
      config(displayModeBar = FALSE)
    
  })
  
  output$leisure_friendsAnimation <- renderPlotly({
    
    accumulate_by <- function(dat, var) {
      var <- lazyeval::f_eval(var, dat)
      lvls <- plotly:::getLevels(var)
      dats <- lapply(seq_along(lvls), function(x) {
        cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
      })
      dplyr::bind_rows(dats)
    }
    
    current_df <- df_ada_zeroes
    if (input$user == "Hania") {
      current_df <- df_hania_zeroes
    } else if (input$user == "Martyna") {
      current_df <- df_martyna_zeroes
    }
    
    current_df <- current_df %>%
      filter(aktywnosc == "spotkania_towarzyskie") %>%
      mutate(cumsum_time = round(cumsum(sum_time)/60, 2)) %>%
      accumulate_by(~data) %>%
      mutate(data_numeric = as.numeric(data))
    
    plot_ly() %>%
      add_trace(
        data = current_df,
        x = ~data_numeric,                
        y = ~cumsum_time,
        frame = ~frame,          
        type = 'scatter',
        mode = 'lines',
        fill = 'tozeroy',
        line = list(color = "#3d9970"),
        fillcolor = "rgba(61, 153, 112, 0.3)",
        hoverinfo = "none"
      ) %>%
      animation_opts(
        frame = 70,
        transition = 0,
        redraw = TRUE) %>%
      layout(
        showlegend = FALSE,
        xaxis = list(
          title = "Date",
          showgrid = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          title = "Hours"
        )
      ) %>%
      animation_button(
        x = 0,
        xanchor = 'left',
        y = 0,
        yanchor = 'bottom'
      ) %>%
      onRender("
        function(el, x) {
          Plotly.animate(el);
        }
      ") %>% 
      config(displayModeBar = FALSE)
    
  })
  
  output$leisure_lineChart <- renderPlotly({
    current_df <- df_ada_zeroes
    if (input$user == "Hania") {
      current_df <- df_hania_zeroes
    } else if (input$user == "Martyna") {
      current_df <- df_martyna_zeroes
    }
    
    current_df <- current_df %>%
      select(-osoba) %>%
      group_by(data, aktywnosc) %>%
      summarise(sum_time = sum(sum_time)) %>%
      ungroup() %>%
      pivot_wider(names_from = data, values_from = sum_time) %>%
      mutate_all( ~ replace(., is.na(.), 0)) %>%
      pivot_longer(cols = !aktywnosc,
                   names_to = "data",
                   values_to = "sum_time") %>%
      mutate(data = as.Date(data, '%Y-%m-%d')) %>%
      mutate(
        aktywnosc = case_when(
          aktywnosc == "czas_z_rodzina" ~ "time with family",
          aktywnosc == "sen" ~ "sleep",
          aktywnosc == "dojazdy_na_uczelnie" ~ "commute to uni",
          aktywnosc == "nauka" ~ "independent studying",
          aktywnosc == "odpoczynek_rozrywka" ~ "relax and entertainment",
          aktywnosc == "przerwy_na_uczelni" ~ "breaks at uni",
          aktywnosc == "spotkania_towarzyskie" ~ "time with friends",
          aktywnosc == "zajecia_na_uczelni" ~ "uni classes",
          aktywnosc == "hobby" ~ "hobby",
          aktywnosc == "sport" ~ "sport"
        )
      ) %>%
      mutate(
        aktywnosc = fct_relevel(
          aktywnosc,
          "sleep",
          "time with family",
          "time with friends",
          "hobby",
          "sport",
          "relax and entertainment",
          "independent studying",
          "uni classes",
          "breaks at uni",
          "commute to uni"
        )
      )
    
    if (input$leisure_line_date[1] <= input$leisure_line_date[2]) {
      current_df <- current_df %>%
        filter(data >= input$leisure_line_date[1],
               data <= input$leisure_line_date[2])
    }
    
    plot_ly() %>%
      add_trace(
        data = current_df %>% filter(aktywnosc %in% c("sleep", "hobby")),
        x = ~ data,
        y = ~ sum_time,
        color = ~ aktywnosc,
        type = 'scatter',
        mode = 'lines',
        colors = c("#0073b7", "#f39c12", "#dd4b39", 
                   "#39cccc", "#3d9970", "#001f3f", "#ff851b", "#f012be", 
                   "#d81b60", "#605ca8")
        
      ) %>%
      add_trace(
        data = current_df %>% filter(!(
          aktywnosc %in% c("sleep", "hobby")
        )),
        x = ~ data,
        y = ~ sum_time,
        color = ~ aktywnosc,
        type = 'scatter',
        mode = 'lines',
        visible = 'legendonly',
        colors = scales::hue_pal()(length(unique(
          current_df$aktywnosc
        )))
      ) %>%
      layout(
        legend = list(title = list(text = "Activities")),
        plot_bgcolor = "#ECEFF3",
        paper_bgcolor = "#FFFFFF",
        xaxis = list(
          title = "Date",
          gridcolor = "#FFFFFF"),
        yaxis = list(
          title = "Time (minutes)",
          zeroline = FALSE,
          rangemode = "tozero",
          gridcolor = "#FFFFFF"
        ),
        margin = list(
          t = 0,
          b = 30,
          l = 60,
          r = 20
        )
      ) %>% 
      config(displayModeBar = FALSE)
  })
  
  output$leisure_sleepChart <- renderPlotly({
    current_df <- df_everyone_zeroes %>%
      filter(aktywnosc == "sen") %>%
      group_by(data, osoba) %>%
      summarise(sen = sum(sum_time) / 60)
    
    if (input$sleeping_date[1] <= input$sleeping_date[2]) {
      current_df <- current_df %>%
        filter(data >= input$sleeping_date[1],
               data <= input$sleeping_date[2])
    }
    
    p <- current_df %>%
      ggplot(aes(x = osoba, y = sen)) +
      
      geom_rect(
        aes(
          xmin = 0,
          xmax = 4,
          ymin = 6,
          ymax = 11,
          fill = "Appropriate sleeping\ntime (6-11h)"
        ),
        size = 0,
        alpha = 0.2
      ) +
      geom_rect(
        aes(
          xmin = 0,
          xmax = 4,
          ymin = 7,
          ymax = 9,
          fill = "Recommended sleeping\ntime (7-9h)"
        ),
        size = 0,
        alpha = 0.4
      ) +
      
      geom_boxplot(
        fill = "rgba(96, 92, 168, 0.8)",
        color = "#605ca8",
        show.legend = FALSE
      ) +
      # ylim(0, 15) +
      scale_y_continuous(limits = c(0, 15), expand = c(0, 0)) +
      labs(x = "", y = "Time in hours") +
      theme_grey() +
      theme(
        # axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "#ECEFF3", color = NA),
        plot.title = element_text(hjust = 0.5),
        plot.title.position = "plot",
        plot.margin = margin(t = 20),
        legend.position = "bottom"
      ) +
      scale_fill_manual(
        values = c(
          "Recommended sleeping\ntime (7-9h)" = "#3d9970",
          "Appropriate sleeping\ntime (6-11h)" = "#ff851b"
        ),
        name = ""
      )
    
    ggplotly(p) %>% 
      layout(margin=list(l=60, t=0)) %>% # decrease upper margin, increase left margin so axis text doesnt touch axis label
      config(displayModeBar = FALSE)
    
  })
  
}

shinyApp(ui = shinyUI, server = server)
