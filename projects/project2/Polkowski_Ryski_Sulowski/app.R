library(shiny)
library(bslib)
library(shinyjs)
library(dplyr)
library(readxl)
library(ggplot2)
library(plotly)
library(dplyr)
library(shiny)
library(readxl)
library(ggplot2)
library(plotly)
library(dplyr)
library(scales)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(stringr)
library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(dplyr)
library(readxl)
library(tidyr)
library(lubridate)
### Sleep related
all_hours <- 0:23
sleepData <- read_excel("data/sen.xlsx")

sleepData %>%
  mutate(
    SleepDuration = as.numeric(End - Start) / 60,
    StartDate = as.Date(Start),
    EndDate = as.Date(End),
    SleepStartDuration = ifelse(StartDate != EndDate, 24 - (hour(Start) * 60 + minute(Start)) / 60, as.numeric(End - Start) / 60),
    SleepEndDuration = ifelse(StartDate != EndDate, (hour(End) * 60 + minute(End)) / 60, 0),
    sleepStartHour = hour(Start),
    sleepEndHour = hour(End)
  ) -> sleepDataProcessed

hourlyData <- sleepDataProcessed %>% select(Name, sleepStartHour, sleepEndHour)
sleepDataProcessed %>%
  group_by(Name, StartDate) %>%
  summarise(sum(SleepStartDuration)) -> startData

sleepDataProcessed %>%
  group_by(Name, EndDate) %>%
  summarise(sum(SleepEndDuration)) -> endData

mergedSleepData <- full_join(startData, endData, by = c("Name" = "Name", "StartDate" = "EndDate"))
colnames(mergedSleepData) <- c("Name", "Date", "StartDuration", "EndDuration")
mergedSleepData <- mergedSleepData %>%
  mutate(
    StartDuration = ifelse(is.na(StartDuration), 0, StartDuration),
    TotalSleepDuration = StartDuration + EndDuration
  ) %>%
  select(Name, Date, TotalSleepDuration) %>%
  arrange(Date)

startHeatmapData <- hourlyData %>%
  group_by(Name, sleepStartHour) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  complete(Name, sleepStartHour = all_hours, fill = list(Frequency = 0)) %>%
  arrange(Name, sleepStartHour)

endHeatmapData <- hourlyData %>%
  group_by(Name, sleepEndHour) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  complete(Name, sleepEndHour = all_hours, fill = list(Frequency = 0)) %>%
  arrange(Name, sleepEndHour)



sleepScreen <- card(
  full_screen = TRUE,
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("sleepPerson",
                         "Select person:",
                         unique(mergedSleepData$Name), selected = c("Adam","Czarek","Michał")),
      dateRangeInput("sleepDateRange",
                     "Date range:",
                     start = min(mergedSleepData$Date),
                     end = max(mergedSleepData$Date),
                     min = min(mergedSleepData$Date),
                     max = max(mergedSleepData$Date),
                     format = "yyyy-mm-dd",
                     separator = " - "),
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("sleepPointPlot", height = "600px"),
    )
  ),
  
  
  
)

timesOfSleepScreen <- card(
  full_screen = TRUE,
  sidebarLayout(
    sidebarPanel(
      "No selection",
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("sleepStartHeatmap", height = "300px"),
      plotlyOutput("sleepEndHeatmap", height = "300px")
    )
  ),
  
  
  
)



### Steps related
steps_data <- read_excel("data/TWD 2.xlsx", sheet = "Steps",
                         col_types = c("text", "numeric", "date"))


stepsScreen <- card(
  full_screen = TRUE,
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("stepsPerson",
                         "Select person:",
                         c(
                           "Adam",
                           "Czarek",
                           "Michał"
                         ),
                         selected = c("Adam","Czarek","Michał")),
      dateRangeInput("daterange", "Date range:",
                     start  = min(steps_data$Day),
                     end    = max(steps_data$Day),
                     min    = min(steps_data$Day),
                     max    = max(steps_data$Day),
                     format = "yyyy-mm-dd",
                     separator = " - ")
      
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("steps_Lineplot"),
      plotOutput("steps_Barplot"),
      
    )
  )
)




### Hydration related
hydration_data <- read_excel("data/TWD 2.xlsx", sheet = "Drinks", col_types = c("text", "numeric", "text", "text", "date", "skip"))
drink_categories <- read_excel("data/TWD 2.xlsx", sheet = "Drinks", col_types = c("skip", "skip", "skip", "skip", "skip", "text"), n_max = 6)
drink_categories <- drink_categories$`Categories:`[!is.na(drink_categories$`Categories:`)]


hydrationScreen <- card(
  full_screen = TRUE,
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("hydrationPerson",
                         "Select person:",
                         c(
                           "Adam",
                           "Czarek",
                           "Michał"
                         ),
                         selected = c("Adam","Czarek","Michał")),
      dateRangeInput("daterange", "Date range:",
                     start  = min(hydration_data$Date),
                     end    = max(hydration_data$Date),
                     min    = min(hydration_data$Date),
                     max    = max(hydration_data$Date),
                     format = "yyyy-mm-dd",
                     separator = " - "),
      checkboxGroupInput("categories",
                         "Drinks categories:",
                         choiceNames = drink_categories,
                         choiceValues = drink_categories,
      )
      
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("drinks_Barplot"),
      plotOutput("water_Barplot")
      
    )
  )
)

### Time outdoors related

outdoorData <- read_excel("data/outdoor.xlsx")

outdoorData %>%
  mutate(
    OutdoorDuration = as.numeric(End - Start) / 60,
    StartDate = as.Date(Start),
    EndDate = as.Date(End),
    OutdoorStartDuration = ifelse(StartDate != EndDate, 24 - (hour(Start) * 60 + minute(Start)), as.numeric(End - Start)),
    OutdoorEndDuration = ifelse(StartDate != EndDate, (hour(End) * 60 + minute(End)), 0),
    outdoorStartHour = hour(Start),
    outdoorEndHour = hour(End)
  ) -> outdoorDataProcessed

hourlyOutdoorData <- outdoorDataProcessed %>% select(Name, outdoorStartHour, outdoorEndHour)

outdoorDataProcessed %>%
  group_by(Name, StartDate) %>%
  summarise(sum(OutdoorStartDuration)) -> outdoorStartData

outdoorDataProcessed %>%
  group_by(Name, EndDate) %>%
  summarise(sum(OutdoorEndDuration)) -> outdoorEndData

mergedOutdoorData <- full_join(outdoorStartData, outdoorEndData, by = c("Name" = "Name", "StartDate" = "EndDate"))
colnames(mergedOutdoorData) <- c("Name", "Date", "OutdoorStartDuration", "OutdoorEndDuration")
mergedOutdoorData <- mergedOutdoorData %>%
  mutate(
    OutdoorStartDuration = ifelse(is.na(OutdoorStartDuration), 0, OutdoorStartDuration),
    TotalOutdoorDuration = OutdoorStartDuration + OutdoorEndDuration
  ) %>%
  select(Name, Date, TotalOutdoorDuration) %>%
  arrange(Date)

all_hours <- 0:23

outdoorStartHeatmap <- hourlyOutdoorData %>%
  group_by(Name, outdoorStartHour) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  complete(Name, outdoorStartHour = all_hours, fill = list(Frequency = 0)) %>%
  arrange(Name, outdoorStartHour)

outdoorEndHeatmap <- hourlyOutdoorData %>%
  group_by(Name, outdoorEndHour) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  complete(Name, outdoorEndHour = all_hours, fill = list(Frequency = 0)) %>%
  arrange(Name, outdoorEndHour)



timeOutdoorsScreen <- card(
  full_screen = TRUE,
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      
      checkboxGroupInput("outdoorPerson",
                         "Select person:",
                         unique(mergedOutdoorData$Name), selected = c("Adam","Czarek","Michał")),
      dateRangeInput("outdoorDateRange",
                     "Date range:",
                     start = min(mergedOutdoorData$Date),
                     end = max(mergedOutdoorData$Date),
                     min = min(mergedOutdoorData$Date),
                     max = max(mergedOutdoorData$Date),
                     format = "yyyy-mm-dd",
                     separator = " - ")
      
    ),
    
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("outdoorPointPlot"),
      plotlyOutput("dayOfWeekPlot")
    )
  )
)


### Screen time
screenTimeIcons <- read.csv('data/icons.csv')
screenTimeDf <- read.csv(('data/screen_time_2.csv'))
screenTimeIcons <- read.csv('data/icons.csv')
names(screenTimeDf) <- c('name', 'app', 'day', 'time')

cutLast <- function(str) {
  as.numeric(substr(str, 1, nchar(str)-1))
}

toSeconds <- function(date) {
  date <- ifelse(grepl("m", date), date, paste(date, "0m"))
  date <- ifelse(grepl("s", date), date, paste(date, "0s"))
  x <- strsplit(date, " +")[[1]]
  len <- length(x)
  hours <- ifelse(len == 3, cutLast(x[1]), 0)
  minutes <- ifelse(len == 3, cutLast(x[2]), ifelse(len == 2, cutLast(x[1]), 0))
  seconds <- ifelse(len == 3, cutLast(x[3]), ifelse(len == 2, cutLast(x[2]), ifelse(len == 1, cutLast(x[1]), 0)))
  (hours * 60 + minutes) * 60 + seconds
}

screenTimeDf2 <- data.frame(sapply(screenTimeDf$time,toSeconds)) 
names(screenTimeDf2) <- c('time')
screenTimeDf$time <- screenTimeDf2$time
screenTimeDf

generateButton <- function(inputId, url) {
  actionButton(inputId = inputId, label = NULL, style = paste("width: 40px; height: 40px; background: url(", url, ");  background-size: cover; background-position: center; outline-color: #387eff !important;"))
}

screenTimeScreen <- card(
  full_screen = TRUE,
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("screenTimePerson",
                         "Select person:",
                         c(
                           "Adam",
                           "Czarek",
                           "Michał"
                         ),
                         selected = c("Adam","Czarek","Michał")),
      dateRangeInput("daterange", "Date range:",
                     start  = min(hydration_data$Date),
                     end    = max(hydration_data$Date),
                     min    = min(hydration_data$Date),
                     max    = max(hydration_data$Date),
                     format = "yyyy-mm-dd",
                     separator = " - "),
      h5('Wybór aplikacji'),
      checkboxInput("allAppsCheckbox", "Zaznacz wszystko", TRUE), 
      div(
        style = "display: grid;grid-template-columns: repeat(5, 1fr);grid-template-rows: repeat(6, 1fr);",
        lapply(
          X = 1:length(screenTimeIcons$app),
          FUN = function(i) {
            div(
              generateButton(screenTimeIcons$app[i], screenTimeIcons$icon_url[i]),
              br(),
              p(
                screenTimeIcons$app[i],
                style = "text-align: center;max-width: 40px; text-overflow: ellipsis; width: 100%;clear:left;align-items: center;justify-content: center;display: inline;margin: auto;outline-color: #387eff !important;",
              ),
              style = "font-size: 10px",
            )
          }
        ),
      ),
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("screenTimePointPlot"),
    )
  )
)


### Main app

if (interactive()) {
  ui <- navbarPage(
    "The lifestyle of a MiNi student",
    tabPanel("Sleep 🛏️",sleepScreen),
    tabPanel("Times of sleep",timesOfSleepScreen),
    tabPanel("Steps 👣️", stepsScreen),
    tabPanel("Hydration 🥛", hydrationScreen),
    tabPanel("Time outdoors ⛰️", timeOutdoorsScreen),
    tabPanel("Screen time 🖥️", screenTimeScreen)
  )
  
  isClicked = FALSE
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    ### Sleep
    name_colors <- c("Adam" = "#1f77b4", "Czarek" = "#ff7f0e", "Michał" = "#2ca02c")
    
    filteredSleepData <- reactive({
      mergedSleepData %>%
        filter(Date >= input$sleepDateRange[1] & Date <= input$sleepDateRange[2])
    })
    
    output$sleepPointPlot <- renderPlotly({
      plot_ly(filteredSleepData() %>% 
                filter(Name %in% input$sleepPerson),  
              x = ~Date,
              y = ~TotalSleepDuration,
              color = ~Name,  
              colors = name_colors,
              type = 'scatter',
              mode = 'lines+markers'
      ) %>% 
        layout(
          title = "Length of sleep by day", 
          xaxis = list(title = 'Date', tickmode = "array", tickvals = unique(filteredSleepData()$Date), tickangle = -45), 
          yaxis = list(title = 'Length of sleep (in hours)', range = c(0, 16)),
          margin = list(l = 60, r = 60, t = 60, b = 100)
        ) %>% 
        config(displayModeBar = F)
    })
    
    output$sleepStartHeatmap <- renderPlotly({
      plot_ly(startHeatmapData, 
              x = ~Name, 
              y = ~sleepStartHour, 
              z = ~Frequency, 
              type = "heatmap",
              hovertemplate = '<b>Osoba:</b> %{x}<br><b>Godzina rozpoczęcia snu:</b> %{y}<br><b>Frequency:</b> %{z}<extra></extra>',
              colorscale = "Reds") %>%
        layout(
          title = "Start of the sleep",
          xaxis = list(title = "Person"),
          yaxis = list(title = "Hour of going to sleep"),
          hoverlabel = list(
            namelength = 20,    
            align = "left",          
            xanchor = "left"            
          )
        )
    })
    
    output$sleepEndHeatmap <- renderPlotly({
      plot_ly(endHeatmapData, 
              x = ~Name, 
              y = ~sleepEndHour, 
              z = ~Frequency, 
              type = "heatmap", 
              colorscale = "Reds",
              hovertemplate = '<b>Osoba:</b> %{x}<br><b>Hour of waking up:</b> %{y}<br><b>Frequency:</b> %{z}<extra></extra>',
              colorbar = list(title = "Frekwencja")) %>%
        layout(
          title = "End of the sleep",
          xaxis = list(title = "Person"),
          yaxis = list(title = "Hour of waking up"),
          hoverlabel = list(
            namelength = 20,    
            align = "left",           
            xanchor = "left"            
          )
        )
    })
    
    ### Steps
    output$steps_Lineplot <- renderPlot(
      steps_data %>% 
        filter(as.Date(Day) >= input$daterange[1], as.Date(Day) <= input$daterange[2]) %>% 
        filter(Name %in% input$stepsPerson) %>%
        ggplot(aes(x = Day, y = Steps, color = Name)) + geom_line(size = 1.5) + 
        scale_y_continuous(expand = expansion(mult = c(0, 0))) +
        theme(
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          
        ) +
        labs(title = "Amount of steps taken each day",
             x = "Date",
             y = "Amount of steps taken") + scale_fill_manual(values=name_colors)
    )
    
    output$steps_Barplot <- renderPlot(
      steps_data %>% 
        filter(as.Date(Day) >= input$daterange[1], as.Date(Day) <= input$daterange[2]) %>% 
        filter(Name %in% input$stepsPerson) %>%
        group_by(Name) %>% summarise(st = sum(Steps)) %>% 
        ggplot(aes(x = Name, y = st, fill = Name)) + geom_col() + 
        scale_y_continuous(expand = expansion(mult = c(0, 0)), labels = label_comma()) +
        labs(title = paste("Sum of steps taken between ", input$daterange[1]," and ", input$daterange[2]),
             x = "Person",
             y = "Sum of steps taken") + scale_fill_manual(values=name_colors)
    )
    
    ### Time outdoors
    
    
    
    filteredOutdoorData <- reactive({
      mergedOutdoorData %>%
        filter(Date >= input$outdoorDateRange[1] & Date <= input$outdoorDateRange[2])
    })
    
    dayOfWeekOutdoorData <- reactive({
      filteredOutdoorData() %>%
        mutate(DayOfWeek = wday(Date, label = TRUE, abbr = FALSE,week_start=1)) %>%
        group_by(Name, DayOfWeek) %>%
        summarise(TotalOutdoorTime = sum(TotalOutdoorDuration), .groups = 'drop') %>%
        arrange(DayOfWeek)
    })
    
    output$outdoorPointPlot <- renderPlotly({
      plot_ly(filteredOutdoorData() %>% 
                filter(Name %in% input$outdoorPerson),  
              x = ~Date,
              y = ~TotalOutdoorDuration,
              color = ~Name,  
              colors = name_colors,
              type = 'scatter',
              mode = 'lines+markers'
      ) %>% 
        layout(
          title = "Długość czasu na dworze w poszczególnych dniach", 
          xaxis = list(title = 'Data', tickmode = "array", tickvals = unique(filteredOutdoorData()$Date), tickangle = -45), 
          yaxis = list(title = 'Czas spędzony na dworze (w minutach)', range = c(0, 300)),
          margin = list(l = 60, r = 60, t = 60, b = 100)
        ) %>% 
        config(displayModeBar = F)
    })
    
    output$dayOfWeekPlot <- renderPlotly({
      plot_ly(dayOfWeekOutdoorData(), 
              x = ~DayOfWeek, 
              y = ~TotalOutdoorTime, 
              color = ~Name, 
              colors = name_colors,
              type = "bar") %>%
        layout(
          title = "Czas spędzony na dworze a dni tygodnia",
          xaxis = list(title = "Dzień tygodnia"),
          yaxis = list(title = "Czas spędzony łącznie na dworze (w minutach)"),
          barmode = 'group'
        )
    })
    
    
    ### Screen time
    selectAllApps <- reactiveVal()
    selectAllApps(0)
    values <- reactiveValues()
    lapply(
      X = 1:length(screenTimeIcons$app),
      FUN = function(i) {
        observeEvent(input[[screenTimeIcons$app[i]]], {
          newValue <- ifelse(values[[screenTimeIcons$app[i]]] == 0, 1, 0)
          if (newValue) {
            insertUI("head", ui = tags$style(HTML(paste("#", screenTimeIcons$app[i], " { outline: solid !important; outline-color: #387eff !important;}", sep=""))))
          } else {
            insertUI("head", ui = tags$style(HTML(paste("#", screenTimeIcons$app[i], " { outline: none !important; outline-color: #387eff !important;}", sep=""))))
          }
          values[[screenTimeIcons$app[i]]] <- newValue
          print("UPDATED")
          print(newValue)
        })
      }
    )
    
    observeEvent(input$allAppsCheckbox, {
      val = ifelse(selectAllApps() == 0, 1, 0)
      selectAllApps(val)
      if (val) {
        # Select all apps left
        lapply(
          X = 1:length(screenTimeIcons$app),
          FUN = function(i) {
            insertUI("head", ui = tags$style(HTML(paste("#", screenTimeIcons$app[i], " { outline: solid !important; outline-color: #387eff !important;}", sep=""))))
            values[[screenTimeIcons$app[i]]] <- 1
          }
        )
      } else {
        # Deselect all apps left
        lapply(
          X = 1:length(screenTimeIcons$app),
          FUN = function(i) {
            values[[screenTimeIcons$app[i]]] <- 0
            insertUI("head", ui = tags$style(HTML(paste("#", screenTimeIcons$app[i], " { outline: none !important; outline-color: #387eff !important;}", sep=""))))
          }
        )
      }
      updateCheckboxInput(session, "allAppsCheckbox", "Zaznacz wszystko")
    })
    
    output$screenTimePointPlot <- renderPlot({
      screenTimePerson <- input$screenTimePerson
      getSelected <- function(listName) {
        names(listName)[unlist(lapply(names(listName), FUN = function(val) {
          listName[[val]] == 1
        }))]
      }
      selectedApps <- getSelected(values)
      n <- min(10, length(selectedApps))
      most_time_spent <- screenTimeDf  %>% filter(name %in% screenTimePerson)  %>% filter(app %in% selectedApps)
      best_apps <- (most_time_spent %>% group_by(app) %>%
                      summarize(total_best_time = sum(time)) %>% arrange(desc(total_best_time)) %>% top_n(n))
      most_time_spent_2 <- most_time_spent %>% filter(app %in% best_apps$app) %>% group_by(name, app) %>%  summarize(total_time = sum(time))
      most_time_spent_3 <- most_time_spent_2 %>% left_join(best_apps)
      most_time_spent_3 %>% mutate(total_time = total_time / 60) %>% ggplot() +
        geom_col(aes(x=total_time, y=reorder(app, total_best_time), fill = name), width = 0.6) +
        labs(y = "App", 
             x = "Time (in minutes)", 
             fill = "Person",
             title = "Smartphone screentime from 24-09-24 to 50-40-40",
             subtitle = paste("top", n, "most time consuming apps (from selected)"),
        ) +  theme(
          panel.grid.major.x = element_line(colour="grey"),
          panel.grid.minor.x = element_line(colour="grey"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank()) + scale_fill_manual(values=name_colors)
    })
    
    
    output$drinks_Barplot <- renderPlot(
      hydration_data %>% 
        filter(as.Date(Date) >= input$daterange[1], as.Date(Date) <= input$daterange[2]) %>% 
        filter(Name %in% input$hydrationPerson) %>%
        ggplot(aes(x = factor(Date), y = Amount, fill = Name)) + geom_col() + 
        scale_y_continuous(expand = expansion(mult = c(0, 0))) +
        theme(
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
        )  + scale_fill_manual(values=name_colors)
    )
    
    output$water_Barplot <- renderPlot(
      hydration_data %>% filter(Category %in% input$categories) %>% 
        ggplot(aes(x = Name, y = Amount, fill = Name)) + geom_col() +
        scale_y_continuous(expand = expansion(mult = c(0, 0))) +
        theme(
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
        ) + scale_fill_manual(values=name_colors)
    )
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}

