library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(dplyr)
library(readxl)
library(tidyr)
library(lubridate)
all_hours <- 0:23
sleepData <- read_excel("sen.xlsx")

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

outdoorData <- read_excel("outdoor.xlsx")

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

sen <- card(
  full_screen = TRUE,
  card_header("Analiza snu"),
  card_body(
    fluidRow(
      column(3, 
             checkboxGroupInput("sleepPerson",
                                "Którą osobę wybierasz?",
                                unique(mergedSleepData$Name))
      ),
      column(3, 
             dateRangeInput("sleepDateRange",
                            "Wybierz zakres dat dla wykresu punktowego:",
                            start = min(mergedSleepData$Date),
                            end = max(mergedSleepData$Date),
                            min = min(mergedSleepData$Date),
                            max = max(mergedSleepData$Date),
                            format = "yyyy-mm-dd",
                            separator = " do ")
      )
    ),
    fluidRow(
      column(8, 
             plotlyOutput("sleepPointPlot", height = "600px")
      ),
      column(4, 
             plotlyOutput("sleepStartHeatmap", height = "300px"),
             plotlyOutput("sleepEndHeatmap", height = "300px")
      )
    )
  )
)

czasNaDworze <- card(
  full_screen = TRUE,
  card_header("Analiza czasu spędzonego na dworze"),
  card_body(
    fluidRow(
      column(3, 
             checkboxGroupInput("outdoorPerson",
                                "Którą osobę wybierasz?",
                                unique(mergedOutdoorData$Name))
      ),
      column(3, 
             dateRangeInput("outdoorDateRange",
                            "Wybierz zakres dat dla wykresu punktowego:",
                            start = min(mergedOutdoorData$Date),
                            end = max(mergedOutdoorData$Date),
                            min = min(mergedOutdoorData$Date),
                            max = max(mergedOutdoorData$Date),
                            format = "yyyy-mm-dd",
                            separator = " do ")
      )
    ),
    fluidRow(
      column(8, 
             plotlyOutput("outdoorPointPlot", height = "600px")
      ),
      column(4, 
             plotlyOutput("dayOfWeekPlot", height = "600px")
      )
    )
  )
)
ui <- navbarPage(
  "Styl życia studenta MiNi",
  theme = bs_theme(
    version = 5,
    bootswatch = "cosmo",   
    primary = "#3498db",    
    secondary = "#2ecc71",  
    base_font = font_google("Roboto"), 
    heading_font = font_google("Open Sans")  
  ),
  tabPanel("Sen 🛏️", sen),
  tabPanel("Kroki 👣️", kroki),
  tabPanel("Nawodnienie 🥛", nawodnienie),
  tabPanel("Czas na dworze ⛰️", czasNaDworze),
  tabPanel("Czas przed ekranem 🖥️", czasPrzedEkranem)
)

server <- function(input, output) {
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
        title = "Długość snu w poszczególnych dniach", 
        xaxis = list(title = 'Data', tickmode = "array", tickvals = unique(filteredSleepData()$Date), tickangle = -45), 
        yaxis = list(title = 'Długość snu (w godzinach)', range = c(0, 16)),
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
            hovertemplate = '<b>Osoba:</b> %{x}<br><b>Godzina rozpoczęcia snu:</b> %{y}<br><b>Frekwencja:</b> %{z}<extra></extra>',
            colorscale = "Reds") %>%
      layout(
        title = "Start snu",
        xaxis = list(title = "Osoba"),
        yaxis = list(title = "Godzina rozpoczęcia snu"),
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
            hovertemplate = '<b>Osoba:</b> %{x}<br><b>Godzina zakończenia snu:</b> %{y}<br><b>Frekwencja:</b> %{z}<extra></extra>',
            colorbar = list(title = "Frekwencja")) %>%
      layout(
        title = "Koniec snu",
        xaxis = list(title = "Osoba"),
        yaxis = list(title = "Godzina zakończenia snu"),
        hoverlabel = list(
        namelength = 20,    
        align = "left",           
        xanchor = "left"            
        )
      )
  })
  
  
  
  
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
}

shinyApp(ui = ui, server = server)
