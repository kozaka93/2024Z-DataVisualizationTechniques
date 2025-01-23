library(dplyr)
library(ggplot2)


OlekRaw <- read.csv2("data/sleepdataOlek.csv")
SebastianRaw <- read.csv2("data/sleepdataSebastian.csv")
PiotrRaw <- read.csv2("data/sleepdata.xls.csv")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

OlekRaw <- OlekRaw %>% 
  mutate(Name = "Olek")

SebastianRaw <- SebastianRaw %>% 
  mutate(Name = "Seba")

PiotrRaw <- PiotrRaw %>% 
  mutate(Name = "Piotrek")

combined <- rbind(OlekRaw, SebastianRaw, PiotrRaw)
combined$Sleep.Quality <- as.numeric(gsub("%", "", combined$Sleep.Quality)) / 100
combined$Movements.per.hour <- as.numeric(combined$Movements.per.hour)
combined <- combined[c(-1,-2),]


########## WYKRESY GĘSTOŚĆI ##########

mean_df <- combined %>% 
  group_by(Name) %>% 
  summarise(mean_SQ = mean(Sleep.Quality))

density_plot <- ggplot(combined, aes(x = Sleep.Quality, fill = Name)) + 
  geom_density(alpha = 0.3) +
  geom_vline(data = mean_df, aes(xintercept = mean_SQ, color = Name),
             linetype = "dashed") + 
  theme_minimal()

########## WYKRESY PUNKTOWE ##########

point_plot <- ggplot(combined, aes(x = Sleep.Quality,
                                  y = Movements.per.hour, 
                                  colour = Name)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()



point_plot2 <- ggplot(combined, aes(x = Sleep.Quality,
                                   y = Asleep.after..seconds., 
                                   colour = Name)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()


########## WYKRESY SŁUPKOWE ##########


library(shiny)
library(ggplot2)

data <- combined %>% 
  mutate(Date = as.Date(Went.to.bed)) %>% 
  select(Date, Name, Time.asleep..seconds.) %>% 
  group_by(Name) %>%
  arrange(Date) %>% 
  mutate(CumulativeSleep = cumsum(Time.asleep..seconds.))

data$Name <- as.factor(data$Name)

ui <- fluidPage(
  titlePanel("Kumulatywna ilość snu"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("date", "Wybierz dzień:", 
                  min = min(data$Date), 
                  max = max(data$Date), 
                  value = min(data$Date), 
                  step = 1, 
                  animate = animationOptions(interval = 500, loop = FALSE))
    ),
    mainPanel(
      plotOutput("bar_plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  output$bar_plot <- renderPlot({
    # Filtrowanie danych na podstawie wybranego dnia
    filtered_data <- data %>% 
      filter(Date <= input$date) %>% 
      group_by(Name) %>%
      summarise(CumulativeSleep = max(CumulativeSleep, na.rm = TRUE)) %>% 
      mutate(CumulativeSleepHours = CumulativeSleep / 3600) %>% 
      arrange(desc(CumulativeSleepHours))
    
    #Set colors for persons
    custom_colors <- c("Olek" = "blue", "Seba" = "red", "Piotrek" = "green")
    
    
    # Rysowanie wykresu słupkowego
    ggplot(filtered_data, aes(x = reorder(Name, CumulativeSleepHours), y = CumulativeSleep, fill = Name)) +
      geom_col(alpha = 0.8) +
      scale_fill_manual(values = custom_colors) +
      labs(
        title = paste("Kumulatywna ilość snu do dnia", input$date),
        x = "Osoba",
        y = "Czas snu (sekundy)"
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      coord_flip()
      
  })
}


# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)





