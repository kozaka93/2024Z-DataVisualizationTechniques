plot_01_prepareData <- function(data = read.csv("data/training_data.csv")) {
  # creates proper data for violin_plot
  prepareData2 <- function(dataFrame){
    # supporting function for 'prepareData'
    # creates sufficient data for violin plot
    dane <- data.frame()
    for (index in 1:nrow(dataFrame)){
      row <- dataFrame[index, ]
      odstep_czasowy = 15 # w minutach
      start = (row$statring_minute %/% odstep_czasowy) * odstep_czasowy
      period = (row$DurationInMinutes %/% odstep_czasowy) * odstep_czasowy
      minutes <- seq(start, start+period, by=odstep_czasowy)
      weekday <- rep(row$weekday, times=length(minutes))
      date <- rep(row$Date, times=length(minutes))
      dane <- rbind(dane, cbind(date, minutes, weekday))
    }
    dane
  }
  
  data <- data %>% 
    distinct(Owner, Date, DurationInMinutes) %>% 
    mutate(
      hour = as.numeric(str_sub(Date, 11, 13)),
      minute = as.numeric(str_sub(Date, 15, 16)),
      Date = str_sub(Date, 1, 10)
    ) %>% 
    mutate(
      statring_minute = 60*hour+minute,
      weekday = weekdays(as.Date(Date))
    ) %>% 
    distinct(Date, weekday, Owner, DurationInMinutes, statring_minute)
  
  ludwik <- data %>% filter(Owner == "Ludwik")
  yahor <- data %>% filter(Owner == "Yahor")
  maxim <- data %>% filter(Owner == "Maxim")
  
  to_return <- rbind(
    prepareData2(ludwik) %>% mutate(Owner = "Ludwik"),
    prepareData2(yahor) %>% mutate(Owner = "Yahor"),
    prepareData2(maxim) %>% mutate(Owner = "Maxim")
  )
  
  to_return %>% 
    mutate(
      #weekday = factor(weekday, levels = c("poniedziałek", "wtorek", "środa", 
      #                                         "czwartek", "piątek", "sobota", "niedziela")),
      weekday = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday",
                                           "Thursday", "Friday", "Saturday", "Sunday"
      )),
      hour = as.numeric(minutes)/60,
      Owner = factor(Owner, levels = c("Ludwik", "Yahor", "Maxim"))
    ) %>% 
    select(-minutes) %>% 
    group_by(weekday, hour, Owner) %>% 
    summarise(count = n())
}

plot_01 <- function(data = plot_01_prepareData()){
  library(dplyr)
  library(ggnewscale)
  library(ggplot2)
  library(stringr)
  ggplot(
    data,
    aes(
      x = weekday,
      y = hour
    )
  ) + 
    # skala dla Ludwika
    geom_tile(
      data = subset(data, Owner == "Ludwik"), 
      aes(fill = count)
    ) +
    scale_fill_gradientn(colors = c("#325728", "#3de60e")) +
    
    # skala dla Yahora
    ggnewscale::new_scale_fill() + 
    geom_tile(
      data = subset(data, Owner == "Yahor"), 
      aes(fill = count)
    ) +
    scale_fill_gradient(low = "#335f61", high = "#0edfe6") +
    
    # skala dla Maxima
    ggnewscale::new_scale_fill() + 
    geom_tile(
      data = subset(data, Owner == "Maxim"), 
      aes(fill = count)
    ) +
    scale_fill_gradient(low = "#826105", high = "#dbf20a") +
    
    scale_y_continuous(breaks = 12:24) +
    facet_wrap(~ Owner) +
    theme_minimal() +
    theme(
      # axes and plot title
      axis.title = element_text(color = "white", size = 15, face = "bold"),
      axis.text = element_text(color = "white", size=11, face="bold"),
      plot.title = element_text(colour = "white", size=20, face="bold", hjust = 0.5),
      axis.text.x = element_text(color = "white", size=10, face="bold", angle = 90),
      axis.text.y = element_text(color = "white", size=10, face="bold"),
      
      # background
      plot.background = element_rect("#3d3a36"),
      strip.text = element_text(colour = "white", size=17, face="bold"),
      
      #legend
      legend.text = element_text(color="white", size=10),
      legend.title = element_blank(),
      legend.position = "none",
      
      
      # grid
      panel.grid.major = element_line(color = "#5b5c55", linetype = "dashed"),
      panel.grid.minor = element_line(color = "#4f4f4c", linetype = "dotted"),
      
      # space between plots
      panel.spacing = unit(2, "lines")
    ) +
    labs(
      title = "Distribution of Gym Visit Hours",
      y = "Hour",
      x = "Day of The Week"
    ) #+
  # scale_x_discrete(labels = c(
  #   "poniedziałek" = "Monday", 
  #   "wtorek" = "Tuesday", 
  #   "środa" = "Wednesday", 
  #   "czwartek" = "Thursday", 
  #   "piątek" = "Friday", 
  #   "sobota" = "Saturday", 
  #   "niedziela" = "Sunday"
  #   )
  #  )
}

# plot_01  <- function(
    #     data = plot_01_prepareData(read.csv("data/training_data.csv")),
#     people = c("Ludwik", "Maxim", "Yahor")
#     ){
#   library(ggplot2)
#   library(dplyr)
#   library(stringr)
#   source("functions/adjust_plot.R")
#   # returns plot_1
#   if (length(people) != 3){
#     data <- data %>% 
#       filter(Owner %in% people)
#   }
#   adjust_plot(data %>% 
#       ggplot(
#         aes(
#           x = weekday,
#           y = hour,
#           fill = Owner
#         )
#       ) +
#       geom_violin() +
#       scale_y_continuous(
#         #limits = c(min(data$hour, max(data$hour))),
#         breaks = 1:24
#         ) +
#       labs(
#           title = "Dystrybucja godzin treningu",
#           y = "Godzina",
#           x = "Dzień tygodnia",
#           fill = ""
#       )
#   )
# }
plot_01()
# plot_01  <- function(
#     data = plot_01_prepareData(read.csv("data/training_data.csv")),
#     people = c("Ludwik", "Maxim", "Yahor")
#     ){
#   library(ggplot2)
#   library(dplyr)
#   library(stringr)
#   source("functions/adjust_plot.R")
#   # returns plot_1
#   if (length(people) != 3){
#     data <- data %>% 
#       filter(Owner %in% people)
#   }
#   adjust_plot(data %>% 
#       ggplot(
#         aes(
#           x = weekday,
#           y = hour,
#           fill = Owner
#         )
#       ) +
#       geom_violin() +
#       scale_y_continuous(
#         #limits = c(min(data$hour, max(data$hour))),
#         breaks = 1:24
#         ) +
#       labs(
#           title = "Dystrybucja godzin treningu",
#           y = "Godzina",
#           x = "Dzień tygodnia",
#           fill = ""
#       )
#   )
# }