data<-read.csv("danepr1twd.csv")

library(ggplot2)
library(dplyr)
library(ggforce)

library(grid)
library(readxl)
data$Work_Location <- factor(data$Work_Location, levels = c("Remote", "Hybrid", "Onsite"))
stress_data<- data %>%
  group_by(Work_Location, Stress_Level) %>%
  mutate( Stress_Level = factor(Stress_Level, levels = c("High","Medium", "Low"))
  ) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) 


a<-ggplot(stress_data, aes(x = Work_Location, y = percentage, fill = Stress_Level)) +
  geom_bar(stat = "identity", position = position_dodge(), width=0.7) +
  scale_x_discrete(labels = c("Onsite" = "Stacjonarnie", "Remote" = "Zdalnie", "Hybrid" = "Hybrydowo")) +
  scale_fill_manual(
    values = c("Low" = "#203971", "High" = "#C97748", "Medium" = "#295DC2"),  
    labels = c("Low" = "Niski", "Medium" = "Średni", "High" = "Wysoki")
  ) +
  labs(title = "Poziom stresu",
       x = "Sposób pracy",
       y = "Procent pracowników",
       fill = "") +
  coord_cartesian(ylim = c(30, NA)) + 
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold",hjust = 0.5),    
    axis.title = element_text(face = "bold"), 
    legend.title = element_text(face = "bold",size = 14),   
    legend.text = element_text(face = "bold", size=14),
    plot.background = element_rect(fill = "transparent", color = NA),   
    panel.background = element_rect(fill = "transparent", color = NA), 
    panel.grid = element_blank(),
    legend.position = "top",
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_line(color = NA),
    
  )
a



remote_data <- data %>% filter(Work_Location == "Remote")
b<-data%>%
  group_by(Work_Location) %>%
  summarise(Average_Isolation = mean(Social_Isolation_Rating)) %>%
  ggplot(aes(x = Work_Location, y = 5-Average_Isolation, fill = Work_Location)) +  #tutaj obracam skalę ponieważ skala 1-5 jest bardziej czytelna niż 5-1
  scale_x_discrete(labels = c("Onsite" = "Stacjonarnie", "Remote" = "Zdalnie", "Hybrid" = "Hybrydowo")) +
  scale_fill_manual(
    values = c("Remote" = "#C97748", "Hybrid" = "#295DC2", "Onsite"="#203971"), 
    labels = c("Decrease" = "Spadek", "Increase" = "Wzrost")
  ) +
  geom_col() + 
  labs(title = "Poziom oceny izolacji społecznej w pięciostopniowej skali",
       x = "Sposób pracy",
       y = "Średnia ocena") +
  coord_cartesian(ylim = c(1.90, NA)) + 
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold",hjust = 0.5),       
    axis.title = element_text(face = "bold"),       
    legend.title = element_text(face = "bold"),     
    legend.text = element_text(face = "bold"),
    plot.background = element_rect(fill = "transparent", color = NA),   
    panel.background = element_rect(fill = "transparent", color = NA), 
    panel.grid = element_blank(),
    legend.position = "none",
    panel.grid.major = element_line(color = NA),  
    panel.grid.minor = element_line(color = NA), 
  )
b


survey <- read.csv("survey.csv")

# zmiana z angielskiego na polski
survey$phys_health_interview = ifelse(survey$phys_health_interview == "No", "Często", ifelse(survey$phys_health_interview == "Yes", "Prawie nigdy", "Czasami"))
# zmiana z angielskiego na polski + swap odpowiedzi przy jednoczesnym przeciwnym pytaniu
survey$mental_health_interview = ifelse(survey$mental_health_interview == "Yes", "Tak", ifelse(survey$mental_health_interview == "No", "Nie", "Być może"))

ggplot(data = survey) +
  geom_mosaic(aes(
    x = product(mental_health_interview),
    fill = phys_health_interview
  )) +
  labs(
    title = "Zależność między zdrowiem psychicznym a fizycznym",
    x = "Czy występują problemy psychiczne?",
    y = "Proporcje",
    fill = "Częstość aktywności fizycznej"
  ) +
  scale_fill_manual(values = c("Prawie nigdy" = "#c97748", "Często" = "#295dc2", "Czasami" = "#203971")) +
  theme_minimal(base_size = 5) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Pogrubienie tytułu
    axis.title.x = element_text(face = "bold"),             # Pogrubienie opisu osi X
    axis.title.y = element_text(face = "bold"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

dane <- read_excel("Raw_data.xlsx")


data_clean <- dane %>%
  select(
    isolation = `2. How often do you feel isolated from others?`,
    mental_health = `2. Over the last 2 weeks, how often have you been bothered by feeling down, depressed, or hopeless?`
  ) %>%
  mutate(
    isolation = case_when(
      isolation == "Hardly ever" ~ "Prawie nigdy",
      isolation == "Some of the time" ~ "Czasami",
      isolation == "Often" ~ "Często",
      TRUE ~ NA_character_
    ),
    mental_health = case_when(
      mental_health == "Not at all" ~ "Wcale",
      mental_health == "Several days" ~ "Kilka dni",
      mental_health == "More than half the days" ~ "Ponad połowę dni",
      mental_health == "Nearly every day" ~ "Prawie codziennie",
      TRUE ~ NA_character_
    )
  )


a <- ggplot(data_clean, aes(x = isolation, y = mental_health)) + 
  geom_jitter(
    width = 0.2, 
    height = 0.2, 
    color = "#295dc2", 
    alpha = 2.5, 
    size = 3
  ) +
  labs(
    title = "Zależność między izolacją społeczną a problemami psychicznymi",
    x = "Jak często czujesz się odizolowany od społeczeństwa?",
    y = "Jak często zmagasz się z problemami natury psychicznej?"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Wyśrodkowanie tytułu
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12)
  )
print(a)












