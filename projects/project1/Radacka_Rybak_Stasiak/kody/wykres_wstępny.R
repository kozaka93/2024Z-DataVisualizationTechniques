library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)

install.packages("readxl")
library(readxl)

df2 <- read_excel("mental_health_survey.xlsx", sheet = 1)  


influence <- df2 %>% 
  select(c(`7. How much time do you spend daily in social media?`,`17. Does your emotion get influenced by other's posts (success, failure, loss)?`)) %>% 
  rename(c(time_spent = `7. How much time do you spend daily in social media?`, answer = `17. Does your emotion get influenced by other's posts (success, failure, loss)?`) ) %>% 
  group_by(time_spent, answer) %>% 
  summarise(n = n()) %>% 
  filter(answer %in% c('Always', 'Not at all')) %>% 
  group_by(time_spent) %>%
  mutate(percent = n / sum(n) * 100) 

  
install.packages('showtext')
library(showtext)
font_add_google("Montserrat")
showtext_auto()

influence$time_spent <- factor(influence$time_spent, levels = c("Less than 1 hour", "1-3 hours", "3-5 hours", "More than 5 hours"))

influence_plot <- ggplot(influence,aes(x = time_spent, y = percent, fill = answer)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Does your emotion get influenced by other's posts (success, failure, loss)?",
       x = "Time spent on social media",
       y = "Percent of users",
       fill = "Answer") +
  theme_minimal() + theme(
    text = element_text(family = "Montserrat"),  # Ustawienie czcionki na Roboto
    plot.title = element_text(size = 16, face = "bold"),  # Zmiana rozmiaru tytułu
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),  # Kolor tekstu na osiach
    legend.title = element_text(size = 14),  # Kolor tytułu legendy
    legend.text = element_text(size = 12) # Zmiana rozmiaru tekstu osi
  ) + scale_fill_manual(values = c("Always" = "#ff66c4", "Not at all" = "#5271ff"))


ggsave("plot.png", plot = influence_plot, bg = "transparent")
