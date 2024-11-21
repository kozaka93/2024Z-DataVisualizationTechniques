install.packages("tidyr")
library(tidyr)
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)

df1 <- read.csv("C:/Users/macie/OneDrive/Pulpit/PW/III semestr/TWD/projekt 1/Table1.csv")
df2 <- read.csv("C:/Users/macie/OneDrive/Pulpit/PW/III semestr/TWD/projekt 1/Table2_5.csv")
df3 <- read.csv("C:/Users/macie/OneDrive/Pulpit/PW/III semestr/TWD/projekt 1/Table3.csv")
df4 <- read.csv("C:/Users/macie/OneDrive/Pulpit/PW/III semestr/TWD/projekt 1/Table4.csv")


intercept <- -1.998
coef_divorced <- 0.366
coef_divorce_rate <- 0.094
interaction_coef <- -0.188


df2 <- df2 %>%
  mutate(
    predicted_visits_married = exp(intercept + coef_divorce_rate * Divorce_rate),
    predicted_visits_divorced = exp(intercept + coef_divorced + (coef_divorce_rate + interaction_coef) * Divorce_rate)
  ) %>%
  pivot_longer(cols = c(predicted_visits_married, predicted_visits_divorced), 
               names_to = "Status", values_to = "Predicted_Visits") %>%
  mutate(Status = recode(Status, 
                         predicted_visits_married = "Married", 
                         predicted_visits_divorced = "Divorced/Separated"))

ggplot(df2, aes(x = Divorce_rate, y = Predicted_Visits, color = Status)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Predicted Mental Health Visits by Divorce Rate and Marital Status",
    x = "Divorce Rate in Country",
    y = "Predicted Visits to Specialists"
  ) +
  scale_color_manual(values = c("Married" = "navyblue", "Divorced/Separated" = "cyan")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  ) +
  coord_cartesian(ylim = c(0, 0.5), xlim = c(0, 4)) +
  guides(color = guide_legend(title = "Marital Status"))


ggsave("przewidywane_wizyty.png", 
       plot = last_plot(), 
       width = 8, height = 6, dpi = 300)