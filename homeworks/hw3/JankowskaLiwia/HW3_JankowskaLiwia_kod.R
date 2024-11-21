# Praca domowa 3

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

country <- c("Germany", "France", "Netherlands", "Poland", "Italy", "Ireland", "Spain", "Other EU")
value_perc <- c(22.4, 16.2, 9.6, 9, 8.9, 6, 5.1, 22.9)

df_milk_by_country = data.frame(Country = country, Value_perc = value_perc)

df_milk_by_country %>% 
  mutate(Country = fct_reorder(Country, Value_perc)) %>%
  ggplot() +
  geom_col(aes(y = Value_perc, x = Country, fill = Country)) +
  scale_fill_manual(values = c("#C5C7B7", "#AAAE8E", "#969E88",
                               "#828E82", "#607B7D", "#4D6E76",
                               "#3A606E", "#355764")) +
  theme_minimal() +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Collection of cows' milk by dairies",
    subtitle = "(%, 2023)\n\nEU total: 145.0 milion tonnes",
    y = "Percentage"
  )



