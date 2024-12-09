library(ggplot2)
library(dplyr)

dane <- read.csv("dane_elekcje.csv")


p <- dane %>% 
  ggplot(aes(x = factor(year), y = votes_in_milions, fill = party)) +
  geom_col(position = "dodge2") +
  scale_y_continuous(limits = c(0,90),
                     expand = expansion(mult = c(0, 0)),
                     breaks = seq(0,90,10),
                     minor_breaks = seq(0,90,5)) + 
  scale_fill_manual(values = c("blue","red")) + 
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) + 
  labs(
    title = "US Presidential Popular Vote",
    y = "Number of votes in milions",
    fill = "Party"
  )
p
