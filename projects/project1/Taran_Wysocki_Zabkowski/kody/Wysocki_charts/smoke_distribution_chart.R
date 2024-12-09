library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(grid)
frm2 <- read.csv("C:/Users/Admin/Downloads/frmgham2.csv")
wiek_zgounu_palacza <- frm2 %>% filter(DEATH == 1 & PERIOD == 1 & (ANYCHD ==1 | STROKE == 1)) %>% 
  group_by(RANDID) %>% 
  mutate(zgon = AGE + round(TIMEDTH/365), Smoker = case_when(CURSMOKE ==1 ~"yes", .default = "no"))
jakiscos <- wiek_zgounu_palacza %>% filter(Smoker == "yes") %>% pull(zgon)
jakiscos2 <- wiek_zgounu_palacza %>% filter(Smoker == "no") %>% pull(zgon)
quantiles1 <- quantile(jakiscos, probs= c(0.1,0.5,0.9)) 
quantiles2 <- quantile(jakiscos2, probs= c(0.1,0.5,0.9)) 
means <- aggregate(zgon ~ Smoker, data = wiek_zgounu_palacza, FUN = median)
canva <- ggplot(wiek_zgounu_palacza, aes(x = zgon, y = 0, fill = Smoker)) +
  geom_density_ridges(scale = 1, alpha = 0.3, 
                      quantile_lines = TRUE, quantiles = c(0.1, 0.9)) +
  scale_x_continuous(expand = c(0,0)) +
  coord_cartesian(clip = "off") + 
  theme_ridges() + 
  theme(
    panel.background = element_rect(fill = "#a09386", color = NA), 
    plot.background = element_rect(fill = "#a09386", color = NA),
    axis.text.x = element_text(size = 14, color = "white"), 
    axis.title.x = element_text(size = 14,hjust = 0.5, color = "white"),
    axis.title.y = element_text(size = 14,hjust = 0.5, color = "white"), 
    axis.text.y = element_text(color = "white"), 
    text = element_text(color = "white"),
    axis.title = element_text(color = "white"), 
    legend.text = element_text(color = "white"), 
    legend.title = element_text(color = "white")
  ) + scale_fill_manual(values = c("#f0dfd3","#A24033")) + 
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  annotate("text", x = quantiles1, y = -0.003, label = quantiles1, color = "white") +
  annotate("text", x = quantiles2, y = -0.003, label = quantiles2, color = "white") +
  geom_segment(aes(x = 67, xend = 74, y = 0.038, yend = 0.038),
               arrow = arrow(type = "closed", length = unit(0.1, "inches"))) +
  geom_segment(aes(x = 74, xend = 67, y = 0.038, yend = 0.038),
               arrow = arrow(type = "closed", length = unit(0.1, "inches"))) + 
  labs(x = "Death age", y = "Density") +
  annotate("text", x = 70, y = 0.044, label = "7 years", color = "white") +
  geom_segment(data = means, aes(color = Smoker, y =0, yend = 0.04), 
             linetype = "dashed", size = 1) +
  scale_color_manual(values = c("no" = "#7B4B33", "yes" = "#7B4B33")) +
  guides(fill = guide_legend(override.aes = list(shape = NA)),color = "none")


ggsave("wykres.png", plot = canva, width = 8, height = 4, dpi = 300) 
