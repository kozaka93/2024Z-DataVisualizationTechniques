library(ggplot2)
library(dplyr)


health_music <- read.csv("C:\\Users\\alapr\\Desktop\\mxmh_survey_results.csv")

#4
health_music %>% filter(While.working != "", !is.na(While.working)) %>%
  ggplot(aes(x=Anxiety, group=While.working, fill=While.working), color = "white") +
geom_density(adjust=1.5, alpha=0.6) + scale_fill_manual(values = c( "#c1ff72", "magenta"))  +
  theme_ipsum() + labs(x = "Anxiety level", y  = NULL, fill = "Do you listen to music while working?", title = "Anxiety level among people who (don't) listen to music during work")  + 
  theme(plot.background = element_rect(fill = "#6d58f5", color = NA),
        axis.text = element_text(colour = "white"),
        legend.text = element_text(color = "white", face = "bold", size = 10),
        legend.title = element_text(colour = "white", face = "bold", size = 13),
        axis.title = element_text(colour = 'white', size = 13, face = 'bold'),
        plot.title = element_text(color = 'white', face = 'bold', size = 15, hjust = 0.5),
        legend.position = 'bottom')
  
#3
#
health_music %>% ggplot(aes(x= Anxiety, group =Frequency..Gospel. , fill = Frequency..Gospel.)) +
  geom_density(adjust=1.5, alpha=.5) + theme_ipsum() +   scale_fill_manual(values = c("magenta","#0097b2", "#c1ff72", "#ffbd57")) +
  labs(x = "Anxiety rate", y = NULL, fill = "Frequency Listening", title = "Anxiety level among people who listen to Gospel with different frequency") +
  theme(plot.background = element_rect(fill = "#6d58f5", color = NA),
        axis.text = element_text(colour = "white"),
        legend.text = element_text(color = "white", face = "bold"),
        legend.title = element_text(colour = "white", face = "bold"), 
        axis.title = element_text(size = 13, face = 'bold', colour = 'white'), 
        legend.position = c(0.2,0.8),
        plot.title =element_text(color = 'white', face = 'bold', size = 15, hjust = 0.5), 
        legend.key.size = unit(1.1, "cm"))


#2

#
health_music %>%filter(!is.na(BPM)) %>%  mutate(temp = case_when(BPM < 100 ~ "Low-range",
                                                                 BPM >= 100 & BPM < 130 ~"Mid-range", 
                                                                 BPM >= 130 ~"High-range"), temp = factor(temp, levels =c("Low-range", "Mid-range", "High-range"))) %>% 
  mutate(Anxiety = case_when(Anxiety < 3~"small",
                             Anxiety < 6~"medium",
                             TRUE~"big"), Anxiety = factor(Anxiety, levels = c("small", "medium", "big"))) %>% 
  ggplot(aes(factor(Anxiety, levels = ), group = temp, fill = temp)) + geom_bar(position = "fill") + theme_ipsum() +
  labs(x = "Anxiety level", y = NULL, fill = "BPM of music", title = "Impact of Music BPM Levels on Anxiety level") +
  theme(plot.background = element_rect(fill = "#6d58f5", color = NA),
        axis.text = element_text(colour = "white"), 
        legend.text = element_text(color = "white", face = "bold"),
        legend.title = element_text(colour = "white", face = "bold"), 
        axis.title = element_text(face = 'bold', colour = 'white'), 
        axis.title.x = element_text(size =12, hjust = 0.5), 
        legend.position = 'top', 
        plot.title = element_text(color = 'white', face = 'bold', size = 15, hjust = 0.5),
        legend.key.size = unit(1.2, 'cm')) +
  scale_fill_manual(values = c("Low-range" = "#c1ff72", "Mid-range" = "#0097b2","High-range" = "magenta"))
  
#1
health_music$Hours.per.day.cat <- factor(cut(health_music$Hours.per.day,
                                             breaks = c(0, 2.01, 8.01, Inf),
                                             labels = c("Small", "Medium", "Large"),
                                             right = FALSE),levels = c("Large", "Medium", "Small"))
description <- c("Small: 0-2h", "Medium: 2-8h", "Large: 8h+")

custom_colors <- c("Small" = "#c1ff72", "Medium" = "#0097b2", "Large" = "#ff00ff")

health_music%>%
  ggplot(aes(x = Hours.per.day.cat, y = Anxiety, fill = Hours.per.day.cat)) + 
  geom_boxplot(outliers = FALSE,color="white") + 
  coord_flip() + 
  scale_fill_manual(values = custom_colors, labels = description,breaks = c("Small", "Medium", "Large")) + 
  labs(fill = "Music Listening Duration",
       y="Anxiety rate",
       x="Music Listening Duration", title = "Impact of time spent listening to music on Anxiety rate")+
  theme(
    axis.title = element_text(color = "white",face = "bold", size = 13),          
    axis.text = element_text(color = "white",face = "bold", size =13),           
    legend.title = element_text(color = "white", hjust = 0.5,face = "bold", size =12),   
    legend.text = element_text(color = "white",face = "bold", size = 12),
    legend.background = element_rect(fill = "#6d58f5"), 
    legend.justification = "center",
    panel.background = element_rect(fill = "#6d58f5"),     
    plot.background = element_rect(fill = "#6d58f5"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),     
    panel.grid.minor = element_blank(), 
    plot.title = element_text(colour = 'white', size = 17, hjust = 0.5, vjust = -2, face = 'bold'), 
    legend.key.size = unit(1.5, 'cm'))

