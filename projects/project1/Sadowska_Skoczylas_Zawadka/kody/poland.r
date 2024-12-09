library(dplyr)
library(ggplot2)
library(scales)
poland <- read.csv("Polska_1966.csv", sep = ";", na.strings = "–")

poland_sum <- poland %>% 
  mutate(kidney = rowSums(poland[,c("Kidney_DD","Kidney_LD","Kidney_Pancreas","Liver_Kidney","Heart_Kidney","Kidney_Lung")], na.rm = TRUE),
         heart = rowSums(poland[,c("Heart","Heart_Lung","Heart_Kidney","Heart_Liver")],na.rm=TRUE),
         liver = rowSums(poland[,c("Liver_DD","Liver_LD","Liver_Kidney","Heart_Liver","Lung_Liver")],na.rm=TRUE),
         pancreas = rowSums(poland[,c("Pancreas","Kidney_Pancreas","Liver_Pancreas")],na.rm=TRUE),
         lung = rowSums(poland[,c("Lung_LD","Lung_DD","Heart_Lung","Lung_Liver","Kidney_Lung")],na.rm=TRUE))
poland_sum <- poland_sum %>% 
  mutate(total = rowSums(poland_sum[,c("kidney","heart","liver","pancreas","lung")], na.rm = TRUE))

poland_plot <- ggplot(poland_sum, aes(x=factor(Year))) +
  geom_bar(aes(y=total), stat="identity", fill="#aecfee") +
  labs(x="Year", y="Number of organs") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,2000),
                     breaks = seq(0, 2000, by = 500))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1, family = "Calibri", size = 11),
        axis.text = element_text(color = "white", family = "Calibri", size = 11),
        axis.title = element_text(color = "white", family = "Calibri", size = 11),
        panel.grid.major.x = element_blank(),  
        panel.grid.major.y = element_line() 
  )
poland_plot
ggsave("poland_plot.png", poland_plot, width = 10, height = 5)
