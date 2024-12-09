library(dplyr)
library(tidyr)
library(ggplot2)

data <- read.delim("table333.txt", sep = "\t", header = TRUE)


fatal_data <- data %>% select(Year, Industry, Fatality.rate.per.100.000.workers)

fat_data <- fatal_data[1:192,]
fat_data <- fat_data %>% mutate(Fatality.rate.per.100.000.workers = as.numeric(gsub(",", ".", Fatality.rate.per.100.000.workers)),
                                  ) %>% 
    filter(Industry!="All industries",Industry!="Retail trade",Industry!= "Government", Industry!="Professional and business services",
           Industry!="Leisure and hospitality",  Industry!="Financial activities", Industry!="Manufacturing"
           , Industry!="Utilities")   #Industry!="Utilities", Industry!="Manufacturing"

fat_data <- fat_data %>%
  mutate(
    Industry = case_when(
      Industry == "Wholesale trade" ~ "Trade",
      Industry == "Agriculture, forestry, fishing, and hunting" ~ "Agriculture",
      Industry == "Transportation and warehousing" ~ "
Transportation",
      Industry == "Education and health services" ~ "Education",
      TRUE ~ Industry
    )
  )

my_theme <- function() {
  color.background = "white"
  color.text = "#22211d"
  theme_bw(base_size=15) +
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    theme(legend.position = "none") +
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
    theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

moja_paleta <- c( "#AE2012", "#9B2226", "#BB3E03","#94D2BD", "#0A9396","#CA6702","#005F73", "#EE9B00",
                   "#E9D8A6" )

plot<- ggplot(data = fat_data, aes(x = Year, y = Fatality.rate.per.100.000.workers, group = Industry)) +
  geom_hline(yintercept = seq(0, 27.5, by = 2.5), color = "white", linetype = "dotted", size = 0.5) + # Poziome linie co 5
  geom_vline(xintercept = c(min(fat_data$Year), max(fat_data$Year)), color = "white", size = 0.5) +
  geom_line(aes(color = Industry, alpha = 1), size = 2) +
  geom_point(aes(color = Industry, alpha = 1), size = 4)  +
  geom_point(color = 'transparent', size = 1) +
  scale_x_continuous(breaks = 2011:2022, minor_breaks = 2011:2022, expand = c(0.2,0.2))+
  scale_y_continuous(expand = c(0,0)) +
  scale_color_manual(values = moja_paleta) +
  scale_y_continuous(
    breaks = seq(0, 27.5, by = 2.5),  
    limit = c(0,30),
    expand = c(0,0)
  ) +
  theme(legend.position = "none") +
  labs(x = "Year",
       y ="Industry",
      ) +                    
  my_theme() + 
  theme(
    axis.title.x = element_blank(),  
    axis.title.y = element_blank(),  
    axis.text.x = element_blank(),   
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "white", size = 0.5),
    panel.background = element_rect(fill = "transparent", color = NA), 
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(0, 0, 0, 0)
  ) 
  

plot

#output_path <- file.path("C:/Users/karo/Desktop/TWD", "dominika.png")


#ggsave(output_path, plot = plot, bg = "transparent", width = 8, height = 6)


