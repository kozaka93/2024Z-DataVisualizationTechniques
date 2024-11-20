library(dplyr)
library(ggplot2)
library(maps)
library(ggridges)
library(stringr)

toothpaste_affordability <- read.csv('toothpaste_affordability.csv')
toothpaste_labour_days <- read.csv('toothpaste_labour_days.csv')
dentists <- read.csv('dentists.csv')

toothpaste_affordability <- toothpaste_affordability %>%  select(where(~ !all(is.na(.))))
toothpaste_labour_days  <- toothpaste_labour_days %>%  select(where(~ !all(is.na(.))))
dentists <- dentists %>%  select(where(~ !all(is.na(.))))

######## RIDGE
region_labels <- c(
    "Africa" = "Afryka",
    "Americas" = "Ameryka",
    "Eastern Mediterranean" = "Wschodnie Śródziemnomorze",
    "Europe" = "Europa",
    "South-East Asia" = "Azja Południowo-Wschodnia",
    "Western Pacific" = "Zachodni Pacyfik"
)

wrapped_region_labels <- sapply(region_labels, function(x) str_wrap(x, width = 10))

custom_colors <- c(
    "Africa" = "#5de1e6",               
    "Americas" = "#3d9ca4",          
    "Eastern Mediterranean" = "#f9dca0", 
    "Europe" = "#f27c7d",           
    "South-East Asia" = "#ffb8b1",   
    "Western Pacific" = "#ffea7f"   
)

plot <- toothpaste_labour_days %>%
    ggplot(aes(x = FactValueNumeric, y = ParentLocation, fill = ParentLocation)) + 
    geom_density_ridges(color = "white", scale = 0.9, show.legend = FALSE) +
    labs(
        x = "Dni pracy", 
        y = "Region"
    ) +
    scale_y_discrete(labels = wrapped_region_labels) + 
    scale_fill_manual(values = custom_colors) +  
    theme(
        text = element_text(color = "white", size = 27, face = "bold"), 
        axis.text.x = element_text(color = "white", size = 27, face = "bold"), 
        axis.text.y = element_text(color = "white", size = 20, face = "bold"),  
        axis.ticks = element_line(color = "white"),  
        panel.background = element_rect(fill = "transparent", color = NA), 
        plot.background = element_rect(fill = "transparent", color = NA),  
        legend.background = element_rect(fill = "transparent"),             
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()  
    )


ggsave("ridgeline_plot_transparent.png", plot = plot, width = 12, height = 8, bg = "transparent")


####################################################################################################################
# AVAILABILITY OF DENTISTS 
####################################################################################################################

min_year <- min(dentists$Period)
max_year <- max(dentists$Period)

interval_labels <- c(glue::glue("{min_year}-2000"), "2001-2011", glue::glue("2012-{max_year}"))

df <- dentists %>%
  mutate(YearInterval = cut(Period, 
                            breaks = c(min_year, 2000, 2011, max_year),
                            labels = interval_labels,
                            include.lowest = TRUE))

plot2_box <- df %>% 
  ggplot(aes(x = YearInterval, y = FactValueNumeric, fill = ParentLocation)) +
  geom_boxplot(color = "white", outlier.color = "white", size=2) +
  labs(x = "Przedział lat", 
       y = "Liczba dentystów na 10000 mieszkańców",
       fill = "Region") +
  scale_fill_manual(values = custom_colors,
            labels = region_labels) +  
  theme(text = element_text(color = "white", size = 27, face = "bold"),
        axis.text.x = element_text(color = "white", size = 27, face = "bold"), 
        axis.text.y = element_text(color = "white", size = 27, face = "bold"), 
        axis.ticks = element_line(color = "white"),  
        panel.border = element_rect(color = "white", fill = NA, size = 2),
        panel.background = element_rect(fill = "transparent", color = NA), 
        plot.background = element_rect(fill = "transparent", color = NA),   
        legend.background = element_rect(fill = "transparent"),            
        legend.box.background = element_rect(fill = "transparent", color = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "top"
    ) + guides(fill = guide_legend(override.aes = list(color = NA, size = 5)))

ggsave("dentist_availability_boxplot.png", plot = plot2_box, width = 20, height = 20)
