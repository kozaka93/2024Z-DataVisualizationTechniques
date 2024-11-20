library(readxl);
library(dplyr);
library(tidyr);
library(stringr);
library(ggplot2);
library(patchwork);
library(cowplot);

icd_10 <- read.csv("C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_PLAKAT/morticd10_part5/Morticd10_part5_rev", 
                          header = TRUE,         
                          sep = ",",               
                          stringsAsFactors = FALSE 
)

icd_10_with_names <- icd_10 %>% 
  filter(List==104) %>% 
  left_join(country_codes, by=c('Country'='country')) %>% 
  filter(str_detect(Cause, "I10")) %>% 
  filter(name=="United States of America") %>% 
  select(Year, Sex, Deaths1) %>% 
  group_by(Year, Sex)

df_yearly_deaths <- icd_10_with_names %>%
  group_by(Year) %>%
  summarise(Total_Deaths = sum(Deaths1))

ggplot(df_yearly_deaths, aes(x = Year, y = Total_Deaths)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Zgony spowodowane nadciśnieniem w USA w latach 2017-2021",
       x = "Rok",
       y = "Liczba zgonów") +
  theme_minimal()

civ_dis_df <- read.csv("C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_PLAKAT/IHME-GBD_2021_DATA-435dea94-1/IHME-GBD_2021_DATA-435dea94-1.csv", 
                       header = TRUE,          
                       sep = ",",              
                       stringsAsFactors = FALSE
)


cigarettes <- read.csv("C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_PLAKAT/sales-of-cigarettes-per-adult-per-day/sales-of-cigarettes-per-adult-per-day.csv",
                       header = TRUE,         
                       sep = ",",              
                       stringsAsFactors = FALSE 
)

lung_cancer_usa <- read.csv("C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_PLAKAT/IHME-GBD_2021_DATA-b9661281-1/IHME-GBD_2021_DATA-b9661281-1.csv",
                            header = TRUE,         
                            sep = ",",              
                            stringsAsFactors = FALSE 
)

cig_us <- cigarettes %>% 
  filter(Entity == "United States") %>% 
  filter(Year >= 1940 & Year <= 2021)

cig_us_cut <- cigarettes %>% 
  filter(Entity == "United States") %>% 
  filter(Year >= 1962 & Year <= 1993)

plot1 <- ggplot(cig_us_cut, aes(x = Year, y = Manufactured.cigarettes.sold.per.adult.per.day)) +
  annotate("rect", xmin = 1962, xmax = 1993, ymin = -Inf, ymax = Inf, fill = "white", alpha = 0.2) +
  geom_line(color = "#74a9cf", linewidth = 1) +
  geom_point(color = "#74a9cf") +
  labs(
    title = "Sprzedaż papierosów w USA (1962-1993)",
    subtitle = "(dziennie na osobę dorosłą)",
    x = "Rok",
    y = "Sprzedane papierosy"
  ) +
  theme(
    plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(color = "white", hjust = 0.5, face = "bold"),
    panel.background = element_rect(fill = "transparent", color = "transparent"), #transparent panel bg
    plot.background = element_rect(fill = "transparent", color = "transparent"), #transparent plot bg
    legend.background = element_rect(fill = "transparent"), #transparent legend bg
    legend.box.background = element_rect(fill = "transparent"), #transparent legend panel
    legend.text = element_text(color = "white"),       # Legend text in white
    legend.title = element_text(color = "white"),      # Legend title in white
    axis.text = element_text(color = "white"),         # Axis tick labels in white
    axis.title = element_text(color = "white"),        # Axis titles in white
  )

plot1

plot2 <- ggplot(lung_cancer_usa, aes(x = year, y = val)) +
  geom_line(color = "#74a9cf", linewidth = 1) +
  geom_point(color = "#74a9cf") +
  labs(
    title = "Liczba zachorowań na raka płuc w USA (1990-2021)",
    x = "Rok",
    y = "Zachorowania / 100 tys. mieszkańców"
  ) +
  theme(
    plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
    panel.background = element_rect(fill = "transparent", color = "transparent"), #transparent panel bg
    plot.background = element_rect(fill = "transparent", color = "transparent"), #transparent plot bg
    legend.background = element_rect(fill = "transparent"), #transparent legend bg
    legend.box.background = element_rect(fill = "transparent"), #transparent legend panel
    legend.text = element_text(color = "white"),       # Legend text in white
    legend.title = element_text(color = "white"),      # Legend title in white
    axis.text = element_text(color = "white"),         # Axis tick labels in white
    axis.title = element_text(color = "white"),        # Axis titles in white
  )

cig_lung_usa <- lung_cancer_usa %>%
  select(year, val) %>%
  cbind(cig_us_cut) %>%
  rename(cig_sold_per_day = Manufactured.cigarettes.sold.per.adult.per.day) %>% 
  select(val, cig_sold_per_day) %>% 
  mutate(rate = (val / cig_sold_per_day))

plot3 <- ggplot(cig_lung_usa, aes(x = 1:nrow(cig_lung_usa), y = rate)) +
  geom_smooth(method = "lm", color = "#f1eef6", se = TRUE, alpha = 0.1) +
  geom_point(color = "#74a9cf", size = 2) +
  scale_y_log10() +
  labs(
    title = "Zachorowania na raka płuc a sprzedaż papierosów",
    x = "Kolejne odpowiadające sobie lata",
    y = "Zachorowania / Sprzedane papierosy"
  ) +
  theme(
    plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
    panel.background = element_rect(fill = "transparent", color = "transparent"), #transparent panel bg
    plot.background = element_rect(fill = "transparent", color = "transparent"), #transparent plot bg
    legend.background = element_rect(fill = "transparent"), #transparent legend bg
    legend.box.background = element_rect(fill = "transparent"), #transparent legend panel
    legend.text = element_text(color = "white"),       # Legend text in white
    legend.title = element_text(color = "white"),      # Legend title in white
    axis.text = element_text(color = "white"),         # Axis tick labels in white
    axis.title = element_text(color = "white"),        # Axis titles in white
  )

cig_us_1961_1962 <- cig_us %>% filter(Year %in% c(1961, 1962))
cig_us_1993_1994 <- cig_us %>% filter(Year %in% c(1993, 1994))

plot4 <- ggplot() +
  annotate("rect", xmin = 1962, xmax = 1993, ymin = 6.5, ymax = 11, fill = "white", alpha = 0.2) +
  geom_vline(xintercept = 1962, linetype = "dashed", color = "white") +
  geom_vline(xintercept = 1993, linetype = "dashed", color = "white") +
  geom_line(data = cig_us_before, aes(x = Year, y = Manufactured.cigarettes.sold.per.adult.per.day), color = "#256faf", linewidth = 1) +
  geom_point(data = cig_us_before, aes(x = Year, y = Manufactured.cigarettes.sold.per.adult.per.day), color = "#256faf") +
  geom_line(data = cig_us_highlight, aes(x = Year, y = Manufactured.cigarettes.sold.per.adult.per.day), color = "#74a9cf", linewidth = 1) +
  geom_point(data = cig_us_highlight, aes(x = Year, y = Manufactured.cigarettes.sold.per.adult.per.day), color = "#74a9cf") +
  geom_line(data = cig_us_after, aes(x = Year, y = Manufactured.cigarettes.sold.per.adult.per.day), color = "#256faf", linewidth = 1) +
  geom_point(data = cig_us_after, aes(x = Year, y = Manufactured.cigarettes.sold.per.adult.per.day), color = "#256faf") +
  geom_line(data = cig_us_1961_1962, aes(x = Year, y = Manufactured.cigarettes.sold.per.adult.per.day), color = "#256faf", linewidth = 1) +
  geom_line(data = cig_us_1993_1994, aes(x = Year, y = Manufactured.cigarettes.sold.per.adult.per.day), color = "#256faf", linewidth = 1) +
  labs(
    title = "Sprzedaż papierosów w USA (1940-2014)",
    subtitle = "(dziennie na osobę dorosłą)",
    x = "Rok",
    y = "Sprzedane papierosy"
  ) +
  theme(
    plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(color = "white", hjust = 0.5, face = "bold"),
    panel.background = element_rect(fill = "transparent", color = "transparent"), #transparent panel bg
    plot.background = element_rect(fill = "transparent", color = "transparent"), #transparent plot bg
    legend.background = element_rect(fill = "transparent"), #transparent legend bg
    legend.box.background = element_rect(fill = "transparent"), #transparent legend panel
    legend.text = element_text(color = "white"),       # Legend text in white
    legend.title = element_text(color = "white"),      # Legend title in white
    axis.text = element_text(color = "white"),         # Axis tick labels in white
    axis.title = element_text(color = "white"),        # Axis titles in white
  )

combined_plot <- plot_grid(plot1, plot4, plot2, plot3, ncol = 2)


combined_plot 
  

ggsave("wykres_x.png",
       plot = last_plot(), bg = "transparent",
       width = 3000, height = 2000, units = "px")


#256faf
#74a9cf
#bdc9e1
#f1eef6






