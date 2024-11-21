library(dplyr)
library(ggplot2)
library(showtext)
library(systemfonts)
library(svglite)
library(tidyr)

# Ustawianie ścieżki do czcionki------------------------------------------------
arial_path <- match_fonts("Arial")
font_add("Arial", "C:\\WINDOWS\\Fonts\\arial.ttf") # Dodaj czcionkę ręcznie
showtext_auto()

# Kolory: 
c_baza <- "#EADCCE"
c_11 <- "#B0ACB1"
c_12 <- "#767B93"
c_13 <- "#1A274C"
c_21 <- "#E9B26E"
c_22 <- "#FBEAC5"

# Stałe:
plot_title_size = 40
axis_text_size = 30
axis_title_size = 30
legend_title_size = 30
legend_text_size = 30
line_width = 3

# Wspólny theme:
theme_df <- theme(
  panel.background = element_rect(fill='transparent'), 
  plot.background = element_rect(fill='transparent', color=NA), 
  legend.background = element_rect(fill='transparent'),
  plot.title = element_text(size = plot_title_size, color = c_baza, family = 'Arial'),
  axis.text = element_text(color=c_baza,size = axis_text_size),
  axis.title = element_text(size=axis_title_size,color=c_baza),
  legend.title = element_text(size=legend_title_size, color = c_baza),
  legend.text = element_text(size=legend_text_size, color = c_baza),
)

################################################################################
# Ramki ------------------------------------------------------------------------
df_sleep_health <-  read.csv("Sleep_health_and_lifestyle_dataset.csv")
df_mental_health <- read.csv("mental_health_dataset.csv")

df_sleep_health <- df_sleep_health %>%
  mutate(IT_or_not = ifelse(Occupation == "Engineer" | Occupation == "Software Engineer", "IT", "notIT"))

df_mental_health <- df_mental_health %>%
  mutate(IT_or_not = ifelse(Occupation == "Engineering" | Occupation == "IF", "IT", "notIT"))

#-------------------------------------------------------------------------------
# Rozkład poziomu stresu w IT i nie IT

ggplot(df_sleep_health, aes(x = Stress.Level, fill = IT_or_not)) +
  geom_density(alpha = 0.9) +
  labs(x = "stress level", y = "density", fill = "industry") +
  ggtitle("Stress Level Distribution Among IT and Non-IT Workers") +
  scale_fill_manual(values = c(c_21, c_13)) +
  theme_df +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  ) +
  labs(
    x = "Stress level",
    y = "Density",
    fill = "Industry"
  )

ggsave("density_stress_sized .svg", width = 18, height = 10, dpi = 300)

# ------------------------------------------------------------------------------
# Średnia powaga choroby psychicznej od zawodu

df_plot_4 <- df_mental_health %>%
  mutate(
    Severity_numeric = case_when(
      Severity == "None" ~ 0,
      Severity == "Low" ~ 1,
      Severity == "Medium" ~ 3,
      Severity == "High" ~ 5
    )
  )

df_gender_occupation_4 <- df_plot_4 %>%
  group_by(Occupation, Gender) %>%
  summarise(avg_severity = mean(Severity_numeric), .groups = 'drop') %>%
  filter(!Gender %in% c("Non-binary", "Prefer not to say"))

ggplot(df_gender_occupation_4, aes(x = Occupation, y = Gender, fill = avg_severity)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = c_21, high = c_13, name = "Average Severity") +
  labs(x = "occupation", y = "gender") +
  theme_minimal() +
  ggtitle("Average severity of mental illness by occupation and gender") +
  theme_df +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Zakrzywia napisy
    panel.grid = element_blank(),          # Usuwa linie siatki
    panel.border = element_blank(),
    legend.key.height = unit(1, "cm"), # Rozciągnięcie legendy
    legend.title = element_text(margin = margin(b = 15)) 
  ) +
  labs(
    x = "Occupation",
    y = "Gender",
  )

ggsave("averege_severity_gender.svg", width = 18, height = 10, dpi = 300)


#-------------------------------------------------------------------------------
# Średnia jakość snu i poziom stresu od wieku

# normalizacja min-max od 0 do 10
df_sleep_health_scaled <- df_sleep_health %>%
  mutate(
    scaled_stress_level = ((Stress.Level - min(Stress.Level) +1) / (max(Stress.Level) - min(Stress.Level))) * 10,
    scaled_sleep_qual = ((Quality.of.Sleep - min(Quality.of.Sleep)) / (max(Quality.of.Sleep) - min(Quality.of.Sleep))) * 10,
    scaled_sleep_dur = ((Sleep.Duration - min(Sleep.Duration)) / (max(Sleep.Duration) - min(Sleep.Duration))) * 10
  )

df_sleep_health_scaled %>% 
  filter(Occupation == "Software Engineer" | Occupation == "Engineer") %>% 
  group_by(Age) %>% 
  summarise(mean_sleep_duration = mean(scaled_sleep_dur, na.rm = TRUE), # Obliczanie średnich
            mean_quality_of_sleep = mean(scaled_sleep_qual, na.rm = TRUE),
            mean_stress_level = mean(scaled_stress_level, na.rm = TRUE)) %>% 
  arrange(Age) %>% 
  ggplot(aes(x = Age)) +
  geom_line(aes(y = mean_sleep_duration, color = "Sleep len"), size = line_width) + # Tworzenie linii oddzielnie
  geom_line(aes(y = mean_quality_of_sleep, color = "Sleep qual"), size = line_width) +
  geom_line(aes(y = mean_stress_level, color = "Stress level"), size = line_width) +
  labs(title = "Average Stress Level, Sleep Quality and Duration by Age",
       x = "Age",
       y = "Average") +
  scale_color_manual( # Kolory
    values = c("Stress level" = c_12, "Sleep qual" = c_21, "Sleep len" = c_22),
    labels = c("Stress Level", "Sleep Quality", "Sleep Duration"),
    breaks = c("Stress level", "Sleep len", "Sleep qual")  # Zmiana kolejności kategorii w legendzie
  ) +
  theme_minimal() +
  theme_df + # Wspólny theme
  theme(
    panel.grid = element_blank(), # Usuwa linie siatki
    legend.title = element_blank()
  )


ggsave("average_stress_sleep_by_age.svg", width = 18, height = 10, dpi = 300)
?legend.title

#-------------------------------------------------------------------------------
# Porównanie w IT ze względu na płeć

# Przekształcenie na format long żeby użyć facet_wrap
sleep_health_long <- df_sleep_health_scaled %>%
  filter(IT_or_not == "IT") %>% 
  pivot_longer(cols = c(scaled_sleep_qual, scaled_sleep_dur, scaled_stress_level),  # Wybieramy kolumny do przekształcenia
               names_to = "category",  # Nazwa kategorii (jak jakości snu, ilości snu, poziomu stresu)
               values_to = "value") %>% 
  select(category, value, Gender)

# Obliczanie średnich 
sleep_health_summary <- sleep_health_long %>%
  group_by(Gender, category) %>%
  summarize(avg_value = mean(value, na.rm = TRUE), .groups = "drop")

sleep_health_summary %>%
  group_by(Gender, category) %>% 
  ggplot(aes(x = Gender, y = avg_value, fill = Gender)) +
  geom_col() +
  facet_wrap(~ category, # Rozbicie na trzy wykresy
             labeller = labeller(category = c("scaled_sleep_dur" = "Sleep Duration", 
                                              "scaled_sleep_qual" = "Sleep Quality", 
                                              "scaled_stress_level" = "Stress Level"))) +
  labs(title = "Comparison in the IT Industry by Gender",
       x = "Gender",
       y = "Average") +
  scale_fill_manual( # Kolor
    values = c(c_13, c_21),
    ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), # Usuwa male/female
        axis.title.x = element_blank(), # Usuwa gender
        panel.grid = element_blank(), # Usuwa linie siatki
        strip.text = element_text(size = axis_text_size, colour = c_baza) # Podpis do grid
  ) +
  theme_df # Wspólny theme

ggsave("comparison_IT_by_gender.svg", width = 18, height = 10, dpi = 300)




