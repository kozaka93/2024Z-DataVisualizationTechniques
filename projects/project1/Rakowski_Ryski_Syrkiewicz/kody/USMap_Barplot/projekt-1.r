# loading the libraries
library(dplyr)
library(ggplot2)
library(usmap)
library(stringr)

# loading the data frames
fatalities_by_county <- read.csv("/home/antoni/Uni/Semestr-3/TWD/Projekty/Projekt-1/fatalities_by_county.csv") #nolint
overdoses_by_group <- read.csv("/home/antoni/Uni/Semestr-3/TWD/Projekty/Projekt-1/overdoses_by_group.csv") # nolint
# sources:
# https://catalog.data.gov/dataset/nchs-drug-poisoning-mortality-by-county-united-states # nolint
# https://wisqars.cdc.gov/reports/?o=MORT&y1=2018&y2=2022&t=0&i=1&m=21170&g=00&me=0&s=0&r=0&ry=2&e=0&yp=65&a=ALL&g1=0&g2=199&a1=0&a2=199&r1=SEX&r2=YEAR&r3=AGEGP&r4=RACE-SINGLE #nolint

# Plot 1

# checking the range of years
fatalities_by_county %>%
  select(Year) %>%
  unique() %>%
  arrange(Year)
# 2003 - 2021

# renaming a column
fatalities_by_county <- fatalities_by_county %>%
  rename(fips = FIPS)

# checking if all counties have records for all years
fatalities_by_county %>%
  group_by(fips) %>%
  summarise(n = n()) %>%
  filter(n != 19) %>%
  ungroup()
# affirmative

# extracting the death rate for each county
# for the most recent year (2021)
death_rate_by_county <- fatalities_by_county %>%
  filter(Year == 2021) %>%
  select(fips, death_rate = Model.based.Death.Rate)

# plotting the data
usmap_plot <- plot_usmap(data = death_rate_by_county, values = "death_rate") +
  scale_fill_gradient(name = "Death rate per 100k\n(log scale)",
                      low = "#ffffff", high = "#5b2360", trans = "log10") +
  labs(title = "Death rate by county (2021)") +
  theme_void() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"))

usmap_plot

# Plot 2

# Exploring data

overdoses_by_group %>%
  pull(Sex) %>%
  unique()

overdoses_by_group %>%
  pull(Year) %>%
  unique()

overdoses_by_group %>%
  pull(Race) %>%
  unique()

overdoses_by_group %>%
  pull(Age.Group) %>%
  unique()

# Analysing drug use in 2022 by social group

df <- overdoses_by_group %>%
  select(Sex, Year, Age.Group, Race, Deaths, Population) %>%
  filter(Sex %in% c("Males", "Females")) %>%
  mutate(Sex = factor(Sex, levels = c("Males", "Females"))) %>%
  filter(Year == 2022) %>%
  mutate(Race = str_replace_all(Race, "American Indian / Alaska Native",
                                "Native")) %>%
  filter(Race %in% c("White", "Black", "Asian",
                     "Native")) %>%
  filter(Age.Group != "Unknown" & Age.Group != "")  %>%
  mutate(Age.Group = case_when(Age.Group %in% c("0 to 4", "5 to 9", "10 to 14", "15 to 19") ~ "0-19", #nolint
                               Age.Group %in% c("20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44") ~ "20-44", #nolint
                               Age.Group %in% c("45 to 49", "50 to 54", "55 to 59", "60 to 64") ~ "45-64", #nolint
                               TRUE ~ "65+")) %>% 
  mutate(Deaths = str_extract(Deaths, "[0-9,]+")) %>%
  filter(!is.na(Deaths)) %>%
  mutate(Deaths = as.numeric(str_replace_all(Deaths, ",", ""))) %>%
  mutate(Population = as.numeric(str_replace_all(Population, ",", ""))) %>%
  group_by(Sex, Race, Age.Group) %>%
  summarise(Death_rate = 100000 * sum(Deaths) / sum(Population)) %>%
  ungroup()

# Plotting the data

barplot <- ggplot(df, aes(x = Race, y = Death_rate, fill = Age.Group)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~ Sex) +
  scale_fill_manual(values = c("#77567A", "#C47AC0", "#E39EC1", "#DEBAC0")) +
  labs(title = "Opioid Death Rate by Social Group  (2022)",
       x = "Race",
       y = "Death Rate per 100k",
       fill = "Age Group") +
  scale_x_discrete(limits = c("Black", "Native", "White", "Asian")) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12, face = "bold")
  )

barplot