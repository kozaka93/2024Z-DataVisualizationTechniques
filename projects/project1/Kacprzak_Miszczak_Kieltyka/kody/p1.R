doctors <- read.csv("C:/Users/szymo/Desktop/TWD/Projekt 1/data.csv")
life_expectancy <- read.csv("C:/Users/szymo/Desktop/TWD/Projekt 1/life_expectancy.csv")
health_general <- read.csv("C:/Users/szymo/Desktop/TWD/Projekt 1/health-financing/data/GHED_CHEGDP_SHA2011.csv")
health_general_pc <- read.csv("C:/Users/szymo/Desktop/TWD/Projekt 1/health-financing/data/GHED_CHE_pc_US_SHA2011.csv")
health_private <- read.csv("C:/Users/szymo/Desktop/TWD/Projekt 1/health-financing/data/GHED_OOP_pc_US_SHA2011.csv")
health_government <- read.csv("C:/Users/szymo/Desktop/TWD/Projekt 1/health-financing/data/GHED_PVT-D_pc_US_SHA2011.csv")
country <- read.csv("C:/Users/szymo/Desktop/TWD/Projekt 1/health-financing/codes/COUNTRY.csv")
happiness <- read.csv("C:/Users/szymo/Desktop/TWD/Projekt 1/poprawne_daneszczescie2021.csv")
country_codes <- read.csv("C:/Users/szymo/Desktop/TWD/Projekt 1/country-and-continent-codes-list-csv.csv")
health_oop_percent <- read.csv("C:/Users/szymo/Desktop/TWD/Projekt 1/health-financing/data/GHED_OOPSCHE_SHA2011.csv")
health_government_percent <- read.csv("C:/Users/szymo/Desktop/TWD/Projekt 1/health-financing/data/GHED_GGHE-DCHE_SHA2011.csv")
health_private_percent <- read.csv("C:/Users/szymo/Desktop/TWD/Projekt 1/health-financing/data/GHED_PVT-DCHE_SHA2011.csv")
health_external_percent <- read.csv("C:/Users/szymo/Desktop/TWD/Projekt 1/health-financing/data/GHED_EXTCHE_SHA2011.csv")


library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)


health_general_pc %>% 
  inner_join(life_expectancy %>% filter(Dim1 == "Both sexes", IndicatorCode == "WHOSIS_000001"),
             by = join_by("SpatialDimensionValueCode" == "SpatialDimValueCode", "TimeDimensionValue" == "Period")) %>%
  filter(TimeDimensionValue == 2021) %>%
  rename("Country" = "SpatialDimensionValueCode", "Year" = "TimeDimensionValue",
         "GeneralSpendings" = "Value.x", "LifeExpectancy" = "FactValueNumeric") %>%
  left_join(country_codes, by = c("Country" = "Three_Letter_Country_Code")) %>% 
  select(Country_Name, Year, GeneralSpendings, LifeExpectancy, Continent_Name) %>% 
  ggplot(aes(x = GeneralSpendings, y = LifeExpectancy)) +
  scale_x_log10() +
  geom_smooth(color = "black") +
  aes(x = GeneralSpendings, y = LifeExpectancy, colour = Continent_Name) +
  geom_point() +
  labs(title = "Life expectancy vs general health spendings",
       x = "General health spendings per capita",
       y = "Life expectancy",
       colour = "Continent") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  scale_colour_manual(values = c("#e41a1c",
                                 "#dddd33",
                                 "#4daf4a",
                                 "#377eb8",
                                 "#984ea3",
                                 "#df8f11"))


health_oop_percent %>%
  inner_join(health_government_percent, by = c("SpatialDimensionValueCode", "TimeDimensionValue")) %>%
  rename("OOP" = "NumericValue.x", "Government" = "NumericValue.y") %>%
  inner_join(health_private_percent, by = c("SpatialDimensionValueCode", "TimeDimensionValue")) %>%
  rename("Private" = "NumericValue") %>%
  inner_join(health_external_percent, by = c("SpatialDimensionValueCode", "TimeDimensionValue")) %>%
  rename("External" = "NumericValue") %>%
  inner_join(health_general_pc, by = c("SpatialDimensionValueCode", "TimeDimensionValue")) %>%
  rename("TotalSpendings" = "NumericValue") %>%
  left_join(life_expectancy %>% filter(Dim1 == "Both sexes", IndicatorCode == "WHOSIS_000001"),
             by = join_by("SpatialDimensionValueCode" == "SpatialDimValueCode", "TimeDimensionValue" == "Period")) %>%
  rename("LifeExpectancy" = "FactValueNumeric") %>%
  mutate(External = ifelse(is.na(External), 0, External),
         LifeExpectancy = case_when(LifeExpectancy < 65 ~ "<65",
                                    LifeExpectancy < 70 ~ "65-70",
                                    LifeExpectancy < 75 ~ "70-75",
                                    LifeExpectancy < 80 ~ "75-80",
                                    .default = "80-85"),
         Private = Private/100,
         Government = Government/100,
         External = External/100) %>%
  filter(TimeDimensionValue == 2021) %>%
  rename("Country" = "SpatialDimensionValueCode", "Year" = "TimeDimensionValue") %>%
  inner_join(country_codes, by = c("Country" = "Three_Letter_Country_Code")) %>% 
  select(Country, Year, Government, Private, External, TotalSpendings, LifeExpectancy) %>%
  group_by(LifeExpectancy) %>%
  summarise(
    avg_total_spendings = mean(TotalSpendings, na.rm = TRUE),
    avg_government = mean(Government, na.rm = TRUE) * avg_total_spendings,
    avg_private = mean(Private, na.rm = TRUE) * avg_total_spendings,
    avg_external = mean(External, na.rm = TRUE) * avg_total_spendings
  ) %>%
  pivot_longer(cols = c(avg_government, avg_private, avg_external), 
               names_to = "Spending_Type", values_to = "Average_Spending") %>%
  mutate(Spending_Type = factor(Spending_Type, levels = c("avg_external", "avg_private", "avg_government"))) %>%
  ggplot(aes(x = LifeExpectancy, y = Average_Spending, fill = Spending_Type)) +
  geom_col(position = "fill") +
  labs(
    title = "Health expenditure by life expectancy",
    x = "Life expectancy",
    y = "Percentage of total spendings",
    fill = "Average\nspending type"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank()
  ) +
  scale_fill_manual(values = c("#00BFFF", "#4169E1", "#00008B"), labels = c("External", "Private", "Government"))
