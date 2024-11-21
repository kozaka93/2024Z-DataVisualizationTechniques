library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)

# map - drug consumption in Europe
deaths <- read.csv("drug_induced_mortality_europe.csv")
Europe <- map_data("world") %>%
  filter(long > -14 & long < 45 & lat > 36 & lat < 73)

deaths %>%
  select(Country,Total) %>%
  mutate(Country=ifelse(Country=="Czechia","Czech Republic",Country)) %>% 
  mutate(Country=ifelse(Country=="Türkiye","Turkey",Country)) %>% 
  right_join(Europe,join_by(Country==region)) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = Total))+
  geom_polygon(color = "#F0F4EF",size=0.2) +
  scale_fill_gradient(high="#F2059F",low="#433df2", na.value = "gray90", name = "Mortality rates\nper million") +
  theme_void() +  
  labs(title = "Drug-induced mortality rates per million among adults (15-64).") +
  theme(legend.position = "right",
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(color = "#F0F4EF",face="bold",size=12),
        legend.title = element_text(color = "#F0F4EF",size=10),
        legend.text = element_text(color = "#F0F4EF",size=10))
ggsave("drugInducedMortalityEurope.png", bg = "transparent", width = 10, height = 6)

# drug consumption UCI - dataframe preparation
drug_consumption_specific_drugs <- read.csv("drug_consumption_uci.csv")
head(drug_consumption_specific_drugs)

drug_consumption_specific_drugs <- drug_consumption_specific_drugs %>% 
  filter(Semer == "CL0") %>% # removes reponses that claimed they did take a non-existent drug (these are likely overclaimers)
  select(-Nscore, -Escore, -Oscore, -AScore, -Cscore, -Impulsive, -SS, -Semer, -Choc) # removes unnecessary data dimensions

# CL0 Never Used
# CL1 Used over a Decade Ago
# CL2 Used in Last Decade
# CL3 Used in Last Year
# CL4 Used in Last Month
# CL5 Used in Last Week
# CL6 Used in Last Day

# grouped bar chart - drug type (depressant / psychedelic / stimulant) vs age group (only active users)
drug_consumption_specific_drugs %>% 
  mutate(psychedelic_user = ifelse((Ketamine != "CL0" & Ketamine != "CL1" & Ketamine != "CL2") | (LSD != "CL0" & LSD != "CL1" & LSD != "CL2") | (Mushrooms != "CL0" & Mushrooms != "CL1" & Mushrooms != "CL2"), 1, 0),
         stimulant_user= ifelse((Amphet != "CL0" & Amphet != "CL1" & Amphet != "CL2") | (Coke != "CL0" & Coke != "CL1" & Coke != "CL2") | (Crack != "CL0" & Crack != "CL1" & Crack != "CL2"), 1, 0), 
         depressant_user = ifelse((Amyl != "CL0" & Amyl != "CL1" & Amyl != "CL2") | (Benzos != "CL0" & Benzos != "CL1" & Benzos != "CL2") | (Heroin != "CL0" & Heroin != "CL1" & Heroin != "CL2") | (Meth != "CL0" & Meth != "CL1" & Meth != "CL2"), 1, 0)) %>% 
  select(Age, psychedelic_user, stimulant_user, depressant_user) %>% 
  group_by(Age) %>% 
  mutate(ag_cnt = n(), 
         psych_cnt = sum(psychedelic_user), 
         stim_cnt = sum(stimulant_user), 
         depr_cnt = sum(depressant_user), 
         psych_percent = psych_cnt / ag_cnt * 100, 
         stim_percent = stim_cnt / ag_cnt * 100, 
         depr_percent = depr_cnt / ag_cnt * 100) %>% 
  select(Age, psych_percent, stim_percent, depr_percent) %>% 
  ungroup() %>% 
  distinct() %>% 
  arrange(Age) %>% 
  pivot_longer(!Age, names_to='drug_type', values_to = 'ag_percent') %>% 
  ggplot(aes(fill=drug_type, y=ag_percent, x=Age)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_discrete(name = "Drug type", labels = c("Depressants", "Psychedelics", "Stimulants")) + 
  labs(x = "Age group", 
       y = "% of active users in age group", 
       title = "Illicit drug use by drug type and age group", 
       subtitle = "Including only active users, ie. people who have taken such drug in the past year") +
  theme(axis.title = element_text(color = "#f0f4ef", size = 16), 
        axis.text = element_text(color = "#f0f4ef", size = 14), 
        legend.title = element_text(color = "#f0f4ef", size = 16), 
        legend.text = element_text(color = "#f0f4ef", size = 16), 
        plot.title = element_text(color = "#f0f4ef", face = "bold", size = 20), 
        plot.subtitle = element_text(color = "#f0f4ef", size = 16), 
        plot.background = element_blank(), 
        panel.background = element_blank(), 
        legend.background = element_blank(), 
        panel.grid = element_line(color = "#D1D9E0", linetype = "dotted")) +
  scale_fill_manual(values = c('#73f250', '#433df2', '#f2059f'), 
                    name="Type of drugs taken",
                    breaks=c("depr_percent", "psych_percent", "stim_percent"),
                    labels=c("Depressants", "Psychedelics", "Stimulants"))
ggsave("drugTypeActiveUsersByAgeGroup.png", bg = "transparent", width = 10, height = 6)

# lollipop chart - % of users of a given drug type, by gender
drug_consumption_specific_drugs %>% 
  mutate(psychedelic_user = ifelse((Ketamine != "CL0" & Ketamine != "CL1" & Ketamine != "CL2"& Ketamine != "CL3") | (LSD != "CL0" & LSD != "CL1" & LSD != "CL2" & LSD != "CL3") | (Mushrooms != "CL0" & Mushrooms != "CL1" & Mushrooms != "CL2" & Mushrooms != "CL3"), 1, 0),
         stimulant_user= ifelse((Amphet != "CL0" & Amphet != "CL1" & Amphet != "CL2" & Amphet != "CL3") | (Coke != "CL0" & Coke != "CL1" & Coke != "CL2" & Coke != "CL3") | (Crack != "CL0" & Crack != "CL1" & Crack != "CL2" & Crack != "CL3"), 1, 0), 
         depressant_user = ifelse((Amyl != "CL0" & Amyl != "CL1" & Amyl != "CL2" & Amyl != "CL3") | (Benzos != "CL0" & Benzos != "CL1" & Benzos != "CL2" & Benzos != "CL3") | (Heroin != "CL0" & Heroin != "CL1" & Heroin != "CL2" & Heroin != "CL3") | (Meth != "CL0" & Meth != "CL1" & Meth != "CL2" & Meth != "CL3"), 1, 0),
         alcohol_user = ifelse(Alcohol != "CL0" & Alcohol != "CL1" & Alcohol != "CL2" & Alcohol != "CL3", 1, 0),
         nicotine_user = ifelse(Nicotine != "CL0" & Nicotine != "CL1" & Nicotine != "CL2" & Nicotine != "CL3", 1, 0)) %>% 
  select(Gender, psychedelic_user, stimulant_user, depressant_user, alcohol_user, nicotine_user) %>% 
  group_by(Gender) %>% 
  mutate(gender_cnt = n(), 
         psych_cnt = sum(psychedelic_user), 
         stim_cnt = sum(stimulant_user), 
         depr_cnt = sum(depressant_user), 
         alc_cnt = sum(alcohol_user),
         nic_cnt = sum(nicotine_user),
         psych_percent = psych_cnt / gender_cnt * 100, 
         stim_percent = stim_cnt / gender_cnt * 100, 
         depr_percent = depr_cnt / gender_cnt * 100,
         alc_percent = alc_cnt / gender_cnt * 100,
         nic_percent = nic_cnt / gender_cnt * 100) %>% 
  select(Gender, psych_percent, stim_percent, depr_percent, alc_percent, nic_percent) %>% 
  ungroup() %>% 
  distinct() %>% 
  rename("illicit_stimulants" = "stim_percent", 
         "illicit_depressants" = "depr_percent", 
         "illicit_psychedelics" = "psych_percent", 
         "alcohol" = "alc_percent", 
         "nicotine" = "nic_percent") %>% 
  pivot_longer(!Gender, names_to='drug_type', values_to = 'gender_percent') %>% 
  pivot_wider(names_from = Gender, values_from = gender_percent) %>% 
  rename("Female" = "F", "Male" = "M") %>% 
  mutate(drug_type = fct_relevel(drug_type, "illicit_psychedelics", "illicit_stimulants", "illicit_depressants", "nicotine", "alcohol")) %>%
  ggplot() + 
  geom_segment(aes(x = drug_type, xend = drug_type, y = Female, yend = Male), color = "white", linewidth = 1.5) +
  geom_point(aes(x = drug_type, y = Female, color = "Female"), size = 4) +
  geom_point(aes(x = drug_type, y = Male, color = "Male"), size = 4) +
  scale_color_manual(values = c("#f2059f", "#433df2"),
                     guide = guide_legend(), 
                     name = "Gender") +
  coord_flip() +
  expand_limits(y = 0) +
  labs(x = "Drug type", 
       y = "% of active users in gender", 
       title = "Drug use by drug type and gender", 
       subtitle = "Including only active users, ie. people who have taken such \ndrug in the past month") +
  theme(axis.title = element_text(color = "#f0f4ef", size = 16), 
        axis.text = element_text(color = "#f0f4ef", size = 14), 
        legend.title = element_text(color = "#f0f4ef", size = 16), 
        legend.text = element_text(color = "#f0f4ef", size = 16), 
        plot.title = element_text(color = "#f0f4ef", face = "bold", size = 20), 
        plot.subtitle = element_text(color = "#f0f4ef", size = 16), 
        plot.background = element_blank(), 
        panel.background = element_blank(), 
        legend.background = element_blank(), 
        panel.grid = element_line(color = "#D1D9E0", linetype = "dotted")) +
  scale_x_discrete(labels = c('other psychedelics','other stimulants','other depressants', 'nicotine', 'alcohol'))
ggsave("drugUseByGender.png", bg = "transparent", width = 10, height = 6)

# boxplot - sleep efficiency vs alcohol consumption
sleep_efficiency <- read.csv('sleep_efficiency.csv')
sleep_efficiency %>% 
  mutate(Alcohol.consumption = ifelse(Alcohol.consumption=="NA", NA, Alcohol.consumption),
         Caffeine.consumption = ifelse(Caffeine.consumption=="NA", NA, Caffeine.consumption),
         Sleep.efficiency = ifelse(Sleep.efficiency=="NA", NA, Sleep.efficiency),
         Awakenings = ifelse(Awakenings=="NA", NA, Awakenings))%>% 
  filter(!is.na(Alcohol.consumption), !is.na(Awakenings)) %>%
  mutate(Alcohol.consumption = factor(Alcohol.consumption)) %>% 
  group_by(Alcohol.consumption) %>% 
  mutate(
    avg_awakenings = mean(Awakenings),
    awakening_bins = cut(
      avg_awakenings, 
      breaks = c(0, 1.5, 2.0, 2.5, Inf), 
      labels = c("[0, 1.5)", "[1.5, 2.0)", "[2.0, 2.5)", "[2.5, Inf)"),
      include.lowest = TRUE
    )
  ) %>% 
  ggplot(aes(x=Alcohol.consumption, y=Sleep.efficiency, fill=awakening_bins)) +
  geom_boxplot(lwd=1.1,aes(color=awakening_bins))+
  scale_fill_manual(
    values = c(
      "[0, 1.5)" = "#fd93d8",
      "[1.5, 2.0)" = "#f2059f",
      "[2.0, 2.5)" = "#8e035d",
      "[2.5, Inf)" = "#5c023d"  
    ),
    name = "Mean Number\nof Awakenings"
  ) +
  scale_color_manual(
    values = rep("#f0f4ef", 4),
    guide = "none"
  ) +
  labs(title = "Sleep Efficiency by Alcohol Consumption",
       subtitle="Boxplots colored by mean number of awakenings\nfor each alcohol consumption level",
       x = "Alcohol Consumption [oz]",
       y = "Sleep Efficiency") +
  
  theme(
    axis.title = element_text(color = "#f0f4ef", size=16),
    axis.text = element_text(color = "#f0f4ef", size=14),
    legend.title = element_text(color = "#f0f4ef", size=16),
    legend.text = element_text(color = "#f0f4ef", size=16), 
    plot.title = element_text(color = "#f0f4ef", size=20, face="bold"),
    plot.subtitle = element_text(color = "#f0f4ef", size=16),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    panel.grid = element_line(color="#D1D9E0", linetype="dotted")
  )
ggsave("alcoholSleepEfficiency.png", bg = "transparent", width = 10, height = 6)