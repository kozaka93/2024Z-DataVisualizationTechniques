depressions <- read.csv("wgm_full_wave2_public_file_final (1)_csv.csv")
suicides <- read.csv("16BBF41_ALL_LATEST.csv")

library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(forcats)

paleta <- c("#93b7be", "#f1fffa", "#d5c7bc", "#12263a", "#52154e", "#96476e", "#D97A8E")


suicides %>% # stacked
  filter(DIM_GEO_CODE_TYPE == "WHOREGION") %>% 
  filter(DIM_SEX == "TOTAL") %>% 
  mutate(GEO_NAME_SHORT = case_when(
    GEO_NAME_SHORT == "Africa" ~ "Afryka",
    GEO_NAME_SHORT == "Europe" ~ "Europa",
    GEO_NAME_SHORT == "Americas" ~ "Ameryki",
    GEO_NAME_SHORT == "Eastern Mediterranean" ~ "Kraje Wschodniego Morza Śródziemnomorskiego",
    GEO_NAME_SHORT == "South-East Asia" ~ "Azja Południowo-Wschodnia",
    TRUE ~ "Kraje Zachodniego Pacyfiku"
  )) %>% 
  ggplot(aes(fill=GEO_NAME_SHORT, y=RATE_PER_100000_N, x=DIM_TIME)) + 
  geom_col(position="stack") +
  labs(title = "Liczba samobójstw na 100000 mieszkańców w latach",
       x = "Rok",
       y = "Liczba samobójstw na 100000 mieszkańców",
       fill = "Regiony") +
  scale_fill_manual(values = paleta[2:7]) +
  theme_minimal()  +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),  # panel background
    plot.background = element_rect(fill = "transparent", color = NA),    # plot background
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent")
  )


ggsave("samobojstwa w czasie.png", bg = "transparent")



depressions %>% 
  filter(MH7A == 1) %>% # ci z depresja
  filter(MH7B != 99) %>% # ci co podali grupe wiekowa
  group_by(MH7B, Gender) %>% 
  mutate(num_of_depressions = n()) %>% 
  mutate(Plec = ifelse(Gender == 1,"M","K")) %>% 
  mutate(grupa_wiekowa = case_when(
    MH7B <= 12 ~ "0-12",
    MH7B <= 19 ~ "13-19",
    MH7B <= 29 ~ "20-29",
    MH7B <= 39 ~ "30-39",
    TRUE ~ "40+"
  )) %>% 
  select(grupa_wiekowa, Plec, num_of_depressions) %>% 
  group_by(grupa_wiekowa, Plec, num_of_depressions) %>% 
  slice(1) %>% 
  ungroup() %>% 
  ggplot(aes(y=num_of_depressions, x=grupa_wiekowa, fill=Plec)) + 
  geom_bar(stat="identity", position="dodge") +
  labs(x="Grupy wiekowe", y="Liczba depresji", title="Liczba depresji w 2020/2021 w danych grupach wiekowych", fill = "Płeć") +
 # scale_fill_manual(values = c("cc2233", "aa2233", name = "Plec")) + jestes glupi jozek
  scale_fill_manual(values = paleta[2:3]) +
  theme_minimal()  +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),  # panel background
    plot.background = element_rect(fill = "transparent", color = NA),    # plot background
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent")
  )

ggsave("depresje po plci.png", bg = "transparent")


opinia_o_psych <- depressions %>% 
  filter(!is.na(MH9A)) %>% # ci z depresja
  mutate(all_with_dep = n()) %>% 
  group_by(MH9A) %>% 
  mutate(num_of_same_opinions = n()) %>% 
  select(num_of_same_opinions, all_with_dep) %>% 
  group_by(num_of_same_opinions) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(percent_of_opinions = floor(100*num_of_same_opinions/all_with_dep)) %>% 
  mutate(MH9A = case_when(MH9A == 99 ~ paste0("Brak Odpowiedzi: ", "1%"),
                          MH9A == 1 ~ paste0("Bardzo pomocny: ", as.character(percent_of_opinions + 1), "%"),
                          MH9A == 2 ~ paste0("Trochę pomocny: ", as.character(percent_of_opinions + 1), "%"),
                          MH9A == 3 ~ paste0("Nie pomógł: ", as.character(percent_of_opinions), "%"))) %>% 
  ungroup() %>% 
  mutate(Opinia = fct_reorder(MH9A, num_of_same_opinions))

ggplot(opinia_o_psych, aes(x = "", y = num_of_same_opinions, fill = Opinia)) + 
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +  # This converts the bar chart to a pie chart
  labs(title = "Jak pomocny był psycholog dla ankietowanych", y = "") + 
  scale_fill_manual(values = paleta[2:5]) +
  theme_void()  +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA), 
    plot.background = element_rect(fill = "transparent", color = NA),  
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent")
  )

ggsave("opinie o psych.png", bg = "transparent")
  
  # rownames(opinia_o_psych) <- c("Brak Odpowiedzi", "Nie pomógł", "Trochę pomocny", "Bardzo pomocny")
  

ile_u__psych <- depressions %>% 
  filter(!is.na(MH8A)) %>% # ci z depresja
  mutate(all_with_dep = n()) %>% 
  group_by(MH8A) %>% 
  mutate(num_of_goers = n()) %>% 
  select(num_of_goers, all_with_dep) %>% 
  group_by(num_of_goers) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(percent_of_goers = as.character(floor(100*num_of_goers/all_with_dep))) %>% 
  mutate(MH8A = case_when(MH8A == 99 ~ paste0("Brak Odpowiedzi: " , as.character(1), "%"),
                          MH8A == 1 ~ paste0("Byłem/Byłan u psychologa: ", as.character(percent_of_goers), "%"),
                          MH8A == 2 ~ paste0("Nie byłem/byłan u psychologa: ", as.character(percent_of_goers), "%")
                          ))%>% 
  mutate(MH8A = fct_reorder(MH8A, num_of_goers))

ggplot(ile_u__psych, aes(x = "", y = num_of_goers, fill = MH8A)) + 
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +  
  labs(title = "Ilu ankietowanych było u psychologa") +  
  scale_fill_manual(values = paleta[2:5]) +
  theme_void()  +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),  # panel background
    plot.background = element_rect(fill = "transparent", color = NA),    # plot background
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent"),
    legend.title = element_blank()
  )


ggsave("ile u psych.png", bg = "transparent")



   
  