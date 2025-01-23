library(dplyr)

merged_df <- read.csv("../data/merged_data.csv")
lok_names_joz <- read.csv("../data/lok_names_jo.csv")
lok_names_mi <- read.csv("../data/lok_names_mi.csv")

lok_names <- merged_df %>% 
  select(placeID, placeName, category) %>% 
  mutate(name = case_when(
    placeID == "ChIJF8u2SOnMHkcR7TrJJ2_WP80" ~ "Politechnika",
    TRUE ~ placeName
  )) %>% 
  mutate(category = case_when(
    category == "other" ~ "entertainment", # fryzjer
    category == "shoppping" ~ "shopping",
    category == "holiday_home" ~ "home",
    category == "studies" ~ "university",
    category == "Shopping" ~ "shopping",
    category == "Bułkę przez bibułkę" ~ "shopping",
    category == " entertainment" ~ "entertainment",
    category == " university" ~ "university",
    category == " transport" ~  "transport",
    TRUE ~ category
  )) %>% 
  group_by(placeID) %>% 
  slice(1) %>% 
  select(placeID, name, category)

lok_names_joz2 <- lok_names_joz %>% 
  mutate(placeID = placeId) %>% 
  select(placeID, name, category) %>% 
  filter(!placeID %in% lok_names$placeID) %>% 
  mutate(category = case_when(
    category == "other" ~ "entertainment", # fryzjer
    category == "shoppping" ~ "shopping",
    category == "holiday_home" ~ "home",
    category == "studies" ~ "university",
    category == "Shopping" ~ "shopping",
    category == "Bułkę przez bibułkę" ~ "shopping",
    category == " entertainment" ~ "entertainment",
    category == " university" ~ "university",
    category == " transport" ~  "transport",
    TRUE ~ category
  ))

lok_names_mi2 <- lok_names_mi %>% 
  filter(!placeID %in% lok_names$placeID) %>% 
  mutate(category = case_when(
    category == "other" ~ "entertainment", # fryzjer
    category == "shoppping" ~ "shopping",
    category == "holiday_home" ~ "home",
    category == "studies" ~ "university",
    category == "Shopping" ~ "shopping",
    category == "Bułkę przez bibułkę" ~ "shopping",
    category == " entertainment" ~ "entertainment",
    category == " university" ~ "university",
    category == " transport" ~  "transport",
    TRUE ~ category
  ))
  
lok_names <- rbind(lok_names, lok_names_joz2, lok_names_mi2)
write.csv(lok_names, "lok_names.csv", row.names = FALSE)
