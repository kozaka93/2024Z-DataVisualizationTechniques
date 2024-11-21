library(usmap)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(fmsb)

podzial <- read.csv("podzial.csv")
podzial2 <- podzial %>% mutate(Regiony6 = case_when(
  State %in% c("Maine", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island", "Connecticut", 
               "New York", "New Jersey", "Pennsylvania", "District of Columbia") ~ "Northeast",
  State %in% c("Ohio", "Michigan", "Indiana", "Illinois", "Wisconsin", "Minnesota", "Iowa", "Missouri", 
               "North Dakota", "South Dakota", "Nebraska", "Kansas") ~ "Midwest",
  State %in% c("Delaware", "Maryland", "Virginia", "West Virginia", "Kentucky", "Tennessee", "North Carolina", 
               "South Carolina", "Georgia", "Florida", "Alabama", "Mississippi", "Arkansas", "Louisiana", 
               "Oklahoma", "Texas") ~ "South",
  State %in% c("Montana", "Wyoming", "Colorado", "New Mexico", "Idaho", "Utah", "Nevada", "Arizona") ~ "Mountain",
  State %in% c("Washington", "Oregon", "California", "Alaska", "Hawaii") ~ "Pacific",
))


Wszystkie <- read.csv("All_fatalities.csv") %>% filter (!Deaths == "")
Samobojstwa_Stany <- read.csv("Suicides_fatalities.csv") %>%
  filter(Deaths != "") %>%                                
  mutate(
    Population = Population %>%
      str_replace_all(",", "") %>% 
      str_replace(" M$", "") %>%
      as.numeric() %>%                              
      { ifelse(. > 100000, . / 1e6, .) },                                       
    Combined.Costs.Per.Capita = Combined.Costs.Per.Capita %>%
      str_replace_all("[$,]", "") %>%                  
      as.numeric() ,                                      
    `Cost of suicides` = Combined.Costs.Per.Capita * Population  
  )
Narkotyki_Stany <- read.csv("Drugs_fatalities.csv") %>%
  filter(Deaths != "") %>%
  mutate(
    Population = Population %>% 
      str_replace_all(",", "") %>%
      str_replace(" M$", "") %>%
      as.numeric() %>%                                   
      { ifelse(. > 100000, . / 1e6, .) }, 
    
    Combined.Costs.Per.Capita = Combined.Costs.Per.Capita %>%
      str_replace_all("[$,]", "") %>%  
      as.numeric()
  ) %>%
  transmute(
    `Cost of drugs overdose` = Combined.Costs.Per.Capita * Population, 
    State = State
  )

Pojazdy_Stany <- read.csv("Traffic_fatalities.csv") %>%
  filter(Deaths != "") %>%
  mutate(
    Population = Population %>% 
      str_replace_all(",", "") %>%   
      str_replace(" M$", "") %>%
      as.numeric() %>%                                
      { ifelse(. > 100000, . / 1e6, .) }, 
    Combined.Costs.Per.Capita = Combined.Costs.Per.Capita %>%
      str_replace_all("[$,]", "") %>%  
      as.numeric()
  ) %>%
  transmute(
    `Cost of traffic fatalities` = Combined.Costs.Per.Capita * Population,
    State = State
  )

Upadki_Stany <- read.csv("Falls.csv") %>%
  filter(Deaths != "") %>%
  mutate(
    Population = Population %>% 
      str_replace_all(",", "") %>%
      str_replace(" M$", "") %>%
      as.numeric() %>%                                    
      { ifelse(. > 100000, . / 1e6, .) }, 
    Combined.Costs.Per.Capita = Combined.Costs.Per.Capita %>%
      str_replace_all("[$,]", "") %>%  
      as.numeric()
  ) %>%
  transmute(
    `Cost of falls` = Combined.Costs.Per.Capita * Population, 
    State = State
  )

Przestepstwa_Stany <- read.csv("Crimes_fatalities.csv") %>%
  filter(Deaths != "") %>%
  mutate(
    Population = Population %>% 
      str_replace_all(",", "") %>% 
      str_replace(" M$", "") %>%
      as.numeric() %>%                                   
      { ifelse(. > 100000, . / 1e6, .) }, 
    Combined.Costs.Per.Capita = Combined.Costs.Per.Capita %>%
      str_replace_all("[$,]", "") %>%  
      as.numeric()
  ) %>%
  transmute(
    `Cost of crime based fatalities` = Combined.Costs.Per.Capita * Population,
    State = State
  )
wynik <- left_join(Samobojstwa_Stany, Narkotyki_Stany, join_by(State==State)) %>% 
left_join(Pojazdy_Stany, join_by(State==State))  %>% left_join(Upadki_Stany, join_by(State==State))  %>% left_join(Przestepstwa_Stany, join_by(State==State))  %>% mutate(across(c(3,9:13), ~ as.numeric(gsub("[$,]", "", .)))) %>% mutate(Combined.Costs.Total = Combined.Costs.Total %>%  str_replace_all("[$,B]", "") %>%   
as.numeric())
test  <- wynik %>% left_join(podzial2, join_by(State==State)) %>%
  pivot_longer(cols = starts_with("Cost"), names_to = "type", values_to = "value") %>% group_by(Regiony6,type) %>% summarise(value = sum(value)/sum(Population), .groups = "drop")

radar_data <- head(test, 26 * 5) %>%
  filter(Regiony6 %in% unique(Regiony6)[1:5]) %>% 
  select(Regiony6, type, value) %>%
  spread(key = Regiony6, value = value) 
rows <- radar_data$type
radar_data <- radar_data[, -1]
row.names(radar_data) <- rows
radar_data <- rbind(rep(4500, ncol(radar_data)), rep(0, ncol(radar_data)), radar_data)
colors <- c("#DEBAC0", "#77567A", "#C47AC0", "#E39EC1", "#2F323A")
accident_types <- row.names(radar_data)[-c(1, 2)]
radarchart(radar_data,
           axistype = 1,                              
           pcol = colors,                            
           pfcol = NA,                               
           plwd = 3,                               
           plty = 1,                                  
           axislabcol = "grey",                      
           cglcol = "grey",                          
           cglty = 1,                                 
           cglwd = 0.8,                            
           caxislabels = c("", "", "", "", ""),  
           title = "Costs of fatalities per capita in USD by region (2022)",
           maxmin = TRUE                   
)
legend("topright",
       legend = accident_types,
       col = colors,
       lty = 1,
       lwd = 3,
       bty = "n",
       cex = 0.8)





