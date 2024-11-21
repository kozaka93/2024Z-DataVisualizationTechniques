library(dplyr)
library(tidyr)
library(ggplot2)

#Loading data
premier_player_injuries <- read.csv("/home/yahor/Documents/uni/sem_3/twd/football-injuries/data/premier_player_injuries.csv")
france_player_injuries <- read.csv("/home/yahor/Documents/uni/sem_3/twd/football-injuries/data/france_player_injuries")
spain_player_injuries <- read.csv("/home/yahor/Documents/uni/sem_3/twd/football-injuries/data/spain_player_injuries")
italy_player_injuries <- read.csv("/home/yahor/Documents/uni/sem_3/twd/football-injuries/data/italy_player_injuries")
germany_player_injuries <- read.csv("/home/yahor/Documents/uni/sem_3/twd/football-injuries/data/germany_player_injuries")


# Total injuries within the top 5 football leagues during the season 23/24
premier_injuries_23_24 <- unique(na.omit(premier_player_injuries[premier_player_injuries$season_injured == "23/24",]))
france_injuries_23_24 <- unique(na.omit(france_player_injuries[france_player_injuries$season_injured == "23/24",]))
laliga_injuries_23_24 <- unique(na.omit(spain_player_injuries[spain_player_injuries$season_injured == "23/24",]))
italy_injuries_23_24 <- unique(na.omit(italy_player_injuries[italy_player_injuries$season_injured == "23/24",]))
bundesliga_injuries_23_24 <- unique(na.omit(germany_player_injuries[germany_player_injuries$season_injured == "23/24",]))

total_injuries_23_24 <- data.frame(premier = dim(premier_injuries_23_24)[1],
                                   france = dim(france_injuries_23_24)[1],
                                   laliga = dim(laliga_injuries_23_24)[1],
                                  italy = dim(italy_injuries_23_24)[1],
                                  bundesliga = dim(bundesliga_injuries_23_24)[1])

injuries_long <- total_injuries_23_24 %>%
  pivot_longer(cols = everything(), names_to = "league", values_to = "injuries") %>% 
  mutate(league = fct_reorder(league, injuries, .desc = TRUE))

# building a plot
ggplot(injuries_long, aes(x = league, y = injuries)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c(premier = "Premier League",
                              france = "Ligue 1",
                              laliga = "LaLiga",
                              italy = "Serie A",
                              bundesliga = "Bundesliga")) +
  labs(title = "Total Injuries in Top 5 Football Leagues",
       subtitle = "Season 2023/2024",
       x = "League",
       y = "Number of Injuries") +
  theme_minimal() 

