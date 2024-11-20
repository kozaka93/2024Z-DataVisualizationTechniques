library(worldfootballR)
library(dplyr)
?tm_player_market_values
# Premier League
premier_league_valuations <- tm_player_market_values(country_name = "England",
                                               start_year = 2022)
glimpse(premier_league_valuations)
premier_player_injuries <- tm_player_injury_history(player_urls = premier_league_valuations$player_url)
glimpse(premier_player_injuries)

# France Football League 1
france_league_valuations <- tm_player_market_values(country_name = "France",
                                                     start_year = 2022)
glimpse(france_league_valuations)
france_player_injuries <- tm_player_injury_history(player_urls = france_league_valuations$player_url)
glimpse(france_player_injuries)

# Spain La Liga

spain_league_valuations <- tm_player_market_values(country_name = "Spain",
                                                    start_year = 2022)
glimpse(spain_league_valuations)
spain_player_injuries <- tm_player_injury_history(player_urls = spain_league_valuations$player_url)
glimpse(spain_player_injuries)


# Italian League A

italy_league_valuations <- tm_player_market_values(country_name = "Italy",
                                                    start_year = 2022)
glimpse(italy_league_valuations)
italy_player_injuries <- tm_player_injury_history(player_urls = italy_league_valuations$player_url)
glimpse(italy_player_injuries)


# Bundesliga

germany_league_valuations <- tm_player_market_values(country_name = "Germany",
                                                    start_year = 2022)
glimpse(germany_league_valuations)
germany_player_injuries <- tm_player_injury_history(player_urls = germany_league_valuations$player_url)
glimpse(germany_player_injuries)


#save dataframes

write.csv(premier_player_injuries, "premier_player_injuries.csv", row.names=FALSE)
write.csv(france_player_injuries, "france_player_injuries", row.names=FALSE)
write.csv(spain_player_injuries, "spain_player_injuries", row.names=FALSE)
write.csv(italy_player_injuries, "italy_player_injuries", row.names=FALSE)
write.csv(germany_player_injuries, "germany_player_injuries", row.names=FALSE)