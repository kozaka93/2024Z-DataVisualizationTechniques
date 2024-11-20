#install.packages("worldfootballR")
library(worldfootballR)
library(dplyr)

#https://github.com/JaseZiv/worldfootballR - info about library

#----- for a single player: -----#
hazard_injuries <- tm_player_injury_history(player_urls = "https://www.transfermarkt.com/eden-hazard/profil/spieler/50202")
dplyr::glimpse(hazard_injuries)

hazard_injuries

#----- for multiple players: -----#
# # can make use of a tm helper function:
# burnley_player_urls <- tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-burnley/startseite/verein/1132/saison_id/2021")
# # then pass all those URLs to the tm_player_injury_history
# burnley_player_injuries <- tm_player_injury_history(player_urls = burnley_player_urls)