library(dplyr)
library(stringr)
library(ggplot2)

playoffs = read.csv("data_sets/NBA_playoffs.csv",
                    stringsAsFactors = FALSE)

team_stats = read.csv("data_sets/NBA_team_stats.csv",
                      stringsAsFactors = FALSE)
team_stats$Team = NULL

player_stats = read.csv("data_sets/NBA_player_stats.csv",
                        stringsAsFactors = FALSE)

join1 = left_join(playoffs, team_stats, 
                  by=c("Team_Link", "Year", "Season"))
#add opponent stats
join_opp1 = left_join(join1, team_stats,
                     by=c("Opp_Link"="Team_Link",
                          "Year"="Year",
                          "Season"="Season"), 
                     suffix=c("", "_._Opponent"))

join2 = left_join(join_opp1, player_stats, 
                  by=c("Team_Link"))

join2$id = paste0(join2$Team_Link, join2$Series_Type)

write.csv(join2, "data_sets/join_stats.csv", row.names=FALSE)
