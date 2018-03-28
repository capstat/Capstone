library(rvest)
library(dplyr)
library(stringr)

years = seq(1984, 2017, 1)

base = "https://www.basketball-reference.com/"

get_table = function(HTML, t){
  temp_table = HTML %>%
    #table we want
    html_node(paste0("table#", t)) %>%
    html_table() %>%
    #remove unwanted columns
    .[colSums(is.na(.)) < nrow(.)]
}

tables = c("team-stats-per_game", "opponent-stats-per_game",
           "team-stats-base", "opponent-stats-base",
           "team-stats-per_poss", "opponent-stats-per_poss",
           "misc_stats")

abbrs = c(".Per_Game", ".Opp_Per_Game",
          ".Total", ".Opp_Total",
          ".Per_Poss", ".Opp_Per_Poss",
          ".Misc")

team_stats = data.frame()

for(year in years){
  print(year)
  stats_html = read_html(
    paste0(base, sprintf("leagues/NBA_%s.html", year))) 
  table_html = stats_html %>%
    #stats are commented out
    html_nodes(xpath="//comment()") %>%
    html_text() %>%
    #single string
    paste(collapse="") %>%
    #reparse
    read_html()
  team_links = table_html %>% 
    html_nodes("table#team-stats-per_game") %>% 
    html_nodes("td a") %>% 
    html_attr("href")
  temp_table = get_table(table_html, "team-stats-per_game")
  colnames(temp_table) = paste0(colnames(temp_table), abbrs[1])
  colnames(temp_table)[2] = "Team"
  print("team-stats-per_game")
  for(i in 2:(length(tables))){
    temp_t = get_table(table_html, tables[i])
    if(tables[i]=="misc_stats"){
      colnames(temp_t) = temp_t[1,]
      colnames(temp_t)[c(21,22,24)] = c(
        "Opp.eFG%", "Opp.TOV%", "Opp.FT/FGA"
      )
    }
    colnames(temp_t) = paste0(colnames(temp_t), abbrs[i])
    colnames(temp_t)[2] = "Team"
    temp_table = left_join(temp_table, temp_t, by=c("Team"))
    print(tables[i])
  }
  temp_table$Year = year
  temp_table$Season = paste(as.numeric(year)-1, year, sep="-")
  temp_table = temp_table[!is.na(temp_table[,c(1)]),]
  temp_table$Team_Link = team_links
  if(year!=1984) colnames(temp_table) = colnames(team_stats)
  team_stats = rbind(team_stats, temp_table)
  print(paste(year, "DONE!"))
}

write.csv(team_stats, "data_sets/NBA_team_stats.csv",
          row.names=FALSE)
