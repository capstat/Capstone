library(rvest)
library(stringr)
library(dplyr)

base = "https://www.basketball-reference.com/"

playoffs = read.csv("data_sets/NBA_playoffs.csv",
                    stringsAsFactors = FALSE)

get_playoff_minutes = function(HTML, y){
  roster = HTML %>%
    html_nodes("td a") %>% 
    html_attr("href")
  roster = unique(roster[str_detect(roster, "players")])
  minutes = 0
  for(player in roster){
    print(player)
    temp_html = read_html(paste0(base, player)) %>%
      html_nodes(xpath="//comment()") %>%
      html_text() %>%
      paste(collapse="") %>%
      read_html()
    po_table = temp_html %>%
      html_node(paste0("table#", "playoffs_totals")) 
    temp_text = po_table %>% html_text()
    if(is.na(temp_text)) next
    po_table = po_table %>% html_table()
    po_table$Year = as.numeric(
      paste0(substr(po_table$Season, 1, 2),
             substr(po_table$Season, 6, 7)))
    po_table = po_table[str_detect(po_table$Season, "\\d{4}.\\d{2}"),]
    po_table$Year = ifelse(unique(po_table$Year)==1900,
                           2000, po_table$Year)
    temp_min = sum(po_table$MP[po_table$Year<y])
    print(paste(player, temp_min))
    minutes = minutes + temp_min
  }
  return(minutes)
}

get_team_balance = function(HTML){
  HTML = HTML %>%
    html_nodes(xpath="//comment()") %>%
    html_text() %>%
    paste(collapse="") %>%
    read_html()
  per_game = HTML %>%
    html_node(paste0("table#", "per_game")) %>%
    html_table()
  colnames(per_game)[2] = "Name"
  adv_table = HTML %>%
    html_node(paste0("table#", "advanced")) %>%
    html_table()
  colnames(adv_table)[2] = "Name"
  both = left_join(adv_table[,c("Name", "WS/48")], 
                   per_game[,c("Name", "G", "MP")], 
                   by=c("Name"))
  both = both[both$MP > 12 & both$G > 10,]
  b = sum(both$`WS/48` > 0.1)
  return(b)
}

team_links = unique(playoffs$Team_Link)
opp_links = unique(playoffs$Opp_Link)

experience = c()
us = c()
po_minutes = c()
balance = c()

for(each in team_links){
  print(each)
  yr = as.numeric(str_extract(each, "\\d{4}"))
  temp_html = read_html(paste0(base, each)) 
  roster_table = temp_html %>%
    html_node(paste0("table#", "roster")) %>%
    html_table()
  colnames(roster_table)[7] = "Country"
  roster_table$Exp[roster_table$Exp=="R"] = 0
  avg_exp = sum(as.numeric(roster_table$Exp))/nrow(roster_table)
  experience = c(experience, avg_exp)
  us_roster = length(roster_table$Country[roster_table$Country=="us"])/
    nrow(roster_table)
  us = c(us, us_roster)
  temp_min = get_playoff_minutes(temp_html, yr)
  po_minutes = c(po_minutes, temp_min)
  temp_balance = get_team_balance(temp_html)
  balance = c(balance, temp_balance)
}

player_stats = data.frame(
  Team_Link = team_links,
  Player.Exp=experience,
  US.Players.Prop=us,
  Playoff.Minutes=po_minutes,
  Balance=balance
)

write.csv(player_stats, "data_sets/NBA_player_stats.csv",
          row.names=F)
