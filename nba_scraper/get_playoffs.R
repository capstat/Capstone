library(rvest)
library(stringr)
library(dplyr)

#years we are interested in
years = seq(1984, 2017, 1)

#html file with playoff history
playoffs_html = read_html(
  "https://www.basketball-reference.com/playoffs/series.html"
)

#read the history as a table
playoffs_raw = playoffs_html %>% 
  html_table() %>%
  .[[1]] 

#clean the table
playoffs1 = playoffs_raw[c(2:nrow(playoffs_raw)), c(1,3,6,7,9,10)]
colnames(playoffs1) = c(
  "Year", "Type", "Winner", "W_Wins", "Loser", "L_Wins" 
)
playoffs = playoffs1 %>%
  filter(Winner!="", Year %in% years) %>%
  mutate(
    W_Seed = as.numeric(str_match(Winner, "\\((\\d)")[,2]),
    Winner = str_replace(Winner, " \\(\\d\\)", ""),
    L_Seed = as.numeric(str_match(Loser, "\\((\\d)")[,2]),
    Loser = str_replace(Loser, " \\(\\d\\)", ""),
    W_Wins = as.numeric(W_Wins),
    L_Wins = as.numeric(L_Wins),
    Season = paste(as.numeric(Year)-1, Year, sep="-"),
    Series_Length = W_Wins + W_Wins - 1,
    Home_Team = ifelse(W_Seed < L_Seed, T, F),
    Conference = word(Type, 1),
    Series_Type = word(Type, 3)
)

#get team links from html page
temp_team_links = playoffs_html %>% 
  html_nodes("td a") %>% 
  html_attr("href") 
team_links = temp_team_links[str_detect(temp_team_links, "teams")]
playoffs$W_Link = team_links[seq(1, nrow(playoffs)*2, 2)]
playoffs$L_Link = team_links[seq(2, nrow(playoffs)*2, 2)]

#remove winner/loser variables
playoffs$Team = ifelse(playoffs$W_Seed < playoffs$L_Seed,
                       playoffs$Winner, playoffs$Loser)
playoffs$Opponent = ifelse(playoffs$W_Seed < playoffs$L_Seed,
                           playoffs$Loser, playoffs$Winner)
playoffs$Seed = ifelse(playoffs$W_Seed < playoffs$L_Seed,
                         playoffs$W_Seed, playoffs$L_Seed)
playoffs$Opp_Seed = ifelse(playoffs$W_Seed < playoffs$L_Seed,
                         playoffs$L_Seed, playoffs$W_Seed)
playoffs$Series_Wins = ifelse(playoffs$W_Seed < playoffs$L_Seed,
                              playoffs$W_Wins, playoffs$L_Wins)
playoffs$Series_Losses = ifelse(playoffs$W_Seed < playoffs$L_Seed,
                                playoffs$L_Wins, playoffs$W_Wins)
playoffs$Won_Series = ifelse(playoffs$Series_Win > playoffs$Series_Losses, 
                             T, F)
playoffs$Team_Link = ifelse(playoffs$W_Seed < playoffs$L_Seed,
                            playoffs$W_Link, playoffs$L_Link)
playoffs$Opp_Link = ifelse(playoffs$W_Seed < playoffs$L_Seed,
                           playoffs$L_Link, playoffs$W_Link)

playoffs_final = playoffs %>%
  select(-c(3:8,11,14,15)) %>%
  filter(Type!="Finals")
 
write.csv(playoffs_final, "data_sets/NBA_playoffs.csv", 
          row.names=FALSE)
