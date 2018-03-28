joined = read.csv("data_sets/join_stats.csv", stringsAsFactors=FALSE)

joined$Seed_Diff = joined$Opp_Seed - joined$Seed
pw = joined$PW.Misc/(joined$PW.Misc+joined$PL.Misc)
pw_opp = joined$PW.Misc_._Opponent/
  (joined$PW.Misc_._Opponent+joined$PL.Misc_._Opponent)
joined$Pyt_Win_Pct_Diff = pw-pw_opp

joined$Won_Series = ifelse(joined$Won_Series==TRUE,1,0)
joined$id = joined$id

joined = joined[,c(13,361,362,175:181,345:352,360)]

write.csv(joined, "data_sets/4factor_model_data.csv", row.names=FALSE)
