library(stringr)

joined = read.csv("data_sets/join_stats.csv", stringsAsFactors=FALSE)

joined$Seed_Diff = joined$Opp_Seed - joined$Seed
pw = joined$PW.Misc/(joined$PW.Misc+joined$PL.Misc)
pw_opp = joined$PW.Misc_._Opponent/
  (joined$PW.Misc_._Opponent+joined$PL.Misc_._Opponent)
joined$Pyt_Win_Pct_Diff = pw-pw_opp

#remove
joined$Seed = NULL
joined$Opp_Seed = NULL
joined$PW.Misc = NULL
joined$PL.Misc = NULL
joined$PW.Misc_._Opponent = NULL
joined$PL.Misc_._Opponent = NULL


keepers = c()
cs = ncol(joined)
for(i in c(1:cs)){
  pattern = "(Rk)|(Series)"
  if(str_detect(colnames(joined)[i], pattern)) next
  temp = as.numeric(joined[,c(i)])
  if(sum(is.na(temp))==0) joined[,c(i)] = as.numeric(joined[,c(i)])
  if(!is.numeric(joined[,c(i)])) next
  v = FALSE
  f = var.test(joined[joined$Won_Series==TRUE, c(i)], 
               joined[joined$Won_Series==FALSE, c(i)])
  if(f$p.value > 0.05) v = TRUE
  t = t.test(joined[joined$Won_Series==TRUE, c(i)], 
             joined[joined$Won_Series==FALSE, c(i)],
             var.equal = v)
  if(t$p.value < 0.05) keepers = c(keepers, (colnames(joined)[i]))
}

model_data1 = joined[,c("Won_Series", keepers)]
removes = paste0("(X2P)|(Per_Game)|(Total)|(W.Misc)|(L.Misc)|",
                 "(MOV)|(TS\\.\\.)|(SRS)|(PTS.Per_Poss)|",
                 "(PTS.Opp.Per.Poss...Opponent)|(PTS.Per_Poss)|",
                 "(FG.Opp.Per.Poss...Opponent)|",
                 "(FG..Opp.Per.Poss...Opponent)")
model_data1 = 
  model_data1[, colnames(model_data1)
              [!str_detect(colnames(model_data1), removes)]]

model_cor = as.data.frame(cor(model_data1))
model_cor = sapply(model_cor, abs)
row.names(model_cor) = colnames(model_data1)
for(i in c(1:ncol(model_cor))){
  for(j in c(1:nrow(model_cor))){
    if(colnames(model_cor)[i]==rownames(model_cor)[j]) next
    if(model_cor[i,j] > 0.66){
      print(paste(
        colnames(model_cor)[i], "|||",
        rownames(model_cor)[j], "|||",
        model_cor[i,j]))
    }
  }
}

model_data1$Won_Series = ifelse(model_data1$Won_Series==TRUE,1,0)
model_data1$id = joined$id

write.csv(model_data1, "data_sets/model_data.csv", row.names=FALSE)