library(dplyr)

x = read.csv("models/results/four_fact_KNN_all.csv")
x$m = "Four Factor"
x$m2 = "KNN"
y = read.csv("models/results/four_fact_RFC_all.csv")
y$m = "Four Factor"
y$m2 = "RFC"
z = read.csv("models/results/four_fact_SVC_all.csv")
z$m = "Four Factor"
z$m2 = "SVC"

xx = read.csv("models/results/pyt_thm_KNN_all.csv")
xx$m = "Pythagorean Win %"
xx$m2 = "KNN"
yy = read.csv("models/results/pyt_win_RFC_all.csv")
yy$m = "Pythagorean Win %"
yy$m2 = "RFC"
zz = read.csv("models/results/pyt_win_SVC_all.csv")
zz$m = "Pythagorean Win %"
zz$m2 = "SVC"

t = rbind(x[,c("Won_Series","Prediction","Seed_Diff","m","m2")],
          rbind(xx[,c("Won_Series","Prediction","Seed_Diff","m","m2")],
                rbind(y[,c("Won_Series","Prediction","Seed_Diff","m","m2")],
                      rbind(yy[,c("Won_Series","Prediction",
                                  "Seed_Diff","m","m2")],
                            rbind(z[,c("Won_Series","Prediction",
                                       "Seed_Diff","m","m2")],
                                  rbind(zz[,c("Won_Series","Prediction",
                                              "Seed_Diff","m","m2")]))))))

#just precision
t$Won_Series = ifelse(t$Won_Series==1,T,F)
t$Prediction = toupper(t$Prediction)
t = t[t$Prediction==TRUE,]


t$Seed_Diff = ifelse(t$Seed_Diff %in% c("5","6","7"), "5 or more", t$Seed_Diff)
t = t %>% group_by(Seed_Diff, m, m2) %>%
  summarize(pct=sum(Won_Series==Prediction)/length(Won_Series==Prediction))

d = read.csv("data_sets/model_data.csv", stringsAsFactors=FALSE)
d$Seed_Diff = as.character(d$Seed_Diff)
d$Seed_Diff = ifelse(d$Seed_Diff %in% c("5","6","7"), "5 or more", d$Seed_Diff)
x = d %>% group_by(Seed_Diff) %>%
  summarize(pct=sum(Won_Series==TRUE)/length(Won_Series==TRUE)) %>%
  mutate(m="Basic Model", m2="Basic Model")

all = bind_rows(x, t)

write.csv(all, "models/results/combine_all.csv", row.names=F)
