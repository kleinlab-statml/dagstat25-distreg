setwd("~/Desktop/Dagstat_code")
library("bamlss")

election <- read.csv("data/election_germany_2017.csv")
election$nuts <- as.factor(as.character(election$nuts))
mapdata <- R2BayesX::read.bnd("data/germany_nuts.bnd")

K <- neighbormatrix(mapdata)

f <- list(
  Sonstige ~  s(nuts, bs = "mrf", xt = list(penalty = K))
  + s(AQ)+s(BIPpEW)+s(Wahlbeteiligung),
  LINKE ~ s(nuts, bs = "mrf", xt = list(penalty = K)
  )+ s(AQ)+s(BIPpEW)+s(Wahlbeteiligung),
  FDP ~ s(nuts, bs = "mrf", xt = list(penalty = K)
  )+ s(AQ)+s(BIPpEW)+s(Wahlbeteiligung),
  GRUENE ~ s(nuts, bs = "mrf", xt = list(penalty = K)
  )+ s(AQ)+s(BIPpEW)+s(Wahlbeteiligung),
  SPD ~ s(nuts, bs = "mrf", xt = list(penalty = K)
  )+ s(AQ)+s(BIPpEW)+s(Wahlbeteiligung),
  CDUCSU ~ s(nuts, bs = "mrf", xt = list(penalty = K)
  )+ s(AQ)+s(BIPpEW)+s(Wahlbeteiligung),
  AfD ~ s(nuts, bs = "mrf", xt = list(penalty = K)
  )+ s(AQ)+s(BIPpEW)+s(Wahlbeteiligung)
)

m <- bamlss(f, data = election, family = dirichlet_bamlss(k=7), optimizer = FALSE, n.iter = 12000, burnin = 2000, thin = 10)

# Scenario anlysis

nd <- election[election$Kreisnummer %in% c(11000),]
pred <- predict(m,newdata=nd)
names(pred) <- names(m[["y"]])
Nenner <-exp(pred$CDUCSU)+exp(pred$SPD)+exp(pred$GRUENE)+exp(pred$FDP)+exp(pred$LINKE)+exp(pred$AfD)+exp(pred$Sonstige)
nd$CDUCSU_pred <- exp(pred$CDUCSU) / Nenner
nd$AfD_pred <- exp(pred$AfD) / Nenner
nd$SPD_pred <- exp(pred$SPD) / Nenner
nd$FDP_pred <- exp(pred$FDP) / Nenner
nd$LINKE_pred <- exp(pred$LINKE) / Nenner
nd$GRUENE_pred <- exp(pred$GRUENE) / Nenner
nd$Sonstige_pred <- exp(pred$Sonstige) / Nenner

nd['AQ']<-5.5
pred <- predict(m,newdata=nd)
names(pred) <- names(m[["y"]])
Nenner <-exp(pred$CDUCSU)+exp(pred$SPD)+exp(pred$GRUENE)+exp(pred$FDP)+exp(pred$LINKE)+exp(pred$AfD)+exp(pred$Sonstige)
nd$CDUCSU_aq <- exp(pred$CDUCSU) / Nenner
nd$AfD_aq <- exp(pred$AfD) / Nenner
nd$SPD_aq <- exp(pred$SPD) / Nenner
nd$FDP_aq <- exp(pred$FDP) / Nenner
nd$LINKE_aq <- exp(pred$LINKE) / Nenner
nd$GRUENE_aq <- exp(pred$GRUENE) / Nenner
nd$Sonstige_aq <- exp(pred$Sonstige) / Nenner

# plotting of spatial effects
parteien <- colnames(election)[6:12]

nd <- unique(election[, "nuts", drop = FALSE])
nd$AQ = mean(election$AQ)
nd$BIPpEW = mean(election$BIPpEW)
nd$Wahlbeteiligung = mean(election$Wahlbeteiligung)
pred <-
  predict(
    m,
    newdata = nd,
    what = "samples",
    term =  c("s(AQ)", "s(BIPpEW)", "s(Wahlbeteiligung)", "s(nuts)"),
    FUN = function(x) {x},
    intercept = TRUE
  )

names(pred) <- names(m[["y"]])

Nenner <-
  rowMeans(
    data.frame(exp(pred$CDUCSU)) + data.frame(exp(pred$SPD)) + data.frame(exp(pred$GRUENE)) +
      data.frame(exp(pred$FDP)) + data.frame(exp(pred$LINKE)) + data.frame(exp(pred$AfD)) +
      data.frame(exp(pred$Sonstige))
  ) 

nd$CDUCSU_real <- rowMeans(exp(pred$CDUCSU)) / Nenner
nd$AfD_real <- rowMeans(exp(pred$AfD)) / Nenner
nd$SPD_real <- rowMeans(exp(pred$SPD)) / Nenner
nd$FDP_real <- rowMeans(exp(pred$FDP)) / Nenner
nd$LINKE_real <- rowMeans(exp(pred$LINKE)) / Nenner
nd$GRUENE_real <- rowMeans(exp(pred$GRUENE)) / Nenner
nd$Sonstige_real <- rowMeans(exp(pred$Sonstige)) / Nenner

plotmap(mapdata, x = 100*nd$CDUCSU_real,id = nd$nuts)
plotmap(mapdata, x = 100*nd$AfD_real,id = nd$nuts)
plotmap(mapdata, x = 100*nd$SPD_real,id = nd$nuts)
plotmap(mapdata, x = 100*nd$FDP_real,id = nd$nuts)
plotmap(mapdata, x = 100*nd$LINKE_real,id = nd$nuts)
plotmap(mapdata, x = 100*nd$GRUENE_real,id = nd$nuts)
plotmap(mapdata, x = 100*nd$Sonstige_real,id = nd$nuts)