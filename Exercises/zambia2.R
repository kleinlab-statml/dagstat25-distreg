setwd("~/Desktop/Dagstat_code")
library(bamlss)

# load data
load(file="data/ZambiaNutrition.rda")
load(file="data/ZambiaBnd.rda")

# use different hyperpriors for tau
m_ig <- bamlss(stunting ~ s(agechild,xt = list(prior = "ig", a = 0.001, b = 0.001)), data = ZambiaNutrition, family = "gaussian")
m_sd <- bamlss(stunting ~ s(agechild,xt = list(prior = "sd")), data = ZambiaNutrition, family = "gaussian")
m_h <- bamlss(stunting ~ s(agechild,xt = list(prior = "hc")), data = ZambiaNutrition, family = "gaussian")
m_u <- bamlss(stunting ~ s(agechild,xt = list(prior = "u")), data = ZambiaNutrition, family = "gaussian")
# inspect mcmc samples
plot(m_ig, which = "samples")

#
ZambiaNutrition$district <- as.factor(ZambiaNutrition$district)
K <- neighbormatrix(ZambiaBnd)
rn <- rownames(K)
lv <- levels(ZambiaNutrition$district)
i <- rn %in% lv
K <- K[i, i]

m1 <- bamlss(list(stunting~s(mbmi)+s(agechild)+s(district, bs = "mrf", xt = list("penalty" = K)),sigma~1), data = ZambiaNutrition, family = "gaussian")
m2 <- bamlss(list(stunting~s(mbmi)+s(agechild)+s(district, bs = "mrf", xt = list("penalty" = K)),sigma~s(mbmi)+s(agechild)+s(district, bs = "mrf", xt = list("penalty" = K))), data = ZambiaNutrition, family = "gaussian")

# compare models in terms of DIC and qq-plots
DIC(m1,m2)
plot(m1,which='qq')
plot(m2,which='qq')
