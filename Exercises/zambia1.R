setwd("~/Desktop/Dagstat_code")
library(bamlss)

# load data
load(file="data/ZambiaNutrition.rda")#data("ZambiaNutrition", package = "R2BayesX")
head(ZambiaNutrition)

plot(ZambiaNutrition$agechild, ZambiaNutrition$stunting, xlab="age of the child", ylab="stunting")


# fit simple linear model, plot effects, 

m1 <- bamlss(stunting ~ s(mbmi, bs='ps') + s(agechild, bs='bs'), data = ZambiaNutrition, family = "gaussian")
plot(m1)

m2 <- bamlss(stunting ~ s(mbmi, bs='ps',m=c(1,1), k=5) + s(agechild, bs='bs', m=c(1,1), k=5), data = ZambiaNutrition, family = "gaussian")
plot(m2)

m3 <- bamlss(stunting ~ s(mbmi, bs='ps',m=c(5,2), k=50) + s(agechild, bs='bs', m=c(5,2), k=50), data = ZambiaNutrition, family = "gaussian")
plot(m3)

##
m4 <- bamlss(stunting ~ s(mbmi) + s(agechild, by=gender), data = ZambiaNutrition, family = "gaussian")
plot(m4)

## add linear effects and spatial effects with markov random field prior and 
load(file="data/ZambiaBnd.rda")#data("ZambiaBnd", package = "R2BayesX")
ZambiaNutrition$district <- as.factor(ZambiaNutrition$district)
K <- neighbormatrix(ZambiaBnd)
# remove unobserved regions from the penalty matrix
rn <- rownames(K)
lv <- levels(ZambiaNutrition$district)
i <- rn %in% lv
K <- K[i, i]
# fit the model
m5 <- bamlss(stunting ~ memployment + urban + gender + meducation +s(mbmi) + s(agechild) + s(district, bs = "mrf", xt = list("penalty" = K)) + s(district, bs = "re"), data = ZambiaNutrition, family = "gaussian")

# predict the spatial effects
nd <- data.frame("district" = unique(ZambiaNutrition$district))
p_mrf <- predict(m5, newdata = nd, term = "s(district,id='mrf1')", intercept = FALSE)
p_re <- predict(m5, newdata = nd, term = "s(district,id='re2')", intercept = FALSE)

plotmap(ZambiaBnd, x = p_mrf$mu,id = nd$district)
plotmap(ZambiaBnd, x = p_re$mu,id = nd$district)
plotmap(ZambiaBnd, x = p_mrf$mu-p_re$mu,id = nd$district)

# decide wether the spatial effects are significant according to 95% credible intervals
# Test if all effects contain zero, i.e., are not significant
p_mrf <- predict(m5, newdata = nd, term = "s(district,id='mrf1')", intercept = FALSE, FUN = c95)
p_re <- predict(m5, newdata = nd, term = "s(district,id='re2')", intercept = FALSE, FUN = c95)

all(p_mrf$mu[["2.5%"]] < 0 & p_mrf$mu[["97.5%"]] > 0) # MRF is significant
all(p_re$mu[["2.5%"]] < 0 & p_re$mu[["97.5%"]] > 0) # The unstructured spatial effect is not significant
