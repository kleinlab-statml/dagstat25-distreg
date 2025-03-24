setwd("~/Desktop/Dagstat_code")
library(bamlss)

# load data
forest <- read.table("data/foresthealth.dat", header=TRUE)

# additive model with age and canopy density as covariates
b1 <- bamlss(def ~ s(age) + s(canopy),family="binomial", data=forest)
plot(b1)

# probit rather than logit model
b2 <- bamlss(def ~ s(age) + s(canopy),family=binomial_bamlss(link="probit"), data=forest)
plot(b2)

# add random effect per id and a spatial effect based on coordinates
b3 <- bamlss(def ~ s(age) + s(canopy) + s(x,y) + s(id, bs="re"), family="binomial", data=forest)
plot(b3)