setwd("~/Desktop/Dagstat_code")

library("bamlss")

# read the data
rent <- read.table("data/rent.dat", header=TRUE)
rent_test <- read.table("data/rent_test.dat", header=TRUE)

rent$location <- as.factor(rent$location)
rent_test$location <- as.factor(rent_test$location)
rent$district <- as.factor(rent$district)
rent_test$district <- factor(rent_test$district)
rent$size <- rent$rent/rent$rentsqm
rent_test$size <- rent_test$rent/rent_test$rentsqm

# function to calculate log-score

# example regression model
m1 <- bamlss(list(rentsqm ~ s(area) + s(yearc) + s(size) + location + s(district, bs="re"),sigma~s(area) + s(yearc) + s(size) + location + s(district, bs="re")),family="lognormal", data=rent)

# evaluate hold-out log-likelihood
p_m1 <- predict(m1, newdata=rent_test, type = "parameter")
ll <- sum(dlnorm(rent_test$rentsqm, mean=p_m1$mu, sd=p_m1$sigma, log=TRUE))
ll