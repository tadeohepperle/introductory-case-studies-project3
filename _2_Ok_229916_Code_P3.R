library(readr)
library(lessR)
library(dplyr)
library(ggplot2)
library(xtable)

#version of packages
sessionInfo()

setwd("C:/Users")
surveydata <- read.csv("vwcars.csv", TRUE)
View(surveydata)

sum(is.na(surveydata)) #count missing values

#1-1. Compute the log of the car price (logprice).
surveydata$logprice <- log(surveydata$price)

#1-2. lp100 = 282.48/mpg.
surveydata$lp100 <- 282.48 / surveydata$mpg

#1-3. Use the variable year to calculate the cars¡¯ age and use the new variable in the following.
surveydata$age <- 2021 - surveydata$year
#surveydata <- subset(surveydata, select = -c(year))

#Descriptive analysis
#list of categotical variables
table(surveydata$transmission)
table(surveydata$fuelType)
table(surveydata$model)
#mileage, engineSize, tax, logprice, lp100, age
summary(surveydata$mileage)

hist(surveydata$logprice, col = "orange")
boxplot(surveydata$logprice, horizontal = TRUE, ylab="logprice", col = "orange")

#2-1. Decide whether to use price or logprice as the response variable
names(surveydata)
fit_1 = lm(price ~ model+mileage+fuelType+engineSize+tax+transmission+lp100+age, data = surveydata)
plot(fit_1, which = 1, sub.caption = "")
plot(fit_1, which = 2, sub.caption = "")

fit_2 = lm(logprice ~ model+mileage+fuelType+engineSize+tax+transmission+lp100+age, data = surveydata)
plot(fit_2, which = 1, sub.caption = "")
plot(fit_2, which = 2, sub.caption = "")

#2-2. find the ¡°best¡± set of explanatory variables for the price using Best Subset Selection
library(leaps)
bestsub.model <- regsubsets(logprice ~ model+mileage+fuelType+engineSize+tax+transmission+lp100+age, data = surveydata, nvmax =11)
bestsub.sum = summary(bestsub.model)

n <- length(surveydata$logprice)
p <- apply(bestsub.sum$which, 1, sum)
aic <- bestsub.sum$bic - log(n)*p +2*p

table1<- cbind(AIC = aic, BIC = bestsub.sum$bic)

#AIC variables
plot(aic, xlab = "Number of Variables", ylab = "AIC", type = 'b', cex.lab=1.5)
best_aic = which.min(aic)
points(best_aic, aic[best_aic],
       col = "red", cex = 2, pch = 20)

aicbest <- seq(along = aic)[aic == min(aic)]
aicb <- bestsub.sum$which[aicbest, ]
names(aicb)[aicb][-1]

#BIC variables
plot(bestsub.sum$bic, xlab = "Number of Variables", ylab = "BIC", type = 'b', cex.lab=1.5)
best_bic = which.min(bestsub.sum$bic)
points(best_bic, bestsub.sum$bic[best_bic],
       col = "red", cex = 2, pch = 20)

bicbest <- seq(along = bestsub.sum$bic)[bestsub.sum$bic == min(bestsub.sum$bic)]
bicb <- bestsub.sum$which[bicbest, ]
names(bicb)[bicb][-1]

#2-3. Estimate the ¡°best¡± linear model for the dependent variable w.r.t. the BIC from ii.
newfit <- lm(logprice~model+mileage+fuelType+engineSize+transmission+age, data = surveydata)
plot(newfit, which=1, sub.caption = "")

qqnorm(resid(newfit), cex.axis=1)
qqline(resid(newfit), col=2)

coef(bestsub.model, 8)
summary(newfit)
table_coef<-summary(newfit)$coef
xtable(table_coef, digits = 5)
table_confint<-confint(newfit, level = 0.95)
xtable(table_confint, digits = 5)
bestsub.sum$adjr2