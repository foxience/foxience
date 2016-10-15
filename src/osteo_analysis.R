# load required libraries
library(tidyr)
library(dplyr)
library(ggplot2)

# set working directory
setwd('r-source/')

# read osteo data file into a variable
osteo <- read.csv('osteo.csv', sep = ';')

head(osteo)
str(osteo)

# change fracture to factor data type
osteo$fx <- as.factor(osteo$fx)

# plot to see the combination of 2 variables to predict fx
ggplot(osteo, aes(bmd, bmi, col = fx)) + geom_jitter()
ggplot(osteo, aes(ictp, pinp, col = fx)) + geom_jitter()

# create random sample for training and test data
s <- sample(nrow(osteo), 100)
osteo.training <- osteo[s,]
osteo.test <- osteo[-s,]

# create logistic model to reflect the relationship of fx to other predictors
osteo.logis <- glm(fx ~ bmd + bmi + age + ictp + pinp,
                   family = binomial, data = osteo.training)

summary(osteo.logis)

osteo.prediction <- predict(osteo.logis, osteo.test, type = "response")

cbind(osteo.test, probability = osteo.prediction)