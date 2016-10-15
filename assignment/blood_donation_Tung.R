library(ggplot2)
library(class)
library(gmodels)
library(plyr)

setwd("/home/rook/Projects/Code/r-source/assignment")

bld <- read.csv('blood_donation.csv', stringsAsFactors = TRUE)

bld <- bld[-1]
bld <- rename(bld, c("Months.since.Last.Donation" = "MonthsLD",
                     "Number.of.Donations" = "times",
                     "Total.Volume.Donated..c.c.." = "TVolume",
                     "Months.since.First.Donation"="MonthsFD",
                     "Made.Donation.in.March.2007" = "March.2007"))

bld_fit <- bld

bld_fit$times <- bld_fit$times + bld_fit$March.2007
bld_fit$March.2007 <- abs(bld_fit$March.2007 - 1)
bld_fit$MonthsLD5 <- bld_fit$March.2007*bld_fit$MonthsLD + 2
bld_fit$March.2007 <- abs(bld_fit$March.2007 - 1)
bld_fit$MonthsFD <- bld_fit$MonthsFD + 2

bld2 <- subset(bld_fit, times > 1)

bld2$average <- (bld2$MonthsFD - bld2$MonthsLD5)/bld2$times
bld2$average2 <- abs(bld2$MonthsLD - bld2$average)

ggplot(data = bld2, aes(x = March.2007, y = MonthsLD, col = March.2007)) + geom_jitter()
ggplot(data = bld2, aes(x = March.2007, y = MonthsLD5, col = March.2007)) + geom_jitter()
ggplot(data = bld2, aes(x = March.2007, y = average2, col = March.2007)) + geom_jitter()
ggplot(data = bld2, aes(x = March.2007, y = times, col = March.2007)) + geom_jitter()

s <- sample(nrow(bld2), 74)
bld2.test <- bld2[s,]
bld2.training <- bld2[-s,]

bld2.logis <- glm(March.2007 ~ average2, family = binomial, data = bld2.training)

summary(bld2.logis)

bld2.prediction <- predict(bld2.logis, bld2.test, type = "response")

bld2.test <- cbind(bld2.test, probability = bld2.prediction)
CrossTable(x = bld2.test$March.2007, y = round(bld2.prediction), prop.chisq = TRUE)

#knn
normalizeNum <- function (x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

bld_n <- as.data.frame(lapply(bld2[c(6,8)], normalizeNum))
bld_c <- bld2[5]

s <- sample(304, 220)
bld_train <- bld_n[s,]
bld_train_labels <- bld_c[s,]
bld_test <- bld_n[-s,]
bld_test_labels <- bld_c[-s,]

bld_test_pred <- knn(train = bld_train, test = bld_test,
                     cl = bld_train_labels, k=10)

CrossTable(x = bld_test_labels, y = bld_test_pred, prop.chisq = TRUE)