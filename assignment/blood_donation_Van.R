library(ggplot2)
library(class)
library(gmodels)

bd <- read.csv('blood_donation.csv', stringsAsFactors = TRUE)

head(bd)
str(bd)

table(bd$Made.Donation.in.March.2007)

ggplot(data = bd, aes(x = Made.Donation.in.March.2007, y = Months.since.Last.Donation, col = Made.Donation.in.March.2007)) + geom_jitter()
ggplot(data = bd, aes(x = Made.Donation.in.March.2007, y = Number.of.Donations, col = Made.Donation.in.March.2007)) + geom_jitter()
ggplot(data = bd, aes(x = Made.Donation.in.March.2007, y = Total.Volume.Donated..c.c.., col = Made.Donation.in.March.2007)) + geom_jitter()
ggplot(data = bd, aes(x = Made.Donation.in.March.2007, y = Months.since.First.Donation, col = Made.Donation.in.March.2007)) + geom_jitter()

# create random sample for training and test data
s <- sample(nrow(bd), 300)
bd.training <- bd[s,]
bd.test <- bd[-s,]

# create logistic model to reflect the relationship of dr to other predictors
bd.logis <- glm(Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations,
                 family = binomial, data = bd.training)

summary(bd.logis)

bd.prediction <- predict(bd.logis, bd.test, type = "response")

#bd.test <- cbind(bd.test, Made.Donation.in.March.2007 = bd.test.results)
bd.test <- cbind(bd.test, probability = bd.prediction)
CrossTable(x = bd.test$Made.Donation.in.March.2007, y = round(bd.prediction), prop.chisq = TRUE)

# k-nearest neighbor
normalizeNum <- function (x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

bd_n <- as.data.frame(lapply(bd[2:5], normalizeNum))
bd_c <- bd[6]

s <- sample(376, 300)
bd_train <- bd_n[s,]
bd_train_labels <- bd_c[s,]
bd_test <- bd_n[-s,]
bd_test_labels <- bd_c[-s,]

bd_test_pred <- knn(train = bd_train, test = bd_test,
                     cl = bd_train_labels, k=17)

CrossTable(x = bd_test_labels, y = bd_test_pred, prop.chisq = TRUE)

#May
# Predict the number of people who would donate in May
bd15 <- bd 
for(i in 1:length(bd15$Made.Donation.in.March.2007)){
if(bd15$Made.Donation.in.March.2007[i] == 1){
  bd15$Months.since.Last.Donation[i] = 2
  bd15$Number.of.Donations[i] <- bd15$Number.of.Donations[i] + 1
}else{
  bd15$Months.since.Last.Donation[i] <- bd15$Months.since.Last.Donation[i] + 2
}
}

# create random sample for training and test data
bd.training <- bd
bd.test <- bd15

# create logistic model to reflect the relationship of dr to other predictors
bd.logis <- glm(Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations,
                family = binomial, data = bd.training)

summary(bd.logis)

bd.prediction <- predict(bd.logis, bd.test, type = "response")
prediction.May <- round(bd.prediction, digits = 0)
bd15 <- cbind(bd15, prediction.May)

#the number of people who would donate in May
sum(bd15$prediction.May)