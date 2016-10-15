library(ggplot2)
library(class)
library(gmodels)

setwd('/home/rook/Projects/Code/r-source')

prc <- read.csv('prostate_cancer.csv', stringsAsFactors = TRUE)

head(prc)
str(prc)

# remove id column
prc <- prc[-1]
prc$diagnosis_result <- as.factor(as.numeric(prc$diagnosis_result == 'M'))

table(prc$diagnosis_result)

ggplot(data = prc, aes(x = diagnosis_result, y = radius, col = diagnosis_result)) + geom_jitter()
ggplot(data = prc, aes(x = diagnosis_result, y = texture, col = diagnosis_result)) + geom_jitter()
ggplot(data = prc, aes(x = diagnosis_result, y = perimeter, col = diagnosis_result)) + geom_jitter()
ggplot(data = prc, aes(x = diagnosis_result, y = area, col = diagnosis_result)) + geom_jitter()
ggplot(data = prc, aes(x = diagnosis_result, y = smoothness, col = diagnosis_result)) + geom_jitter()
ggplot(data = prc, aes(x = diagnosis_result, y = compactness, col = diagnosis_result)) + geom_jitter()
ggplot(data = prc, aes(x = diagnosis_result, y = symmetry, col = diagnosis_result)) + geom_jitter()
ggplot(data = prc, aes(x = diagnosis_result, y = fractal_dimension, col = diagnosis_result)) + geom_jitter()

# create random sample for training and test data
s <- sample(nrow(prc), 70)
prc.training <- prc[s,]
prc.test <- prc[-s,]

# create logistic model to reflect the relationship of dr to other predictors
prc.logis <- glm(diagnosis_result ~ area + compactness,
                   family = binomial, data = prc.training)

summary(prc.logis)

prc.prediction <- predict(prc.logis, prc.test, type = "response")

prc.test <- cbind(prc.test, diagnosis_result = prc.test.results)
prc.test <- cbind(prc.test, probability = prc.prediction)
CrossTable(x = prc.test$diagnosis_result, y = round(prc.prediction), prop.chisq = TRUE)

# k-nearest neighbor
normalizeNum <- function (x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

prc_n <- as.data.frame(lapply(prc[2:9], normalizeNum))
prc_c <- prc[1]

s <- sample(100, 70)
prc_train <- prc_n[s,]
prc_train_labels <- prc_c[s,]
prc_test <- prc_n[-s,]
prc_test_labels <- prc_c[-s,]

prc_test_pred <- knn(train = prc_train, test = prc_test,
                     cl = prc_train_labels, k=10)

CrossTable(x = prc_test_labels, y = prc_test_pred, prop.chisq = TRUE)
