library(tidyr)
library(dplyr)
library(ggplot2)
library(e1071)

s <- sample(nrow(iris), 100)
col <- c("Species", "Petal.Length", "Petal.Width")
trainingSample <- iris[s, col]
testingSample <- iris[-s, col]

svmodel <- svm(Species ~ ., data = trainingSample, cost = .1)

plot(svmodel, trainingSample, Petal.Length ~ Petal.Width)

prediction <- predict(svmodel, testingSample, type = "class")
table(testingSample[, 1], prediction)
