library(tidyr)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
iris
# plot
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, col = Species)) +
  geom_point()
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point()

iris.new <-
  iris %>%
  mutate(Sepal.Area = Sepal.Length * Sepal.Width, Petal.Area = Petal.Length * Petal.Width)

ggplot(iris.new, aes(x = Sepal.Area, y = Petal.Area, col = Species)) + geom_point()

s <- sample(nrow(iris), 100)
col <- c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
trainingSample <- iris[s, col]
testingSample <- iris[-s, col]

dtm <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = trainingSample, method = "class")
rpart.plot(dtm, type = 4, extra = 101)

prediction <- predict(dtm, testingSample, method = "class")

prediction <- as.data.frame(round(prediction))
cbind(prediction, Actual = testingSample[, 1])
# table(testingSample[, 1], as.vector(prediction))
