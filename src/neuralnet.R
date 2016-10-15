library(neuralnet)
library(tidyr)
library(dplyr)
library(ggplot2)


iris.prepared <- iris %>%
                  mutate(tmp = 1) %>%
                  distinct %>%
                  spread(Species, tmp, 0)

s <- sample(150, 100)
iris.train <- iris.prepared[s,]
iris.test <- iris[-s,]

iris.nn <- neuralnet(setosa + versicolor + virginica ~
                       Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                     iris.train, hidden = c(3), threshold = 0.01)

iris.nn.results <- neuralnet::compute(iris.nn, iris.test[,1:4])$net.result
iris.nn.results <- round(iris.nn.results)
