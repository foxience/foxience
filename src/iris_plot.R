library(tidyr)
library(dplyr)
library(ggplot2)
library(psych)

head(iris)

ggplot(iris, aes(x = Species, fill = Species)) +
  geom_bar()

# starting with a simple point plotting
ggplot(iris, aes(x = Species, y = Sepal.Length, col = Species)) +
  geom_jitter()

ggplot(iris, aes(x = Species, y = Sepal.Length, col = Species)) +
  geom_boxplot()

ggplot(iris, aes(x = Species, y = Sepal.Width, col = Species)) +
  geom_boxplot()

ggplot(iris, aes(x = Species, y = Petal.Length, col = Species)) +
  geom_boxplot()

ggplot(iris, aes(x = Species, y = Petal.Width, col = Species)) +
  geom_boxplot()

# plot 2 variables
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point(position = "jitter")

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, col = Species)) +
  geom_point(position = "jitter")

# restructure data to seperate Petal and Sepal
iris.long <- iris %>%
  gather(tmp, Value, -Species, -ID) %>%
  separate(tmp, c("Part", "Measure"), sep = "\\.")

iris.wide <- iris.long %>%
  spread(Measure, Value)

ggplot(iris.wide, aes(x = Length, y = Width, col = Part)) +
  geom_jitter()

ggplot(iris.wide, aes(x = Length, y = Width, col = Part)) +
  geom_jitter() +
  facet_grid(Species ~ .)

ggplot(iris.setosa, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter() +
  geom_smooth(method = "lm")

ggplot(iris.setosa, aes(x = Petal.Length, y = Petal.Width)) +
  geom_jitter() +
  geom_smooth(method = "lm")

ggplot(iris.setosa, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_jitter() +
  geom_smooth(method = "lm")

ggplot(iris.setosa, aes(x = Sepal.Width, y = Petal.Width)) +
  geom_jitter() +
  geom_smooth(method = "lm")
