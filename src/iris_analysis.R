library(tidyr)
library(dplyr)
library(psych)

# anova for different groups
sepal.length.anova <- aov(Sepal.Length ~ Species, iris)
summary(sepal.length.anova)
TukeyHSD(sepal.length.anova)

sepal.width.anova <- aov(Sepal.Width ~ Species, iris)
summary(sepal.width.anova)
TukeyHSD(sepal.width.anova)

petal.length.anova <- aov(Petal.Length ~ Species, iris)
summary(petal.length.anova)
TukeyHSD(petal.length.anova)

petal.width.anova <- aov(Petal.Width ~ Species, iris)
summary(petal.width.anova)
TukeyHSD(petal.width.anova)

# linear regression
iris.setosa <- iris %>%
  filter(Species == 'setosa')

sepal.length.width <- lm(Sepal.Length ~ Sepal.Width, iris.setosa)
sepal.length.width
summary(sepal.length.width)

petal.length.width <- lm(Petal.Length ~ Petal.Width, iris.setosa)
petal.length.width
summary(petal.length.width)
