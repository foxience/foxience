library(WHO)
library(tidyr)
library(dplyr)
library(ggplot2)
leab <- get_data('WHOSIS_000001')

ggplot(leab, aes(x = region, y = value)) + geom_point()
ggplot(leab, aes(x = region, y = value)) + geom_point(position = "jitter")

ggplot(leab, aes(x = value)) + geom_histogram()

ggplot(leab, aes(x = value)) + geom_histogram() + facet_grid(region ~ .)
ggplot(filter(leab, region == 'Africa'), aes(x = value)) + geom_histogram()

ggplot(sleep, aes(x = group, y = extra)) +
  geom_point(position = position_jitter(width = .2))

ggplot(iris, aes(x = Species, y = Sepal.Length, col = Species)) +
  geom_point(position = position_jitter(width = .2))

ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(aes(y = ..density..), binwidth = .1, fill = 'blue') +
  facet_grid(Species ~ .)

with(iris, t.test(Sepal.Length[Species == 'setosa'], Sepal.Length[Species == 'versicolor']))
with(sleep, t.test(extra[group == 1], extra[group == 2]))
