library(WHO);
library(xml2);
library(ggplot2);

codes <- get_codes();

codes[grepl("[Ll]ife expectancy", codes$display), ]

leab <- get_data('WHOSIS_000001')

ggplot(df, aes(x = value)) +
  geom_histogram(binwidth = 2, aes(y = ..density..)) +
  geom_density() +
  stat_function(fun=dnorm,
                color="red",
                args=list(mean=mean(df$value), sd=sd(df$value)))

# dice sample
rollDicesAndSum <- function (trial) {
  sum(sample(1:6, trial, replace = TRUE))
}

diceSum <- replicate(10000, rollDicesAndSum(10))
sds.data <- data.frame(sum = diceSum)

ggplot(sds.data, aes(x = sum)) + geom_histogram(binwidth = 1)
qqnorm(diceSum)

iris$ID <- 1:nrow(iris)

iris_long <- iris %>%
  gather(key = "temp", value = "Value", -ID, -Species) %>%
  separate(col = temp, into = c("Part", "Measurement"))

iris_wide <- iris_long %>%
  spread(key = Measurement, value = Value)

ggplot(iris_long, aes(x = Species, y = Value, col = Species)) +
  geom_point(position = position_jitterdodge()) +
  facet_grid(Measurement ~ Part)

ggplot(iris_long, aes(x = Species, y = Value, col = Species)) +
  geom_boxplot() +
  facet_grid(Measurement ~ Part)

ggplot(iris_wide, aes(x = Length, y = Width, col = Species)) +
  geom_point(position = position_jitter()) +
  facet_grid(. ~ Part)

ggplot(iris, aes(x = Sepal.Length * Sepal.Width,
                 y = Petal.Length * Petal.Width,
                 col = Species)) +
  geom_point(position = "jitter")