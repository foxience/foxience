
# select, filter, mutate, arrange, summarise
iris %>%
  mutate(Sepal.Ratio = Sepal.Length / Sepal.Width) %>%
  mutate(Petal.Ratio = Petal.Length / Petal.Width) %>%
  select(Sepal.Ratio, Petal.Ratio, Species, ID) %>%
  filter(Species == 'setosa') %>%
  arrange(Sepal.Ratio) %>%
  mutate(greaterThan1.5 = Sepal.Ratio > 1.5) %>%
  group_by(greaterThan1.5) %>%
  summarise(meanSepalRatio = mean(Sepal.Ratio))

# gather, separate, spread
iris %>%
  gather(key = flower_att, value = Value,
         Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
  separate(col = flower_att, into = c("Part", "Measurement"), sep = '\\.') %>%
  spread(key = Measurement, value = Value)