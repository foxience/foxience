library(neuralnet)
library(tidyr)
library(dplyr)
library(gmodels)
library(ggplot2)

setwd('assignment')

bd <- read.csv('blood_donation.csv')

head(bd)

ggplot(bd, aes(Number.of.Donations, Total.Volume.Donated..c.c..)) + geom_jitter()
ggplot(bd, aes(Number.of.Donations, Months.since.First.Donation)) + geom_jitter()
ggplot(bd, aes(Number.of.Donations, Months.since.Last.Donation)) + geom_jitter()
ggplot(bd, aes(Months.since.First.Donation, Months.since.Last.Donation)) + geom_jitter()

bd.prepared <- bd %>%
  rename(MonthsLD = Months.since.Last.Donation,
        NumberDonations = Number.of.Donations,
        TotalVolume = Total.Volume.Donated..c.c..,
        MonthsFD = Months.since.First.Donation,
        March.2007 = Made.Donation.in.March.2007) %>%
  mutate(March.2007 = as.factor(March.2007)) %>%
  mutate(DonateDuration = MonthsFD - MonthsLD + 1) %>%
  mutate(DonationPerMonth = NumberDonations / DonateDuration) %>%
  mutate(MonthPerDonation = DonateDuration / NumberDonations) %>%
  select(NumberDonations,
        MonthsFD,
        MonthsLD,
        # TotalVolume,
        # DonateDuration,
        DonationPerMonth,
        MonthPerDonation,
        March.2007)

ggplot(bd.prepared, aes(March.2007, MonthsFD)) +
  geom_boxplot()

ggplot(bd.prepared, aes(March.2007, MonthsLD)) +
  geom_boxplot()

ggplot(bd.prepared, aes(March.2007, NumberDonations)) +
  geom_boxplot()

ggplot(bd.prepared, aes(MonthsFD, MonthsLD, col = March.2007)) +
    geom_point()

ggplot(bd.prepared, aes(TotalVolume, NumberDonations, col = March.2007)) +
  geom_point()

n <- nrow(bd.prepared)
s <- sample(1:n, 2*n/3)
bd.training <- bd.prepared[s,]
bd.validating <- bd.prepared[-s,]

bd.nn <- neuralnet(March.2007 ~ MonthsFD + MonthsLD + NumberDonations + DonationPerMonth + MonthPerDonation,
                   bd.training, hidden = c(5), threshold = 0.01, stepmax = 10000)

bd.nn.validating <- neuralnet::compute(bd.nn, select(bd.validating,
                                                    MonthsFD,
                                                    MonthsLD,
                                                    NumberDonations,
                                                    DonationPerMonth,
                                                    MonthPerDonation))

CrossTable(bd.validating$March.2007, round(bd.nn.validating$net.result))
