library(neuralnet)
library(tidyr)
library(dplyr)
library(ggplot2)

setwd('/home/rook/Projects/Code/r-source/assignment')

bd <- read.csv('blood_donation.csv')

ggplot(bd, aes(Number.of.Donations, Total.Volume.Donated..c.c..)) + geom_jitter()

ggplot(bd, aes(Number.of.Donations, Months.since.First.Donation)) + geom_jitter()

bd_prepared <- bd %>%
  rename(MonthsLD = Months.since.Last.Donation,
          NumberDonations = Number.of.Donations,
          TotalVolume = Total.Volume.Donated..c.c..,
          MonthsFD = Months.since.First.Donation,
          March.2007 = Made.Donation.in.March.2007) %>%
  # mutate(March.2007 = as.factor(March.2007)) %>%
  mutate(DonateDuration = MonthsFD - MonthsLD + 1) %>%
  mutate(DonationPerMonth = NumberDonations / DonateDuration) %>%
  mutate(MonthPerDonation = DonateDuration / NumberDonations) %>%
  select(NumberDonations,
        MonthsFD,
        MonthsLD,
        TotalVolume,
        DonationPerMonth,
        MonthPerDonation,
        March.2007)

ggplot(bd_prepared, aes(March.2007, MonthsFD)) +
  geom_boxplot()

ggplot(bd_prepared, aes(March.2007, MonthsLD)) +
  geom_boxplot()

ggplot(bd_prepared, aes(March.2007, NumberDonations)) +
  geom_boxplot()

ggplot(bd_prepared, aes(MonthsFD, MonthsLD, col = March.2007)) +
    geom_point()

ggplot(bd_prepared, aes(TotalVolume, NumberDonations, col = March.2007)) +
  geom_point()

n <- nrow(bd_prepared)  
s <- sample(1:n, 2*n/3)
bd.training <- bd_prepared[s,]
bd.testing <- bd_prepared[-s,]

bd.nn <- neuralnet(March.2007 ~ MonthsFD + MonthsLD + NumberDonations + DonationPerMonth + MonthPerDonation,
                   bd.training, hidden = c(5), threshold = 0.01, stepmax = 20000)

bd.nn.results <- neuralnet::compute(bd.nn, select(bd.testing,
                                                  MonthsFD,
                                                  MonthsLD,
                                                  NumberDonations,
                                                  DonationPerMonth,
                                                  MonthPerDonation))$net.result
bd.nn.results <- round(bd.nn.results)
cbind(bd.testing, bd.nn.results)