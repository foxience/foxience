library(ggplot2)
library(tidyr)
library(dplyr)
library(gmodels)
library(class)
setwd('C:/Users/HanhNguyen/Downloads')
bl <- read.csv('blood_donation.csv', header = TRUE)
str(bl)
attach(bl)
bl$Made.Donation.in.March.2007<- as.factor(bl$Made.Donation.in.March.2007)
bl <- bl[-1]
ggplot(bl, aes(Made.Donation.in.March.2007, Months.since.Last.Donation, col = Made.Donation.in.March.2007)) + geom_boxplot()
ggplot(bl, aes(Made.Donation.in.March.2007, Number.of.Donations, col = Made.Donation.in.March.2007)) + geom_boxplot()
ggplot(bl, aes(Made.Donation.in.March.2007, Total.Volume.Donated..c.c.., col = Made.Donation.in.March.2007)) + geom_boxplot()
ggplot(bl, aes(Made.Donation.in.March.2007, Months.since.First.Donation, col = Made.Donation.in.March.2007)) + geom_boxplot()
ggplot(bl, aes(Number.of.Donations, Total.Volume.Donated..c.c.., col = Made.Donation.in.March.2007)) + geom_jitter()

s<- sample(376, 270)  
bl_train <- bl[s,]
bl_test <- bl[-s,]
bl_model <- glm(Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations, family = binomial, data = bl_train)
bl_pred <- predict(bl_model, bl_test, type = 'response')
bl_1 <- cbind(select(bl_test, Made.Donation.in.March.2007), bl_pred)
head(bl_1, 10)
CrossTable(bl_test$Made.Donation.in.March.2007, round(bl_pred), prop.chisq = TRUE)
#knn
normalizenum <- function(x){(x-min(x))/(max(x)- min(x))}
bl_n <- as.data.frame(lapply(bl[1:4], normalizenum))
bl_c <- bl[5]
s <- sample(376, 270)
bl_testing <- bl_n[s,]
bl_testing_lable <- bl_c[s,]
bl_training <- bl_n[-s,]
bl_training_lable <- bl_c[-s,]
bl_knn <- knn(bl_training, bl_testing, cl= bl_training_lable, k = 16)
CrossTable(bl_testing_lable, bl_knn, prop.chisq = TRUE)
# du doan thang 5
n1<-filter(bl, Made.Donation.in.March.2007=='1')
n1$Months.since.Last.Donation<-2
nf1<- function(x){x+1}
n1$Number.of.Donations <- lapply(n1$Number.of.Donations, nf1)
n1$Number.of.Donations<- as.integer(n1$Number.of.Donations)
head(n1)
str(n1)
n2<-filter(bl, Made.Donation.in.March.2007=='0')
nf2<-function(x){x+2}
n2$Months.since.Last.Donation<- lapply(n2$Months.since.Last.Donation, nf2)
n2$Months.since.Last.Donation<- as.numeric(n2$Months.since.Last.Donation)
head(n2)
bl_pd5<- predict(bl_model, n1, type = 'response')
bl_pd5_non<-predict(bl_model, n2, type = 'response')
table(round(bl_pd5))
table(round(bl_pd5_non))
