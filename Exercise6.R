library(dplyr)

## Initializing dataset and preprocessing
election_data = read.csv("data/Presidential_race_2016_-_Exercise_6.csv")
election_data['vote'] = ifelse(election_data$Percent.Clinton > election_data$Percent.Trump,1, 0)
clinton = filter(election_data, vote == 1)
trump = filter(election_data, vote == 0)
cdi = clinton$Diversity.Index
tdi = trump$Diversity.Index

## Check of the dataset is normally distributed
par(mfrow=c(2,2))
d <- density(cdi) # returns the density data 
plot(d, main="Clinton Diversity Index")
qqnorm(cdi, main="Clinton Diversity Index");qqline(cdi, col = 2)

g <- density(tdi)
plot(g, main="Trump Diversity Index")
qqnorm(tdi, main="Trump Diversity Index");qqline(tdi, col = 2)


##Test for normality
shapiro.test(cdi)
shapiro.test(tdi)


## Welch two Sample t-test
t.test(cdi,tdi, conf.level = 0.95)

## Wilcox Test
wilcox.test(cdi, tdi)

## Check distribution of Dataset
f <- density(election_data$Diversity.Index)

par(mfrow=c(1,2))
plot(f, main="Diversity Index (Entire dataset)")
qqnorm(election_data$Diversity.Index);qqline(election_data$Diversity.Index, col = 2)

shapiro.test(election_data$Diversity.Index)

t.test(election_data$Diversity.Index, mu=50, conf.int=0.05)
wilcox.test(election_data$Diversity.Index, mu=50, conf.int=0.05)
