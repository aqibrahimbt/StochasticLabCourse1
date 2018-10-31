library(dplyr)
set.seed(1)

##Loading Dataset and preprocessing
wine_data = read.csv(file = "data/winequality-white.csv", sep = ";", header = TRUE)
wine_data_sample = wine_data[, c("volatile.acidity", "residual.sugar", "pH", "quality")]
wine_data_sample['good'] = ifelse(wine_data_sample$quality > 5,1, 0)
wine_data_sample$good = as.factor(wine_data_sample$good)
wine_data_sample_good = filter(wine_data_sample, good == 1)
wine_data_sample_bad = filter(wine_data_sample, good == 0)

## Function to compare Bin width algorithms
NC <- function(x) c(Sturges = nclass.Sturges(x), Scott = nclass.scott(x), FD = nclass.FD(x))
NC(wine_data_sample_good$volatile.acidity)

## Compute square root bin width for Volatile Acidity
sqrtN_good = sqrt(length(wine_data_sample_good$volatile.acidity))
sqrtN_bad = sqrt(length(wine_data_sample_bad$volatile.acidity))


## Histogram plot of the Various Bin Width for Volatile Acidity
par(mfrow=c(4,2))
hist(wine_data_sample_good$volatile.acidity, breaks = "Sturges", main='Sturges', xlab = "Volatile Acidity (Good Wine)")
hist(wine_data_sample_bad$volatile.acidity, breaks = "Sturges", main='Sturges', xlab = "Volatile Acidity (Bad Wine)")

hist(wine_data_sample_good$volatile.acidity, breaks = "Scott", main='Scott', xlab = "Volatile Acidity (Good Wine)")
hist(wine_data_sample_bad$volatile.acidity, breaks = "Scott", main='Scott', xlab = "Volatile Acidity (Bad Wine)")

hist(wine_data_sample_good$volatile.acidity, breaks = "FD", main='FD', xlab = "Volatile Acidity (Good Wine)")
hist(wine_data_sample_bad$volatile.acidity, breaks = "FD", main='FD', xlab = "Volatile Acidity (Bad Wine)")

hist(wine_data_sample_good$volatile.acidity, breaks = sqrtN_good, main='Square Root', xlab = "Volatile Acidity (Good Wine)")
hist(wine_data_sample_bad$volatile.acidity, breaks = sqrtN_bad, main='Square Root', xlab = "Volatile Acidity (Bad Wine)")


## Histogram plot of the Various Bin Width for Residual Sugar
par(mfrow=c(4,2))
sqrtN_good = sqrt(length(wine_data_sample_good$residual.sugar))
sqrtN_bad = sqrt(length(wine_data_sample_bad$residual.sugar))
hist(wine_data_sample_good$residual.sugar, breaks = "Sturges", main='Sturges', xlab = "Residual Sugar (Good Wine)")
hist(wine_data_sample_bad$residual.sugar, breaks = "Sturges", main='Sturges', xlab = "Residual Sugar (Bad Wine)")

hist(wine_data_sample_good$residual.sugar, breaks = "Scott", main='Scott', xlab = "Residual Sugar (Good Wine)")
hist(wine_data_sample_bad$residual.sugar, breaks = "Scott", main='Scott', xlab = "Residual Sugar (Bad Wine)")

hist(wine_data_sample_good$residual.sugar, breaks = "FD", main='FD', xlab = "Residual Sugar (Good Wine)")
hist(wine_data_sample_bad$residual.sugar, breaks = "FD", main='FD', xlab = "Residual Sugar (Bad Wine)")

hist(wine_data_sample_good$residual.sugar, breaks = sqrtN, main='Square Root', xlab = "Residual Sugar (Good Wine)")
hist(wine_data_sample_bad$residual.sugar, breaks = sqrtN, main='Square Root', xlab = "Residual Sugar (Bad Wine)")


## Summary Statistics for dataset
summary(wine_data_sample_good$volatile.acidity)
summary(wine_data_sample_bad$volatile.acidity)
sd(wine_data_sample_good$volatile.acidity)
sd(wine_data_sample_bad$volatile.acidity)
IQR(wine_data_sample_good$volatile.acidity)
IQR(wine_data_sample_bad$volatile.acidity)


summary(wine_data_sample_good$residual.sugar)
summary(wine_data_sample_bad$residual.sugar)
sd(wine_data_sample_good$residual.sugar)
sd(wine_data_sample_bad$residual.sugar)
IQR(wine_data_sample_good$residual.sugar)
IQR(wine_data_sample_bad$residual.sugar)

## Boxplot for comparing Good and Bad Quality Wine
par(mfrow=c(1,2))
boxplot(volatile.acidity~good,data=wine_data_sample, main="Wine Quality Data", xlab="Wine Quality", ylab="Volatile Acidity")
boxplot(residual.sugar~good,data=wine_data_sample, main="Wine Quality Data", xlab="Wine Quality", ylab="Residual Sugars")


## QQPlot to compare Wine samples 
par(mfrow=c(1,1))
qqplot(wine_data_sample_good$residual.sugar, wine_data_sample_bad$residual.sugar, xlim = c(0, 10), 
       ylim = c(0, 10), xlab = "Good Quality Wine",
       ylab = "Bad Quality Wine", main = "Q-Q Plot (Residual Sugars)")
abline(a = 0, b = 1, col = "blue", lwd = 2)

qqplot(wine_data_sample_good$volatile.acidity, wine_data_sample_bad$volatile.acidity, xlim = c(0, 2), 
       ylim = c(0, 2), xlab = "Good Quality Wine",
       ylab = "Bad Quality Wine", main = "Q-Q Plot Volatile Acidity)")
abline(a = 0, b = 1, col = "blue", lwd = 2)


## Empirical Cummulative Distribution Plot
par(mfrow=c(1,2))
plot(ecdf(wine_data_sample_good$residual.sugar),xlab="Residual Sugar", main="Empirical Cumluative Distribution",verticals=T, col="red")
lines(ecdf(wine_data_sample_bad$residual.sugar),lty=1,verticals=T, col="green") 
legend(20,0.9,c("Good Quality Wine","Bad Quality Wine"),col = c("red", "green"), pch = c(19,19))

plot(ecdf(wine_data_sample_good$volatile.acidity),xlab="Volatile Acidity", main="Empirical Cumluative Distribution",verticals=T, col="red")
lines(ecdf(wine_data_sample_bad$volatile.acidity),lty=1,verticals=T, col="green")
legend(20,0.9,c("Good Quality Wine","Bad Quality Wine"),col = c("red", "green"), pch = c(19,19))
