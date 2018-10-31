library(dplyr)
library(ggplot2)
library(devtools)
library(qqplotr)
library(sfsmisc)

## Loading dataset and preprocessing
set.seed(1)
wine_data = read.csv(file = "data/winequality-white.csv", sep = ";", header = TRUE)
wine_data_sample = wine_data[, c("volatile.acidity", "residual.sugar", "pH", "quality")]
wine_data_sample['good'] = ifelse(wine_data_sample$quality > 5,1, 0)
wine_data_sample$good = as.factor(wine_data_sample$good)
wine_data_sample_good = filter(wine_data_sample, good == 1)
wine_data_sample_bad = filter(wine_data_sample, good == 0)


## Calculating the Mean and Standard deviation
mean = mean(wine_data_sample$pH)
sd = sd(wine_data_sample$pH)

## Histogram and Density Plot for Wine Sample
ggplot(wine_data_sample, aes(x = pH, mean = mean, sd = sd)) +
  geom_histogram(aes(y=..density..),colour = "white", fill = "cornflowerblue") +
  stat_function(fun = function(x) dnorm(x, mean = mean, sd = sd),
                color = "darkred", size = 1)

## Histogram and Density Plot for Good Quality Wine Sample
par(mfrow=c(2,2))
mean= mean(wine_data_sample_good$pH)
sd = sd(wine_data_sample_good$pH)

ggplot(wine_data_sample_good, aes(x = pH, mean = mean, sd = sd)) +
  geom_histogram(aes(y=..density..), colour = "white", fill = "cornflowerblue") +
  stat_function(fun = function(x) dnorm(x, mean = mean, sd = sd),
                color = "darkred", size = 1) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


## Histogram and Density Plot for Bad Quality Wine Sample
mean= mean(wine_data_sample_bad$pH)
sd = sd(wine_data_sample_bad$pH)

ggplot(wine_data_sample_bad, aes(x = pH, mean = mean, sd = sd)) +
  geom_histogram(aes(y=..density..), colour = "white", fill = "green") +
  stat_function(fun = function(x) dnorm(x, mean = mean, sd = sd),color = "darkred", size = 1) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


## Normal QQ-plot for Wine Samples
ggplot(data = wine_data_sample, mapping = aes(sample = pH)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

## Normal QQ-plot for Good Quaity Wine Samples
ggplot(data = wine_data_sample_good, mapping = aes(sample = pH)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


## Normal QQ-plot for Bad Quaity Wine Samples
ggplot(data = wine_data_sample_bad, mapping = aes(sample = pH)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


## PP-plot for Wine Samples
ggplot(data = wine_data_sample, mapping = aes(sample = pH)) +
  stat_pp_band() +
  stat_pp_line() +
  stat_pp_point() +
  labs(x = "Probability Points", y = "Cumulative Probability") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

## PP-plot for Good Quality Wine Samples
ggplot(data = wine_data_sample_good, mapping = aes(sample = pH)) +
  stat_pp_band() +
  stat_pp_line() +
  stat_pp_point() +
  labs(x = "Probability Points", y = "Cumulative Probability") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

## PP-plot for Bad Quality Wine Samples
ggplot(data = wine_data_sample_bad, mapping = aes(sample = pH)) +
  stat_pp_band() +
  stat_pp_line() +
  stat_pp_point() +
  labs(x = "Probability Points", y = "Cumulative Probability") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


## Empirical Cummulative Distribution Plot for Wine Samples
ggplot(wine_data_sample, aes(pH)) + stat_ecdf(geom = "point") + 
  labs(title="Empirical Distributions for pH")

## Empirical Cummulative Distribution Plot for Good Quality Wine Samples
ggplot(wine_data_sample_good, aes(pH)) + stat_ecdf(geom = "point") + 
  labs(title="Empirical Distributions for pH")

## Empirical Cummulative Distribution Plot for Bad Quality Wine Samples
ggplot(wine_data_sample_bad, aes(pH)) + stat_ecdf(geom = "point") + 
  labs(title="Empirical Distributions for pH")


## Empirical Cummulative Distribution with confidence bands
x = wine_data_sample_bad$pH
n			<-	length(wine_data_sample_bad$pH)  			
FF			<-	ecdf(x)			
# then we calculate the confidence band
alpha		<-	0.05
eps 		<-	sqrt(log(2/alpha)/(2*n))
xx			<-	seq(min(x)-1,max(x)+1,length.out=1000)
ll			<-	pmax(FF(xx)-eps,0)
uu 			<-	pmin(FF(xx)+eps,1)
# then we plot everything
plot(FF, cex=0.25, xlab="Bad Quality Wine")

lines(xx, ll, col="red") 	
lines(xx, uu, col="red")


m = mean(wine_data_sample$pH)
s = sd(wine_data_sample$pH)
ll = m - 1.96 * s/sqrt(length(wine_data_sample$pH))
uu = m + 1.96 * s/sqrt(length(wine_data_sample$pH))
ecdf1 = ecdf(wine_data_sample$pH)



ecdf2 = ecdf(wine_data_sample_good$pH)
ecdf3 = ecdf(wine_data_sample_bad$pH)
plot(ecdf1, verticals = TRUE, do.points=FALSE)
plot(ecdf2, verticals = TRUE, do.points=FALSE, add=TRUE, col="brown")
plot(ecdf3, verticals = TRUE, do.points=FALSE, add=TRUE, col="orange")
