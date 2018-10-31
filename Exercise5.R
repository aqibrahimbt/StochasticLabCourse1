library(dplyr)
library(ggplot2)
theme_set(theme_classic())

##Initialize dataset and pre-processing
cities = c("Los Angeles", "Phoenix", "San Diego", "San Francisco", "Seattle", "Los Angeles", "Phoenix", "San Diego", "San Francisco", "Seattle")
ontime = c(497, 221, 212, 503, 1841, 694, 4840, 383, 320, 201)
delayed = c(62, 12, 20, 102, 305, 117, 415, 65, 129, 61)
flight = c("Alaska Airlines", "Alaska Airlines", "Alaska Airlines", "Alaska Airlines", "Alaska Airlines", "America West", "America West", "America West", "America West", "America West")

data = data.frame(cities, delayed, ontime, flight)

## Chi-seq test for Dependence By Cities

##Alaska Airlines
data = filter(data, flight=="Alaska Airlines")
rownames(data) = unique(data$Cities)
data$flight <- NULL
data = aggregate(list(delayed = data$delayed, ontime=data$ontime), by=list(Cities=data$cities), FUN=sum)
rownames(data) = unique(data$Cities)
data$Cities <- NULL
Xsq <- chisq.test(data)
Xsq$statistic
Xsq$p.value

##Formula = sum((observed - expected)^2 /expected)
expected <- outer(rowSums(data), colSums(data))/sum(data) 
chi_squared_statistic <-  sum(((data-expected)^2)/expected) 
chi_squared_statistic
pvalue = 1 - pchisq(q=chi_squared_statistic, df=4)
pvalue


##America West
data = data.frame(cities, delayed, ontime, flight)
data = filter(data, flight=="America West")
rownames(data) = unique(data$Cities)
data$flight <- NULL
data = aggregate(list(delayed = data$delayed, ontime=data$ontime), by=list(Cities=data$cities), FUN=sum)
rownames(data) = unique(data$Cities)
data$Cities <- NULL
Xsq <- chisq.test(data)
Xsq$statistic
Xsq$p.value

##Formula = sum((observed - expected)^2 /expected)
expected <- outer(rowSums(data), colSums(data))/sum(data) 
chi_squared_statistic <-  sum(((data-expected)^2)/expected) 
chi_squared_statistic
pvalue = 1 - pchisq(q=chi_squared_statistic, df=4)


## Chi-seq test for Dependence By Airline
data = data.frame(cities, delayed, ontime, flight)
data$cities <- NULL
data = aggregate(list(delayed = data$delayed, ontime=data$ontime), by=list(flight=data$flight), FUN=sum)
rownames(data) = unique(data$flight)
data$flight <- NULL
Xsq <- chisq.test(data)
Xsq$statistic
Xsq$p.value

##Formula = sum((observed - expected)^2 /expected)
expected <- outer(rowSums(data), colSums(data))/sum(data) 
chi_squared_statistic <-  sum(((data-expected)^2)/expected) 
chi_squared_statistic
pvalue = 1 - pchisq(q=chi_squared_statistic, df=1)
pvalue


## Binomial Test for Alaska Airlines
data = filter(data, flight=="Alaska Airlines")
rownames(data) = unique(data$Cities)
data$flight <- NULL
data = aggregate(list(delayed = data$delayed, ontime=data$ontime), by=list(Cities=data$cities), FUN=sum)
rownames(data) = unique(data$Cities)
data$Cities <- NULL
datatable = as.data.frame(colSums(data))
x = datatable["delayed", ]
n = colSums(datatable)

binom.test(x=x, n=n, p = 0.14,conf.level = 0.95)

binom.test(x=x, n=n, p = 0.14, alternative = c("greater"),
           conf.level = 0.95)

binom.test(x=x, n=n, p = 0.14, alternative = c("less"),
           conf.level = 0.95)


## Binomial Test for America West
data = filter(data, flight=="America West")
rownames(data) = unique(data$Cities)
data$flight <- NULL
data = aggregate(list(delayed = data$delayed, ontime=data$ontime), by=list(Cities=data$cities), FUN=sum)
rownames(data) = unique(data$Cities)
data$Cities <- NULL
datatable = as.data.frame(colSums(data))

x = datatable["delayed", ] ## Number of Successes
n = colSums(datatable) ## Number of trials

binom.test(x=x, n=n, p = 0.14,conf.level = 0.95)

binom.test(x=x, n=n, p = 0.14, alternative = c("greater"),
           conf.level = 0.95)

binom.test(x=x, n=n, p = 0.14, alternative = c("less"),
           conf.level = 0.95)

