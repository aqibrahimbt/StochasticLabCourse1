set.seed(12547)
library(rmutil)
library("gridExtra")
library(ggplot2)
library(qqplotr)


#Function to compare the different types of quantiles
quantAll <- function(x, prob, ...)
  t(vapply(1:9, function(typ) quantile(x,0.5, type = typ, ...), quantile(x, 0.5, type=1)))

# sample n=20
data_20 = rlaplace(20, m=1, s=1)
quantile(x=data_20, 0.5)
signif(quantAll(data_20), 4)


# sample n=1000
data_1000 = rlaplace(1000, m=1, s=1)
quantile(x=data_1000, 0.5)
signif(quantAll(data_1000),4)


### Generate n = c(20,1000) independent variables with mu = 1 and sigma = 1.
data_20<-rlaplace(20, 1, 1)
data_1000<-rlaplace(1000, 1, 1)


## Function to compute estimates for laplace sample
MLEstimator <- function(a, x){
  -sum(dlaplace(x, m = a, s = 1, log = T))
}


### Calculate the maximum likelihood estimators based on both samples with n=20, mu=1 and sd =1
optimize(MLEstimator, c(data_20), interval = range(data_20))
quantile(x=data_20, 0.5)


### Calculate the maximum likelihood estimators based on both samples with n=1000, mu=1 and sd =1
optimize(MLEstimator, c(data_1000), interval = range(data_1000))
quantile(x=data_1000, 0.5)


####### Calculate Generate estimators = 5000 for 20 independent variables with mu = 1 and sigma = 1
M = 5000
values = setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("iter", "est"))
for (i in 1:M){
  data_20 = rlaplace(20, m=1, s=1)
  a <- optimize(MLEstimator, c(data_20), interval = range(data_20))
  values[i, 1] <- i
  values[i, 2] <- a
}

g1=ggplot(values, aes(est))+
  geom_histogram(aes(y = ..density..), color = 'white')+
  stat_function(fun = dnorm, args = list(mean = mean(values$est), sd = sd(values$est)), color = 'darkred')+
  ggtitle("Histogram: n = 20")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


g2=ggplot(data = values, mapping = aes(sample = est)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

grid.arrange(g1, g2, nrow = 1)

mean_values <- mean(values$est) #Mean 
sd_values = sd(values$est)

####### Calculate Generate estimators = 5000 for 1000 independent variables with mu = 1 and sigma = 1
M = 5000
values = setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("iter", "est"))
for (i in 1:M){
  data_1000 = rlaplace(1000, m=1, s=1)
  a <- optimize(MLEstimator, c(data_1000), interval = range(data_1000))
  values[i, 1] <- i
  values[i, 2] <- a
}

g1= ggplot(values, aes(est))+
  geom_histogram(aes(y = ..density..), color = 'white')+
  stat_function(fun = dnorm, args = list(mean = mean(values$est), sd = sd(values$est)), color = 'darkred')+
  ggtitle("Histogram: n = 1000")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

g2= ggplot(data = values, mapping = aes(sample = est)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

grid.arrange(g1, g2, nrow = 1)

#Mean and sd
mean_values <- mean(values$est)
sd_values = sd(values$est)


