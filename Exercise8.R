library(caret)

##Initialize dataset and pre-processing
data = read.csv('data/transfusion.data', header = TRUE, na.strings=c("","NA"))
names(data) <- c("recency", "frequency", "amount", "time", "donation")

##GLM model with frequency as covariates
freq_model<-glm(donation~frequency,data = data, family=binomial)
summary(freq_model)

##GLM model with amount as covariates
amt_model <- glm(donation~amount, data=data, family = binomial)
summary(amt_model)

plot(data$amount, data$frequency, main = "Frequency vs Amount", xlab = "Anount", ylab = "Frequency")

#A likelihood ratio test can be used to compare these nested models
anova(freq_model , amt_model , test="Chisq")


rec_model_bn <- glm(donation~recency, data=data, family = binomial)
summary(rec_model_bn)

rec_model_gau <- glm(donation~recency, data=data, family = gaussian)
summary(rec_model_gau)

rec_model_gam <- glm(donation~recency, data=data, family = Gamma)
summary(rec_model_gam)

rec_model_ig <- glm(donation~recency, data=data, family = inverse.gaussian)
summary(rec_model_ig)

rec_model_pos <- glm(donation~recency, data=data, family = poisson)
summary(rec_model_pos)

rec_model_quasi <- glm(donation~recency, data=data, family = quasi)
summary(rec_model_quasi)

rec_model_quasib <- glm(donation~recency, data=data, family = quasibinomial)
summary(rec_model_quasib)

rec_model_quasip <- glm(donation~recency, data=data, family = quasipoisson)
summary(rec_model_quasip)


## Plot of donation vs Covariates
par(mfrow=c(3,3))
plot(data$donation, data$amount, main = "Donation vs Amount", xlab = "Donation", ylab = "Anount")
plot(data$donation, data$frequency, main = "Donation vs Frequency", xlab = "Donation", ylab = "Frequency")
plot(data$donation, data$recency, main = "Donation vs Recency", xlab = "Donation", ylab = "Recency")
plot(data$donation, data$indices, main = "Donation vs Indices", xlab = "Donation", ylab = "Indices")
plot(data$donation, data$time, main = "Donation vs Time", xlab = "Donation", ylab = "Time")



#prediction
set.seed(1122)
train_ind <- sample(seq_len(nrow(data)), size = 374)
train <- data[train_ind, ]
test <- data[-train_ind, ]


amt_model_train <- glm(donation~amount, data=train, family = binomial)
summary(amt_model_train)


amount = data.frame(amount = test$amount)
pred_prob = cbind(amount, predicted = predict(amt_model_train, type="response", newdata=amount))
pred_prob$donation <- NA
pred_prob$donation_or = test$donation
pred_prob['donation'] = ifelse(pred_prob$predicted < 0.5,0, 1)


#Classification Error
sum(abs(pred_prob$donation_or - pred_prob$donation))/nrow(test)


#prediction improving the model
set.seed(1122)
train_ind <- sample(seq_len(nrow(data)), size = 374)
train <- data[train_ind, ]
test <- data[-train_ind, ]

rec_model_train <- glm(donation~recency , data=train, family = binomial)
summary(rec_model_train)

recency = data.frame(recency = test$recency)
pred_prob = cbind(recency, predicted = predict(rec_model_train, type="response", newdata=recency))
pred_prob$donation <- NA
pred_prob$donation_or = test$donation
pred_prob['donation'] = ifelse(pred_prob$predicted < 0.5,0, 1)


#Classification Error
sum(abs(pred_prob$donation_or - pred_prob$donation) /nrow(test))
