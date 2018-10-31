

## Initializing dataset and preprocessing
houses_data <- read.csv("data/House_prices_-_Exercise_7.csv")
houses_data_subset = houses_data[, c("price", "bedrooms", "bathrooms", "sqft_living", "floors", "view", "condition", "grade", "yr_built")]
houses_data_subset['log_price'] = log(houses_data_subset$price)
houses_data_subset['sqr_yr_built'] = houses_data_subset$yr_built**2
houses_data_subset['sqr_sqft_living'] = houses_data_subset$sqft_living**2
houses_data_subset['variable'] = expand.grid()


## Histogram and QQ plot for House Prices
par(mfrow=c(1,2))
hist(houses_data_subset$price, main="Histogram of House Prices", xlab="Prices")
qqnorm(houses_data_subset$price);
qqline(houses_data_subset$price, col = 2)

##Linear Model Using HOUSE Prices
fit <- lm(price ~ bedrooms + bathrooms + sqft_living + floors + view + condition + grade + yr_built , data=houses_data_subset)
summary(fit)

## Resdiaul Analysis
houses_data_subset$prices_predicted  = predict(fit)
par(mfrow=c(1,1))
plot(houses_data_subset$prices_predicted,fit$residuals ,pch=21,bg="red",col="red")
abline(0,0)


## Histogram and QQ plot for log (House Prices)
log_price = houses_data_subset$log_price
hist(log_price)
qqnorm(log_price)
qqline(log_price, col = 2)

##Linear Model Using log(HOUSE Prices)
fit1 <- lm(log_price ~ bedrooms + bathrooms + sqft_living + floors + view + condition + grade + yr_built , data=houses_data_subset)
summary(fit1)
#Residual Analysis
houses_data_subset$prices_predicted  = predict(fit1)
par(mfrow=c(1,1))
plot(houses_data_subset$prices_predicted,fit1$residuals ,pch=21,bg="red",col="red")
abline(0,0)


######Comparing covariates
## log(price) vs Covariates pLot
par(mfrow=c(2,2))
## Price vs. Sqft_living
plot(log_price~sqft_living, data=houses_data_subset, 
           main="Price vs. Sqft_living", xlab="Sqft_living", ylab="Price")

## Price vs. Bathrooms 
plot(log_price~bathrooms, data=houses_data_subset, 
           main="Price vs. Bathrooms", xlab="Bathrooms", ylab="Price")

## Price vs. Grade 
plot(log_price~grade, data=houses_data_subset, 
           main="Price vs. Grade", xlab="Grade", ylab="Price")

## Price vs. View 
plot(log_price~view, data=houses_data_subset, 
           main="Price vs. View", xlab="View", ylab="Price")

## Price vs. Bedrooms 
plot(log_price~bedrooms, data=houses_data_subset, 
           main="Price vs. Bedrooms", xlab="Bedrooms", ylab="Price")

## Price vs. Floor 
plot(log_price~floors, data=houses_data_subset, 
           main="Price vs. Floor", xlab="Floor", ylab="Price")

## Price vs. condition 
plot(log_price~condition, data=houses_data_subset, 
           main="Price vs. condition", xlab="condition", ylab="Price")


##Linear Model Using log(HOUSE Prices)
fit2 <- lm(log_price ~ bedrooms + bathrooms + sqft_living + floors + view + condition + grade + yr_built + sqr_yr_built + sqr_sqft_living, data=houses_data_subset)
summary(fit2)
#Residual Analysis
houses_data_subset$prices_predicted  = predict(fit2)
par(mfrow=c(1,1))
plot(houses_data_subset$prices_predicted,fit2$residuals ,pch=21,bg="red",col="red")
abline(0,0)


## Extending the model