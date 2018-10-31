library(ggfortify)

## initialize dataset
data = as.data.frame(iris)
data = data[, -(5)]

#PCA using Covariance Matrix
pca_cov = prcomp(data, scale=FALSE)
summary(pca_cov)
pca_cov$rotation

#PCA using Correlation Matrix
pca_cor = prcomp(data, scale=TRUE)
summary(pca_cor)
pca_cor$rotation


# Convert Petals dataset to millimeters
data_mm = data
data_mm$Petal.Length <- data_mm$Petal.Length * 10

#PCA using Covariance Matrix
pca_cov_mm = prcomp(data_mm, scale=FALSE)
summary(pca_cov_mm)

#PCA using Correlation Matrix
pca_cor_mm = prcomp(data_mm, scale=TRUE)
summary(pca_cor_mm)

##Plot PCA
autoplot(pca_cov, data = iris, colour = 'Species', loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)


# Perform KMEANS ON 1-2PC
set.seed(47)
comp <- data.frame(pca_cov$x[,1:2])
k <- kmeans(comp, 3)

## Rename columns
# iris_sample = iris
# iris_sample$new_species <- NA
# 
# iris_sample <- iris_sample %>% mutate(new_species = replace(new_species, which(Species == "setosa"), 2))
# iris_sample <- iris_sample %>% mutate(new_species = replace(new_species, which(Species == "versicolor"), 1))
# iris_sample <- iris_sample %>% mutate(new_species = replace(new_species, which(Species == "virginica"), 3))
# 
# iris_sample$new_species <- as.integer(iris_sample$new_species)

#KMEANS ON entire dataset
set.seed(47)
k_all <- kmeans(data, 3)

##Prediction Error
table(k$cluster, iris$Species)
table(k_all$cluster, iris$Species)

