data1 <- read.csv("dataI.csv", header=T)
data2 <- read.csv("dataII.csv", header=T)
data3 <- read.csv("dataIII.csv", header=T)
data4 <- read.csv("dataIV.csv", header=T)
data5 <- read.csv("dataV.csv", header=T)
irisData <- read.csv("iris.csv", header=T)

iris_means <- apply(irisData, 2, function(x) { return(mean(x)) })

#Translate the dataset by the iris means
data1_translated_iris <- t(t(data1) - iris_means)
#Compute covariance matrix and associated eigenvaluese and eigenvectors
covmat_data1_iris <- cov(data1_translated_iris)
ev_data1_iris <- eigen(covmat_data1_iris)
#After this transformation, each feature will have a mean of 0, and the covariance of this will be a diagonal matrix with the eigenvalues
eigen_transform_data1_iris <- apply(data1_translated_iris, 1, function(x) {return(t(ev_data1_iris$vectors) %*% x)})
#Reconstruct the data set with the varying number of principal components.
reconstructed_data1_iris_4_pc <- t(ev_data1_iris$vectors %*% eigen_transform_data1_iris + iris_means)
eigen_transform_data1_iris[4,] <- 0
reconstructed_data1_iris_3_pc <- t(ev_data1_iris$vectors %*% eigen_transform_data1_iris + iris_means)
eigen_transform_data1_iris[3,] <- 0
reconstructed_data1_iris_2_pc <- t(ev_data1_iris$vectors %*% eigen_transform_data1_iris + iris_means)
eigen_transform_data1_iris[2,] <- 0
reconstructed_data1_iris_1_pc <- t(ev_data1_iris$vectors %*% eigen_transform_data1_iris + iris_means)
eigen_transform_data1_iris[1,] <- 0
reconstructed_data1_iris_0_pc <- t(ev_data1_iris$vectors %*% eigen_transform_data1_iris + iris_means)


data1_means <- apply(data1, 2, function(x) { return(mean(x)) })
#Translate the dataset by the self means
data1_translated_self <- t(t(data1) - data1_means)
#Compute covariance matrix and associated eigenvaluese and eigenvectors
covmat_data1_self <- cov(data1_translated_self)
ev_data1_self <- eigen(covmat_data1_self)
#After this transformation, each feature will have a mean of 0, and the covariance of this will be a diagonal matrix with the eigenvalues
eigen_transform_data1_self <- apply(data1_translated_self, 1, function(x) {return(t(ev_data1_self$vectors) %*% x)})
#Reconstruct the data set with the varying number of principal components.
reconstructed_data1_self_4_pc <- t(ev_data1_self$vectors %*% eigen_transform_data1_self + data1_means)
eigen_transform_data1_self[4,] <- 0
reconstructed_data1_self_3_pc <- t(ev_data1_self$vectors %*% eigen_transform_data1_self + data1_means)
eigen_transform_data1_self[3,] <- 0
reconstructed_data1_self_2_pc <- t(ev_data1_self$vectors %*% eigen_transform_data1_self + data1_means)
eigen_transform_data1_self[2,] <- 0
reconstructed_data1_self_1_pc <- t(ev_data1_self$vectors %*% eigen_transform_data1_self + data1_means)
eigen_transform_data1_self[1,] <- 0
reconstructed_data1_self_0_pc <- t(ev_data1_self$vectors %*% eigen_transform_data1_self + data1_means)

#Translate the dataset by the iris means
data2_translated_iris <- t(t(data2) - iris_means)
#Compute covariance matrix and associated eigenvaluese and eigenvectors
covmat_data2_iris <- cov(data2_translated_iris)
ev_data2_iris <- eigen(covmat_data2_iris)
#After this transformation, each feature will have a mean of 0, and the covariance of this will be a diagonal matrix with the eigenvalues
eigen_transform_data2_iris <- apply(data2_translated_iris, 1, function(x) {return(t(ev_data2_iris$vectors) %*% x)})
#Reconstruct the data set with the varying number of principal components.
reconstructed_data2_iris_4_pc <- t(ev_data2_iris$vectors %*% eigen_transform_data2_iris + iris_means)
eigen_transform_data2_iris[4,] <- 0
reconstructed_data2_iris_3_pc <- t(ev_data2_iris$vectors %*% eigen_transform_data2_iris + iris_means)
eigen_transform_data2_iris[3,] <- 0
reconstructed_data2_iris_2_pc <- t(ev_data2_iris$vectors %*% eigen_transform_data2_iris + iris_means)
eigen_transform_data2_iris[2,] <- 0
reconstructed_data2_iris_1_pc <- t(ev_data2_iris$vectors %*% eigen_transform_data2_iris + iris_means)
eigen_transform_data2_iris[1,] <- 0
reconstructed_data2_iris_0_pc <- t(ev_data2_iris$vectors %*% eigen_transform_data2_iris + iris_means)


data2_means <- apply(data2, 2, function(x) { return(mean(x)) })
#Translate the dataset by the self means
data2_translated_self <- t(t(data2) - data2_means)
#Compute covariance matrix and associated eigenvaluese and eigenvectors
covmat_data2_self <- cov(data2_translated_self)
ev_data2_self <- eigen(covmat_data2_self)
#After this transformation, each feature will have a mean of 0, and the covariance of this will be a diagonal matrix with the eigenvalues
eigen_transform_data2_self <- apply(data2_translated_self, 1, function(x) {return(t(ev_data2_self$vectors) %*% x)})
#Reconstruct the data set with the varying number of principal components.
reconstructed_data2_self_4_pc <- t(ev_data2_self$vectors %*% eigen_transform_data2_self + data2_means)
eigen_transform_data2_self[4,] <- 0
reconstructed_data2_self_3_pc <- t(ev_data2_self$vectors %*% eigen_transform_data2_self + data2_means)
eigen_transform_data2_self[3,] <- 0
reconstructed_data2_self_2_pc <- t(ev_data2_self$vectors %*% eigen_transform_data2_self + data2_means)
eigen_transform_data2_self[2,] <- 0
reconstructed_data2_self_1_pc <- t(ev_data2_self$vectors %*% eigen_transform_data2_self + data2_means)
eigen_transform_data2_self[1,] <- 0
reconstructed_data2_self_0_pc <- t(ev_data2_self$vectors %*% eigen_transform_data2_self + data2_means)

#Translate the dataset by the iris means
data3_translated_iris <- t(t(data3) - iris_means)
#Compute covariance matrix and associated eigenvaluese and eigenvectors
covmat_data3_iris <- cov(data3_translated_iris)
ev_data3_iris <- eigen(covmat_data3_iris)
#After this transformation, each feature will have a mean of 0, and the covariance of this will be a diagonal matrix with the eigenvalues
eigen_transform_data3_iris <- apply(data3_translated_iris, 1, function(x) {return(t(ev_data3_iris$vectors) %*% x)})
#Reconstruct the data set with the varying number of principal components.
reconstructed_data3_iris_4_pc <- t(ev_data3_iris$vectors %*% eigen_transform_data3_iris + iris_means)
eigen_transform_data3_iris[4,] <- 0
reconstructed_data3_iris_3_pc <- t(ev_data3_iris$vectors %*% eigen_transform_data3_iris + iris_means)
eigen_transform_data3_iris[3,] <- 0
reconstructed_data3_iris_2_pc <- t(ev_data3_iris$vectors %*% eigen_transform_data3_iris + iris_means)
eigen_transform_data3_iris[2,] <- 0
reconstructed_data3_iris_1_pc <- t(ev_data3_iris$vectors %*% eigen_transform_data3_iris + iris_means)
eigen_transform_data3_iris[1,] <- 0
reconstructed_data3_iris_0_pc <- t(ev_data3_iris$vectors %*% eigen_transform_data3_iris + iris_means)


data3_means <- apply(data3, 2, function(x) { return(mean(x)) })
#Translate the dataset by the self means
data3_translated_self <- t(t(data3) - data3_means)
#Compute covariance matrix and associated eigenvaluese and eigenvectors
covmat_data3_self <- cov(data3_translated_self)
ev_data3_self <- eigen(covmat_data3_self)
#After this transformation, each feature will have a mean of 0, and the covariance of this will be a diagonal matrix with the eigenvalues
eigen_transform_data3_self <- apply(data3_translated_self, 1, function(x) {return(t(ev_data3_self$vectors) %*% x)})
#Reconstruct the data set with the varying number of principal components.
reconstructed_data3_self_4_pc <- t(ev_data3_self$vectors %*% eigen_transform_data3_self + data3_means)
eigen_transform_data3_self[4,] <- 0
reconstructed_data3_self_3_pc <- t(ev_data3_self$vectors %*% eigen_transform_data3_self + data3_means)
eigen_transform_data3_self[3,] <- 0
reconstructed_data3_self_2_pc <- t(ev_data3_self$vectors %*% eigen_transform_data3_self + data3_means)
eigen_transform_data3_self[2,] <- 0
reconstructed_data3_self_1_pc <- t(ev_data3_self$vectors %*% eigen_transform_data3_self + data3_means)
eigen_transform_data3_self[1,] <- 0
reconstructed_data3_self_0_pc <- t(ev_data3_self$vectors %*% eigen_transform_data3_self + data3_means)

#Translate the dataset by the iris means
data4_translated_iris <- t(t(data4) - iris_means)
#Compute covariance matrix and associated eigenvaluese and eigenvectors
covmat_data4_iris <- cov(data4_translated_iris)
ev_data4_iris <- eigen(covmat_data4_iris)
#After this transformation, each feature will have a mean of 0, and the covariance of this will be a diagonal matrix with the eigenvalues
eigen_transform_data4_iris <- apply(data4_translated_iris, 1, function(x) {return(t(ev_data4_iris$vectors) %*% x)})
#Reconstruct the data set with the varying number of principal components.
reconstructed_data4_iris_4_pc <- t(ev_data4_iris$vectors %*% eigen_transform_data4_iris + iris_means)
eigen_transform_data4_iris[4,] <- 0
reconstructed_data4_iris_3_pc <- t(ev_data4_iris$vectors %*% eigen_transform_data4_iris + iris_means)
eigen_transform_data4_iris[3,] <- 0
reconstructed_data4_iris_2_pc <- t(ev_data4_iris$vectors %*% eigen_transform_data4_iris + iris_means)
eigen_transform_data4_iris[2,] <- 0
reconstructed_data4_iris_1_pc <- t(ev_data4_iris$vectors %*% eigen_transform_data4_iris + iris_means)
eigen_transform_data4_iris[1,] <- 0
reconstructed_data4_iris_0_pc <- t(ev_data4_iris$vectors %*% eigen_transform_data4_iris + iris_means)


data4_means <- apply(data4, 2, function(x) { return(mean(x)) })
#Translate the dataset by the self means
data4_translated_self <- t(t(data4) - data4_means)
#Compute covariance matrix and associated eigenvaluese and eigenvectors
covmat_data4_self <- cov(data4_translated_self)
ev_data4_self <- eigen(covmat_data4_self)
#After this transformation, each feature will have a mean of 0, and the covariance of this will be a diagonal matrix with the eigenvalues
eigen_transform_data4_self <- apply(data4_translated_self, 1, function(x) {return(t(ev_data4_self$vectors) %*% x)})
#Reconstruct the data set with the varying number of principal components.
reconstructed_data4_self_4_pc <- t(ev_data4_self$vectors %*% eigen_transform_data4_self + data4_means)
eigen_transform_data4_self[4,] <- 0
reconstructed_data4_self_3_pc <- t(ev_data4_self$vectors %*% eigen_transform_data4_self + data4_means)
eigen_transform_data4_self[3,] <- 0
reconstructed_data4_self_2_pc <- t(ev_data4_self$vectors %*% eigen_transform_data4_self + data4_means)
eigen_transform_data4_self[2,] <- 0
reconstructed_data4_self_1_pc <- t(ev_data4_self$vectors %*% eigen_transform_data4_self + data4_means)
eigen_transform_data4_self[1,] <- 0
reconstructed_data4_self_0_pc <- t(ev_data4_self$vectors %*% eigen_transform_data4_self + data4_means)

#Translate the dataset by the iris means
data5_translated_iris <- t(t(data5) - iris_means)
#Compute covariance matrix and associated eigenvaluese and eigenvectors
covmat_data5_iris <- cov(data5_translated_iris)
ev_data5_iris <- eigen(covmat_data5_iris)
#After this transformation, each feature will have a mean of 0, and the covariance of this will be a diagonal matrix with the eigenvalues
eigen_transform_data5_iris <- apply(data5_translated_iris, 1, function(x) {return(t(ev_data5_iris$vectors) %*% x)})
#Reconstruct the data set with the varying number of principal components.
reconstructed_data5_iris_4_pc <- t(ev_data5_iris$vectors %*% eigen_transform_data5_iris + iris_means)
eigen_transform_data5_iris[4,] <- 0
reconstructed_data5_iris_3_pc <- t(ev_data5_iris$vectors %*% eigen_transform_data5_iris + iris_means)
eigen_transform_data5_iris[3,] <- 0
reconstructed_data5_iris_2_pc <- t(ev_data5_iris$vectors %*% eigen_transform_data5_iris + iris_means)
eigen_transform_data5_iris[2,] <- 0
reconstructed_data5_iris_1_pc <- t(ev_data5_iris$vectors %*% eigen_transform_data5_iris + iris_means)
eigen_transform_data5_iris[1,] <- 0
reconstructed_data5_iris_0_pc <- t(ev_data5_iris$vectors %*% eigen_transform_data5_iris + iris_means)


data5_means <- apply(data5, 2, function(x) { return(mean(x)) })
#Translate the dataset by the self means
data5_translated_self <- t(t(data5) - data5_means)
#Compute covariance matrix and associated eigenvaluese and eigenvectors
covmat_data5_self <- cov(data5_translated_self)
ev_data5_self <- eigen(covmat_data5_self)
#After this transformation, each feature will have a mean of 0, and the covariance of this will be a diagonal matrix with the eigenvalues
eigen_transform_data5_self <- apply(data5_translated_self, 1, function(x) {return(t(ev_data5_self$vectors) %*% x)})
#Reconstruct the data set with the varying number of principal components.
reconstructed_data5_self_4_pc <- t(ev_data5_self$vectors %*% eigen_transform_data5_self + data5_means)
eigen_transform_data5_self[4,] <- 0
reconstructed_data5_self_3_pc <- t(ev_data5_self$vectors %*% eigen_transform_data5_self + data5_means)
eigen_transform_data5_self[3,] <- 0
reconstructed_data5_self_2_pc <- t(ev_data5_self$vectors %*% eigen_transform_data5_self + data5_means)
eigen_transform_data5_self[2,] <- 0
reconstructed_data5_self_1_pc <- t(ev_data5_self$vectors %*% eigen_transform_data5_self + data5_means)
eigen_transform_data5_self[1,] <- 0
reconstructed_data5_self_0_pc <- t(ev_data5_self$vectors %*% eigen_transform_data5_self + data5_means)

#############CALCULATING THE MEAN SQUARE ERROR################
find_mean_square_error <- function(df1, df2) {
  if ((dim(df1)[1] != dim(df2)[1]) || (dim(df1)[2] != dim(df2)[2])) {
    print("The data frames must have the same dimesions")
    return(-1)
  }
  errors <- apply(df1 - df2, 1, function(x) {return(sum(x^2))})
  #squared_errors <- unlist(lapply(errors, function(x) {return(x^2)}))
  mean_squared_error <- sum(errors)/nrow(df1)
  return(mean_squared_error)
}



data1_0N <- find_mean_square_error(irisData, reconstructed_data1_iris_0_pc)
data1_1N <- find_mean_square_error(irisData, reconstructed_data1_iris_1_pc)
data1_2N <- find_mean_square_error(irisData, reconstructed_data1_iris_2_pc)
data1_3N <- find_mean_square_error(irisData, reconstructed_data1_iris_3_pc)
data1_4N <- find_mean_square_error(irisData, reconstructed_data1_iris_4_pc)
data1_0C <- find_mean_square_error(irisData, reconstructed_data1_self_0_pc)
data1_1C <- find_mean_square_error(irisData, reconstructed_data1_self_1_pc)
data1_2C <- find_mean_square_error(irisData, reconstructed_data1_self_2_pc)
data1_3C <- find_mean_square_error(irisData, reconstructed_data1_self_3_pc)
data1_4C <- find_mean_square_error(irisData, reconstructed_data1_self_4_pc)

data2_0N <- find_mean_square_error(irisData, reconstructed_data2_iris_0_pc)
data2_1N <- find_mean_square_error(irisData, reconstructed_data2_iris_1_pc)
data2_2N <- find_mean_square_error(irisData, reconstructed_data2_iris_2_pc)
data2_3N <- find_mean_square_error(irisData, reconstructed_data2_iris_3_pc)
data2_4N <- find_mean_square_error(irisData, reconstructed_data2_iris_4_pc)
data2_0C <- find_mean_square_error(irisData, reconstructed_data2_self_0_pc)
data2_1C <- find_mean_square_error(irisData, reconstructed_data2_self_1_pc)
data2_2C <- find_mean_square_error(irisData, reconstructed_data2_self_2_pc)
data2_3C <- find_mean_square_error(irisData, reconstructed_data2_self_3_pc)
data2_4C <- find_mean_square_error(irisData, reconstructed_data2_self_4_pc)

data3_0N <- find_mean_square_error(irisData, reconstructed_data3_iris_0_pc)
data3_1N <- find_mean_square_error(irisData, reconstructed_data3_iris_1_pc)
data3_2N <- find_mean_square_error(irisData, reconstructed_data3_iris_2_pc)
data3_3N <- find_mean_square_error(irisData, reconstructed_data3_iris_3_pc)
data3_4N <- find_mean_square_error(irisData, reconstructed_data3_iris_4_pc)
data3_0C <- find_mean_square_error(irisData, reconstructed_data3_self_0_pc)
data3_1C <- find_mean_square_error(irisData, reconstructed_data3_self_1_pc)
data3_2C <- find_mean_square_error(irisData, reconstructed_data3_self_2_pc)
data3_3C <- find_mean_square_error(irisData, reconstructed_data3_self_3_pc)
data3_4C <- find_mean_square_error(irisData, reconstructed_data3_self_4_pc)

data4_0N <- find_mean_square_error(irisData, reconstructed_data4_iris_0_pc)
data4_1N <- find_mean_square_error(irisData, reconstructed_data4_iris_1_pc)
data4_2N <- find_mean_square_error(irisData, reconstructed_data4_iris_2_pc)
data4_3N <- find_mean_square_error(irisData, reconstructed_data4_iris_3_pc)
data4_4N <- find_mean_square_error(irisData, reconstructed_data4_iris_4_pc)
data4_0C <- find_mean_square_error(irisData, reconstructed_data4_self_0_pc)
data4_1C <- find_mean_square_error(irisData, reconstructed_data4_self_1_pc)
data4_2C <- find_mean_square_error(irisData, reconstructed_data4_self_2_pc)
data4_3C <- find_mean_square_error(irisData, reconstructed_data4_self_3_pc)
data4_4C <- find_mean_square_error(irisData, reconstructed_data4_self_4_pc)

data5_0N <- find_mean_square_error(irisData, reconstructed_data5_iris_0_pc)
data5_1N <- find_mean_square_error(irisData, reconstructed_data5_iris_1_pc)
data5_2N <- find_mean_square_error(irisData, reconstructed_data5_iris_2_pc)
data5_3N <- find_mean_square_error(irisData, reconstructed_data5_iris_3_pc)
data5_4N <- find_mean_square_error(irisData, reconstructed_data5_iris_4_pc)
data5_0C <- find_mean_square_error(irisData, reconstructed_data5_self_0_pc)
data5_1C <- find_mean_square_error(irisData, reconstructed_data5_self_1_pc)
data5_2C <- find_mean_square_error(irisData, reconstructed_data5_self_2_pc)
data5_3C <- find_mean_square_error(irisData, reconstructed_data5_self_3_pc)
data5_4C <- find_mean_square_error(irisData, reconstructed_data5_self_4_pc)

final_df <- data.frame(c(data1_0N, data2_0N, data3_0N, data4_0N, data5_0N),
                       c(data1_1N, data2_1N, data3_1N, data4_1N, data5_1N),
                       c(data1_2N, data2_2N, data3_2N, data4_2N, data5_2N),
                       c(data1_3N, data2_3N, data3_3N, data4_3N, data5_3N),
                       c(data1_4N, data2_4N, data3_4N, data4_4N, data5_4N),
                       c(data1_0C, data2_0C, data3_0C, data4_0C, data5_0C),
                       c(data1_1C, data2_1C, data3_1C, data4_1C, data5_1C),
                       c(data1_2C, data2_2C, data3_2C, data4_2C, data5_2C),
                       c(data1_3C, data2_3C, data3_3C, data4_3C, data5_3C),
                       c(data1_4C, data2_4C, data3_4C, data4_4C, data5_4C))
names(final_df) <- c("0N", "1N", "2N", "3N", "4N", "0C", "1C", "2C", "3C", "4C")
write.csv(final_df, "mzahmed2-numbers.csv")

final_reconstructed_data1_self_2_pc <- as.data.frame(reconstructed_data1_self_2_pc)
names(final_reconstructed_data1_self_2_pc) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")
write.csv(final_reconstructed_data1_self_2_pc, "mzahmed2-recon.csv")
