#1. Do the following program in R 
#a).Simulate 5 variables from normal distribution with means 4, 2, 5, 3, 7 
#and variance 9,  4, 10, 3, 5 respectively of size 100.

set.seed(123)
n <- 100
means <- c(4, 2, 5, 3, 7)
vars  <- c(9, 4, 10, 3, 5)
sds   <- sqrt(vars)

X <- data.frame(
  X1 = rnorm(n, means[1], sds[1]),
  X2 = rnorm(n, means[2], sds[2]),
  X3 = rnorm(n, means[3], sds[3]),
  X4 = rnorm(n, means[4], sds[4]),
  X5 = rnorm(n, means[5], sds[5])
)

# (b)Manually calculate the mean vector and covariance matrix. 
mean_vec_manual <- colMeans(X)
X_centered <- scale(X, center = TRUE, scale = FALSE)
cov_manual <- (t(X_centered) %*% X_centered) / (n - 1)


# (c)Standardize the variables and find the mean vector and covariance matrix.
#Then find generalized variance and interpret. 
Z <- scale(X)  #standarize
mean_Z <- colMeans(Z)
cov_Z  <- cov(Z)
gen_var <- det(cov_Z)


# (d)Show that correlation is nothing but covariance of standardized variables. 
corr_direct <- cor(X)
corr_from_Z <- cov(Z)
all.equal(corr_direct, corr_from_Z)


# (e)Proof the spectral decomposition law using covariance matrix. 
Sigma <- cov(X)
eig <- eigen(Sigma)
P <- eig$vectors
D <- diag(eig$values)

Sigma_reconstructed <- P %*% D %*% t(P)
all.equal(Sigma, Sigma_reconstructed)





#2. Consider the data stated in question No 1 is multivariate normal 
#and perform the following operations. 
set.seed(123)
n <- 100
means <- c(4, 2, 5, 3, 7)
vars  <- c(9, 4, 10, 3, 5)
sds   <- sqrt(vars)
sim_data <- data.frame(
  X1 = rnorm(n, means[1], sds[1]),
  X2 = rnorm(n, means[2], sds[2]),
  X3 = rnorm(n, means[3], sds[3]),
  X4 = rnorm(n, means[4], sds[4]),
  X5 = rnorm(n, means[5], sds[5])
)
X= as.matrix(sim_data)
# Estimate mean and covariance from the data
mu_hat <- colMeans(X)
Sigma_hat  <- cov(X)
p <- ncol(X)

# a. Proof the linear combination property of multivariate normal distribution 
library(MASS)
# Example linear transformation: create Y = A X
A <- matrix(c(1,2, 0,1, 1,0, 2,1, 1,3), nrow=2, byrow=TRUE)
Y <- X %*% t(A)

# Theoretical mean and covariance of Y
mu_Y_theoretical <- A %*% mu_hat
Sigma_Y_theoretical <- A %*% Sigma_hat %*% t(A)
mu_Y_theoretical
Sigma_Y_theoretical


# b. Mean vector of the data is 𝜇 = [6,8,4,10,3]. Test the claim. 
library(ICSNP)
mu0 <- c(6, 8, 4, 10, 3)
HotellingsT2(sim_data, mu = mu0)
#p-value > 0.05 → mean vector is not significantly different from μ₀
#p-value < 0.05 → Reject the claim; sample mean differs from μ.

# c. Proof the linear combination property of multivariate normal distribution 



# d. Find Mahalanobis distances (At least two observation) and interpret for first two 
# Use only the first two variables
X12 <- sim_data[,1:2]
mu12 <- colMeans(X12)
Sigma12 <- cov(X12)
# Mahalanobis distance for first two observations
md <- mahalanobis(X12[1:2, ], mu12, Sigma12)
md




#3. Perform PCA manually and by package on iris data.
#How many PCs take into account? (use ratio and scree plot)

#  Prepare data: remove the Species (non-numeric) column
iris_num <- iris[, 1:4]
#(Optional but recommended) Standardize variables: center and scale
iris_std <- scale(iris_num)
#Compute covariance matrix
cov_mat <- cov(iris_std)

#Compute eigenvalues & eigenvectors
eig <- eigen(cov_mat)
# Extract eigenvalues (variances) and variance ratios
eigenvalues <- eig$values
variance_ratios <- eigenvalues / sum(eigenvalues)

eigenvalues
variance_ratios
cumsum(variance_ratios)   # cumulative variance explained

#Compute the principal component scores (projections)
pc_scores <- as.matrix(iris_std) %*% eig$vectors
head(pc_scores)  # first few observations projected onto PCs




#using packages
# Standard workflow
pca_res <- prcomp(iris[,1:4], center = TRUE, scale. = TRUE)
summary(pca_res)

# Extract variance explained
sdev <- pca_res$sdev
variance_ratios2 <- sdev^2 / sum(sdev^2)
variance_ratios2
cumsum(variance_ratios2)

# Scree plot (base R)
plot(variance_ratios2, type = "b",
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     main = "Scree Plot (Iris PCA)")


