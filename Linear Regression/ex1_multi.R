
print('Loading data ...');
# Load Data
data_ = read.table(file = "ex1data2.txt", header = FALSE, sep = ",")
X = data_[,1:2]
y = data_[,3]
m = length(y)
theta_T = rep(0, times = ncol(X)+1)
dim(theta_T) <- c(1,ncol(X)+1)
iterations = 1500;
alpha = 0.01;

# Print out some data points
print(head(X))
print(head(y))

featureNormalize <- function(X){
  X_norm = X
  for( feat in 1:ncol(X)){
    mu = mean(X[,feat])
    sigma_ = sd(X[, feat])
    X_norm[,feat] = (X[,feat] - mu)/sigma_
  }
  return(X_norm)
}

normalEqn <- function(X, y, theta){
  library(MASS)
  theta = (ginv((X %*% t(X))) %*% X) * y
  return(theta)
}

#Program Code
#Scale features and set them to zero mean
print('Normalizing Features ...')
#X = featureNormalize(X)

#Add Bais x0
X = cbind(bais = rep(1, times = length(y)), X)
X = as.matrix(X)
#normal equation
theta_T <- normalEqn(X,y,theta_T)
