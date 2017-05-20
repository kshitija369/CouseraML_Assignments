
setwd("/Users/sdeshmukh1/Desktop/Summer2017/Coursera_ML/machine-learning-ex1/ex1/")
#read data labels
data_ = read.table(file = "ex1data1.txt", header = FALSE, sep = ",")

#seprate X and y labels
X = data_[,1]
y = data_[,2]
plot(X,y, xlab = "Population of City in 10,000s", ylab = "Profit in $10,000s", ylim = c(-5,25))

m = length(y)
theta_T = c(0,0)
dim(theta_T) <- c(1,2)
X = cbind(matrix(rep(1, times = length(y)), nrow = length(y), ncol = 1), X)
iterations = 1500;
alpha = 0.01;
J_history = c(rep(0, times = length(iterations)))

plotData <- function(X,y){
  plot(X, y ,xlab = 'Population of City in 10,000s', ylab = 'Profit in $10,000s')
}

computeCost <- function(X,y,theta){
  pred = theta %*% t(X)
  J = sum((pred - y)^2)
  J = J/(2*m)
  return(J)
}

gradientDescent <- function(X, y, theta, alpha, num_iters){
  m = length(y)
  for(iter in 1:num_iters){
    pred = theta %*% t(X)
    J_history[iter] <<- computeCost(X, y, theta)
    delta = (alpha * ((pred - y) %*% X))/m
    theta = theta - delta
  }
  return(theta)
}

#Program Code
theta_T <- gradientDescent(X,y,theta_T, alpha, iterations)

# print theta to screen
print(paste0('Theta1 found by gradient descent: ', theta_T[,1]))
print(paste0('Theta2 found by gradient descent: ', theta_T[,2]))
print(paste0('Expected theta values (approx)', ' -3.6303 : 1.1664'))

par(new = TRUE)
plot(X[,2],X %*% t(theta_T),col="green", xlab = "", ylab = "", ylim = c(-5,25))


