
setwd('/Users/kshitijap/Desktop/Summer17/machine-learning-ex2/ex2/')

data_ = read.table(file = 'ex2data2.txt', header = FALSE, sep = ",")
X = data_[, 1:2]
y = data_[, 3]
m = nrow(X)
n = ncol(X)
# Add intercept term to x and X_test
X = cbind(rep(1, times = m), X)
# Initialize fitting parameters
initial_theta = rep(0, times = (n+1))
J=0

print("Plotting data")
plot(data_[,1], data_[,2], xlab = 'Microchip Test 1', ylab = 'Microchip Test 2', col = as.factor(y))
par(new = TRUE)
legend(0.78,1.00, c('y = 1', 'y = 0'), pch = 1, col = c('black','red'))

mapFeatures <- function(){
  X1 = X[,2]
  X2 = X[,3]
  
  degree = 6;
  out = rep(1, times = m)
  for( i in 2:degree){
    for( j in 1:i){
      term1 = (X2^(i - j))
      term2 = (X2^(j - 1))
      out =  cbind(out, term1 * term2)
    }
  }
  return(out)
}

sigmoid <- function(z){
  g = 1/(1 + exp(-z))
  return(g)
}

costFunctionReg <- function(theta){
  term1 = -(y*log(sigmoid(theta %*% t(X))))
  term2 = -((1-y)*log(1 - sigmoid(theta %*% t(X))))
  reg_term = sum((theta^2)[2:length(theta)])*lambda/(2*m)
  J = sum(term1 + term2)/m + reg_term
  return(J)
}

gradientDescentReg <- function(theta, X, y, lambda){
  for(i in 1:iterations){
    J <<- costFunction(theta)
    delta = as.matrix(sigmoid(theta %*% t(X)) - y) %*% as.matrix(X)/m
    delta = delta + lambda*sum(theta[2:length(theta)])/m
    theta = theta - delta
    print(delta)
  }
  return(theta)
}

predict <- function(theta, X){
  g = sigmoid((theta) %*%  t(X))
  g[ g >= 0.5] <- 1
  g[ g < 0.5] <- 0
  return(g)
}

# Add Polynomial Features
# Note that mapFeature also adds a column of ones for us, so the intercept
# term is handled
X = mapFeatures()

# Initialize fitting parameters
initial_theta = rep(0, times = ncol(X))

# Set regularization parameter lambda to 1
lambda = 1;

# Compute and display initial cost and gradient for regularized logistic
# regression
J = costFunctionReg(initial_theta)
print(J)

# Derive theta using gradient descent using optim function
theta_optim <- optim(par=initial_theta,fn=costFunctionReg)

#cost at optimal value of the theta
print(theta_optim$value)
#set theta
theta <- theta_optim$par

p = predict(theta, X)
acc = mean((p == y)) * 100
print(paste('Train Accuracy: ', acc))
