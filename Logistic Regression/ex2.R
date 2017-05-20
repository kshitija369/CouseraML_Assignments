
setwd('/Users/kshitijap/Desktop/Summer17/machine-learning-ex2/ex2/')

data_ = read.table(file = 'ex2data1.txt', header = FALSE, sep = ",")
X = data_[, 1:2]
y = data_[, 3]
m = nrow(X)
n = ncol(X)
# Add intercept term to x and X_test
X = cbind(rep(1), X)
# Initialize fitting parameters
initial_theta = rep(0, n+1)
J=0

print("Plotting data")
plot(data_[,1], data_[,2], xlab = 'Exam 1 score', ylab = 'Exam 2 score', col = as.factor(y))
par(new = TRUE)
legend(78,100, c('Admitted', 'Not-Admitted'), pch = 1, col = c('black','red'))

sigmoid <- function(z){
  g = 1/(1 + exp(-z))
  return(g)
}

costFunction <- function(theta){
  term1 = -(y*log(sigmoid(theta %*% t(X))))
  term2 = -((1-y)*log(1 - sigmoid(theta %*% t(X))))
  J = sum(term1 + term2)/m
  return(J)
}

gradientDescent <- function(theta){
  for(i in 1:iterations){
    J <<- costFunction(theta)
    delta = as.matrix(sigmoid(theta %*% t(X)) - y) %*% as.matrix(X)/m
    print(delta)
    theta = theta - delta
  }
  return(theta)
}

predict <- function(theta, X){
  g = sigmoid((theta) %*%  t(X))
  g[ g >= 0.5] <- 1
  g[ g < 0.5] <- 0
  return(g)
}

iterations = 1
theta = gradientDescent(initial_theta)
print(J)
print('Expected cost (approx): 0.693')

# Compute and display cost and gradient with non-zero theta
test_theta = c(-24, 0.2, 0.2)
test_theta = gradientDescent(test_theta)
print(J)
print('Expected cost (approx): 0.218')
print('Expected gradients (approx):0.043  2.566  2.647')

# Derive theta using gradient descent using optim function
theta_optim <- optim(par=initial_theta,fn=costFunction)

#cost at optimal value of the theta
print(theta_optim$value)
#set theta
theta <- theta_optim$par
print(theta)

test_X = c(1, 45, 85)
g = sigmoid(t(theta) %*% test_X)
print(g)

p = predict(theta, X)

acc = mean((p == y)) * 100
print(paste('Train Accuracy: ', acc))
print('Expected accuracy (approx): 89.0')



