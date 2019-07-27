#Develop a habit writing R to simulate and understand the interesting statistical phenomena in real world application!

#Simulation for high-dim ridge regression and lasso:
library(glmnet)
library(dvmisc)
#Demonstrate that ridge regression and lasso can actually prevent overfitting in high dimensional setting:
high_dim_simulation1 <- function(N, p, noise){
  target <- runif(N, -1, 1)
  training_data <- matrix(nrow = N, ncol = p)
  for(i in 1:p){
    training_data[, i] <- target + rnorm(N, 0, noise)
  }
  png(filename = paste('Simulation', N, 'Sample', p, 'Feature.png', sep = ''), width = 800, height = 580)
  plot(cv.glmnet(training_data, target, alpha = 0, lambda = exp(seq(-5, 3, by = 0.1))))
  dev.off()
  #Evaluate the baseline leave-one-out cv error for OLS:
  mse <- rep(0, N)
  for(i in 1:N){
    targ <- target[-i]
    trai <- training_data[-i, ]
    model_current <- lm(targ ~ trai)
    pred <- predict(model_current, data = training_data[i,])
    mse[i] <- mean((pred - training_data[i,])^2)
  }
  return(mean(mse))
}
high_dim_simulation1(100, 200, 5)
high_dim_simulation1(100, 500, 5)
high_dim_simulation1(100, 1000, 5)
high_dim_simulation1(100, 2000, 5)
high_dim_simulation1(100, 20, 5)
high_dim_simulation1(100, 15, 5)
high_dim_simulation1(100, 50, 5)
#For high dimensional setting, small lambda yields flat mse.
N <- 200
p <- 400
noise <- 6
target <- runif(N, -1, 1)
training_data <- matrix(nrow = N, ncol = p)
for(i in 1:p){
  training_data[, i] <- target + rnorm(N, 0, noise / 2)
}
plot(cv.glmnet(training_data, target, alpha = 0, lambda = exp(seq(-10, 2, by = 0.2))))
#Flat on the left.


#







