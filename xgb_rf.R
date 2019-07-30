############
#The problem here comes from the real application that there are high correlations between the different features
#Here we compare the random forest with xgboost in terms of the feature importance
#Note that here what we care about is what features are potentially important rather than
#how well we can perform prediction on the test set.


#Simulate a dataset to compare various classification models:
simul_X <- function(n, p, corr){
  p1 <- ceiling(p / 2)
  
  #Simulate features sequentially correlated:
  corr_seq <- seq(0, corr, corr/(p1 - 1))
  y <- rnorm(n, 0, 1)
  Y_label <- rnorm(n, 0, 1)
  X_baseline <- Y_label
  X_mat <- X_baseline
  #Simulate a vector with specific correlation: 
  for(i in 1:p1){
    X_curr <- corr_seq[i] * scale(X_baseline)[,1] + sqrt(1 - corr_seq[i] ^ 2) * scale(resid(lm(y ~ X_baseline)))[,1]
    X_mat <- cbind(X_mat, X_curr)
  }
  #Integrate some of the categorical variables:
  cate_prob_level <- sample(c(2:8), p - p1, replace = TRUE)
  for(i in 1:(p - p1)){
    n_level <- cate_prob_level[i]
    prob_seq <- sample(c(10:100), n_level, replace = FALSE)
    normalized_prob_seq <- prob_seq/sum(prob_seq)
    X_cate_curr <- as.factor(sample(LETTERS[1:n_level], n, replace = TRUE, prob = normalized_prob_seq))
    X_mat <- cbind(X_mat, X_cate_curr)
  }
  #Adjust for correlation between continuous and categorical features:
  X_mat <- X_mat[,2:ncol(X_mat)]
  col_seq <- c(paste(rep('Continuous', p1), c(1:p1),sep = ''),
               paste(rep('Categorical', p - p1), c(1:(p - p1)),sep = ''))
  colnames(X_mat) <- col_seq
  return(list(X_mat = X_mat, Y_label = Y_label))
}

simul_Y <- function(n, n_level){
  prob_seq <- sample(c(10:20), n_level, replace = FALSE)
  normalized_prob_seq <- prob_seq/ sum(prob_seq)
  Y_label <- as.factor(sample(LETTERS[1: n_level], n, replace = TRUE, prob = normalized_prob_seq))
  return(Y_label)
}

#


n <- 10000
p <- 100
ist0 <- simul_X(n, p, 0.8)
X_matrix <- ist0$X_mat
Y_label <- ist0$Y_label
train_index <- sample(n, 0.8 *n, replace = FALSE)
test_index <- c(1:n)[-train_index]
data0 <- data.frame(cbind(X_matrix, label = Y_label))
train_df <- data0[train_index,]
test_df <- data0[test_index,]

#Fit and visualize a random forest:
train_rf <- randomForest(label ~. ,data = train_df, importance = TRUE,
                         do.trace = 100, ntree = 300)
#Here we reach a perfect fitting.
#Sort the importance of each continuous features:
order(importance(train_rf)[1:50])


#Fit and visualize random forest:
library(xgboost)
d_train <- xgb.DMatrix(data = matrix(), label = )
order(importance(train_xgb)[1:50])
#We observe that randomforest do better in capturing the high correlated features

#Visualize the feature importance:








