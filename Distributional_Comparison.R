######
#Simulation for comparing two distributions:
#Here we compare the bootstrapped kolmogorov-smirnov test 
#and the Mann-Whitney U test with empirical p-value.

n <- 1000
p <- 500
n_boot <- 300

#Weibull distribution:
X1 <- rweibull(n, 2, 4)
X2 <- rweibull(n, 2, 4.5)

#Quantify the uncertainty of the p-value for different distributional comparison tests:
x1_series <- seq(1, 5, 0.5)
x2_series <- seq(1, 5, 0.5)
p_val_ks <- matrix(0, 9, 9)
p_val_mw <- matrix(0, 9, 9)
for(j in 1:9){
  for(k in 1:9){
    p_val_seq <- rep(0, n_boot)
    p_val_seq_pt <- rep(0, n_boot)
    X1 <- rweibull(n, x1_series[j], 1)
    X2 <- rweibull(n, x2_series[k], 1)
    for(i in 1:n_boot){
      X1_bot <- sample(X1, n, replace = TRUE)
      X2_bot <- sample(X2, n, replace = TRUE)
      X1_pt <- sample(X1, n, replace = FALSE)
      X2_pt <- sample(X2, n, replace = FALSE)
      p_val_seq_pt[i] <- wilcox.test(X1_bot, X2_bot)$p.value
      p_val_seq[i] <- ks.test(X1_bot, X2_bot)$p.value
    }
    p_val_ks[j, k] <- quantile(p_val_seq, 0.95)
    p_val_mw[j, k] <- quantile(p_val_seq_pt, 0.95)
  }
}
#Visualize the heatmap:
par(mfrow = c(1,2))
heatmap(p_val_ks, Rowv = NA, Colv = NA, xlab = 'Shape_dist1',
        ylab = 'Shape_dist2')
heatmap(p_val_pt, Rowv = NA, Colv = NA, xlab = 'Shape_dist1',
        ylab = 'Shape_dist2')
