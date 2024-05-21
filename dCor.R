

dist_cov <- function(X,Y){
  n <- nrow(X)
  A_eu <- dist(X, method = "euclidean") |> as.matrix()
  B_eu <- dist(Y, method = "euclidean") |> as.matrix()
  a_mean <- mean(A_eu)
  a_mean_row <- rowMeans(A_eu)
  a_mean_col <- colMeans(A_eu)
  b_mean <- mean(B_eu)
  b_mean_row <- rowMeans(B_eu)
  b_mean_col <- colMeans(B_eu)
  A <- matrix(NA, nrow = n, ncol = n)
  B <- matrix(NA, nrow = n, ncol = n)
  for (k in 1:n) {
    for (j in 1:n) {
      A[k,j] <- A_eu[k,j]-a_mean_row[k]-a_mean_col[j]+a_mean
      B[k,j] <- B_eu[k,j]-b_mean_row[k]-b_mean_col[j]+b_mean
    }
  }
  return(mean(A*B))
}
