
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

dist_cor <- function(X,Y){
  VX <- dist_cov(X,X)
  VY <- dist_cov(Y,Y)
  if (VX*VY!=0) {
    VXY <- dist_cov(X,Y)
    RXY <- VXY/sqrt(VX*VY)
    return(sqrt(RXY))
  } else {
    0
  }
}


dCor_perm <- function(X,Y, # Our data
                      p=100 # Number of permutations
){
  n <- nrow(X)
  dCor_std <- dist_cor(X,Y)
  dCor_permed <- numeric((p-1))

  p_tilde <- (1+length(which(dCor_permed>dCor_std)))/(p)
  return(p_tilde)
}