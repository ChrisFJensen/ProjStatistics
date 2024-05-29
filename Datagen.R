M1_dat_gen <- function(n,A){
  theta <- runif(n,0,2*pi)
  epsilon <- matrix(rnorm(n),nrow = 2)
  X_1 <- A*cos(theta)+epsilon[1,]/4
  Y_1 <- A*sin(theta)+epsilon[2,]/4
  X_2 <- runif(n)
  Y_2 <- runif(n)
  data <- data.frame(cbind(X_1,X_2),cbind(Y_1,Y_2))
}

M2_dat_gen <- function(n,rho){
  X_1 <- runif(n,-1,1)
  epsilon <- rnorm(n)
  Y_1 <- abs(X_1)^rho*epsilon
  X_2 <- runif(n)
  Y_2 <- runif(n)
  data <- data.frame(cbind(X_1,X_2),cbind(Y_1,Y_2))
}

M3_dat_gen <- function(n,a){
  X_1 <- data.frame("X"=NA)[-1,]
  Y_1 <- data.frame("Y"=NA)[-1,]
  for (i in 1:n) {
    j <- 0
    while (j < 1) {
      u <- runif(1, min = 0, max = 1/(2*pi^2))
      x1 <- runif(1,min = -pi,max = pi)
      y1 <- runif(1,min = -pi,max = pi)
      fxy <- (1+sin(a*x1)*sin(a*y1))/(4*pi^2)
      if (u <= fxy) {
        X_1 <- rbind(X_1,x1)
        Y_1 <- rbind(Y_1,y1)
        j <- 2
      }
    }
  }
  X_2 <- runif(n)
  Y_2 <- runif(n)
  data <- data.frame("X1"=X_1,"X2" = X_2, "Y1" = Y_1, "Y2" = Y_2, row.names = NULL)
  return(data)
}

Gauss_univariate <- function(n, rho=0.5){
  Z <- mvrnorm(n=n,mu=c(0,0),Sigma = matrix(c(1,rho,rho,1),nrow = 2))
  X <- Z[,1]
  Y <- Z[,2]
  return(data.frame("X"=X,"Y"=Y))
}

Gauss_multivariate <- function(n,rho = 0.5,StnR = 1, p = 2, q=2){
  d <- p+q
  I <- sample(p,size = StnR)
  Matrix1 <- diag(1,nrow = p)
  Matrix2 <- matrix(data = 0, nrow = p, ncol = q)
  Matrix2[I,] <- rho
  Matrix3 <- t(Matrix2)
  Matrix4 <- diag(1,nrow = q)
  CovMatrix <- rbind(cbind(Matrix1,Matrix2),cbind(Matrix3,Matrix4))
  Z <- mvrnorm(n=n, mu=numeric(d),Sigma = CovMatrix)
  X <- Z[,1:p]
  Y <- Z[,(p+1):d]
  return(data.frame("X"=X,"Y"=Y))
}
