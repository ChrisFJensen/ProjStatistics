M1_dat_gen <- function(n,A){
  theta <- runif(n,0,2*pi)
  epsilon <- matrix(rnorm(n),nrow = 2)
  X_1 <- A*cos(theta)+epsilon[1,]/4
  Y_1 <- A*sin(theta)+epsilon[2,]/4
  X_2 <- runif(n)
  Y_2 <- runif(n)
  data <- data.frame(cbind(X_1,X_2),cbind(Y_1,Y_2))
}

print(M1_dat_gen(100,2))

M2_dat_gen <- function(n,rho){
  X_1 <- runif(n,-1,1)
  epsilon <- rnorm(n)
  Y_1 <- abs(X_1)^rho*epsilon
  X_2 <- runif(n)
  Y_2 <- runif(n)
  data <- data.frame(cbind(X_1,X_2),cbind(Y_1,Y_2))
}

M3_dat_gen <- function(n,a){
  
}