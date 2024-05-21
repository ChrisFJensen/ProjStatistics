GausKer <- function(X1,X2,sigma){
  exp(-sum(X1-X2)^2/(2*sigma^2))
} # Definition of Gaussian Kernel

LinKer <- function(X1,X2){
  t(X1)%*%X2
} # Definition of Linear Kernel

Kernels <- function(type){ # A function which i can use to define a desired kernel
  switch (type,
    Gaussian = { #Defines for the Gaussian kernel
      function(X1,X2,sigma){
        exp(-sum(X1-X2)^2/(2*sigma^2))
      }
    },
    Linear = {
      function(X1,X2, sigma){ #Defines for the Linear kernel
        t(X1)%*%X2
      }
    },
    stop("Please input valid kernel type (Gaussian/Kernel)")
  )
}

kernelMatrix <- function(X, type, ...){ # Function that creates Kernel Matrices.
  kernelU <- Kernels(type)
  X <- as.matrix(X)
  n <- dim(X)[1]
  K <- matrix(NA,nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in 1:n) {
      K[i,j] <- kernelU(X[i,],X[j,],...)
    }
  }
  return(K)
}

Bandwith <- function(bType){ # a bandwith function
  if (is.numeric(bType)) {
    function(X){
      return(bType) # Returns a number if number is used as a input
    }
  } else {
    function(X){
      median(dist(X, method = "euclidian")) # defines function for Calculating median euclidian distance
    }
  }
}

HSIC_b <- function(X,Y, # Our data
                   typeX,typeY, # The kernel for corresponding RKHS
                   BandX = "Median", # Bandwidth variables
                   BandY = "Median" # can either be a number or a method for calculating the bandwith
                   ){
  n <- nrow(X)
  if (typeX=="Gaussian") {
      sigfunc <- Bandwith(BandX)
      sigx <- sigfunc(X)
      K <- kernelMatrix(X,typeX,sigx)
  } else {
    K <- kernelMatrix(X,typeX)
  }
  if (typeY=="Gaussian"){
    sigfunc <- Bandwith(BandY)
    sigy <- sigfunc(Y)
    L <- kernelMatrix(Y,typeY,sigy)
  } else {
    L <- kernelMatrix(Y,typeY)
  }
  H <- diag(x=1, nrow = n,ncol = n) - matrix(1/n,nrow = n ,ncol = n)
  return(sum(diag((K%*%H%*%L%*%H)))/(n^2))
}

HSIC_perm <- function(X,Y, # Our data
                      typeX = "Gaussian",
                      typeY = "Gaussian", # The kernel for corresponding RKHS
                      BandX = "Median", # Bandwidth variables
                      BandY = "Median", # can either be a number or a method for calculating the bandwith
                      p=100 # Number of permutations
){
  n <- nrow(X)
  if (typeX=="Gaussian") {
    sigfunc <- Bandwith(BandX)
    sigx <- sigfunc(X)
    K <- kernelMatrix(X,typeX,sigx)
  } else {
    K <- kernelMatrix(X,typeX)
  }
  if (typeY=="Gaussian"){
    sigfunc <- Bandwith(BandY)
    sigy <- sigfunc(Y)
    L <- kernelMatrix(Y,typeY,sigy)
  } else {
    L <- kernelMatrix(Y,typeY)
  }
  H <- diag(x=1, nrow = n,ncol = n) - matrix(1/n,nrow = n ,ncol = n)
  HSIC_std <- sum(diag(K%*%H%*%L%*%H))/(n^2)
  HSIC_permed <- numeric((p-1))
  for (k in 1:(p-1)) {
    permutation <- sample(n)
    L_star <- matrix(NA, nrow = n,ncol = n)
    for (i in 1:n) {
      for (j in 1:n) {
        L_star[i,j] <- L[permutation[i],permutation[j]]
        L_star[j,i] <- L_star[i,j]
      }
    }
    HSIC_permed[k]<- mean(K%*%H%*%L_star%*%H)
  }
  p_tilde <- (1+length(which(HSIC_permed>HSIC_std)))/(p)
  return(p_tilde)
}

HSIC_perm(X,Y,typeX = "Linear", BandY = 4, p=100)
