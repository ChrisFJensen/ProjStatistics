GausKer <- function(X1,X2,sigma){
  exp(-sum(X1-X2)^2/(2*sigma^2))
} # Definition of Gaussian Kernel

LinKer <- function(X1,X2){
  t(X1)%*%X2
} # Definition of Linear Kernel

Kernels <- function(type,){ # A function which i can use to define a desired kernel
  switch (type,
    gaussian = { #Defines for the Gaussian kernel
      function(X1,X2,sigma){
        exp(-sum(X1-X2)^2/(2*sigma^2))
      }
    },
    linear = {
      function(X1,X2){ #Defines for the Linear kernel
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

dim(kernelMatrix(Z[,c(1,2)],"linear"))
kernelMatrix(Z[,c(1,2)],"gaussian",sigma=2)
