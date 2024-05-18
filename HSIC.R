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

sigma_tilde <- function(x){
  min(sd(x,na.rm = T),IQR(x,na.rm = T)/1.34)
}

Bandwith <- function(bType){ # a bandwith function
  if (is.numeric(bType)) {
    function(X){
      return(bType) # Returns a number if number is used as a input
    }
  } else {
    function(X){
      median(dist(X, method = "euclidian")) # defines function for Calculating midean euclidian distance
    }
  }
}

HSIC_b <- function(X,Y, # Our data
                   typeX,typeY, # The kernel for corresponding RKHS
                   BandX = "Median", # Bandwidth variables
                   BandY = "Median" # can either be median or numbers
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
  return(sum(K%*%H%*%L%*%H)/(n^2))
}

HSIC_b(X,Y, "Gaussian","Gaussian")

HSIC_b(X,Y,"Gaussian","Linear",bTypeX = "Median")

test2 <- Bandwith("Silverman")

test2(Z[,c(1,2)])

dim(kernelMatrix(Z[,c(1,2)],"linear"))
kernelMatrix(Z[,c(1,2)],"gaussian",sigma=2)
