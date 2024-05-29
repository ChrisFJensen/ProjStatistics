
Calibration_plots <- function(datatype, rho,
                                 reps=100,
                                 BW = c(0.25,0.5,0.75,1),
                                 sampleS = c(200,300,500),
                                 Permutations = c(250,500,750)){
  P_val_df <- data.frame("Pval"=NA,"SampleSize"=NA,"Permutations"=NA,"Estimator"=NA)[-1,]
  set.seed(2309)
  n <- max(sampleS)
  for (it in 1:reps) {
    for (ss in sampleS) {
    switch(datatype,
      M1 = {
        Z <- M1_dat_gen(ss,rho)
        X <- Z[,c(1,2)]
        Y <- Z[,c(3,4)]
      },
      M2 = {
        Z <- M2_dat_gen(ss,rho)
        X <- Z[,c(1,2)]
        Y <- Z[,c(3,4)]
      },
      M3 = {
        stop("Not yet implemented")
      },
      Gauss = {
        Z <- Gauss_univariate(ss,rho)
        X <- data.frame(Z$X)
        Y <- data.frame(Z$Y)
      }
    )
      d <- ncol(Z)
      for (perm in Permutations) {
        Pval1 <- dhsic.test(X,Y,kernel = "gaussian.fixed", bandwidth = d^BW[1] , B=perm)$p.value
        P1 <- data.frame("Pval"=Pval1,"SampleSize"=ss,"Permutations"=perm,"Estimator"=as.character(BW[1]))
        Pval2 <- dhsic.test(X,Y,kernel = "gaussian.fixed", bandwidth = d^BW[2] , B=perm)$p.value
        P2 <- data.frame("Pval"=Pval2,"SampleSize"=ss,"Permutations"=perm,"Estimator"=as.character(BW[2]))
        Pval3 <- dhsic.test(X,Y,kernel = "gaussian.fixed", bandwidth = d^BW[3] , B=perm)$p.value
        P3 <- data.frame("Pval"=Pval3,"SampleSize"=ss,"Permutations"=perm,"Estimator"=as.character(BW[3]))
        Pval4 <- dhsic.test(X,Y,kernel = "gaussian.fixed", bandwidth = d^BW[4] , B=perm)$p.value
        P4 <- data.frame("Pval"=Pval4,"SampleSize"=ss,"Permutations"=perm,"Estimator"=as.character(BW[4]))
        Pval5 <- dhsic.test(X,Y,kernel = "gaussian", B=perm)$p.value
        P5 <- data.frame("Pval"=Pval5,"SampleSize"=ss,"Permutations"=perm,"Estimator"="median")
        Pval6 <- dhsic.test(X,Y,kernel = "LinKer", B=perm)$p.value
        P6 <- data.frame("Pval"=Pval6,"SampleSize"=ss,"Permutations"=perm,"Estimator"="linear")
        Pval7 <- dcor.test(X,Y,R=perm)$p.value
        P7 <- data.frame("Pval"=Pval7,"SampleSize"=ss,"Permutations"=perm,"Estimator"="dCor")
        P_val_df <- rbind(P_val_df,P1,P2,P3,P4,P5,P6,P7)
      }
    }
  }
  P_val_df$inta <- interaction(P_val_df$SampleSize,P_val_df$Permutations,P_val_df$Estimator)
  type1_rej_rate <- data.frame("SampleSize" = NA, "Permutations" = NA, "Estimator" = NA, "inta" = NA, "alpha"=NA, "type1error" = NA)[-1,]
  for (a in c(seq(from = 0, to = 1, by = 0.02))) {
    DF1 <- P_val_df |> group_by(inta) |> filter(row_number()==1)
    DFT1E <- P_val_df |> group_by(inta) |> summarise(type1error = mean(Pval <= a))
    DF2 <- DF1[order(DF1$inta),-1]
    DF2$alpha <- a
    DF2$type1error <- DFT1E$type1error
    type1_rej_rate <- rbind(type1_rej_rate,DF2)
  }
  return(type1_rej_rate)
}

Calibration_plots_fbw <- function(datatype, rho,
                                  BW = c(0.25,0.5,0.75,1),
                                  reps=100,
                                  sampleS = c(200,300,500),
                                  Permutations = c(250,500,750)){
  P_val_df <- data.frame("Pval"=NA,"SampleSize"=NA,"Permutations"=NA,"Estimator"=NA)[-1,]
  set.seed(2309)
  for (it in 1:reps) {
    for (ss in sampleS) {
    switch(datatype,
           M1 = {
             Z <- M1_dat_gen(ss,rho)
             X <- Z[,c(1,2)]
             Y <- Z[,c(3,4)]
           },
           M2 = {
             Z <- M2_dat_gen(ss,rho)
             X <- Z[,c(1,2)]
             Y <- Z[,c(3,4)]
           },
           M3 = {
             stop("Not yet implemented")
           },
           Gauss = {
             Z <- Gauss_univariate(ss,rho)
             X <- data.frame(Z$X)
             Y <- data.frame(Z$Y)
           }
    )
      d <- ncol(Z)
      for (perm in Permutations) {
        Pval1 <- dhsic.test(X,Y,kernel = "gaussian.fixed", bandwidth = d^BW[1] , B=perm)$p.value
        P1 <- data.frame("Pval"=Pval1,"SampleSize"=ss,"Permutations"=perm,"Estimator"=as.character(BW[1]))
        Pval2 <- dhsic.test(X,Y,kernel = "gaussian.fixed", bandwidth = d^BW[2] , B=perm)$p.value
        P2 <- data.frame("Pval"=Pval1,"SampleSize"=ss,"Permutations"=perm,"Estimator"=as.character(BW[2]))
        Pval3 <- dhsic.test(X,Y,kernel = "gaussian.fixed", bandwidth = d^BW[3] , B=perm)$p.value
        P3 <- data.frame("Pval"=Pval1,"SampleSize"=ss,"Permutations"=perm,"Estimator"=as.character(BW[3]))
        Pval4 <- dhsic.test(X,Y,kernel = "gaussian.fixed", bandwidth = d^BW[4] , B=perm)$p.value
        P4 <- data.frame("Pval"=Pval1,"SampleSize"=ss,"Permutations"=perm,"Estimator"=as.character(BW[4]))
        P_val_df <- rbind(P_val_df,P1,P2,P3,P4)
      }
    }
  }
  P_val_df$inta <- interaction(P_val_df$SampleSize,P_val_df$Permutations,P_val_df$Estimator)
  type1_rej_rate <- data.frame("SampleSize" = NA, "Permutations" = NA, "Estimator" = NA, "inta" = NA, "alpha"=NA, "type1error" = NA)[-1,]
  for (a in c(seq(from = 0, to = 1, by = 0.02))) {
    DF1 <- P_val_df |> group_by(inta) |> filter(row_number()==1)
    DFT1E <- P_val_df |> group_by(inta) |> summarise(type1error = mean(Pval < a))
    DF2 <- DF1[order(DF1$inta),-1]
    DF2$alpha <- a
    DF2$type1error <- DFT1E$type1error
    type1_rej_rate <- rbind(type1_rej_rate,DF2)
  }
  return(type1_rej_rate)
}

Power_plots_ld <- function(datatype, rho,
                           sampleS = 200,
                           Permutations = 200,
                           reps=100,
                           BW = c(0.25,0.5,0.75,1),
                           level = 0.05){
  P_val_df <- data.frame("Pval"=NA,"rho" = NA,"Estimator"=NA)[-1,]
  set.seed(2309)
  n <- max(sampleS)
  ss <- sampleS
  perm <- Permutations
  for (it in 1:reps) {
    for (r in rho) {
      switch(datatype,
             M1 = {
               Z <- M1_dat_gen(ss,r)
               X <- Z[,c(1,2)]
               Y <- Z[,c(3,4)]
               d <- 4
             },
             M2 = {
               Z <- M2_dat_gen(ss,r)
               X <- Z[,c(1,2)]
               Y <- Z[,c(3,4)]
               d <- 4
             },
             M3 = {
               stop("Not yet implemented")
             },
             Gauss = {
               Z <- Gauss_univariate(ss,r)
               X <- data.frame(Z$X)
               Y <- data.frame(Z$Y)
               d <- 2
             }
      )
      Pval1 <- dhsic.test(X,Y,kernel = "gaussian.fixed", bandwidth = d^BW[1] , B=perm)$p.value
      P1 <- data.frame("Pval"=Pval1,"rho"=r,"Estimator"=as.character(BW[1]))
      Pval2 <- dhsic.test(X,Y,kernel = "gaussian.fixed", bandwidth = d^BW[2] , B=perm)$p.value
      P2 <- data.frame("Pval"=Pval2,"rho"=r,"Estimator"=as.character(BW[2]))
      Pval3 <- dhsic.test(X,Y,kernel = "gaussian.fixed", bandwidth = d^BW[3] , B=perm)$p.value
      P3 <- data.frame("Pval"=Pval3,"rho"=r,"Estimator"=as.character(BW[3]))
      Pval4 <- dhsic.test(X,Y,kernel = "gaussian.fixed", bandwidth = d^BW[4] , B=perm)$p.value
      P4 <- data.frame("Pval"=Pval4,"rho"=r,"Estimator"=as.character(BW[4]))
      Pval5 <- dhsic.test(X,Y,kernel = "gaussian", B=perm)$p.value
      P5 <- data.frame("Pval"=Pval5,"rho"=r,"Estimator"="median")
      Pval6 <- dhsic.test(X,Y,kernel = "LinKer", B=perm)$p.value
      P6 <- data.frame("Pval"=Pval6,"rho"=r,"Estimator"="linear")
      Pval7 <- dcor.test(X,Y,R=perm)$p.value
      P7 <- data.frame("Pval"=Pval7,"rho"=r,"Estimator"="dCor")
      P_val_df <- rbind(P_val_df,P1,P2,P3,P4,P5,P6,P7)
    }
  }
  P_val_df$inta <- interaction(P_val_df$rho ,P_val_df$Estimator)
  type1_rej_rate <- data.frame("rho"= NA, "Estimator" = NA, "inta" = NA,"power" = NA)[-1,]
    DF1 <- P_val_df |> group_by(inta) |> filter(row_number()==1)
    DFT1E <- P_val_df |> group_by(inta) |> summarise(power = mean(Pval <= level))
    DF2 <- DF1[order(DF1$inta),-1]
    DF2$power <- DFT1E$power
    type1_rej_rate <- rbind(type1_rej_rate,DF2)
  return(type1_rej_rate)
}

