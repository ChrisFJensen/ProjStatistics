
Calibration_plots_ld <- function(datatype, rho,
                                 n=1000, reps=200,
                                 sampleS = c(200,500,1000),
                                 Permutations = c(100,200,500)){
  P_val_df <- data.frame("Pval"=NA,"SampleSize"=NA,"Permutations"=NA,"Estimator"=NA)[-1,]
  set.seed(2309)
  for (it in 1:reps) {
    switch(datatype,
      M1 = {
        Z <- M1_dat_gen(n,rho)
        X <- Z[,c(1,2)]
        Y <- Z[,c(3,4)]
      },
      M2 = {
        Z <- M2_dat_gen(n,rho)
        X <- Z[,c(1,2)]
        Y <- Z[,c(3,4)]
      },
      M3 = {
        stop("Not yet implemented")
      },
      Gauss = {
        Z <- Gauss_univariate(n,rho)
        X <- Z$X
        Y <- Z$Y
      }
    )
    for (ss in sampleS) {
      for (perm in Permutations) {
        Pval1 <- dhsic.test(X[1:ss,],Y[1:ss,],kernel = "gaussian", B=perm)$p.value
        P1 <- data.frame("Pval"=Pval1,"SampleSize"=ss,"Permutations"=perm,"Estimator"="median")
        Pval2 <- dhsic.test(X[1:ss,],Y[1:ss,],kernel = "LinKer", B=perm)$p.value
        P2 <- data.frame("Pval"=Pval1,"SampleSize"=ss,"Permutations"=perm,"Estimator"="linear")
        Pval3 <- dcor.test(X,Y,R=perm)
        P3 <- data.frame("Pval"=Pval1,"SampleSize"=ss,"Permutations"=perm,"Estimator"="dCor")
        P_val_df <- rbind(P_val_df,P1,P2,P3)
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

tester1 <- Calibration_plots_ld("M2",rho=0,reps = 10,n=100,sampleS = c(20,50,100),Permutations = c(10,20,50))

ggplot(type1_rej_rate[which(type1_rej_rate$Estimator=="linear"),])+
  geom_line(mapping = aes(x = alpha, y = type1error, 
                          colour=as.factor(Permutations),
                          linetype = as.factor(SampleSize),
                          group=interaction(SampleSize,Permutations)))
