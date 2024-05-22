

Abe2 <- M2_dat_gen(1000,0)


P_val_df <- data.frame("Pval"=rep(NA,90),"SampleSize"=rep(NA,90),"Permutations"=rep(NA,90))
j <- 1
for (it in 1:10) {
  Z <- M2_dat_gen(100,0)
  X <- Z[,c(1,2)]
  Y <- Z[,c(3,4)]
  for (ss in c(10,25,50)) {
    for (perm in c(10,20,50)) {
      Pval <- dhsic.test(X[1:ss,],Y[1:ss,],kernel = "gaussian", B=perm)$p.value
      P_val_df[j,] <- c(Pval,ss,perm)
      j <- j+1
    }
  }
}

P_val_df$inta <- interaction(P_val_df$SampleSize,P_val_df$Permutations)
type1_rej_rate <- data.frame("SampleSize" = NA, "Permutations" = NA, "inta" = NA, "alpha"=NA, "type1error" = NA)[-1,]
for (a in c(seq(from = 0, to = 1, by = 0.02))) {
  DF1 <- P_val_df |> group_by(inta) |> filter(row_number()==1)
  DFT1E <- P_val_df |> group_by(inta) |> summarise(type1error = mean(Pval < a))
  DF2 <- DF1[order(DF1$inta),-1]
  DF2$alpha <- a
  DF2$type1error <- DFT1E$type1error
  type1_rej_rate <- rbind(type1_rej_rate,DF2)
}

order(DFT1E$inta)
order(DF2$inta)

ggplot(type1_rej_rate)+
  geom_line(mapping = aes(x = alpha, y = type1error, 
                          colour=as.factor(Permutations),
                          linetype = as.factor(SampleSize),
                          group=interaction(SampleSize,Permutations)))



