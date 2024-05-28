source("Lib.R")
source("Datagen.R")
source("HSIC.R")
source("dCor.R")
source("Rejectionplottingtemp.R")


source("readsimulations.R") ## These are my simulations,

d1_cal <- Calibration_plots("Gauss",rho = 0)

saveRDS(d1_cal,"d1_cal.rds")

ggplot(d1_cal)+
  geom_line(mapping = aes(x = alpha, y = type1error, 
                          colour=as.factor(Estimator),
                          linetype = as.factor(Permutations),
                          group=inta))+
  geom_abline(intercept = 0,slope = 1,color="black")+
  facet_wrap(~SampleSize)
## Chose n=300 since
ggplot(d1_cal[which(d1_cal$SampleSize==300),])+
  geom_line(mapping = aes(x = alpha, y = type1error, 
                          colour=as.factor(Estimator),
                          linetype = as.factor(SampleSize),
                          group=inta))+
  geom_abline(intercept = 0,slope = 1,color="black")+
  facet_wrap(~Permutations)

d2_cal <- Calibration_plots("M1",rho = 0)

saveRDS(d2_cal,"d2_cal.rds")

ggplot(d2_cal)+
  geom_line(mapping = aes(x = alpha, y = type1error, 
                          colour=as.factor(Estimator),
                          linetype = as.factor(Permutations),
                          group=inta))+
  geom_abline(intercept = 0,slope = 1,color="black")+
  facet_wrap(~SampleSize)

## Choose SampleSize 500 because most error below the diagonal
ggplot(d2_cal[which(d2_cal$SampleSize==500),])+
  geom_line(mapping = aes(x = alpha, y = type1error, 
                          colour=as.factor(Estimator),
                          linetype = as.factor(SampleSize),
                          group=inta))+
  geom_abline(intercept = 0,slope = 1,color="black")+
  facet_wrap(~Permutations)

d3_cal <- Calibration_plots("M2",rho = 0)

saveRDS(d3_cal,"d3_cal.rds")

ggplot(d3_cal)+
  geom_line(mapping = aes(x = alpha, y = type1error, 
                          colour=as.factor(Estimator),
                          linetype = as.factor(Permutations),
                          group=inta))+
  geom_abline(intercept = 0,slope = 1,color="black")+
  facet_wrap(~SampleSize)

ggplot(d3_cal[which(d3_cal$SampleSize==200),])+
  geom_line(mapping = aes(x = alpha, y = type1error, 
                          colour=as.factor(Estimator),
                          linetype = as.factor(SampleSize),
                          group=inta))+
  geom_abline(intercept = 0,slope = 1,color="black")+
  facet_wrap(~Permutations)

d1_pow <- Power_plots_ld("Gauss", rho = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), sampleS = 300,Permutations = 500)

saveRDS(d1_pow,"d1_pow.rds")

ggplot(d1_pow)+
  geom_line(mapping = aes(x = rho, y = power, 
                          colour=as.factor(Estimator)))+
  geom_abline(intercept = 0,slope = 1,color="black")
