source("Lib.R")
source("Datagen.R")
source("HSIC.R")
source("dCor.R")
source("Rejectionplottingtemp.R")




d1_cal <- Calibration_plots_ld("Gauss",rho = 0)

saveRDS(d1_cal,"d1_cal.rds")

d1_cal_fbw <- Calibration_plots_fbw("Gauss", rho = 0)

wsaveRDS(d1_cal_fbw,"d1_cal_fbw.rds")

d2_cal <- Calibration_plots_ld("M1",rho = 0)

ggplot(d2_cal)+
  geom_line(mapping = aes(x = alpha, y = type1error, 
                          colour=as.factor(Permutations),
                          linetype = as.factor(SampleSize),
                          group=interaction(SampleSize,Permutations)))+
  geom_abline(intercept = 0,slope = 1,color="black")+
  facet_wrap(~Estimator)

ggplot(d1_cal_fbw)+
  geom_line(mapping = aes(x = alpha, y = type1error, 
                          colour=as.factor(Permutations),
                          linetype = as.factor(SampleSize),
                          group=interaction(SampleSize,Permutations)))+
  geom_abline(intercept = 0,slope = 1,color="black")+
  facet_wrap(~Estimator)


ggplot(d1_cal)+
  geom_line(mapping = aes(x = alpha, y = type1error, 
                        colour=as.factor(Permutations),
                      linetype = as.factor(SampleSize),
                    group=interaction(SampleSize,Permutations)))+
  geom_abline(intercept = 0,slope = 1,color="black")+
  facet_wrap(~Estimator)

ggplot(d1_cal[which(d1_cal$SampleSize==200),])+
  geom_line(mapping = aes(x = alpha, y = type1error, 
                          colour=as.factor(Permutations),
                          linetype = as.factor(SampleSize),
                          group=interaction(SampleSize,Permutations)))+
  geom_abline(intercept = 0,slope = 1,color="black")+
  facet_wrap(~Estimator)
