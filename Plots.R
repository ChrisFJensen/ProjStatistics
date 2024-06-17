ggplot(d1_cal)+
  geom_line(mapping = aes(x = alpha, y = type1error, 
                          colour=as.factor(Estimator),
                          linetype = as.factor(Permutations),
                          group=inta))+
  geom_abline(intercept = 0,slope = 1,color="black")+
  facet_wrap(~SampleSize)+
  ggtitle("Calibration plot of data 1", subtitle = "Each graph for different n")

d1cpp <- ggplot(d1_cal[which(d1_cal$SampleSize==200),])+
  geom_line(mapping = aes(x = alpha, y = type1error, 
                          colour=as.factor(Estimator),
                          group=inta))+
  geom_abline(intercept = 0,slope = 1,color="black")+
  facet_wrap(~Permutations)+
  ggtitle("Calibration plot of data 1", subtitle = "Each graph for different permutations with n=200")

ggplot(d2_cal)+
  geom_line(mapping = aes(x = alpha, y = type1error, 
                          colour=as.factor(Estimator),
                          linetype = as.factor(Permutations),
                          group=inta))+
  geom_abline(intercept = 0,slope = 1,color="black")+
  facet_wrap(~SampleSize)+
  ggtitle("Calibration plot of data 2", subtitle = "Each graph for different n")

d2cpp<-ggplot(d2_cal[which(d2_cal$SampleSize==500),])+
  geom_line(mapping = aes(x = alpha, y = type1error, 
                          colour=as.factor(Estimator),
                          group=inta))+
  geom_abline(intercept = 0,slope = 1,color="black")+
  facet_wrap(~Permutations)+
  ggtitle("Calibration plot of data 2", subtitle = "Each graph for different permutations with n=500")

ggplot(d3_cal)+
  geom_line(mapping = aes(x = alpha, y = type1error, 
                          colour=as.factor(Estimator),
                          linetype = as.factor(Permutations),
                          group=inta))+
  geom_abline(intercept = 0,slope = 1,color="black")+
  facet_wrap(~SampleSize)+
  ggtitle("Calibration plot of data 3", subtitle = "Each graph for different n")

d3cpp<- ggplot(d3_cal[which(d3_cal$SampleSize==500),])+
  geom_line(mapping = aes(x = alpha, y = type1error, 
                          colour=as.factor(Estimator),
                          group=inta))+
  geom_abline(intercept = 0,slope = 1,color="black")+
  facet_wrap(~Permutations)+
  ggtitle("Calibration plot of data 3", subtitle = "Each graph for different permutations with n=500")

ggplot(d4_cal)+
  geom_line(mapping = aes(x = alpha, y = type1error, 
                          colour=as.factor(Estimator),
                          linetype = as.factor(Permutations),
                          group=inta))+
  geom_abline(intercept = 0,slope = 1,color="black")+
  facet_wrap(~SampleSize)+
  ggtitle("Calibration plot of data 4", subtitle = "Each graph for different n")

d4cpp<-ggplot(d4_cal[which(d4_cal$SampleSize==300),])+
  geom_line(mapping = aes(x = alpha, y = type1error, 
                          colour=as.factor(Estimator),
                          group=inta))+
  geom_abline(intercept = 0,slope = 1,color="black")+
  facet_wrap(~Permutations)+
  ggtitle("Calibration plot of data 4", subtitle = "Each graph for different permutations with n=300")

ggarrange(d1cpp,d2cpp, common.legend = T, legend = "bottom")
ggarrange(d3cpp,d4cpp, common.legend = T, legend = "bottom")

d1pplot <- ggplot(d1_pow)+
  geom_line(mapping = aes(x = rho, y = power, 
                          colour=as.factor(Estimator)))+
  scale_x_continuous(breaks = seq(0.0,1,0.1))+
  scale_y_continuous(breaks = seq(0.0,1,0.1))+
  ggtitle("Power plot of data 1", subtitle = "n=200 and permutations = 500")+
  theme(legend.position = "below")

d2pplot <- ggplot(d2_pow)+
  geom_line(mapping = aes(x = rho, y = power, 
                          colour=as.factor(Estimator)))+
  scale_x_continuous(breaks = seq(1,10,1))+
  ggtitle("Power plot of data 2", subtitle = "n=500 and permutations = 500")+
theme(legend.position = "below")


ggarrange(d1pplot,d2pplot, common.legend = T, legend = "bottom")

grid.arrange(d1pplot,d2pplot,ncol=2, common.label=TRUE, legend="right")

d3pplot <- ggplot(d3_pow)+
  geom_line(mapping = aes(x = rho, y = power, 
                          colour=as.factor(Estimator)))+
  scale_x_continuous(breaks = seq(0.0,1,0.1))+
  scale_y_continuous(breaks = seq(0.0,1,0.1))+
  ggtitle("Power plot of data 3", subtitle = "n=200 and permutations = 500")


d4pplot <- ggplot(d4_pow)+
  geom_line(mapping = aes(x = rho, y = power, 
                          colour=as.factor(Estimator)))+
  scale_x_continuous(breaks = seq(1,10,1))+
  scale_y_continuous(breaks = seq(0.0,1,0.1))+
  ggtitle("Power plot of data 4", subtitle = "n=300 and permutations = 500")


ggarrange(d3pplot,d4pplot, common.legend = T, legend = "bottom")

ggplot(Hdimsim[which(Hdimsim$I==1),])+
  geom_line(mapping = aes(x = rho, y = power, 
                          colour=as.factor(Estimator)))+
  scale_y_continuous(breaks = seq(0.0,1,0.1))+
  scale_x_continuous(breaks = seq(0.0,1,0.02))+
  facet_wrap(~d)+
  ggtitle("Power wrt rho in different dimensions", subtitle = "Sparse case")

ggplot(Hdimsim[which(Hdimsim$I!=1),])+
  geom_line(mapping = aes(x = rho, y = power, 
                          colour=as.factor(Estimator)))+
  scale_y_continuous(breaks = seq(0.0,1,0.1))+
  scale_x_continuous(breaks = seq(0.0,1,0.02))+
  facet_wrap(~d)+
  ggtitle("Power wrt rho in different dimensions", subtitle = "Dense case")
