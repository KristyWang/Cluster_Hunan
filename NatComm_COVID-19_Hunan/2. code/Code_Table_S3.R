require(fitdistrplus)
require(readxl)

#set your working directory
setwd("...\\NatComm_COVID-19_Hunan\\")   

#data used for main analysis
toplot<-read_excel("1. input\\data_Table S3.xlsx",sheet = "Main analysis") #n=268  

set.seed(20200512)
toplot<-data.frame(toplot)
fit1  <-  fitdistcens(toplot,"gamma")
sim.sample = rgamma(10000,shape = fit1$estimate[1], rate =fit1$estimate[2])
estim.param1 <- c("gamma", 
                  paste("shape = ",round(fit1$estimate[1],2),"(",round(fit1$sd[1],2),"), rate = ",
                        round(fit1$estimate[2],2),"(",round(fit1$sd[2],2),")",sep = ""),
                  round(mean(sim.sample, na.rm=T),1),
                  paste(round(quantile(sim.sample,0.025,na.rm=T),1)," - ",
                        round(quantile(sim.sample,0.975,na.rm=T),1),sep=""),
                  round(fit1$aic,1))
estim.param1

fit2  <-  fitdistcens(toplot,"weibull")
sim.sample = rweibull(100000,shape = fit2$estimate[1], scale =fit2$estimate[2])
estim.param2 <- c("Weibull", 
                  paste("shape = ",round(fit2$estimate[1],2),"(",round(fit2$sd[1],2),"), scale = ",
                        round(fit2$estimate[2],2),"(",round(fit2$sd[2],2),")",sep = ""),
                  round(mean(sim.sample, na.rm=T),1),
                  paste(round(quantile(sim.sample,0.025,na.rm=T),1)," - ",
                        round(quantile(sim.sample,0.975,na.rm=T),1),sep=""),
                  round(fit2$aic,1))
estim.param2
#median and IQR
round(quantile(sim.sample,c(0.25,0.5,0.75)),1)

fit3  <-  fitdistcens(toplot,"lnorm")
sim.sample = rlnorm(10000, meanlog = fit3$estimate[1], sdlog =fit3$estimate[2])
estim.param3 <- c("Lognormal", 
                  paste("meanlog = ",round(fit3$estimate[1],2),"(",round(fit3$sd[1],2),"), sdlog = ",
                        round(fit3$estimate[2],2),"(",round(fit3$sd[2],2),")",sep = ""),
                  round(mean(sim.sample, na.rm=T),1),
                  paste(round(quantile(sim.sample,0.025,na.rm=T),1)," - ",
                        round(quantile(sim.sample,0.975,na.rm=T),1),sep=""),
                  round(fit3$aic,1))
estim.param3

Table_S3_1<-data.frame(rbind(estim.param1,estim.param2,estim.param3))
rownames(Table_S3_1)<-c("Main analysis 1","Main analysis 2","Main analysis 3")

#data used for Sensitivity analysis 1
toplot<-read_excel("1. input\\data_Table S3.xlsx",sheet = "Sensitivity analysis 1") #n=258

set.seed(20200512)
toplot<-data.frame(toplot)
fit1  <-  fitdistcens(toplot,"gamma")
sim.sample = rgamma(10000,shape = fit1$estimate[1], rate =fit1$estimate[2])
estim.param1 <- c("gamma", 
                  paste("shape = ",round(fit1$estimate[1],2),"(",round(fit1$sd[1],2),"), rate = ",
                        round(fit1$estimate[2],2),"(",round(fit1$sd[2],2),")",sep = ""),
                  round(mean(sim.sample, na.rm=T),1),
                  paste(round(quantile(sim.sample,0.025,na.rm=T),1)," - ",
                        round(quantile(sim.sample,0.975,na.rm=T),1),sep=""),
                  round(fit1$aic,1))
estim.param1

fit2  <-  fitdistcens(toplot,"weibull")
sim.sample = rweibull(100000,shape = fit2$estimate[1], scale =fit2$estimate[2])
estim.param2 <- c("Weibull", 
                  paste("shape = ",round(fit2$estimate[1],2),"(",round(fit2$sd[1],2),"), scale = ",
                        round(fit2$estimate[2],2),"(",round(fit2$sd[2],2),")",sep = ""),
                  round(mean(sim.sample, na.rm=T),1),
                  paste(round(quantile(sim.sample,0.025,na.rm=T),1)," - ",
                        round(quantile(sim.sample,0.975,na.rm=T),1),sep=""),
                  round(fit2$aic,1))
estim.param2

fit3  <-  fitdistcens(toplot,"lnorm")
sim.sample = rlnorm(10000, meanlog = fit3$estimate[1], sdlog =fit3$estimate[2])
estim.param3 <- c("Lognormal", 
                  paste("meanlog = ",round(fit3$estimate[1],2),"(",round(fit3$sd[1],2),"), sdlog = ",
                        round(fit3$estimate[2],2),"(",round(fit3$sd[2],2),")",sep = ""),
                  round(mean(sim.sample, na.rm=T),1),
                  paste(round(quantile(sim.sample,0.025,na.rm=T),1)," - ",
                        round(quantile(sim.sample,0.975,na.rm=T),1),sep=""),
                  round(fit3$aic,1))
estim.param3

Table_S3_2<-data.frame(rbind(estim.param1,estim.param2,estim.param3))
rownames(Table_S3_2)<-c("Sensitivity analysis 1.1","Sensitivity analysis 1.2","Sensitivity analysis 1.3")

#data used for Sensitivity analysis 2
toplot<-read_excel("1. input\\data_Table S3.xlsx",sheet = "Sensitivity analysis 2") #n=251


set.seed(20200512)
toplot<-data.frame(toplot)
fit1  <-  fitdistcens(toplot,"gamma")
sim.sample = rgamma(10000,shape = fit1$estimate[1], rate =fit1$estimate[2])
estim.param1 <- c("gamma", 
                  paste("shape = ",round(fit1$estimate[1],2),"(",round(fit1$sd[1],2),"), rate = ",
                        round(fit1$estimate[2],2),"(",round(fit1$sd[2],2),")",sep = ""),
                  round(mean(sim.sample, na.rm=T),1),
                  paste(round(quantile(sim.sample,0.025,na.rm=T),1)," - ",
                        round(quantile(sim.sample,0.975,na.rm=T),1),sep=""),
                  round(fit1$aic,1))
estim.param1

fit2  <-  fitdistcens(toplot,"weibull")
sim.sample = rweibull(100000,shape = fit2$estimate[1], scale =fit2$estimate[2])
estim.param2 <- c("Weibull", 
                  paste("shape = ",round(fit2$estimate[1],2),"(",round(fit2$sd[1],2),"), scale = ",
                        round(fit2$estimate[2],2),"(",round(fit2$sd[2],2),")",sep = ""),
                  round(mean(sim.sample, na.rm=T),1),
                  paste(round(quantile(sim.sample,0.025,na.rm=T),1)," - ",
                        round(quantile(sim.sample,0.975,na.rm=T),1),sep=""),
                  round(fit2$aic,1))
estim.param2

fit3  <-  fitdistcens(toplot,"lnorm")
sim.sample = rlnorm(10000, meanlog = fit3$estimate[1], sdlog =fit3$estimate[2])
estim.param3 <- c("Lognormal", 
                  paste("meanlog = ",round(fit3$estimate[1],2),"(",round(fit3$sd[1],2),"), sdlog = ",
                        round(fit3$estimate[2],2),"(",round(fit3$sd[2],2),")",sep = ""),
                  round(mean(sim.sample, na.rm=T),1),
                  paste(round(quantile(sim.sample,0.025,na.rm=T),1)," - ",
                        round(quantile(sim.sample,0.975,na.rm=T),1),sep=""),
                  round(fit3$aic,1))
estim.param3

Table_S3_3<-data.frame(rbind(estim.param1,estim.param2,estim.param3))
rownames(Table_S3_3)<-c("Sensitivity analysis 2.1","Sensitivity analysis 2.2","Sensitivity analysis 2.3")

Table_S3<-rbind(Table_S3_1,Table_S3_2,Table_S3_3)
colnames(Table_S3)<-c("Distribution","Parameters","Mean","Quantiles","AIC")

write.csv(Table_S3,"3. output\\Table_S3.csv")