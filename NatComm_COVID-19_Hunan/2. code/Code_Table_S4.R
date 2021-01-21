require(fitdistrplus)
require(data.table)
#set your working directory
setwd("...\\NatComm_COVID-19_Hunan\\")  

set.seed(20200512)
data<-data.table(read.csv("1. input\\data_Table_S4.csv"))
left<-as.numeric(data$y-data$x.ub+14.5)
right<-as.numeric(data$y-data$x.lb+14.5)
toplot<-data.frame(left,right)
plotdistcens(toplot, NPMLE = FALSE)
fit1  <-  fitdistcens(toplot,"gamma")
sim.sample<-rgamma(100000,fit1$estimate[1],fit1$estimate[2])
estim.param <- c("Overall", nrow(data),
                  paste("shape = ",round(fit1$estimate[1],2),"(",round(fit1$sd[1],2),"), rate = ",
                        round(fit1$estimate[2],2),"(",round(fit1$sd[2],2),"), shift=14.5",sep = ""),
                  round(fit1$estimate[1]/fit1$estimate[2]-14.5,1),
                  paste(round(quantile(sim.sample,0.025,na.rm=T)-14.5,1)," ~ ",
                        round(quantile(sim.sample,0.975,na.rm=T)-14.5,1),sep=""))
estim.param
#median and IQR 
round(quantile(sim.sample,c(0.25,0.5,0.75))-14.5,1)
#proportion of negative serial intervals
xxx<-toplot[toplot$left==toplot$right,]
round(100*nrow(xxx[xxx$left<=14.5,])/nrow(xxx),1)

data1<-data[prd==1,]
left<-as.numeric(data1$y-data1$x.ub+14.5)
right<-as.numeric(data1$y-data1$x.lb+14.5)
toplot<-data.frame(left,right)
plotdistcens(toplot, NPMLE = FALSE)
fit1  <-  fitdistcens(toplot,"gamma")
sim.sample<-rgamma(100000,fit1$estimate[1],fit1$estimate[2])
estim.param1 <- c("January 5 - January 23", nrow(data1),
                  paste("shape = ",round(fit1$estimate[1],2),"(",round(fit1$sd[1],2),"), rate = ",
                        round(fit1$estimate[2],2),"(",round(fit1$sd[2],2),"), shift=14.5",sep = ""),
                  round(fit1$estimate[1]/fit1$estimate[2]-14.5,1),
                  paste(round(quantile(sim.sample,0.025,na.rm=T)-14.5,1)," ~ ",
                        round(quantile(sim.sample,0.975,na.rm=T)-14.5,1),sep=""))
estim.param1
#median and IQR 
round(quantile(sim.sample,c(0.25,0.5,0.75))-14.5,1)

data2<-data[prd==2,]
left<-as.numeric(data2$y-data2$x.ub+14.5)
right<-as.numeric(data2$y-data2$x.lb+14.5)
toplot<-data.frame(left,right)
plotdistcens(toplot, NPMLE = FALSE)
fit1  <-  fitdistcens(toplot,"gamma")
sim.sample<-rgamma(100000,fit1$estimate[1],fit1$estimate[2])
estim.param2 <- c("January 24 - April 2", nrow(data2),
                  paste("shape = ",round(fit1$estimate[1],2),"(",round(fit1$sd[1],2),"), rate = ",
                        round(fit1$estimate[2],2),"(",round(fit1$sd[2],2),"), shift=14.5",sep = ""),
                  round(fit1$estimate[1]/fit1$estimate[2]-14.5,1),
                  paste(round(quantile(sim.sample,0.025,na.rm=T)-14.5,1)," ~ ",
                        round(quantile(sim.sample,0.975,na.rm=T)-14.5,1),sep=""))
estim.param2
#median and IQR 
round(quantile(sim.sample,c(0.25,0.5,0.75))-14.5,1)

Table_S4<-rbind(estim.param,estim.param1,estim.param2)
colnames(Table_S4)<-c("Period","Sample size","Parameters","Mean","IQR")
rownames(Table_S4)<-c(1,2,3)

write.csv(Table_S4,"3. output\\Table_S4.csv")
