require(fitdistrplus)
require(readxl)
#set your working directory
setwd("...\\NatComm_COVID-19_Hunan\\")  

set.seed(20200512)
#data: time interval from symptom onset to the date of collection of the sample for PCR testing
data<-read_excel("1. input\\data_Table_S5.xlsx",sheet = "data_onset_smp")
min.int<-min(data$int_onset_samp)
int<-data$int_onset_samp-min.int+0.1
fit1<-fitdistrplus::fitdist(int,"gamma")
x<-rgamma(1000000,fit1$estimate[1],fit1$estimate[2])
param1 <- c("Gamma",nrow(data),
            paste("shape = ",round(fit1$estimate[1],2),"(",round(fit1$sd[1],2),"), rate = ",
                  round(fit1$estimate[2],2),"(",round(fit1$sd[2],2),") shift = ",-min.int+0.1,sep=""),
            round((fit1$estimate[1]/fit1$estimate[2])-0.1+min.int,1),
            paste(round(quantile(x,0.025)-0.1+min.int,1),"~",
                  round(quantile(x,0.975)-0.1+min.int,1),sep = ""))
#median and IQR
round(quantile(x,c(0.25,0.5,0.75))-0.1+min.int,1)


#data: time interval from symptom onset to laboratory confirmation
data<-read_excel("1. input\\data_Table_S5.xlsx",sheet = "data_onset_diag")
min.int<-min(data$int_onset_diag)
int<-data$int_onset_diag-min.int+1
fit1<-fitdistrplus::fitdist(int,"gamma")
x<-rgamma(1000000,fit1$estimate[1],fit1$estimate[2])
param2 <- c("Gamma",nrow(data),
            paste("shape = ",round(fit1$estimate[1],2),"(",round(fit1$sd[1],2),"), rate = ",
                  round(fit1$estimate[2],2),"(",round(fit1$sd[2],2),") shift = ",-min.int+1,sep=""),
            round((fit1$estimate[1]/fit1$estimate[2])-1+min.int,1),
            paste(round(quantile(x,0.025)-1+min.int,1),"~",
                  round(quantile(x,0.975)-1+min.int,1),sep = ""))
#median and IQR
round(quantile(x,c(0.25,0.5,0.75))-1+min.int,1)

Table_S5<-rbind(param1,param2)
colnames(Table_S5)<-c("Distribution","Sample size","Parameters","Mean","Quantiles")
write.csv(Table_S5,"3. output\\Table_S5.csv")
