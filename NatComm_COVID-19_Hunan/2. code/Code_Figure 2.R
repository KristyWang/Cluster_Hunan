#set your working directory
setwd("...\\NatComm_COVID-19_Hunan\\")

pdf("3. output\\Fig2.pdf",width = 12,height=12)
col1<-c("#D6604D","#FCC275","#C83B36","#CD534C7F","#466EA2","#7AA6DC7F")
layout(matrix(c(1:2),2,1,byrow=TRUE), widths=c(1), heights=c(1,1))
par(mar=c(6,8,3,3))
x=seq(0,40,by=0.01)
plot(type = "n",x=c(-20,20),y=c(0,0.15),cex.axis=1.5,las=1,xlab=list("Days",cex=1.5,font=2),ylab = list("Probability\n",cex=1.5,font=2), main="")
#distribution of serial interval 
lines(x-14.5,dgamma(x,9.6856,0.4850),lty=1,col=col1[5],lwd=4)
#infectiousness profile
lines(x-20.7310048,dgamma(x,18.1896828,0.9095662),lty=1,col=col1[6],lwd=4)
legend(2,0.14,legend=c("Infectiousness profile","Serial interval"),lty=c(1,1),col=col1[c(6,5)],bty="n",lwd=4,cex=1.5, y.intersp = 1.35)

par(mar=c(6,8,3,3))
x=seq(0,40,by=0.01)
plot(type = "n",x=c(0,20),y=c(0,0.25),cex.axis=1.5,las=1,xlab=list("Days",cex=1.5,font=2),ylab = list("Probability\n",cex=1.5,font=2), main="")
#distribution of incubation period 
lines(x,dweibull(x,1.5781, 7.1121 ),lty=1,col=col1[2],lwd=4)
#distribution of generation time
lines(x,dgamma(x,10.555266,1.845717),lty=1,col=col1[1],lwd=4)
legend(11,0.225,legend=c("Incubation period","Generation time"),lty=c(1,1),col=col1[c(2,1)],bty="n",lwd=4,cex=1.5, y.intersp = 1.35)
dev.off()


