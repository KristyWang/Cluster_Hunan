# Code to reproduce Fig.S2
library(ggplot2)
setwd(".../1. input/data_Figure S4/")

a<-expression("">= 65)

age1<- read.csv("age_gender.csv")

ggplot(data=age1,aes(x=age-0.5,y=percent))+
  geom_bar(aes(fill=factor(age1$gender,levels=c(2,1))),stat="identity",position = "dodge",colour="black",width = 0.7,size=0.5)+
  scale_fill_manual(values = c("#FDE9B4","#EA7C1C"),labels=c("Female (n=601)","Male (n=577)"))+
  scale_x_continuous(limits = c(-2,3),expand=c(0,0),breaks = seq(0,3,1),
                     label=c("","","",""),name="")+
  scale_y_continuous(limits=c(-10,115),breaks = c(seq(0,100,25)),expand=c(0,0),name="")+
  coord_cartesian(xlim=c(0,3),ylim = c(0,100), clip = "off")+
  annotate("text", x = 0.5, y =-4.2, label = "0-14",size=4.5) +
  annotate("text", x = 1.5, y =-4.2, label = "15-64",size=4.5) +
  annotate("text", x = 2.5, y =-4.2, label = a,size=4.5) +
  annotate("text", x = -0.2, y =c(0,25,50,75,100), label =c(0,25,50,75,100),size=4.5) +
  annotate("text", x = -0.38, y =50, angle=90,label = "Proportion of cases (%)",size=5.5,fontface="bold") +
  annotate("text", x = 1.5, y =-9.2, angle=0,label = "Age (years)",size=5,fontface="bold") +
  annotate("text", x = -0.38, y =101, angle=0,label = "A",size=9,fontface="bold") +
  theme(legend.position = c(0.0,0.9),
        legend.direction='vertical',
        plot.margin = margin(0.5,1,1,0.4,unit="cm"),
        legend.justification = c(0,0),
        legend.background = element_blank(),
        legend.title = element_text(size = 11, face="bold"),
        panel.background = element_blank(),
        axis.text.x = element_text(colour="black",size=11),
        axis.text.y = element_blank(),
        axis.title.y = element_text(colour="black",size=12,face="bold"),
        legend.text =element_text(colour="black",size=11,lineheight=10),
        legend.text.align = 0,
        axis.line = element_line(colour = "black"),  
        legend.key=element_blank(),
        legend.box.background =element_blank(),
        legend.box.margin=  margin(0, 0, 0, 0, "cm"),
        panel.spacing = unit(5,"cm"))+guides(fill=guide_legend(title="",ncol = 5,title.position = "left"))+
  guides(fill = guide_legend(reverse = FALSE,ncol = 1,title = ""))

###travel-realted cases
age2<- read.csv("age_travel.csv")

ggplot(data=age2,aes(x=age-0.5,y=percent))+
  geom_bar(aes(fill=factor(age2$Travel,levels = c(1,2))),stat="identity",position = "dodge",colour="black",width = 0.7,size=0.5)+
  scale_fill_manual(values = c("#FDE9B4","#EA7C1C"),labels=c("Travel related cases (n=439)","Locally acquired cases (n=739)"))+
  scale_x_continuous(limits = c(-2,3),expand=c(0,0),breaks = seq(0,3,1),
                     label=c("","","",""),name="")+
  scale_y_continuous(limits=c(-10,115),breaks = c(seq(0,100,25)),expand=c(0,0),name="")+
  coord_cartesian(xlim=c(0,3),ylim = c(0,100), clip = "off")+
  annotate("text", x = 0.5, y =-4.2, label = "0-14",size=4.5) +
  annotate("text", x = 1.5, y =-4.2, label = "15-64",size=4.5) +
  annotate("text", x = 2.5, y =-4.2, label = a,size=4.5) +
  annotate("text", x = -0.2, y =c(0,25,50,75,100), label =c(0,25,50,75,100),size=4.5) +
  annotate("text", x = -0.38, y =50, angle=90,label = "Proportion of cases (%)",size=5.5,fontface="bold") +
  annotate("text", x = 1.5, y =-9.2, angle=0,label = "Age (years)",size=5,fontface="bold") +
  annotate("text", x = -0.38, y =101, angle=0,label = "B",size=9,fontface="bold") +
  theme(legend.position = c(0.0,0.9),
        legend.direction='vertical',
        plot.margin = margin(0.5,1,1,0.4,unit="cm"),
        legend.justification = c(0,0),
        legend.background = element_blank(),
        legend.title = element_text(size = 11, face="bold"),
        panel.background = element_blank(),
        axis.text.x = element_text(colour="black",size=11),
        axis.text.y = element_blank(),
        axis.title.y = element_text(colour="black",size=12,face="bold"),
        legend.text =element_text(colour="black",size=11,lineheight=10),
        legend.text.align = 0,
        axis.line = element_line(colour = "black"),  
        legend.key=element_blank(),
        legend.box.background =element_blank(),
        legend.box.margin=  margin(0, 0, 0, 0, "cm"),
        panel.spacing = unit(5,"cm"))+guides(fill=guide_legend(title="",ncol = 5,title.position = "left"))+
  guides(fill = guide_legend(reverse = FALSE,ncol = 1,title = ""))

###Spoadic cases
age3<- read.csv("Age_cluster.csv")

ggplot(data=age3,aes(x=age-0.5,y=percent))+
  geom_bar(aes(fill=factor(age3$cluster,levels = c(1,2))),stat="identity",position = "dodge",colour="black",width = 0.7,size=0.5)+
  scale_fill_manual(values = c("#FDE9B4","#EA7C1C"),labels=c("Sporadic/cluster index cases (n=489)","Cluster successively transmitted cases (n=357)"))+
  scale_x_continuous(limits = c(-2,3),expand=c(0,0),breaks = seq(0,3,1),
                     label=c("","","",""),name="")+
  scale_y_continuous(limits=c(-10,115),breaks = c(seq(0,100,25)),expand=c(0,0),name="")+
  coord_cartesian(xlim=c(0,3),ylim = c(0,100), clip = "off")+
  annotate("text", x = 0.5, y =-4.2, label = "0-14",size=4.5) +
  annotate("text", x = 1.5, y =-4.2, label = "15-64",size=4.5) +
  annotate("text", x = 2.5, y =-4.2, label = a,size=4.5) +
  annotate("text", x = -0.2, y =c(0,25,50,75,100), label =c(0,25,50,75,100),size=4.5) +
  annotate("text", x = -0.38, y =50, angle=90,label = "Proportion of cases (%)",size=5.5,fontface="bold") +
  annotate("text", x = 1.5, y =-9.2, angle=0,label = "Age (years)",size=5,fontface="bold") +
  annotate("text", x = -0.38, y =101, angle=0,label = "C",size=9,fontface="bold") +
  theme(legend.position = c(0.0,0.9),
        legend.direction='vertical',
        plot.margin = margin(0.5,1,1,0.4,unit="cm"),
        legend.justification = c(0,0),
        legend.background = element_blank(),
        legend.title = element_text(size = 11, face="bold"),
        panel.background = element_blank(),
        axis.text.x = element_text(colour="black",size=11),
        axis.text.y = element_blank(),
        axis.title.y = element_text(colour="black",size=12,face="bold"),
        legend.text =element_text(colour="black",size=11,lineheight=10),
        legend.text.align = 0,
        axis.line = element_line(colour = "black"),  
        legend.key=element_blank(),
        legend.box.background =element_blank(),
        legend.box.margin=  margin(0, 0, 0, 0, "cm"),
        panel.spacing = unit(5,"cm"))+guides(fill=guide_legend(title="",ncol = 5,title.position = "left"))+
  guides(fill = guide_legend(reverse = FALSE,ncol = 1,title = ""))

###Case and close contacts
age6<- read.csv("Age_clinic.csv")

ggplot(data=age6,aes(x=age-0.5,y=percent))+
  geom_bar(aes(fill=factor(age6$clinic,levels=c(1,2))),stat="identity",position = "dodge",colour="black",width = 0.7,size=0.5)+
  scale_fill_manual(values = c("#FDE9B4","#EA7C1C"),labels=c("Mild and general case-patients (n=869)","Severe and critical case-patients (n=150)"))+
  scale_x_continuous(limits = c(-2,3),expand=c(0,0),breaks = seq(0,3,1),
                     label=c("","","",""),name="")+
  scale_y_continuous(limits=c(-10,115),breaks = c(seq(0,100,25)),expand=c(0,0),name="")+
  coord_cartesian(xlim=c(0,3),ylim = c(0,100), clip = "off")+
  annotate("text", x = 0.5, y =-4.2, label = "0-14",size=4.5) +
  annotate("text", x = 1.5, y =-4.2, label = "15-64",size=4.5) +
  annotate("text", x = 2.5, y =-4.2, label = a,size=4.5) +
  annotate("text", x = -0.2, y =c(0,25,50,75,100), label =c(0,25,50,75,100),size=4.5) +
  annotate("text", x = -0.38, y =50, angle=90,label = "Proportion of cases (%)",size=5.5,fontface="bold") +
  annotate("text", x = 1.5, y =-9.2, angle=0,label = "Age (years)",size=5,fontface="bold") +
  annotate("text", x = -0.38, y =101, angle=0,label = "D",size=9,fontface="bold") +
  theme(legend.position = c(0.0,0.9),
        legend.direction='vertical',
        plot.margin = margin(0.5,1,1,0.4,unit="cm"),
        legend.justification = c(0,0),
        legend.background = element_blank(),
        legend.title = element_text(size = 11, face="bold"),
        panel.background = element_blank(),
        axis.text.x = element_text(colour="black",size=11),
        axis.text.y = element_blank(),
        axis.title.y = element_text(colour="black",size=12,face="bold"),
        legend.text =element_text(colour="black",size=11,lineheight=10),
        legend.text.align = 0,
        axis.line = element_line(colour = "black"),  
        legend.key=element_blank(),
        legend.box.background =element_blank(),
        legend.box.margin=  margin(0, 0, 0, 0, "cm"),
        panel.spacing = unit(5,"cm"))+guides(fill=guide_legend(title="",ncol = 5,title.position = "left"))+
  guides(fill = guide_legend(reverse = FALSE,ncol = 1,title = ""))

###Case and close contacts

age7<- read.csv("Age_close_contacts.csv")

ggplot(data=age7,aes(x=age7$age-0.5,y=percent))+
  geom_bar(aes(fill=factor(age7$group,levels=c(1,2))),stat="identity",position = "dodge",colour="black",width = 0.7,size=0.5)+
  scale_fill_manual(values = c("#FDE9B4","#EA7C1C"),labels=c("Close contacts of symptomatic cases (n=1175)",
                                                             "Close contacts of asymptomatic subjects (n=3802)"))+
  scale_x_continuous(limits = c(-2,3),expand=c(0,0),breaks = seq(0,3,1),
                     label=c("","","",""),name="")+
  scale_y_continuous(limits=c(-10,115),breaks = c(seq(0,100,25)),expand=c(0,0),name="")+
  coord_cartesian(xlim=c(0,3),ylim = c(0,100), clip = "off")+
  annotate("text", x = 0.5, y =-4.2, label = "0-14",size=4.5) +
  annotate("text", x = 1.5, y =-4.2, label = "15-64",size=4.5) +
  annotate("text", x = 2.5, y =-4.2, label = a,size=4.5) +
  annotate("text", x = -0.2, y =c(0,25,50,75,100), label =c(0,25,50,75,100),size=4.5) +
  annotate("text", x = -0.38, y =50, angle=90,label = "Proportion of cases (%)",size=5.5,fontface="bold") +
  annotate("text", x = 1.5, y =-9.2, angle=0,label = "Age (years)",size=5,fontface="bold") +
  annotate("text", x = -0.38, y =101, angle=0,label = "E",size=9,fontface="bold") +
  theme(legend.position = c(0.0,0.9),
        legend.direction='vertical',
        plot.margin = margin(0.5,1,1,0.4,unit="cm"),
        legend.justification = c(0,0),
        legend.background = element_blank(),
        legend.title = element_text(size = 11, face="bold"),
        panel.background = element_blank(),
        axis.text.x = element_text(colour="black",size=11),
        axis.text.y = element_blank(),
        axis.title.y = element_text(colour="black",size=12,face="bold"),
        legend.text =element_text(colour="black",size=11,lineheight=10),
        legend.text.align = 0,
        axis.line = element_line(colour = "black"),  
        legend.key=element_blank(),
        legend.box.background =element_blank(),
        legend.box.margin=  margin(0, 0, 0, 0, "cm"),
        panel.spacing = unit(5,"cm"))+guides(fill=guide_legend(title="",ncol = 5,title.position = "left"))+
  guides(fill = guide_legend(reverse = FALSE,ncol = 1,title = ""))

### Common exposure
a<-expression("">= 65)

age8<- read.csv("Age_common.csv")

ggplot(data=age8,aes(x=age-0.5,y=percent))+
  geom_bar(aes(fill=factor(age8$common,levels=c(2,1))),stat="identity",position = "dodge",colour="black",width = 0.7,size=0.5)+
  scale_fill_manual(values = c("#FDE9B4","#EA7C1C"),labels=c("Shared same exposure (n=332)","Human-to-human transmission (n=357)"))+
  scale_x_continuous(limits = c(-2,3),expand=c(0,0),breaks = seq(0,3,1),
                     label=c("","","",""),name="")+
  scale_y_continuous(limits=c(-10,115),breaks = c(seq(0,100,25)),expand=c(0,0),name="")+
  coord_cartesian(xlim=c(0,3),ylim = c(0,100), clip = "off")+
  annotate("text", x = 0.5, y =-4.2, label = "0-14",size=4.5) +
  annotate("text", x = 1.5, y =-4.2, label = "15-64",size=4.5) +
  annotate("text", x = 2.5, y =-4.2, label = a,size=4.5) +
  annotate("text", x = -0.2, y =c(0,25,50,75,100), label =c(0,25,50,75,100),size=4.5) +
  annotate("text", x = -0.38, y =50, angle=90,label = "Proportion of cases (%)",size=5.5,fontface="bold") +
  annotate("text", x = 1.5, y =-9.2, angle=0,label = "Age (years)",size=5,fontface="bold") +
  annotate("text", x = -0.38, y =101, angle=0,label = "F",size=9,fontface="bold") +
  theme(legend.position = c(0.0,0.9),
        legend.direction='vertical',
        plot.margin = margin(0.5,1,1,0.4,unit="cm"),
        legend.justification = c(0,0),
        legend.background = element_blank(),
        legend.title = element_text(size = 11, face="bold"),
        panel.background = element_blank(),
        axis.text.x = element_text(colour="black",size=11),
        axis.text.y = element_blank(),
        axis.title.y = element_text(colour="black",size=12,face="bold"),
        legend.text =element_text(colour="black",size=11,lineheight=10),
        legend.text.align = 0,
        axis.line = element_line(colour = "black"),  
        legend.key=element_blank(),
        legend.box.background =element_blank(),
        legend.box.margin=  margin(0, 0, 0, 0, "cm"),
        panel.spacing = unit(5,"cm"))+guides(fill=guide_legend(title="",ncol = 5,title.position = "left"))+
  guides(fill = guide_legend(reverse = FALSE,ncol = 1,title = ""))
