# Code to reproduce Fig.S2
library(openxlsx)
library(ggplot2)
setwd(".../1. input/")
cluster<-read.xlsx("data_Figure_S2.xlsx",sheet=1)

ggplot() + 
  geom_bar(data = cluster, aes(x=x ,y=total,fill=type_b), 
           stat = "identity",position = "stack",color="black",width=1)+
  scale_x_continuous(name ="",expand=c(0,-4),limits = c(-4,64), breaks = seq(0,60,2),
                     labels = c( "1","4","7","10","13","16","19","22","25","28","31",
                                 "3","6","9","12","15","18","21","24","27","1","4","7",
                                 "10","13","16","19","22","25","28","31"))+
  scale_y_continuous(name = "No. of lab-confirmed cases",expand=c(0,0),limits = c(-14,100),breaks = seq(0,100,20))+
  scale_fill_manual(breaks=c(1,2,3),values = c("#f46d2b","#fcba69","#fbea9c"),name="",labels = c("Individuals with asymptomatic infection",
                                                                        "Clustering cases","Sporadic cases"))+
  geom_rect(aes(xmin=42, xmax=43.1, ymin=91.1, ymax=95.9), fill = "#f46d2b",  color="black")+
  geom_rect(aes(xmin=42, xmax=43.1, ymin=85.1, ymax=89.9), fill = "#fcba69",  color="black")+
  geom_rect(aes(xmin=42, xmax=43.1, ymin=79.1, ymax=83.9), fill = "#fbea9c",  color="black")+
  annotate("text", x = 52.2, y =93.5, label = "Individuals with asymptomatic infection" ,size= 6.5)+
  annotate("text", x = 47.55, y =87.5, label = "Clustering cases" ,size= 6.5)+
  annotate("text", x = 47.3, y =81.5, label = "Sporadic cases" ,size= 6.5)+
  
  coord_cartesian(ylim = c(0,100), clip = "off")+
  
  annotate("text", x = c(10,30,50), y =-9, label = c("Jan 2020","Feb 2020","Mar 2020"),size= 8)+
  annotate("text", x = -3.5, y =100, label = "B.",size= 10, fontface = "bold")+
  annotate("text", x = 30, y =-14, label = "Date of symptom onset",size= 8.5, fontface = "bold")+
  
  theme(legend.justification=c(0,0),
        legend.position="none",
        legend.background = element_blank(),
        legend.key.size = unit(20, "pt"),
        legend.key.height = unit(20, "pt"),
        legend.key.width = unit(20, "pt"),
        legend.text = element_text(size = 20),
        legend.spacing.x  = unit(0.3, "cm"),
        axis.line = element_line(colour = "black",size=0.4),
        axis.ticks = element_line(colour = "black",size=0.4),
        axis.ticks.length = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 0, size=20, color = "black"),
        axis.title.x = element_text(size = 24, color = "black", vjust = -2.5, hjust = 0.5,face = "bold"),
        axis.text.y = element_text(size = 20,  color = "black", vjust = 0.5, hjust = 0.5),
        axis.title.y = element_text(size = 24, color = "black", vjust = 3, hjust = 0.5,face = "bold"),
        plot.margin =  margin(1,1,1,1, "cm"),#(up,right,down,left)
        panel.background = element_rect(fill = "white"),
        panel.spacing = unit(6,"cm"))
