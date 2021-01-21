# Code to reproduce Fig.S6
library(openxlsx)
library(ggplot2)
setwd(".../1. input/")
risk_matrix<-read.xlsx("data_Figure_S6.xlsx",sheet=1)

## panel a
ggplot(data = risk_matrix, aes(x=as.numeric(risk_matrix$Age_contact), y=as.numeric(risk_matrix$Age_index)))+ 
  geom_tile(aes(fill=as.numeric(risk_matrix$case)))+
  scale_y_continuous(expand=c(0,0),limits = c(-1,13),breaks =seq(1,12),
                     labels = c("","","","", "","","", "","","","",""),name="")+
  scale_x_continuous(expand=c(0,0),limits = c(-1,13),breaks = seq(1,12),
                     labels = c("","","","", "","","", "","","","",""),name="")+
  scale_fill_gradientn(colours = brewer.pal(9,'Oranges'),
                       breaks=c(0,2,4,6,8),
                       labels=c(0,2,4,6,"8+"),
                       limits=c(0,8),
                       name="No. of infections")+
  coord_cartesian(ylim = c(0.5,12.5),xlim=c(0.5,12.5), clip = "off")+
  annotate("text",x=.08,y=12,label="65+",size=4)+
  annotate("text",x=.1,y=11,label="60",size=4)+
  annotate("text",x=.1,y=10,label="55",size=4)+
  annotate("text",x=.1,y=9,label="50",size=4)+
  annotate("text",x=.1,y=8,label="45",size=4)+
  annotate("text",x=.1,y=7,label="40",size=4)+
  annotate("text",x=.1,y=6,label="35",size=4)+
  annotate("text",x=.1,y=5,label="30",size=4)+
  annotate("text",x=.1,y=4,label="25",size=4)+
  annotate("text",x=.1,y=3,label="20",size=4)+
  annotate("text",x=.1,y=2,label="15",size=4)+
  annotate("text",x=.1,y=1,label="0",size=4)+
  annotate("text",x=1,y=.15,label="0",size=4)+
  annotate("text",x=2,y=.15,label="15",size=4)+
  annotate("text",x=3,y=.15,label="20",size=4)+
  annotate("text",x=4,y=.15,label="25",size=4)+
  annotate("text",x=5,y=.15,label="30",size=4)+
  annotate("text",x=6,y=.15,label="35",size=4)+
  annotate("text",x=7,y=.15,label="40",size=4)+
  annotate("text",x=8,y=.15,label="45",size=4)+
  annotate("text",x=9,y=.15,label="50",size=4)+
  annotate("text",x=10,y=.15,label="55",size=4)+
  annotate("text",x=11,y=.15,label="60",size=4)+
  annotate("text",x=12,y=.15,label="65+",size=4)+
  annotate("text",x=6.5,y=-0.4,label="Age of infectees",size=5.5,fontface="bold")+
  annotate("text",x=-0.6,y=6.1,label="Age of infectors",size=5.5,fontface="bold",angle=90)+
  annotate("text",x=-0.6,y=12.5,label="A.",size=6,fontface="bold",angle=0)+
  theme_bw()+
  theme(legend.position = "right",
        plot.margin = margin(0.5,1,1,1.2,unit="cm"),
        panel.background = element_rect(colour = "black"),
        panel.border = element_rect(colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.line.x.top = element_line(colour = "black"),
        axis.line.y.right = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11))

## panel b
ggplot(data = risk_matrix, aes(x=as.numeric(risk_matrix$Age_contact), y=as.numeric(risk_matrix$Age_index)))+ 
  geom_tile(aes(fill=as.numeric(risk_matrix$mean)))+
  scale_y_continuous(expand=c(0,0),limits = c(-1,13),breaks =seq(1,12),
                     labels = c("","","","", "","","", "","","","",""),name="")+
  scale_x_continuous(expand=c(0,0),limits = c(-1,13),breaks = seq(1,12),
                     labels = c("","","","", "","","", "","","","",""),name="")+
  scale_fill_gradientn(colours = brewer.pal(9,'Oranges'),
                       breaks=c(0,0.2,0.4,0.6,0.8),
                       labels=c(0,0.2,0.4,0.6,0.8),
                       limits=c(0,0.8),
                       name="Mean no. of infections")+
  coord_cartesian(ylim = c(0.5,12.5),xlim=c(0.5,12.5), clip = "off")+
  annotate("text",x=.08,y=12,label="65+",size=4)+
  annotate("text",x=.1,y=11,label="60",size=4)+
  annotate("text",x=.1,y=10,label="55",size=4)+
  annotate("text",x=.1,y=9,label="50",size=4)+
  annotate("text",x=.1,y=8,label="45",size=4)+
  annotate("text",x=.1,y=7,label="40",size=4)+
  annotate("text",x=.1,y=6,label="35",size=4)+
  annotate("text",x=.1,y=5,label="30",size=4)+
  annotate("text",x=.1,y=4,label="25",size=4)+
  annotate("text",x=.1,y=3,label="20",size=4)+
  annotate("text",x=.1,y=2,label="15",size=4)+
  annotate("text",x=.1,y=1,label="0",size=4)+
  annotate("text",x=1,y=.15,label="0",size=4)+
  annotate("text",x=2,y=.15,label="15",size=4)+
  annotate("text",x=3,y=.15,label="20",size=4)+
  annotate("text",x=4,y=.15,label="25",size=4)+
  annotate("text",x=5,y=.15,label="30",size=4)+
  annotate("text",x=6,y=.15,label="35",size=4)+
  annotate("text",x=7,y=.15,label="40",size=4)+
  annotate("text",x=8,y=.15,label="45",size=4)+
  annotate("text",x=9,y=.15,label="50",size=4)+
  annotate("text",x=10,y=.15,label="55",size=4)+
  annotate("text",x=11,y=.15,label="60",size=4)+
  annotate("text",x=12,y=.15,label="65+",size=4)+
  annotate("text",x=6.5,y=-0.4,label="Age of infectees",size=5.5,fontface="bold")+
  annotate("text",x=-0.6,y=6.1,label="Age of infectors",size=5.5,fontface="bold",angle=90)+
  annotate("text",x=-0.6,y=12.5,label="B.",size=6,fontface="bold",angle=0)+
  theme_bw()+
  theme(legend.position = "right",
        plot.margin = margin(0.5,0,1,1.2,unit="cm"),
        panel.background = element_rect(colour = "black"),
        panel.border = element_rect(colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.line.x.top = element_line(colour = "black"),
        axis.line.y.right = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11))

