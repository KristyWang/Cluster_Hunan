# Code to reproduce Fig.1
#panel a
library(openxlsx)
library(ggplot2)
setwd(".../1. input/")
Travel_Wuhan<-read.xlsx("data_Figure_1.xlsx",sheet=1)
color_a<-c("#f46d2b","#fcba69","#fbea9c")

ggplot() +
  geom_bar(data = Travel_Wuhan, aes(x= x , y = total, fill = type_a),
           stat = "identity",position = "stack",color="black",width=1)+
  scale_x_continuous(name ="",expand=c(0,-4),limits = c(-4,64), breaks = seq(0,60,2),
                     labels = c( "1","4","7","10","13","16","19","22","25","28","31",
                                 "3","6","9","12","15","18","21","24","27","1","4","7",
                                 "10","13","16","19","22","25","28","31"))+
  scale_y_continuous(name = "No. of lab-confirmed cases",expand=c(0,0),limits = c(-14,100),breaks = seq(0,100,20))+
  scale_fill_manual(breaks=c(1,2,3),values = color_a,name="",labels = c("Individuals with asymptomatic infection",
                                                                        "Locally-acquired cases","Travel-related cases"))+
  
  geom_rect(aes(xmin=42, xmax=43.1, ymin=91.1, ymax=95.9), fill = "#f46d2b",  color="black")+
  geom_rect(aes(xmin=42, xmax=43.1, ymin=85.1, ymax=89.9), fill = "#fcba69",  color="black")+
  geom_rect(aes(xmin=42, xmax=43.1, ymin=79.1, ymax=83.9), fill = "#fbea9c",  color="black")+
  annotate("text", x = 52.2, y =93.5, label = "Individuals with asymptomatic infection" ,size= 6.5)+
  annotate("text", x = 48.95, y =87.5, label = "Locally-acquired cases" ,size= 6.5)+
  annotate("text", x = 48.4, y =81.5, label = "Travel-related cases" ,size= 6.5)+
  
  coord_cartesian(ylim = c(0,100), clip = "off")+
  
  annotate("text", x = c(10,30,50), y =-9, label = c("Jan 2020","Feb 2020","Mar 2020"),size= 8)+
  annotate("text", x = -3.5, y =100, label = "A.",size= 10, fontface = "bold")+
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

#panel b
plotvar = values(Hunanhubei_mask)
classRaster = classIntervals(plotvar, length(myBreaks), style="fixed",fixedBreaks = myBreaks) 
colcodeRaster <- findColours(classRaster, mycols)
mylgnamesRaster = names(attr(colcodeRaster, "table"))
mylgnamesRaster = c("","1","","5","","","50","","500")
mylgcoloursRaster = attr(colcodeRaster, "palette")

plot(Hunanhubei_mask, lwd = 0.5, breaks = myBreaks, col = mycols, xpd = T, useRaster = T, 
     xlim =  c(108.2,117.6), ylim =  c(24.5,33.3), axes = FALSE, legend= F, box = F, maxpixels = Inf)
plot(hunanhubei_city,  cex=1, lwd=0.5, xlim = c(108.2,117.6), ylim = c(24.5,33.3),  color = "transparent", border = "#202789", add = T)
plot(map_hunanhubei,  cex=1, lwd=1.5, xlim = c(108.2,117.6), ylim = c(24.5,33.3),  color="transparent", add = T)
plot(maphunan_hubei_jiaojiexian, xlim = c(108.2,117.6), ylim = c(24.5,33.3), cex=1, lwd=3, border = "black", color="transparent",axes = FALSE,  add=T)
points(hunan_data$long1,hunan_data$lat1, pch = 16, col = rgb(0,0,0,0.15), cex = hunan_data$LCase, axes = FALSE, add = T) #cex = sqrt(hunan_data$Case)/2
points(hubei_data$long,hubei_data$lat, pch = 16, col = rgb(0,0,0,0.15), cex = hubei_data$Lcase, axes = FALSE, add = T)
grid(nx = NA)
x = rep(115.1,4)
y = c(24.8,25,25.4,25.9)
z = c(1.8,4.0,6.9,10.8)
points(x,y, pch = 16, cex = z, col = rgb(0,0,0,0.15), add = T)  
grid(nx = NA)
text(x+0.5,y,c("6","56","972","50000"), pos = 4, cex=1, add = T)
text(x = 110.9421773900, y = 28.1495069700, c("Hunan"), pos = 3, cex=1.5, add = T, col="black")
text(x = 111.3134584200, y = 31.0658071300, c("Hubei"), pos = 3, cex=1.5, add = T, col="black")
text(114.2,26.8,c("Number of cases"), pos = 4, cex=1, add = T)
legend(x=114.2,y =28.55, title="Population density", title.adj = 0,#"bottomright"
       legend=mylgnamesRaster, fill=mylgcoloursRaster, 
       cex=1, bty="n", xjust=0, y.intersp=0.5, xpd = T)

#panel c
mycol_b = c("#FFFFE9","#feefa7","#FDD88E","#FCC275","#FBAC5C","#FB9643","#F67931","#BB001A")

classcol_b_case = classIntervals(map_hunan_xian$panel_b, 8, style="quantile") 
col_b <- findColours(classcol_b_case, mycol_b)
map_hunan_xian<-cbind(map_hunan_xian, col_b)
mylgnames_b_panel = names(attr(col_b, "table"))
mylgnames_b_panel = c("0","1","2","4","6","8","14","116")
mylgcolor_b_panel = attr(col_b, "palette")

plot(map_hunan_xian$geometry, lwd = 0.5, col = col_b, breaks=classcol_b_case,
     xpd = T, xlim =  c(108.7,114.3), ylim =  c(24.6,30.2), axes = FALSE, 
     legend= F, box = F)  
plot(map_hunan,  cex=1, lwd=0.5, xlim = c(107.0,114.6), ylim = c(24.5,30.3),
     color="transparent", add = T)
plot(map_hunan_sheng,  cex=1, lwd=1.5, xlim = c(107.0,114.6), ylim = c(24.5,30.3),
     color="transparent", add = T)
points(hunan_index$longitude, y=hunan_index$latitude, pch = 16, col = rgb(0,0,1,1),
       axes = FALSE, add = T,cex=0.7) #cex = sqrt(hunan_data$Case)/2
grid(nx = NA)
points(108.25,24.62, pch = 16, cex = 1, col = c(rgb(0,0,1,1)), add = T)  
grid(nx = NA)
text(108.2+0.15,24.6,c("Clusters of COVID-19"), pos = 4, cex=1, add = T)
legend(x=108.2,y =25.9, title = "No. of COVID-19 cases",title.adj = 0,#"bottomright"
       legend=mylgnames_b_panel, fill=mylgcolor_b_panel, 
       cex=1, bty="n", xjust=0, y.intersp=0.5, xpd = T)

### Panel D
setwd(".../1. input/Figure S4")
age5<- read.csv("Age_case_count.csv")
a<-expression("">= 65)

ggplot(data=age5,aes(x=age-0.5,y=percent))+
  geom_bar(aes(fill=factor(case,levels=c(1,2))),stat="identity",position = "dodge",colour="black",width = 0.7,size=0.5)+
  scale_fill_manual(values = c("#FDE9B4","#EA7C1C"),labels=c("Symptoamtic cases","Asymptomatic subjects"))+
  scale_x_continuous(limits = c(-2,7),expand=c(0,0),breaks = seq(0,7,1),
                     label=c("","","","","","","",""),name="")+
  scale_y_continuous(limits=c(-42,250),breaks = c(seq(0,250,50)),expand=c(0,0),name="")+
  coord_cartesian(xlim=c(0,7),ylim = c(0,250), clip = "off")+
  annotate("text", x = 0.5, y =-13, label = "0-14",size=4.5) +
  annotate("text", x = 1.5, y =-13, label = "15-24",size=4.5) +
  annotate("text", x = 2.5, y =-13, label = "25-34",size=4.5) +
  annotate("text", x = 3.5, y =-13, label = "35-44",size=4.5) +
  annotate("text", x = 4.5, y =-13, label = "45-54",size=4.5) +
  annotate("text", x = 5.5, y =-13, label = "55-64",size=4.5) +
  annotate("text", x = 6.5, y =-13, label = a,size=4.5) +
  annotate("text", x = -0.2, y =c(0,50,100,150,200,250), label =c(0,50,100,150,200,250),size=4.5) +
  annotate("text", x = -0.5, y =125, angle=90,label = "Proportion of cases (%)",size=5.5,fontface="bold") +
  annotate("text", x = 3.5, y =-28, angle=0,label = "Age (years)",size=5,fontface="bold") +
  theme(legend.position = c(0.0,0.87),
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
