# Code to reproduce Fig.S3

#panel a
mycols_c = c(rgb(255,255,233,max = 255),"#FEE59B","#FDC272","#F88639", rgb(246,121,49,max = 255),
             rgb(239,76,36,max = 255),rgb(239,76,36,max = 255), rgb(226,26,27,max = 255), 
             rgb(143,0,29,max = 255))
plotvar_c = values(Hunan_mask)
classRaster_c = classIntervals(plotvar_c, length(myBreaks), style="fixed",fixedBreaks = myBreaks) 
colcodeRaster_c <- findColours(classRaster_c, mycols_c)
mylgnamesRaster_c = names(attr(colcodeRaster_c, "table"))
mylgnamesRaster_c = c("","1","","5","","","50","","500")
mylgcoloursRaster_c = attr(colcodeRaster_c, "palette")

hunan_sporadic_c<-panel_c_d_latlong[(which(panel_c_d_latlong$type_c==1)),]
hunan_cluster_c<-panel_c_d_latlong[(which(panel_c_d_latlong$type_c==2)),]
hunan_asymptomatic_c<-panel_c_d_latlong[(which(panel_c_d_latlong$type_c==3)),]

plot(Hunan_mask, lwd = 0.5, col = mycols_c, breaks = myBreaks, xpd = T, useRaster = T, 
     xlim =  c(107.0,114.3), ylim =  c(24.6,30.2), axes = FALSE, legend= F, box = F, maxpixels = Inf)  
plot(hunan_city,  cex=1, lwd=0.5, xlim = c(107.0,114.6), ylim = c(24.5,30.3),  color = "transparent", border = "#202789", add = T)
plot(map_hunan,  cex=1, lwd=1.5, xlim = c(107.0,114.6), ylim = c(24.5,30.3),  color="transparent", add = T)
points(hunan_sporadic_c$longitude,hunan_sporadic_c$latitude, pch = 16, col = rgb(0,1,0,1), axes = FALSE, add = T,cex=0.7) #cex = sqrt(hunan_data$Case)/2
points(hunan_cluster_c$longitude,hunan_cluster_c$latitude, pch = 16, col = rgb(0,0,1,1), axes = FALSE, add = T, cex=0.7)
points(hunan_asymptomatic_c$longitude,hunan_asymptomatic_c$latitude, pch = 16, col = rgb(0,1,1,1), cex=0.7,axes = FALSE, add = T)
grid(nx = NA)
x = rep(108.9,3)
y = c(25.2,25,24.8)
points(x,y, pch = 16, cex = 1, col = c(rgb(0,1,0,1),rgb(0,0,1,1),rgb(0,1,1,1)), add = T)  
grid(nx = NA)
text(x+0.25,y,c("Sporadic cases","Clustering cases","Individuals with asymptomatic infections"), pos = 4, cex=1, add = T)
text(108.9,25.4,c("Type of cases"),font=2, pos = 4, cex=1, add = T)
legend(x=108.65,y =26.9, title = "Population\ndensity",title.adj = 0,
       legend=mylgnamesRaster_c, fill=mylgcoloursRaster_c, 
       cex=1, bty="n", xjust=0, y.intersp=0.5, xpd = T)

#panel b

mycol_d = c(rgb(255,255,233,max = 255),"#feefa7","#FDD88E","#FCC275","#FBAC5C","#F8873A","#D9432A","#BB001A")
classcol_d_case = classIntervals(map_hunan_xian$panel_c, 8, style="quantile") 
col_d <- findColours(classcol_d_case, mycol_d)
map_hunan_xian<-cbind(map_hunan_xian, col_d)

mylgnames_d_panel = names(attr(col_d, "table"))
mylgnames_d_panel = c("0","2","3","5","8","11","21","199")
mylgcolor_d_panel = attr(col_d, "palette")

panel_c_d_latlong_hth<-panel_c_d_latlong[which(panel_c_d_latlong$type_d==1),]
panel_c_d_latlong_sme<-panel_c_d_latlong[which(panel_c_d_latlong$type_d==2),]

panel_c_d_latlong_hth_case<-panel_c_d_latlong_hth[which(panel_c_d_latlong_hth$type_d_1==1),]
panel_c_d_latlong_hth_asymp<-panel_c_d_latlong_hth[which(panel_c_d_latlong_hth$type_d_1==2),]
panel_c_d_latlong_sme_case<-panel_c_d_latlong_sme[which(panel_c_d_latlong_sme$type_d_1==1),]
panel_c_d_latlong_sme_asymp<-panel_c_d_latlong_sme[which(panel_c_d_latlong_sme$type_d_1==2),]

plot(Hunan_mask, lwd = 0.5, col = mycols_c, breaks = myBreaks, xpd = T, useRaster = T, 
     xlim =  c(107.0,114.3), ylim =  c(24.6,30.2), axes = FALSE, legend= F, box = F, maxpixels = Inf)  
plot(hunan_city,  cex=1, lwd=0.5, xlim = c(107.0,114.6), 
     ylim = c(24.5,30.3),  color = "transparent", border = "#202789", add = T)
plot(map_hunan,  cex=1, lwd=1.5, xlim = c(107.0,114.6), ylim = c(24.5,30.3),  
     color="transparent", add = T)
points(x=(panel_c_d_latlong_hth_case$longitude+rnorm(311,mean=0,sd=0.02)+runif(311,min=0,max=0.03)), 
       y=(panel_c_d_latlong_hth_case$latitude+rnorm(311,mean=0,sd=0.02)+runif(311,min=0,max=0.03)), 
       pch = 16, col=rgb(0,0,1,1), axes = FALSE, add = T,cex=0.6,xpd = T)
points(x=(panel_c_d_latlong_sme_case$longitude+rnorm(197,mean=0,sd=0.015)+runif(197,min=0,max=0.03)),
       y=(panel_c_d_latlong_sme_case$latitude+rnorm(197,mean=0,sd=0.02)+runif(197,min=0,max=0.025)),
       pch = 16, col="#23ff06",axes = FALSE, add = T,cex=0.6, xpd = T) 
points(x=(panel_c_d_latlong_hth_asymp$longitude+rnorm(59,mean=0,sd=0.03)+runif(59,min=0,max=0.02)),
       y=(panel_c_d_latlong_hth_asymp$latitude+rnorm(59,mean=0,sd=0.03)+runif(59,min=0,max=0.02)), 
       col="#06d4ff", axes = FALSE, add = T,cex=0.6,xpd = T,pch=16) 
points(x=(panel_c_d_latlong_sme_asymp$longitude+rnorm(21,mean=0,sd=0.025)+runif(21,min=0,max=0.02)),
       y=(panel_c_d_latlong_sme_asymp$latitude+rnorm(21,mean=0,sd=0.025)+runif(21,min=0,max=0.02)), 
       col="#1f845d", axes = FALSE, add = T,cex=0.6, xpd = T,pch=16) 
grid(nx = NA)
x = rep(108.6,4)
y = c(25.2,25.05,24.70,24.55)
points(x,y, pch = 16, cex = 1, col = c(rgb(0,0,1,1),"#06d4ff","#23ff06","#1f845d"), add = T)  
grid(nx = NA)
text(x+0.05,y,c("Clusters with human-to-human transmission","Clusters shared same exposure",
                "Clusters with human-to-human transmission","Clusters shared same exposure"), 
     pos = 4,font=2, cex=1, add = T,  xpd = T)
x1 = rep(108.4,2)
y1 = c(25.4,24.9)
text(x1,y1,c("1.COVID-19 cases","2.Individuals with asymptomatic infections"),
     font=2, pos = 4, cex=1, add = T, xpd = T)
text(108.4,25.6,c("Type of cases"),font=2, pos = 4, cex=1, add = T, xpd = T)
legend(x=108.5,y =26.9, title = "Population density\n(persons/km2))",title.adj = 0,
       legend=mylgnamesRaster_c, fill=mylgcoloursRaster_c, 
       cex=1, bty="n", xjust=0, y.intersp=0.5, xpd = T)

#panel C
contact_e_point_symp<-contact_e_point[which(contact_e_point$whe.sym==1),]
contact_e_point_asymp<-contact_e_point[which(contact_e_point$whe.sym==2),]

plot(Hunan_mask, lwd = 0.5, col = mycols_c, breaks = myBreaks, xpd = T, useRaster = T, 
     xlim =  c(107.0,114.3), ylim =  c(24.6,30.2), axes = FALSE, legend= F, box = F, maxpixels = Inf)  
plot(hunan_city,  cex=1, lwd=0.5, xlim = c(107.0,114.6), ylim = c(24.5,30.3),  color = "transparent", border = "#202789", add = T,xpd = T)
plot(map_hunan,  cex=1, lwd=1.5, xlim = c(107.0,114.6), ylim = c(24.5,30.3),  color="transparent", add = T)
points(x=contact_e$longitude,y=contact_e$latitude,pch = 16, col = rgb(0,0,0,0.15), cex = log(contact_e$panel_e_new), axes = FALSE, add = T)
points(contact_e_point_symp$longitude+rnorm(285,mean=0,sd=0.05)+runif(285,min=-0.05,max=0.05),
       contact_e_point_symp$latitude+rnorm(285,mean=0,sd=0.05)+runif(285,min=-0.05,max=0.05), 
       xpd = T,pch = 16, col = rgb(0,0,1,0.5), axes = FALSE, add = T,cex=0.8) #cex = sqrt(hunan_data$Case)/2
points(contact_e_point_asymp$longitude+rnorm(285,mean=0,sd=0.05)+runif(285,min=-0.05,max=0.05),
       contact_e_point_asymp$latitude+rnorm(285,mean=0,sd=0.05)+runif(285,min=-0.05,max=0.05), 
       xpd = T,pch = 16, col = rgb(0,1,1,0.5), axes = FALSE, add = T,cex=0.8) #cex = sqrt(hunan_data$Case)/2
grid(nx = NA)
points(108.7,25.4, pch = 16, cex = 1, col = c(rgb(0,0,1,0.5)), xpd = T,add = T)
points(108.7,25.2, pch = 16, cex = 1, col = c(rgb(0,1,1,0.5)), xpd = T,add = T)
x1 = rep(109.2,4)
y1 = c(24.6,24.7,24.8,24.9)
z<-c(2.74,4.11,5.26,7.0)
points(x1,y1,cex = z , col=rgb(0,0,0,0.15), xpd = T,add = T, pch=16)
grid(nx = NA)
text(x+0.05,y,c("Close contacts with SARS-CoV-2 infections"),xpd = T,pos = 4, cex=1, add = T)
text(x1+0.15,c(24.6,24.7,24.8,24.9),c("15","61", "192", "1102"),xpd = T,font=2, pos = 4, cex=1, add = T)
legend(x=108.5,y =26.9, title = "Population density\n(persons/km2))",title.adj = 0,
       legend=mylgnamesRaster_c, fill=mylgcoloursRaster_c, 
       cex=1, bty="n", xjust=0, y.intersp=0.5, xpd = T)
