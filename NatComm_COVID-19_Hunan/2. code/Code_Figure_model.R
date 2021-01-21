library(dplyr)
library(tidyr)
library(reshape2)
library(openxlsx)
library(lme4)
library(R2admb)
library(coefplot)
library(GLMMadaptive)
library(DHARMa)
library(broom.mixed)
library(optimx) 
library(ggeffects)
library(splines)

setwd(".../1. input/")
mock_data<-read.csv("data_model.csv")

# Code to reproduce Table 2, Table S8-12
# model specifications: selecting different explanatory variables in each model
model= glmer(Infectee ~ logage_index+
               logage +
               contact_type1 +
               relevel(generation_y,ref="1")+
               no.persons+
               relevel(gender_index,ref="2")+ 
               relevel(gender,ref="2")+ 
               relevel(clinic_index2,ref="1") +
               (1|id_case)+
               (1|cluster_id)
             ,family = binomial(link = "logit"), data = mock_data,
             control=glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))

result<-tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")

## Code to reproduce Table S12
anova(model,model.x)

## Code to reproduce Figure S7
simulationOutput <- simulateResiduals(fittedModel = model,n=3000)
plot(simulationOutput,quantreg = T,asFactor = F)
mtext(side =3,at=-0.27,text="B.", line = 14,cex=2,font = 2)


# Code to reproduce Fig. S8
## Panel a
Data<-matrix(NA,2,4)
Data[1:2,1]<-c(1,2)
Data[1:2,2]<-result$estimate[4:5]
Data[1:2,3]<-result$conf.low[4:5]
Data[1:2,4]<-result$conf.high[4:5]

Data<-as.data.frame(Data)
colnames(Data)<-c("group","mean","low","up")

a<-expression("">= 65~"yrs")

ggplot(data=Data) +
  annotate("rect", xmin = Data$group - 0.3, xmax = Data$group + 0.3,
           ymin = Data$low, ymax = Data$up, fill = "white", color = "black") +
  annotate("segment", x = Data$group - 0.3, xend = Data$group + 0.3, 
           y = Data$mean, yend = Data$mean, size = 1.2,color = "#2d66a4")+
  scale_x_discrete(expand=c(0,0),limits=c(1,2),breaks=c(1,2),labels=c("",""),name="")+
  scale_y_continuous(expand=c(0,0),limits=c(-2,5.5),breaks = c(0,1,2,3,4,5),name="")+
  coord_cartesian(ylim = c(0.0,3),xlim=c(0.5,2.5), clip = "off")+
  annotate("text",x=1,y=-0.23,label="<15 yrs",size=4,angle = 0)+
  annotate("text",x=2,y=-0.23,label=a,size=4,angle = 0)+
  annotate("text",x=0.41,y=0,label="0.0",size=4,angle = 0)+
  annotate("text",x=0.41,y=2,label="2.0",size=4,angle = 0)+
  annotate("text",x=0.41,y=3,label="3.0",size=4,angle = 0)+
  annotate("text",x=0.41,y=4,label="4.0",size=4,angle = 0)+
  annotate("text",x=0.41,y=1,label="1.0",size=4,angle = 0)+
  annotate("text",x=0.41,y=5,label="5.0",size=4,angle = 0)+
  annotate("text",x=0.26,y=1.5,label="Relative susceptibility",size=4.5,angle = 90,fontface="bold")+
  annotate("text",x=1.5,y=-0.55,label="Age of contacts",size=4.5,angle = 0,fontface="bold")+
  geom_hline(yintercept=1.0,linetype="dashed",color="#d6604d",size=1)+
  theme_bw()+
  theme(legend.position = c(0.16,0.9),
        plot.margin = margin(0.7,0.3,1,1.2,unit="cm"),
        panel.background = element_rect(colour = "white"),
        panel.border = element_rect(colour = "white"),
        axis.line = element_line(colour = "black",size =0.45),
        axis.text = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 9),
        legend.title =element_blank())+
  guides(fill = FALSE,size=FALSE)


## Panel b
Data<-matrix(NA,3,4)
Data[1:3,1]<-c(1,2,3)
Data[1:3,2]<-result$estimate[6:8]
Data[1:3,3]<-result$conf.low[6:8]
Data[1:3,4]<-result$conf.high[6:8]
Data<-as.data.frame(Data)
colnames(Data)<-c("group","mean","low","up")

ggplot(data=Data) +
  annotate("rect", xmin = Data$group - 0.4, xmax = Data$group + 0.4,
           ymin = Data$low, ymax = Data$up, fill = "white", color = "black") +
  annotate("segment", x = Data$group - 0.4, xend = Data$group + 0.4, 
           y = Data$mean, yend = Data$mean, size = 1.2,color = "#2d66a4")+
  scale_x_discrete(expand=c(0,0),limits=c(1,2,3),breaks=c(1,2,3),labels=c("","",""),name="")+
  scale_y_continuous(expand=c(0,0),limits=c(-2,5.5),breaks = c(0,0.2,0.4,0.6,0.8,1.0),name="")+
  coord_cartesian(ylim = c(0.0,1),xlim=c(0.5,3.5), clip = "off")+
  annotate("text",x=1,y=-0.049,label="Relative contacts",size=4,angle = 0)+
  annotate("text",x=2,y=-0.049,label="Social contacts",size=4,angle = 0)+
  annotate("text",x=3,y=-0.049,label="Other close contacts",size=4,angle = 0)+
  annotate("text",x=0.36,y=0,label="0.0",size=4,angle = 0)+
  annotate("text",x=0.36,y=0.2,label="0.2",size=4,angle = 0)+
  annotate("text",x=0.36,y=0.4,label="0.4",size=4,angle = 0)+
  annotate("text",x=0.36,y=0.6,label="0.6",size=4,angle = 0)+
  annotate("text",x=0.36,y=0.8,label="0.8",size=4,angle = 0)+
  annotate("text",x=0.36,y=1.0,label="1.0",size=4,angle = 0)+
  annotate("text",x=0.15,y=0.5,label="Relative risk of transmission",size=4.5,angle = 90,fontface="bold")+
  annotate("text",x=2,y=-0.11,label="Type of contacts",size=4.5,angle = 0,fontface="bold")+
  geom_hline(yintercept=0.995,linetype="dashed",color="#d6604d",size=1)+
  theme_bw()+
  theme(legend.position = c(0.16,0.9),
        plot.margin = margin(0.7,0.3,1.0,1.1,unit="cm"),
        panel.background = element_rect(colour = "white"),
        panel.border = element_rect(colour = "white"),
        axis.line = element_line(colour = "black",size =0.45),
        axis.text = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 9),
        legend.title =element_blank())+
  guides(fill = FALSE,size=FALSE)


## Panel c
Data<-matrix(NA,4,4)
Data[1:4,1]<-seq(1,4,1)
Data[1:4,2]<-result$estimate[9:12]
Data[1:4,3]<-result$conf.low[9:12]
Data[1:4,4]<-result$conf.high[9:12]
Data<-as.data.frame(Data)
colnames(Data)<-c("group","mean","low","up")

ggplot(data=Data) +
  annotate("rect", xmin = Data$group - 0.4, xmax = Data$group + 0.4,
           ymin = Data$low, ymax = Data$up, fill = "white", color = "black") +
  annotate("segment", x = Data$group - 0.4, xend = Data$group + 0.4, 
           y = Data$mean, yend = Data$mean, size = 1.2,color = "#2d66a4")+
  scale_x_discrete(expand=c(0,0),limits=c(1,2,3,4),breaks=c(1,2,3,4),labels=c("","","",""),name="")+
  scale_y_continuous(expand=c(0,0),limits=c(-2,5.5),breaks = c(0,0.2,0.4,0.6,0.8,1.0),name="")+
  coord_cartesian(ylim = c(0.0,1),xlim=c(0.5,4.5), clip = "off")+
  annotate("text",x=1,y=-0.049,label="G2",size=4,angle = 0)+
  annotate("text",x=2,y=-0.049,label="G3-4",size=4,angle = 0)+
  annotate("text",x=3,y=-0.049,label="Multiple exposure",size=4,angle = 0)+
  annotate("text",x=4,y=-0.049,label="Unknown",size=4,angle = 0)+
  annotate("text",x=0.36,y=0,label="0.0",size=4,angle = 0)+
  annotate("text",x=0.36,y=0.2,label="0.2",size=4,angle = 0)+
  annotate("text",x=0.36,y=0.4,label="0.4",size=4,angle = 0)+
  annotate("text",x=0.36,y=0.6,label="0.6",size=4,angle = 0)+
  annotate("text",x=0.36,y=0.8,label="0.8",size=4,angle = 0)+
  annotate("text",x=0.36,y=1.0,label="1.0",size=4,angle = 0)+
  annotate("text",x=0.15,y=0.5,label="Relative risk of transmission",size=4.5,angle = 90,fontface="bold")+
  annotate("text",x=2.5,y=-0.11,label="Generation of infector",size=4.5,angle = 0,fontface="bold")+
  geom_hline(yintercept=0.995,linetype="dashed",color="#d6604d",size=1)+
  theme_bw()+
  theme(legend.position = c(0.16,0.9),
        plot.margin = margin(0.7,0.3,1.0,1.1,unit="cm"),
        panel.background = element_rect(colour = "white"),
        panel.border = element_rect(colour = "white"),
        axis.line = element_line(colour = "black",size =0.45),
        axis.text = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 9),
        legend.title =element_blank())+
  guides(fill = FALSE,size=FALSE)


## Panel d
library(ggeffects)
library(splines)
mydf<-ggpredict(model, terms = c("logage [all]","contact_type1"))
mydf$axis<-exp(mydf$x)

ggplot(data=mydf,aes(x=axis))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,fill = group),alpha=0.1)+
  geom_line(data=mydf,aes(y=predicted,colour =group,linetype=group),size=1)+
  scale_y_continuous(expand=c(0.002,0.001),limits = c(-0.5,0.20),breaks=seq(0,10,1)/100,name="")+
  scale_x_continuous(expand=c(0,0.01),limits = c(-14,94.5),breaks=c(0,20,40,60,80,94),name="")+
  scale_fill_manual(breaks = c("Household contacts", "Family contacts","Social contacts","Other close contacts"),
                    values=c("#2d66a4","#d47868","#9fb795","#eca07f"),
                    labels=c("Household contacts", "Relative contacts","Social contacts","Other close contacts"))+
  scale_color_manual(breaks = c("Household contacts", "Family contacts","Social contacts","Other close contacts"),
                     values=c("#2d66a4","#d47868","#9fb795","#eca07f"),
                     labels=c("Household contacts", "Relative contacts","Social contacts","Other close contacts"))+
  scale_linetype_manual(breaks = c("Household contacts", "Family contacts","Social contacts","Other close contacts"),
                        values=c(1,8,1,3),
                        labels=c("Household contacts", "Relative contacts","Social contacts","Other close contacts"))+
  coord_cartesian(ylim = c(0.0,0.0989),xlim=c(-2,94), clip = "off")+
  annotate("text",x=-13,y=0.045,label="Predicted probability of infection (%)",size=5.5,angle = 90,fontface="bold")+
  annotate("text",x=-6.5,y=0,label="0.0",size=5,angle = 0)+
  annotate("text",x=-6.5,y=0.02,label="2.0",size=5,angle = 0)+
  annotate("text",x=-6.5,y=0.04,label="3.0",size=5,angle = 0)+
  annotate("text",x=-6.5,y=0.06,label="4.0",size=5,angle = 0)+
  annotate("text",x=-6.5,y=0.08,label="8.0",size=5,angle = 0)+
  annotate("text",x=-7.3,y=0.10,label="10.0",size=5,angle = 0)+
  annotate("text",x=0,y=-0.0025,label="0",size=5,angle = 0)+
  annotate("text",x=20,y=-0.0025,label="20",size=5,angle = 0)+
  annotate("text",x=40,y=-0.0025,label="40",size=5,angle = 0)+
  annotate("text",x=60,y=-0.0025,label="60",size=5,angle = 0)+
  annotate("text",x=80,y=-0.0025,label="80",size=5,angle = 0)+
  annotate("text",x=94,y=-0.0025,label="94",size=5,angle = 0)+
  annotate("text",x=47,y=-0.0038,label="Age of contacts (years)",size=5.5,angle = 0,fontface="bold")+
  theme_bw()+
  labs(color  = mydf$group, linetype = mydf$group, shape = mydf$group)+
  guides(fill = FALSE,size=FALSE,alpha=FALSE)+
  theme(legend.position = c(0.2,0.95),
        plot.margin = margin(0.7,0.4,1,1.3,unit="cm"),
        panel.background = element_rect(colour = "white"),
        panel.border = element_rect(colour = "white"),
        axis.line = element_line(colour = "black",size =0.45),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 11),
        legend.title =element_blank(),
        legend.key.size = unit(0.8,unit="cm"))


# Code to reproduce Fig. S9
## panel a
mydf<-ggpredict(model, terms = "logage [all]")
mydf$xx<-exp(mydf$x)

ggplot(mydf, aes(xx, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill="#2d66a4",alpha=0.1)+
  geom_line(color="#2d66a4",size=1.1) +
  scale_y_continuous(expand=c(0.01,0.001),limits = c(-0.5,0.20),breaks=c(0,3,6,9,12)/100,name="")+
  scale_x_continuous(expand=c(0,0.1),limits = c(-10,94),breaks=c(0,20,40,60,80,94),name="")+
  coord_cartesian(ylim = c(0.0,0.118),xlim=c(-2,94), clip = "off")+
  annotate("text",x=-10,y=0.06,label="Predicted probability of infection (%)",size=4.5,angle = 90,fontface="bold")+
  annotate("text",x=-5,y=0,label="0",size=4,angle = 0)+
  annotate("text",x=-5,y=0.03,label="3",size=4,angle = 0)+
  annotate("text",x=-5,y=0.06,label="6",size=4,angle = 0)+
  annotate("text",x=-5,y=0.09,label="9",size=4,angle = 0)+
  annotate("text",x=-5.4,y=0.12,label="12",size=4,angle = 0)+
  annotate("text",x=0,y=-0.007,label="0",size=4,angle = 0)+
  annotate("text",x=20,y=-0.007,label="20",size=4,angle = 0)+
  annotate("text",x=40,y=-0.007,label="40",size=4,angle = 0)+
  annotate("text",x=60,y=-0.007,label="60",size=4,angle = 0)+
  annotate("text",x=80,y=-0.007,label="80",size=4,angle = 0)+
  annotate("text",x=94,y=-0.007,label="94",size=4,angle = 0)+
  annotate("text",x=47,y=-0.0135,label="Age of contacts (years)",size=4.5,angle = 0,fontface="bold")+
  annotate("text",x=-10,y=0.13,label="A.",size=6,angle = 0,fontface="bold")+
  theme_bw()+
  theme(legend.position = c(0.16,0.9),
        plot.margin = margin(1.25,0.3,0.8,0.9,unit="cm"),
        panel.background = element_rect(colour = "white"),
        panel.border = element_rect(colour = "white"),
        axis.line = element_line(colour = "black",size =0.45),
        axis.text = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 9),
        legend.title =element_blank())+
  guides(fill = FALSE,size=FALSE)

## panel b
mydf<-ggpredict(model, terms = "contact_type1")

ggplot(mydf, aes(as.numeric(x), predicted)) +
  geom_point(color="#2d66a4",size=4) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),width=0,size=1.2,color="#2d66a4")+
  scale_y_continuous(expand=c(0.01,0.001),limits = c(-0.5,0.20),breaks=c(0,3,6,9,12)/100,name="")+
  scale_x_continuous(expand=c(0,0.1),breaks = c(0.5,1.5,2.5,3.5,4.5),limits = c(-2,4))+
  coord_cartesian(ylim = c(0.0,0.118),xlim=c(0.5,4.40), clip = "off")+
  annotate("text",x=0.05,y=0.06,label="Predicted probability of infection (%)",size=4.5,angle = 90,fontface="bold")+
  annotate("text",x=0.27,y=0,label="0",size=4,angle = 0)+
  annotate("text",x=0.27,y=0.03,label="3",size=4,angle = 0)+
  annotate("text",x=0.27,y=0.06,label="6",size=4,angle = 0)+
  annotate("text",x=0.27,y=0.09,label="9",size=4,angle = 0)+
  annotate("text",x=0.24,y=0.12,label="12",size=4,angle = 0)+
  annotate("text",x=1,y=-0.007,label="Household contacts",size=4,angle = 0)+
  annotate("text",x=2.02,y=-0.007,label="Relative contacts",size=4,angle = 0)+
  annotate("text",x=3,y=-0.007,label="Social contacts",size=4,angle = 0)+
  annotate("text",x=4,y=-0.007,label="Other contacts",size=4,angle = 0)+
  annotate("text",x=2.5,y=-0.0139,label="Type of contacts",size=4.5,angle = 0,fontface="bold")+
  annotate("text",x=0.05,y=0.13,label="B.",size=6,angle = 0,fontface="bold")+
  theme_bw()+
  theme(legend.position = c(0.16,0.9),
        plot.margin = margin(1.25,0.3,1.22,0.6,unit="cm"),
        panel.background = element_rect(colour = "white"),
        panel.border = element_rect(colour = "white"),
        axis.line = element_line(colour = "black",size =0.45),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 9),
        legend.title =element_blank())+
  guides(fill = FALSE,size=FALSE)

## panel c
mydf<-ggpredict(model, terms = "no.persons [all]")

ggplot(mydf, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill="#2d66a4",alpha=0.1)+
  geom_line(color="#2d66a4",size=1.1) +
  scale_y_continuous(expand=c(0.01,0.001),limits = c(-0.5,0.3),breaks=c(0,3,6,9,12)/100,name="")+
  scale_x_continuous(expand=c(0.025,0.1),limits = c(-40,167),breaks=c(seq(0,167,30),167),name="")+
  coord_cartesian(ylim = c(0.0,0.118),xlim=c(0.5,163), clip = "off")+
  annotate("text",x=-17,y=0.06,label="Predicted probability of infection (%)",size=4.5,angle = 90,fontface="bold")+
  annotate("text",x=-8.5,y=0,label="0",size=4,angle = 0)+
  annotate("text",x=-8.5,y=0.03,label="3",size=4,angle = 0)+
  annotate("text",x=-8.5,y=0.06,label="6",size=4,angle = 0)+
  annotate("text",x=-8.5,y=0.09,label="9",size=4,angle = 0)+
  annotate("text",x=-8.5,y=0.12,label="12",size=4,angle = 0)+
  annotate("text",x=0,y=-0.007,label="0",size=4,angle = 0)+
  annotate("text",x=30,y=-0.007,label="30",size=4,angle = 0)+
  annotate("text",x=60,y=-0.007,label="60",size=4,angle = 0)+
  annotate("text",x=90,y=-0.007,label="90",size=4,angle = 0)+
  annotate("text",x=120,y=-0.007,label="120",size=4,angle = 0)+
  annotate("text",x=150,y=-0.007,label="150",size=4,angle = 0)+
  annotate("text",x=167,y=-0.007,label="167",size=4,angle = 0)+
  annotate("text",x=83.5,y=-0.0135,label="Number of contacts causing by an infector",size=4.5,angle = 0,fontface="bold")+
  annotate("text",x=-17,y=0.1275,label="C.",size=6,angle = 0,fontface="bold")+
  theme_bw()+
  theme(legend.position = c(0.16,0.9),
        plot.margin = margin(1,0.3,0.78,0.9,unit="cm"),
        panel.background = element_rect(colour = "white"),
        panel.border = element_rect(colour = "white"),
        axis.line = element_line(colour = "black",size =0.45),
        axis.text = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 9),
        legend.title =element_blank())+
  guides(fill = FALSE,size=FALSE)

# Code to reproduce Table S13 and Fig. S10
### Step 1
model.x<-bam(whe.source~agegroup_index+
               agegroup +
               contact_type1 +
               generation_y+
               s(no.persons,k=3)+
               relevel(gender_index,ref="2")+ 
               relevel(gender,ref="2")+
               s(id_case, bs = "re")+
               s(cluster_id, bs = "re"),
             family = binomial(link = "logit"),
             data = mock_data) 

pp<-get_coefs(model.x,se=T)
pp<-as.data.frame(pp)
pp[3]<-pp[1]-1.96*pp[2]
pp[4]<-pp[1]+1.96*pp[2]
pp<-exp(pp)
pp<-round(pp,2)
pp[5]<-row.names(pp)
pp<-pp[,c("V5","Estimate","Estimate.1","Estimate.2")]
colnames(pp)<-c("rowname","mean","lwr","upr")
rownames(pp)<-NULL

pv<-summary(model.x)$p.pv
pv<-as.data.frame(pv)
pv[2]<-row.names(pv)
pv<-pv[,c("V2","pv")]
colnames(pv)<-c("rowname","p_value")
rownames(pv)<-NULL

pf<-merge(pp,pv,by="rowname",all.x=T)

a<-as.data.frame(matrix(NA,nrow(pf),3))
colnames(a)<-c("term","OR (95%CI)","P-value")
for (i in 1:nrow(pf)) {
  a[i,1]<-pf[i,1]
  a[i,2]<-paste(pf[i,2]," (",pf[i,3],", ",pf[i,4],")",sep="")
  a[i,3]<-trans(pf[i,5])
}

plot_gam(model.x,pred = "no.persons",title="Number of persons)")+theme_minimal()



# Code to reproduce Table S13
### Step 1
model.x<-bam(whe.source~agegroup_index+
               agegroup +
               contact_type1 +
               generation_y+
               s(no.persons,k=3)+
               relevel(gender_index,ref="2")+ 
               relevel(gender,ref="2")+
               s(id_case, bs = "re")+
               s(cluster_id, bs = "re"),
             family = binomial(link = "logit"),
             data = mock_data) 

pp<-get_coefs(model.x,se=T)
pp<-as.data.frame(pp)
pp[3]<-pp[1]-1.96*pp[2]
pp[4]<-pp[1]+1.96*pp[2]
pp<-exp(pp)
pp<-round(pp,2)
pp[5]<-row.names(pp)
pp<-pp[,c("V5","Estimate","Estimate.1","Estimate.2")]
colnames(pp)<-c("rowname","mean","lwr","upr")
rownames(pp)<-NULL

pv<-summary(model.x)$p.pv
pv<-as.data.frame(pv)
pv[2]<-row.names(pv)
pv<-pv[,c("V2","pv")]
colnames(pv)<-c("rowname","p_value")
rownames(pv)<-NULL

pf<-merge(pp,pv,by="rowname",all.x=T)

a<-as.data.frame(matrix(NA,nrow(pf),3))
colnames(a)<-c("term","OR (95%CI)","P-value")
for (i in 1:nrow(pf)) {
  a[i,1]<-pf[i,1]
  a[i,2]<-paste(pf[i,2]," (",pf[i,3],", ",pf[i,4],")",sep="")
  a[i,3]<-trans(pf[i,5])
}

plot_gam(model.x,pred = "no.persons",title="Number of persons)")+theme_minimal()

