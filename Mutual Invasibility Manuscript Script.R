library(lme4)
library(grid)
library(ade4)
library(ggplot2)
library(dplyr)
library(Rmisc)
library(cowplot)
library(gridExtra)
library(forecast)
library(dplyr)
library(cowplot)
library(grid)
library(gridExtra)
library(ggpubr)
library(growthrates)
library(pheatmap)
library(tidyr)

# Main Text Fig.1. Host-associated Bacterial Communities alter the Strength but rarely the Type of Ecological Interactions between Hosts.

#data<-read.csv("C:/Users/sjackrel/Desktop/Lenovo_Backup/Mutual-Invasibility-master/Calculate_Growth_Rates.csv")
data<-read.csv("./Calculate_Growth_Rates.csv")
Monocultures<-data[(data$Monoculture_or_Biculture=="Monoculture"),]

Cm_Monocultures<-data[(Monocultures$Treatment=="Cm"),]
Mm_Monocultures<-data[(Monocultures$Treatment=="Mm"),]
Sa_Monocultures<-data[(Monocultures$Treatment=="Sa"),]
Sc_Monocultures<-data[(Monocultures$Treatment=="Sc"),]

summary(aov(r.D1.D0.~Treatment*Bacteria, data=data))

Comparison.1<-data[(data$Comparisons=="1"),]
Comparison.1$Treatment2<-droplevels(Comparison.1$Treatment2)
Comparison.1$Treatment2<-factor(Comparison.1$Treatment2,levels(Comparison.1$Treatment2)[c(4,1,2,3)])

Comparison.1_Graph.A<-ggplot(Comparison.1,aes(Treatment2,r.D1.D0.perday,colour=Monoculture_or_Biculture,shape=Bacteria,alpha=0.01))+
  geom_point(size=4,stroke=2,position = position_dodge(width = 0.55))+
  scale_colour_manual(name="",values=c("Biculture"="black","Monoculture"="gray60"))+
  scale_shape_manual(name="Axenic Status",values=c(1,16))+
  theme(axis.text.x = element_text(angle = 90,vjust=0.5, hjust = 1,size=26),plot.title = element_text(hjust = 0.5,size=26),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 26),axis.title=element_text(size=26,face="plain"),legend.title=element_text(size=26),legend.text=element_text(size=26))+
  xlab("")+
  ggtitle("Competition")+
  ylab("")+
  guides(alpha=FALSE,size=FALSE, colour = FALSE,shape = FALSE)
Comparison.1_Graph.A

Comparison.2<-data[(data$Comparisons=="2"),]
Comparison.2$Treatment2<-droplevels(Comparison.2$Treatment2)
Comparison.2$Treatment2<-factor(Comparison.2$Treatment2,levels(Comparison.2$Treatment2)[c(4,1,2,3)])

Comparison.2_Graph.A<-ggplot(Comparison.2,aes(Treatment2,r.D1.D0.perday,colour=Monoculture_or_Biculture,shape=Bacteria,alpha=0.01))+
  geom_point(size=4,stroke=2,position = position_dodge(width = 0.55))+
  scale_shape_manual(name="Axenic Status",values=c(1,16))+
  theme(axis.text.x = element_text(angle = 90,vjust=0.5, hjust = 1,size=26),plot.title = element_text(hjust = 0.5,size=26),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 26),axis.title=element_text(size=26,face="plain"),legend.title=element_text(size=26),legend.text=element_text(size=26))+
  xlab("")+
  scale_colour_manual(name="",values=c("Biculture"="black","Monoculture"="gray60"))+
  ylab("")+
  ggtitle("Facilitation")+
  guides(alpha=FALSE,size=FALSE, colour = FALSE,shape = FALSE)
Comparison.2_Graph.A

Comparison.3<-data[(data$Comparisons=="3"),]
Comparison.3$Treatment2<-droplevels(Comparison.3$Treatment2)
Comparison.3$Treatment2<-factor(Comparison.3$Treatment2,levels(Comparison.3$Treatment2)[c(4,3,1,2)])

Comparison.3_Graph.A<-ggplot(Comparison.3,aes(Treatment2,r.D1.D0.perday,colour=Monoculture_or_Biculture,shape=Bacteria,alpha=0.01))+
  geom_point(size=4,stroke=2,position = position_dodge(width = 0.55))+
  theme(axis.text.x = element_text(angle = 90,vjust=0.5, hjust = 1,size=26),plot.title = element_text(hjust = 0.5,size=26),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 26),axis.title=element_text(size=26,face="plain"),legend.title=element_text(size=26),legend.text=element_text(size=26))+
  scale_shape_manual(name="Axenic Status",values=c(1,16))+
  scale_colour_manual(name="",values=c("Biculture"="black","Monoculture"="gray60"))+
  ggtitle("Neutralism")+
  xlab("")+
  ylab("")+
  guides(alpha=FALSE,size=FALSE, colour = FALSE,shape = FALSE)
Comparison.3_Graph.A

Comparison.4<-data[(data$Comparisons=="4"),]
Comparison.4$Treatment2<-droplevels(Comparison.4$Treatment2)
Comparison.4$Treatment2<-factor(Comparison.4$Treatment2,levels(Comparison.4$Treatment2)[c(4,1,2,3)])

Comparison.4_Graph.A<-ggplot(Comparison.4,aes(Treatment2,r.D1.D0.perday,colour=Monoculture_or_Biculture,shape=Bacteria,group=Bacteria,alpha=0.01))+
  geom_point(size=4,stroke=2,position = position_dodge(width = 0.55))+
  scale_shape_manual(name="Axenic Status",values=c(1,16))+
  scale_colour_manual(name="",values=c("Biculture"="black","Monoculture"="gray60"))+
  theme(axis.text.x = element_text(angle = 90,vjust=0.5, hjust = 1,size=26),plot.title = element_text(hjust = 0.5,size=26),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 26),axis.title=element_text(size=26,face="plain"),legend.title=element_text(size=26),legend.text=element_text(size=26))+
  xlab("")+
  ggtitle("Competition")+
  ylab("")+
  guides(alpha=FALSE,size=FALSE,color=FALSE,shape=FALSE)
Comparison.4_Graph.A

legend<-ggplot(Comparison.4,aes(Treatment2,r.D1.D0.perday,colour=Monoculture_or_Biculture,shape=Bacteria,group=Bacteria,alpha=0.01))+
  geom_point(size=4,stroke=2,position = position_dodge(width = 0.55))+
  scale_shape_manual(name="Axenic Status",values=c(1,16))+
  scale_colour_manual(name="# of Algal Species",values=c("Biculture"="black","Monoculture"="gray60"))+
  theme(axis.text.x = element_text(angle = 90,vjust=0.5, hjust = 1,size=26),plot.title = element_text(hjust = 0.5,size=26),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 26),axis.title=element_text(size=26,face="plain"),legend.title=element_text(size=26),legend.text=element_text(size=26))+
  xlab("")+
  ggtitle("Competition")+
  ylab("")+
  guides(alpha=FALSE,size=FALSE)
mylegend<-get_legend(legend)

grid.arrange(ncol=5,Comparison.1_Graph.A,Comparison.4_Graph.A,Comparison.3_Graph.A,Comparison.2_Graph.A,mylegend)

# Main Text Fig. 2. Host-associated Bacterial Communities reduce their Host’s Sensitivity to Interspecific Interactions. 

#data<-read.csv("C:/Users/sjackrel/Desktop/Lenovo_Backup/Mutual-Invasibility-master/Sensitivities.csv")
data<-read.csv("./Sensitivities.csv")

data_D1_D0<-data[(data$Time_Point=="D1_D0"),]
null<-lmer(Sensitivities~(1|Combo),data=data_D1_D0)
full<-lmer(Sensitivities~Bacteria+(1|Combo),data=data_D1_D0)
anova(null,full)

ratio.data<-data[,7:12]
ratio.data<-ratio.data[!is.na(ratio.data$A.X_Sensititives),]
ratio.data$Combo.1<-droplevels(ratio.data$Combo.1)
ratio.data$Combo.1<-factor(ratio.data$Combo.1,levels(ratio.data$Combo.1)[c(1,2,3,10,11,12,4,5,6,7,8,9)])

ratio.data.a<-subset(ratio.data,subset = Combo.1 %in% c("C.m. Invading S.a.","C.m. Invading M.m.","C.m. Invading S.c."))
ratio.data.a<-droplevels(ratio.data.a)
ratio.data.b<-subset(ratio.data,subset = Combo.1 %in% c("S.c. Invading S.a.","S.c. Invading M.m.","S.c. Invading C.m."))
ratio.data.b<-droplevels(ratio.data.b)
ratio.data.c<-subset(ratio.data,subset = Combo.1 %in% c("M.m. Invading S.a.","M.m. Invading S.c.","M.m. Invading C.m."))
ratio.data.c<-droplevels(ratio.data.c)
ratio.data.d<-subset(ratio.data,subset = Combo.1 %in% c("S.a. Invading C.m.","S.a. Invading M.m.","S.a. Invading S.c."))

Fig.2a<-ggplot(ratio.data.a,aes(Combo.1,X.A_Sensitivities,alpha=0.25))+
  geom_point(aes(size=3),width=0.3)+
  labs(y=expression(atop("Sensitivity to Established Species",S[Xenic]-S[Axenic])))+
  annotate("text", size = 6, x=1.25, y=0.75, label= "Competition",hjust =0.0,fontface=2)+
  xlab("")+
  scale_x_discrete(limits=c("C.m. Invading M.m.","C.m. Invading S.a.","C.m. Invading S.c."))+
  ylim(-0.75,0.75)+
  scale_shape_manual(name="Axenic Status",values=c(15,16))+
  scale_colour_manual(name="Axenic Status",values=c("black","gray"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=18),plot.title = element_text(hjust = 0.5),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 16),axis.title=element_text(size=18,face="plain"),legend.title=element_text(size=16),legend.text=element_text(size=16))+
  geom_hline(yintercept=0, linetype="dashed", color = "red",size=1)+
  guides(alpha=FALSE,size=FALSE,shape=guide_legend(override.aes=list(size=6)),colour = guide_legend(override.aes = list(size=6)))
Fig.2a

Fig.2b<-ggplot(ratio.data.b,aes(Combo.1,X.A_Sensitivities,alpha=0.25))+
  geom_point(aes(size=3),width=0.3)+
  labs(y="\n\n")+
  annotate("text", size = 6, x=1.25, y=0.75, label= "Competition",hjust =0.0,fontface=2)+
  xlab("")+
  scale_x_discrete(limits=c("S.c. Invading C.m.","S.c. Invading M.m.","S.c. Invading S.a."))+
  ylim(-0.75,0.75)+
  scale_shape_manual(name="Axenic Status",values=c(15,16))+
  scale_colour_manual(name="Axenic Status",values=c("black","gray"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=18),plot.title = element_text(hjust = 0.5),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 16),axis.title=element_text(size=18,face="plain"),legend.title=element_text(size=16),legend.text=element_text(size=16))+
  geom_hline(yintercept=0, linetype="dashed", color = "red",size=1)+
  guides(alpha=FALSE,size=FALSE,shape=guide_legend(override.aes=list(size=6)),colour = guide_legend(override.aes = list(size=6)))
Fig.2b

Fig.2c<-ggplot(ratio.data.c,aes(Combo.1,X.A_Sensitivities,alpha=0.25))+
  geom_point(aes(size=3),width=0.3)+
  labs(y="\n\n")+
  annotate("text", size = 6, x=1.25, y=0.75, label= "Neutralism",hjust =0.0,fontface=2)+
  xlab("")+
  scale_x_discrete(limits=c("M.m. Invading C.m.","M.m. Invading S.a.","M.m. Invading S.c."))+
  ylim(-0.75,0.75)+
  scale_shape_manual(name="Axenic Status",values=c(15,16))+
  scale_colour_manual(name="Axenic Status",values=c("black","gray"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=18),plot.title = element_text(hjust = 0.5),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 16),axis.title=element_text(size=18,face="plain"),legend.title=element_text(size=16),legend.text=element_text(size=16))+
  geom_hline(yintercept=0, linetype="dashed", color = "red",size=1)+
  guides(alpha=FALSE,size=FALSE,shape=guide_legend(override.aes=list(size=6)),colour = guide_legend(override.aes = list(size=6)))
Fig.2c

Fig.2d<-ggplot(ratio.data.d,aes(Combo.1,X.A_Sensitivities,alpha=0.25))+
  geom_point(aes(size=3),width=0.3)+
  labs(y="")+
  annotate("text", size = 6, x=1.25, y=0.75, label= "Facilitation",hjust =0.0,fontface=2)+
  xlab("")+
  scale_x_discrete(limits=c("S.a. Invading C.m.","S.a. Invading M.m.","S.a. Invading S.c."))+
  ylim(-0.75,0.75)+
  scale_shape_manual(name="Axenic Status",values=c(15,16))+
  scale_colour_manual(name="Axenic Status",values=c("black","gray"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=18),plot.title = element_text(hjust = 0.5),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 16),axis.title=element_text(size=18,face="plain"),legend.title=element_text(size=16),legend.text=element_text(size=16))+
  geom_hline(yintercept=0, linetype="dashed", color = "red",size=1)+
  guides(alpha=FALSE,size=FALSE,shape=guide_legend(override.aes=list(size=6)),colour = guide_legend(override.aes = list(size=6)))
Fig.2d

grid.arrange(Fig.2a,Fig.2b,Fig.2c,Fig.2d,ncol=4)

# Supplementary Fig. S3. Fluorescence growth curves of all phytoplankton population-level experiments with single bacterial isolate additions. 

#All.Chlorella.aggregate<-read.csv("C:/Users/sjackrel/Documents/UMich Projects/Mutual Invasibility/Chlorella_FigS3.csv")
All.Chlorella.aggregate<-read.csv("./Chlorella_FigS3.csv")

All.Chlorella.aggregate$Treatment<-as.factor(All.Chlorella.aggregate$Treatment)
All.Chlorella.aggregate.2.plus.8<-subset(All.Chlorella.aggregate, subset = Treatment %in% c(2,8))
All.Chlorella.aggregate.2.plus.10<-subset(All.Chlorella.aggregate, subset = Treatment %in% c(2,10))
All.Chlorella.aggregate.2.plus.13<-subset(All.Chlorella.aggregate, subset = Treatment %in% c(2,13))
All.Chlorella.aggregate.2.plus.14<-subset(All.Chlorella.aggregate, subset = Treatment %in% c(2,14))
All.Chlorella.aggregate.2.plus.19<-subset(All.Chlorella.aggregate, subset = Treatment %in% c(2,19))
All.Chlorella.aggregate.2.plus.20<-subset(All.Chlorella.aggregate, subset = Treatment %in% c(2,20))
All.Chlorella.aggregate.2.plus.23<-subset(All.Chlorella.aggregate, subset = Treatment %in% c(2,23))
All.Chlorella.aggregate.2.plus.24<-subset(All.Chlorella.aggregate, subset = Treatment %in% c(2,24))

result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Chlorella.aggregate.2.plus.8)
drop1(result,.~.,test="F")# N.S.
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Chlorella.aggregate.2.plus.10)
drop1(result,.~.,test="F")# N.S.
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Chlorella.aggregate.2.plus.13)
drop1(result,.~.,test="F")# N.S.
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Chlorella.aggregate.2.plus.14)
drop1(result,.~.,test="F")# N.S.
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Chlorella.aggregate.2.plus.19)
drop1(result,.~.,test="F")# N.S.
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Chlorella.aggregate.2.plus.23)
drop1(result,.~.,test="F")# N.S.
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Chlorella.aggregate.2.plus.24)
drop1(result,.~.,test="F")# N.S.

All.Chlorella.all<-ggplot(All.Chlorella.aggregate.2.plus.8,aes(x=Day,y=Mean,colour=Treatment))+
  geom_line(data=All.Chlorella.aggregate.2.plus.8,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Chlorella.aggregate.2.plus.10,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Chlorella.aggregate.2.plus.13,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Chlorella.aggregate.2.plus.14,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Chlorella.aggregate.2.plus.19,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Chlorella.aggregate.2.plus.20,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Chlorella.aggregate.2.plus.23,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Chlorella.aggregate.2.plus.24,aes(linetype=Treatment,size=Treatment))+
  scale_color_manual(labels = c("2"="Axenic","8"="+ Blastomonas natatoria","10"="+ Mycobacterium cosmeticum ","13"="+ Microbacterium sp.","14"="+ Sphingomonas sp. or Blastomonas natatoria strain","19"="+ Microbacterium sp.","20"="+ Unknown bacterium","21"="+ isolate 27","22"="+ Sphingomonas sp or Blastomonas natatoria strain","23"="+ isolate 30","24"="+ Rhizobium sp."),values=c("2"="black","8"="red","10"="orange","13"="springgreen3","14"="blue","19"="green","20"="chocolate4","21"="gray","22"="dodgerblue1","23"="deeppink2","24"="plum2","gold2","cyan","firebrick"))+
  scale_linetype_manual(values=c("2"="dashed","8"="solid","10"="solid","13"="solid","14"="solid","19"="solid","20"="solid","23"="solid","24"="solid"))+
  scale_y_log10()+
  scale_size_manual(values=c("2"=2,"8"=1,"10"=1,"13"=1,"14"=1,"19"=1,"20"=1,"23"=1,"24"=1))+
  xlab("Days")+
  guides(color=FALSE,size=FALSE,linetype=FALSE,color=guide_legend(override.aes=list(size=6)))+
  ylab("RFU (Chlorophyll-a)")+
  theme(plot.title = element_text(face='bold.italic',hjust = 0.5),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 16),axis.title=element_text(size=18,face="plain"),legend.title=element_text(size=16),legend.text=element_text(size=16))+
  ggtitle("C. sorokiniana")+
  annotate("text",x=5,y=10^1.7,label=paste("list(ANOVA:~F['1,109']==1.37", ",p==0.17)"),color="red",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.6,label=paste("list(ANOVA:~F['1,123']==13.9", ",p==0.00029)"),color="orange",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.5,label=paste("list(ANOVA:~F['1,95']==0.33", ",p==0.56)"),color="springgreen3",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.4,label=paste("list(ANOVA:~F['1,109']==0.42", ",p==0.52)"),color="blue",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.3,label=paste("list(ANOVA:~F['1,109']==1.29", ",p==0.26)"),color="green",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.2,label=paste("list(ANOVA:~F['1,109']==25.1", ",p==2.1*e^-{6})"),color="chocolate4",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.1,label=paste("list(ANOVA:~F['1,109']==12.6", ",p==0.00058)"),color="deeppink2",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1,label=paste("list(ANOVA:~F['1,109']==0.46", ",p==0.50)"),color="plum2",parse=TRUE,hjust=0.0,size=5)

All.Coelastrum.aggregate<-read.csv("C:/Users/sjackrel/Documents/UMich Projects/Mutual Invasibility/Coelastrum_FigS3.csv")
All.Coelastrum.aggregate$Treatment<-as.factor(All.Coelastrum.aggregate$Treatment)

All.Coelastrum.aggregate.3.plus.42<-subset(All.Coelastrum.aggregate, subset = Treatment %in% c(3,42))
All.Coelastrum.aggregate.3.plus.44<-subset(All.Coelastrum.aggregate, subset = Treatment %in% c(3,44))
All.Coelastrum.aggregate.3.plus.47<-subset(All.Coelastrum.aggregate, subset = Treatment %in% c(3,47))
All.Coelastrum.aggregate.3.plus.48<-subset(All.Coelastrum.aggregate, subset = Treatment %in% c(3,48))
All.Coelastrum.aggregate.3.plus.53<-subset(All.Coelastrum.aggregate, subset = Treatment %in% c(3,53))
All.Coelastrum.aggregate.3.plus.54<-subset(All.Coelastrum.aggregate, subset = Treatment %in% c(3,54))
All.Coelastrum.aggregate.3.plus.55<-subset(All.Coelastrum.aggregate, subset = Treatment %in% c(3,55))
All.Coelastrum.aggregate.3.plus.56<-subset(All.Coelastrum.aggregate, subset = Treatment %in% c(3,56))
All.Coelastrum.aggregate.3.plus.57<-subset(All.Coelastrum.aggregate, subset = Treatment %in% c(3,57))
All.Coelastrum.aggregate.3.plus.58<-subset(All.Coelastrum.aggregate, subset = Treatment %in% c(3,58))

result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Coelastrum.aggregate.3.plus.42)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Coelastrum.aggregate.3.plus.44)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Coelastrum.aggregate.3.plus.47)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Coelastrum.aggregate.3.plus.48)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Coelastrum.aggregate.3.plus.53)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Coelastrum.aggregate.3.plus.54)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Coelastrum.aggregate.3.plus.55)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Coelastrum.aggregate.3.plus.56)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Coelastrum.aggregate.3.plus.57)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Coelastrum.aggregate.3.plus.58)
drop1(result,.~.,test="F")

All.Coelastrum.all<-ggplot(All.Coelastrum.aggregate.3.plus.42,aes(x=Day,y=Mean,colour=Treatment))+
  geom_line(data=All.Coelastrum.aggregate.3.plus.44,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Coelastrum.aggregate.3.plus.47,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Coelastrum.aggregate.3.plus.48,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Coelastrum.aggregate.3.plus.53,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Coelastrum.aggregate.3.plus.54,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Coelastrum.aggregate.3.plus.55,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Coelastrum.aggregate.3.plus.56,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Coelastrum.aggregate.3.plus.57,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Coelastrum.aggregate.3.plus.58,aes(linetype=Treatment,size=Treatment))+
  scale_color_manual(labels = c("3"="Axenic","42"="+ isolate 13","44"="Mycobacterium cosmeticum ","47"="Microbacterium sp.","48"="Sphingomonas sp. or Blastomonas natatoria strain","53"="Microbacterium sp.","54"="Uncultured Bacteria","55"="+ isolate 27","56"="Sphingomonas sp or Blastomonas natatoria strain","57"="+ isolate 30","58"="Rhizobium sp."),values=c("3"="black","42"="red","44"="orange","47"="springgreen3","48"="blue","53"="green","54"="chocolate4","55"="gray","56"="dodgerblue1","57"="deeppink2","58"="plum2","gold2","cyan","firebrick"))+
  scale_linetype_manual(values=c("3"="dashed","42"="solid","13"="solid","44"="solid","47"="solid","48"="solid","53"="solid","54"="solid","55"="solid","56"="solid","57"="solid","58"="solid"))+
  scale_y_log10()+
  scale_size_manual(values=c("3"=2,"42"=1,"13"=1,"44"=1,"47"=1,"48"=1,"53"=1,"54"=1,"55"=1,"56"=1,"57"=1,"58"=1))+
  xlab("Days")+
  guides(colour=FALSE,size=FALSE,linetype=FALSE)+
  ylab("RFU (Chlorophyll-a)")+
  theme(plot.title = element_text(face='bold.italic',hjust = 0.5),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 16),axis.title=element_text(size=18,face="plain"),legend.title=element_text(size=16),legend.text=element_text(size=16))+
  ggtitle("C. microporum")+
  annotate("text",x=5,y=10^1.7,label=paste("list(ANOVA:~F['1,118']==24.4", ",p==3.42*e^-{6})"),color="red",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.6,label=paste("list(ANOVA:~F['1,109']==26.2", ",p==1.32*e^-06)"),color="orange",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.5,label=paste("list(ANOVA:~F['1,109']==29.3", ",p==3.77*e^-07)"),color="springgreen4",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.4,label=paste("list(ANOVA:~F['1,95']==33.1", ",p==1.07*e^-07)"),color="blue",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.3,label=paste("list(ANOVA:~F['1,95']==36.3", ",p==3.19*e^-08)"),color="green",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.2,label=paste("list(ANOVA:~F['1,95']==28.5", ",p==6.37*e^-07)"),color="chocolate4",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.1,label=paste("list(ANOVA:~F['1,95']==32.9", ",p==1.15*e^-07)"),color="deeppink2",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1,label=paste("list(ANOVA:~F['1,95']==2.87", ",p==0.094)"),color="plum2",parse=TRUE,hjust=0.0,size=5)

All.Monoraphidium.aggregate<-read.csv("C:/Users/sjackrel/Documents/UMich Projects/Mutual Invasibility/Monoraphidium_FigS3.csv")

All.Monoraphidium.aggregate.4.plus.25<-subset(All.Monoraphidium.aggregate, subset = Treatment %in% c(4,25))
All.Monoraphidium.aggregate.4.plus.27<-subset(All.Monoraphidium.aggregate, subset = Treatment %in% c(4,27))
All.Monoraphidium.aggregate.4.plus.30<-subset(All.Monoraphidium.aggregate, subset = Treatment %in% c(4,30))
All.Monoraphidium.aggregate.4.plus.31<-subset(All.Monoraphidium.aggregate, subset = Treatment %in% c(4,31))
All.Monoraphidium.aggregate.4.plus.36<-subset(All.Monoraphidium.aggregate, subset = Treatment %in% c(4,36))
All.Monoraphidium.aggregate.4.plus.37<-subset(All.Monoraphidium.aggregate, subset = Treatment %in% c(4,37))
All.Monoraphidium.aggregate.4.plus.40<-subset(All.Monoraphidium.aggregate, subset = Treatment %in% c(4,40))
All.Monoraphidium.aggregate.4.plus.41<-subset(All.Monoraphidium.aggregate, subset = Treatment %in% c(4,41))

result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Monoraphidium.aggregate.4.plus.25)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Monoraphidium.aggregate.4.plus.27)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Monoraphidium.aggregate.4.plus.30)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Monoraphidium.aggregate.4.plus.31)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Monoraphidium.aggregate.4.plus.36)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Monoraphidium.aggregate.4.plus.37)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Monoraphidium.aggregate.4.plus.40)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Monoraphidium.aggregate.4.plus.41)
drop1(result,.~.,test="F")
All.Monoraphidium.all<-ggplot(All.Monoraphidium.aggregate.4.plus.25,aes(x=Day,y=Mean,colour=Treatment))+
  geom_line(data=All.Monoraphidium.aggregate.4.plus.25,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Monoraphidium.aggregate.4.plus.27,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Monoraphidium.aggregate.4.plus.30,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Monoraphidium.aggregate.4.plus.31,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Monoraphidium.aggregate.4.plus.36,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Monoraphidium.aggregate.4.plus.37,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Monoraphidium.aggregate.4.plus.40,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Monoraphidium.aggregate.4.plus.41,aes(linetype=Treatment,size=Treatment))+
  scale_color_manual(labels = c("4"="Axenic","25"="+ isolate 13","27"="Mycobacterium cosmeticum ","30"="Microbacterium sp.","31"="Sphingomonas sp. or Blastomonas natatoria strain","36"="Microbacterium sp.","37"="Uncultured Bacteria","40"="+ isolate 27","41"="Sphingomonas sp or Blastomonas natatoria strain"),values=c("4"="black","25"="red","27"="orange","30"="springgreen3","31"="blue","36"="green","37"="chocolate4","40"="deeppink2","41"="plum2","gold2","cyan","firebrick"))+
  scale_linetype_manual(values=c("4"="dashed","25"="solid","27"="solid","30"="solid","31"="solid","36"="solid","37"="solid","40"="solid","41"="solid"))+
  scale_y_log10()+
  scale_size_manual(values=c("4"=2,"25"=1,"27"=1,"30"=1,"31"=1,"36"=1,"37"=1,"40"=1,"41"=1))+
  xlab("Days")+
  guides(color=FALSE,size=FALSE,linetype=FALSE,color=guide_legend(override.aes=list(size=6)))+
  ylab("RFU (Chlorophyll-a)")+
  theme(plot.title = element_text(face='bold.italic',hjust = 0.5),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 16),axis.title=element_text(size=18,face="plain"),legend.title=element_text(size=16),legend.text=element_text(size=16))+
  ggtitle("M. minutum")+
  annotate("text",x=5,y=10^1.7,label=paste("list(ANOVA:~F['1,109']==3.24", ",p==0.075)"),color="red",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.6,label=paste("list(ANOVA:~F['1,109']==2.23", ",p==0.139)"),color="orange",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.5,label=paste("list(ANOVA:~F['1,109']==8.34", ",p==0.0047)"),color="springgreen3",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.4,label=paste("list(ANOVA:~F['1,109']==2.43", ",p==0.122)"),color="blue",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.3,label=paste("list(ANOVA:~F['1,109']==2.43", ",p==0.122)"),color="green",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.2,label=paste("list(ANOVA:~F['1,109']==2.37", ",p==0.127)"),color="chocolate4",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.1,label=paste("list(ANOVA:~F['1,109']==0.193", ",p==0.847)"),color="deeppink2",parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1,label=paste("list(ANOVA:~F['1,109']==3.77", ",p==0.55)"),color="plum2",parse=TRUE,hjust=0.0,size=5)

All.Oocystis.aggregate<-read.csv("C:/Users/sjackrel/Documents/UMich Projects/Mutual Invasibility/Oocystis_FigS3.csv")
All.Oocystis.aggregate$Treatment<-as.factor(All.Oocystis.aggregate$Treatment)
All.Oocystis.aggregate.5.plus.59<-subset(All.Oocystis.aggregate, subset = Treatment %in% c(5,59))
All.Oocystis.aggregate.5.plus.61<-subset(All.Oocystis.aggregate, subset = Treatment %in% c(5,61))
All.Oocystis.aggregate.5.plus.64<-subset(All.Oocystis.aggregate, subset = Treatment %in% c(5,64))
All.Oocystis.aggregate.5.plus.65<-subset(All.Oocystis.aggregate, subset = Treatment %in% c(5,65))
All.Oocystis.aggregate.5.plus.70<-subset(All.Oocystis.aggregate, subset = Treatment %in% c(5,70))
All.Oocystis.aggregate.5.plus.71<-subset(All.Oocystis.aggregate, subset = Treatment %in% c(5,71))
All.Oocystis.aggregate.5.plus.72<-subset(All.Oocystis.aggregate, subset = Treatment %in% c(5,72))
All.Oocystis.aggregate.5.plus.73<-subset(All.Oocystis.aggregate, subset = Treatment %in% c(5,73))
All.Oocystis.aggregate.5.plus.74<-subset(All.Oocystis.aggregate, subset = Treatment %in% c(5,74))
All.Oocystis.aggregate.5.plus.75<-subset(All.Oocystis.aggregate, subset = Treatment %in% c(5,75))

result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Oocystis.aggregate.5.plus.59)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Oocystis.aggregate.5.plus.61)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Oocystis.aggregate.5.plus.64)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Oocystis.aggregate.5.plus.65)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Oocystis.aggregate.5.plus.70)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Oocystis.aggregate.5.plus.71)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Oocystis.aggregate.5.plus.72)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Oocystis.aggregate.5.plus.73)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Oocystis.aggregate.5.plus.74)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Oocystis.aggregate.5.plus.75)
drop1(result,.~.,test="F")
All.Oocystis.all<-ggplot(All.Oocystis.aggregate.5.plus.59,aes(x=Day,y=Mean,colour=Treatment))+
  geom_line(data=All.Oocystis.aggregate.5.plus.59,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Oocystis.aggregate.5.plus.61,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Oocystis.aggregate.5.plus.64,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Oocystis.aggregate.5.plus.65,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Oocystis.aggregate.5.plus.70,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Oocystis.aggregate.5.plus.71,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Oocystis.aggregate.5.plus.74,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Oocystis.aggregate.5.plus.75,aes(linetype=Treatment,size=Treatment))+
  scale_color_manual(labels = c("5"="Axenic","59"="+ isolate 13","61"="Mycobacterium cosmeticum ","64"="Microbacterium sp.","65"="Sphingomonas sp. or Blastomonas natatoria strain","70"="Microbacterium sp.","71"="Uncultured Bacteria","74"="+ isolate 27","75"="Sphingomonas sp or Blastomonas natatoria strain"),values=c("5"="black","59"="red","61"="orange","64"="springgreen3","65"="blue","70"="green","71"="chocolate4","74"="deeppink2","75"="plum2","gold2","cyan","firebrick"))+
  scale_linetype_manual(values=c("5"="dashed","59"="solid","61"="solid","64"="solid","65"="solid","70"="solid","71"="solid","74"="solid","75"="solid"))+
  scale_y_log10()+
  scale_size_manual(values=c("5"=2,"59"=1,"61"=1,"64"=1,"65"=1,"70"=1,"71"=1,"74"=1,"75"=1))+
  xlab("Days")+
  guides(color=FALSE,size=FALSE,linetype=FALSE,color=guide_legend(override.aes=list(size=6)))+
  ylab("RFU (Chlorophyll-a)")+
  annotate("text",x=5,y=10^1.7,label=paste("list(ANOVA:~F['1,109']==2.87", ",p==0.0929)"),color='red',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.6,label=paste("list(ANOVA:~F['1,109']==3.38", ",p==0.0688)"),color='orange',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.5,label=paste("list(ANOVA:~F['1,109']==3.12", ",p==0.0803)"),color='springgreen3',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.4,label=paste("list(ANOVA:~F['1,109']==3.27", ",p==0.0734)"),color='blue',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.3,label=paste("list(ANOVA:~F['1,109']==6.65", ",p==0.0112)"),color='green',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.2,label=paste("list(ANOVA:~F['1,109']==2.18", ",p==0.143)"),color='chocolate4',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.1,label=paste("list(ANOVA:~F['1,109']==2.94", ",p==0.0891)"),color='deeppink2',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1,label=paste("list(ANOVA:~F['1,109']==25.53", ",p==1.76*e-{6})"),color='plum2',parse=TRUE,hjust=0.0,size=5)+
  theme(plot.title = element_text(face='bold.italic',hjust = 0.5),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 16),axis.title=element_text(size=18,face="plain"),legend.title=element_text(size=16),legend.text=element_text(size=16))+
  ggtitle("O. polymorpha")

All.Scenedesmus.aggregate<-read.csv("C:/Users/sjackrel/Documents/UMich Projects/Mutual Invasibility/Scenedesmus_FigS3.csv")
All.Scenedesmus.aggregate$Treatment<-as.factor(All.Scenedesmus.aggregate$Treatment)

All.Scenedesmus.aggregate.6.plus.93<-subset(All.Scenedesmus.aggregate, subset = Treatment %in% c(6,93))
All.Scenedesmus.aggregate.6.plus.95<-subset(All.Scenedesmus.aggregate, subset = Treatment %in% c(6,95))
All.Scenedesmus.aggregate.6.plus.98<-subset(All.Scenedesmus.aggregate, subset = Treatment %in% c(6,98))
All.Scenedesmus.aggregate.6.plus.99<-subset(All.Scenedesmus.aggregate, subset = Treatment %in% c(6,99))
All.Scenedesmus.aggregate.6.plus.104<-subset(All.Scenedesmus.aggregate, subset = Treatment %in% c(6,104))
All.Scenedesmus.aggregate.6.plus.105<-subset(All.Scenedesmus.aggregate, subset = Treatment %in% c(6,105))
All.Scenedesmus.aggregate.6.plus.108<-subset(All.Scenedesmus.aggregate, subset = Treatment %in% c(6,108))
All.Scenedesmus.aggregate.6.plus.109<-subset(All.Scenedesmus.aggregate, subset = Treatment %in% c(6,109))
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Scenedesmus.aggregate.6.plus.93)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Scenedesmus.aggregate.6.plus.95)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Scenedesmus.aggregate.6.plus.98)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Scenedesmus.aggregate.6.plus.99)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Scenedesmus.aggregate.6.plus.104)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Scenedesmus.aggregate.6.plus.105)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Scenedesmus.aggregate.6.plus.108)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Scenedesmus.aggregate.6.plus.109)
drop1(result,.~.,test="F")

All.Scenedesmus.all<-ggplot(All.Scenedesmus.aggregate.6.plus.93,aes(x=Day,y=Mean,colour=Treatment))+
  geom_line(data=All.Scenedesmus.aggregate.6.plus.93,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Scenedesmus.aggregate.6.plus.95,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Scenedesmus.aggregate.6.plus.98,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Scenedesmus.aggregate.6.plus.99,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Scenedesmus.aggregate.6.plus.104,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Scenedesmus.aggregate.6.plus.105,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Scenedesmus.aggregate.6.plus.108,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Scenedesmus.aggregate.6.plus.109,aes(linetype=Treatment,size=Treatment))+
  scale_color_manual(labels = c("6"="Axenic","93"="+ isolate 13","95"="Mycobacterium cosmeticum ","98"="Microbacterium sp.","99"="Sphingomonas sp. or Blastomonas natatoria strain","104"="Microbacterium sp.","105"="Uncultured Bacteria","108"="+ isolate 27","109"="Sphingomonas sp or Blastomonas natatoria strain"),values=c("6"="black","93"="red","95"="orange","98"="springgreen3","99"="blue","104"="green","105"="chocolate4","108"="deeppink2","109"="plum2","gold2","cyan","firebrick"))+
  scale_linetype_manual(values=c("6"="dashed","93"="solid","95"="solid","98"="solid","99"="solid","104"="solid","105"="solid","108"="solid","109"="solid"))+
  scale_y_log10()+
  scale_size_manual(values=c("6"=2,"93"=1,"95"=1,"98"=1,"99"=1,"104"=1,"105"=1,"108"=1,"109"=1))+
  xlab("Days")+
  guides(color=FALSE,size=FALSE,linetype=FALSE,color=guide_legend(override.aes=list(size=6)))+
  ylab("RFU (Chlorophyll-a)")+
  annotate("text",x=5,y=10^1.7,label=paste("list(ANOVA:~F['1,95']==17.49", ",p==6.44*e-{5})"),color='red',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.6,label=paste("list(ANOVA:~F['1,95']==5.71", ",p==0.0189)"),color='orange',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.5,label=paste("list(ANOVA:~F['1,95']==32.70", ",p==1.24*e^-{7})"),color='springgreen3',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.4,label=paste("list(ANOVA:~F['1,95']==76.2", ",p==8.49*e^-{14})"),color='blue',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.3,label=paste("list(ANOVA:~F['1,95']==61.2", ",p==7.03*e^-{12})"),color='green',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.2,label=paste("list(ANOVA:~F['1,95']==12.1", ",p==0.000764)"),color='chocolate4',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.1,label=paste("list(ANOVA:~F['1,95']==32.15", ",p==1.53*e^-{7})"),color='deeppink2',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1,label=paste("list(ANOVA:~F['1,95']==45.37", ",p==1.24*e^-{9})"),color='plum2',parse=TRUE,hjust=0.0,size=5)+
  theme(plot.title = element_text(face='bold.italic',hjust = 0.5),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 16),axis.title=element_text(size=18,face="plain"),legend.title=element_text(size=16),legend.text=element_text(size=16))+
  ggtitle("S. acuminatus")

All.Selenastrum.aggregate<-read.csv("C:/Users/sjackrel/Documents/UMich Projects/Mutual Invasibility/Selenastrum_FigS3.csv")
All.Selenastrum.aggregate$Treatment<-as.factor(All.Selenastrum.aggregate$Treatment)

All.Selenastrum.aggregate.7.plus.76<-subset(All.Selenastrum.aggregate, subset = Treatment %in% c(7,76))
All.Selenastrum.aggregate.7.plus.78<-subset(All.Selenastrum.aggregate, subset = Treatment %in% c(7,78))
All.Selenastrum.aggregate.7.plus.81<-subset(All.Selenastrum.aggregate, subset = Treatment %in% c(7,81))
All.Selenastrum.aggregate.7.plus.82<-subset(All.Selenastrum.aggregate, subset = Treatment %in% c(7,82))
All.Selenastrum.aggregate.7.plus.87<-subset(All.Selenastrum.aggregate, subset = Treatment %in% c(7,87))
All.Selenastrum.aggregate.7.plus.88<-subset(All.Selenastrum.aggregate, subset = Treatment %in% c(7,88))
All.Selenastrum.aggregate.7.plus.90<-subset(All.Selenastrum.aggregate, subset = Treatment %in% c(7,90))
All.Selenastrum.aggregate.7.plus.91<-subset(All.Selenastrum.aggregate, subset = Treatment %in% c(7,91))
All.Selenastrum.aggregate.7.plus.92<-subset(All.Selenastrum.aggregate, subset = Treatment %in% c(7,92))

result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Selenastrum.aggregate.7.plus.76)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Selenastrum.aggregate.7.plus.78)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Selenastrum.aggregate.7.plus.81)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Selenastrum.aggregate.7.plus.82)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Selenastrum.aggregate.7.plus.87)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Selenastrum.aggregate.7.plus.88)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Selenastrum.aggregate.7.plus.90)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Selenastrum.aggregate.7.plus.91)
drop1(result,.~.,test="F")
result<-lm(Mean.RFU..Chl.a70.460.685.~Day+Treatment,data=All.Selenastrum.aggregate.7.plus.92)
drop1(result,.~.,test="F")

All.Selenastrum.all<-ggplot(All.Selenastrum.aggregate.7.plus.76,aes(x=Day,y=Mean,colour=Treatment))+
  geom_line(data=All.Selenastrum.aggregate.7.plus.76,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Selenastrum.aggregate.7.plus.78,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Selenastrum.aggregate.7.plus.81,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Selenastrum.aggregate.7.plus.82,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Selenastrum.aggregate.7.plus.87,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Selenastrum.aggregate.7.plus.88,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Selenastrum.aggregate.7.plus.91,aes(linetype=Treatment,size=Treatment))+
  geom_line(data=All.Selenastrum.aggregate.7.plus.92,aes(linetype=Treatment,size=Treatment))+
  scale_color_manual(labels = c("7"="Axenic","76"="+ isolate 13","78"="Mycobacterium cosmeticum ","81"="Microbacterium sp.","82"="Sphingomonas sp. or Blastomonas natatoria strain","87"="Microbacterium sp.","88"="Uncultured Bacteria","91"="+ isolate 27","92"="Sphingomonas sp or Blastomonas natatoria strain"), values=c("7"="black","76"="red","78"="orange","81"="springgreen3","82"="blue","87"="green","88"="chocolate4","91"="deeppink2","92"="plum2","gold2","cyan","firebrick"))+
  scale_linetype_manual(values=c("7"="dashed","76"="solid","78"="solid","81"="solid","82"="solid","87"="solid","88"="solid","91"="solid","92"="solid"))+
  scale_y_log10()+
  scale_size_manual(values=c("7"=2,"76"=1,"78"=1,"81"=1,"82"=1,"87"=1,"88"=1,"91"=1,"92"=1))+
  xlab("Days")+
  guides(color=FALSE,size=FALSE,linetype=FALSE,color=guide_legend(override.aes=list(size=6)))+
  ylab("RFU (Chlorophyll-a)")+
  annotate("text",x=5,y=10^1.7,label=paste("list(ANOVA:~F['1,109']==4.28", ",p==0.0414)"),color='red',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.6,label=paste("list(ANOVA:~F['1,109']==18.8", ",p==3.22*e^-{5})"),color='orange',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.5,label=paste("list(ANOVA:~F['1,109']==17.4", ",p==6.14*e^-{5})"),color='springgreen3',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.4,label=paste("list(ANOVA:~F['1,109']==26.6", ",p==1.11*e^-{6})"),color='blue',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.3,label=paste("list(ANOVA:~F['1,109']==31.6", ",p==1.46*e^-{7})"),color='green',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.2,label=paste("list(ANOVA:~F['1,109']==9.71", ",p==0.0023)"),color='chocolate4',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1.1,label=paste("list(ANOVA:~F['1,109']==9.28", ",p==0.0029)"),color='deeppink2',parse=TRUE,hjust=0.0,size=5)+
  annotate("text",x=5,y=10^1,label=paste("list(ANOVA:~F['1,109']==11.10", ",p==0.0012)"),parse=TRUE,hjust=0.0,size=5)+
  theme(plot.title = element_text(face='bold.italic',hjust = 0.5),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 16),axis.title=element_text(size=18,face="plain"),legend.title=element_text(size=16),legend.text=element_text(size=16))+
  ggtitle("S. capricornutum")

grid.arrange(All.Chlorella.all,All.Coelastrum.all, All.Monoraphidium.all,All.Oocystis.all,All.Scenedesmus.all,All.Selenastrum.all,ncol=2)

# Supplementary Fig. S4. Host-associated bacteria alter host population dynamics.
#K
#bacteria.SteadyState.fitted.heatmap<-read.csv("C:/Users/sjackrel/Desktop/Lenovo_Backup/Mutual-Invasibility-master/Algal Bacteria Fitted K Heatmap.csv")
bacteria.SteadyState.fitted.heatmap<-read.csv("./Algal Bacteria Fitted K Heatmap.csv")
rownames(bacteria.SteadyState.fitted.heatmap) <- bacteria.SteadyState.fitted.heatmap[,1]
bacteria.SteadyState.fitted.heatmap$X<-NULL
bacteria.SteadyState.fitted.heatmap<-as.matrix(bacteria.SteadyState.fitted.heatmap)
cols = colorRampPalette(c("blue","steelblue2","white","orange","red"))(19)
draw_colnames_45 <- function (coln, gaps, ...) {
  coord = pheatmap:::find_coordinates(length(coln), gaps)
  x = coord$coord - 0.25 * coord$size
  res = textGrob(coln, x = x, y = unit(1.0, "npc") - unit(2,"bigpts"), vjust = 1.5, hjust = 1.1, rot = 55, gp = gpar(...))
  return(res)}
assignInNamespace(x="draw_colnames", value="draw_colnames_45",
                  ns=asNamespace("pheatmap"))
mylabels=c(expression(paste(italic("Chlorella\nsorokiniana"))),expression(paste(italic("Coelastrum\nmicroporum"))),expression(paste(italic("Monoraphidium\nminutum"))),expression(paste(italic("Oocystis\npolymorpha"))),expression(paste(italic("Scenedesmus\nacuminatus"))),expression(paste(italic("Selenastrum\ncapricornutum"))))
mycols=c(expression(paste(Isolate~1[paste(italic("C.m."))])),expression(paste(Isolate~2[paste(italic("M.m."))])),expression(paste(Isolate~3[paste(italic("M.m."))])),expression(paste(Isolate~4[paste(italic("O.p."))])),expression(paste(Isolate~5[paste(italic("O.p."))])),expression(paste(Isolate~6[paste(italic("S.a."))])),expression(paste(Isolate~7[paste(italic("S.a."))])),expression(paste(Isolate~8[paste(italic("S.c."))])))
mycols2=c(expression(paste(Isolate~1~(italic("C.m.")))),expression(paste(Isolate~2~(italic("M.m.")))),expression(paste(Isolate~3~(italic("M.m.")))),expression(paste(Isolate~4~(italic("O.p.")))),expression(paste(Isolate~5~(italic("O.p.")))),expression(paste(Isolate~6~(italic("S.a.")))),expression(paste(Isolate~7~(italic("S.a.")))),expression(paste(Isolate~8~(italic("S.c.")))))
pheatmap(mat = bacteria.SteadyState.fitted.heatmap,border_color= NA,show_colnames= TRUE,main = "Steady State Density",labels_col=mylabels,labels_row=mycols2,show_rownames= TRUE,color=cols,breaks = c(0.2,0.40,0.50,0.60,0.70,0.80,0.85,0.90,0.95,1.0,1.05,1.1,1.15,1.2,1.3,1.4,1.5,1.6,1.8), drop_levels = TRUE,fontsize= 14,cluster_rows=F, cluster_cols=F)
#mu
bacteria.mu.fitted.heatmap<-read.csv("C:/Users/sjackrel/Desktop/Lenovo_Backup/Mutual-Invasibility-master/Algal Bacteria Fitted Mu Heatmap.csv")
#bacteria.mu.fitted.heatmap<-read.csv("./Algal Bacteria Fitted Mu Heatmap.csv")
rownames(bacteria.mu.fitted.heatmap) <- bacteria.mu.fitted.heatmap[,1]
bacteria.mu.fitted.heatmap$X<-NULL
bacteria.mu.fitted.heatmap<-as.matrix(bacteria.mu.fitted.heatmap)
cols = colorRampPalette(c("blue","steelblue2","white","orange","red"))(19)
assignInNamespace(x="draw_colnames", value="draw_colnames_45",
                  ns=asNamespace("pheatmap"))
pheatmap(mat = bacteria.mu.fitted.heatmap,border_color= NA,show_colnames= TRUE,main = "Exponential Rate of Growth",labels_col=mylabels,labels_row=mycols,show_rownames= TRUE,color=cols,breaks = c(0.2,0.40,0.5,0.60,0.70,0.80,0.85,0.90,0.95,1.0,1.05,1.1,1.15,1.2,1.3,1.4,1.5,1.6,1.8), drop_levels = TRUE,fontsize= 14,cluster_rows=F, cluster_cols=F)

# Supplementary Fig. S5. Fluorescence growth curves of experiment. 

pre_Monos<-read.csv("C:/Users/sjackrel/Desktop/Lenovo_Backup/Mutual-Invasibility-master/All_Monos_bind_cleaned.csv")
Mono_stats_pre <- summarySE(pre_Monos, measurevar="Mean.RFU..Chl.a70.460.685.", groupvars=c("Time","Axenic_State","Species_1"))
All_Monos_graph<-ggplot(Mono_stats_pre,aes(Time,Mean.RFU..Chl.a70.460.685.,colour=Species_1,shape=Axenic_State))+
  geom_errorbar(aes(ymin=Mean.RFU..Chl.a70.460.685.-se, ymax=Mean.RFU..Chl.a70.460.685.+se), width=.1) +
  geom_point(size=3)+
  scale_shape_manual(name="Axenic Status",values=c(15,16),labels=c("axenic"="Axenic","xenic"="Xenic"))+
  theme(plot.title = element_text(hjust = 0.5),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 16),axis.title=element_text(size=18,face="plain"),legend.title=element_text(size=16),legend.text=element_text(size=16))+
  xlab("Hours")+
  scale_y_log10()+
  ylab("RFU (Chlorophyll-a)")+
  guides(color=guide_legend(title="Species"))+
  scale_color_manual(name="Species",labels=c("Cm"=expression(italic("Coelastrum microporum")),"Mm"=expression(italic("Monoraphridium minutum")),"Sa"=expression(italic("Selenastrum accuminatus")),"Sc"=expression(italic("Scenedesmus capricornutum"))),values=c("Cm"="#E69F00","Mm"="#56B4E9","Sa"="#009E73","Sc"="#0072B2"))
All_Monos_graph

# Supplementary Fig. S6. Fitted curves to cell count data.  
#data<-read.csv("C:/Users/sjackrel/Documents/UMich Projects/Mutual Invasibility/Algal_Growth_Curves.csv")
data<-read.csv("./Algal_Growth_Curves.csv")

data_1<-data[(data$Treatment_Category=="1"),]
data_2<-data[(data$Treatment_Category=="2"),]
data_3<-data[(data$Treatment_Category=="3"),]
data_4<-data[(data$Treatment_Category=="4"),]
data_5<-data[(data$Treatment_Category=="5"),]
data_6<-data[(data$Treatment_Category=="6"),]
data_7<-data[(data$Treatment_Category=="7"),]
data_8<-data[(data$Treatment_Category=="8"),]
data_9<-data[(data$Treatment_Category=="9"),]
data_10<-data[(data$Treatment_Category=="10"),]
data_11<-data[(data$Treatment_Category=="11"),]
data_12<-data[(data$Treatment_Category=="12"),]
data_13<-data[(data$Treatment_Category=="13"),]
data_14<-data[(data$Treatment_Category=="14"),]
data_15<-data[(data$Treatment_Category=="15"),]
data_16<-data[(data$Treatment_Category=="16"),]
data_17<-data[(data$Treatment_Category=="17"),]
data_18<-data[(data$Treatment_Category=="18"),]
data_19<-data[(data$Treatment_Category=="19"),]
data_20<-data[(data$Treatment_Category=="20"),]
data_21<-data[(data$Treatment_Category=="21"),]
data_22<-data[(data$Treatment_Category=="22"),]
data_23<-data[(data$Treatment_Category=="23"),]
data_24<-data[(data$Treatment_Category=="24"),]
data_25<-data[(data$Treatment_Category=="25"),]
data_26<-data[(data$Treatment_Category=="26"),]
data_27<-data[(data$Treatment_Category=="27"),]
data_28<-data[(data$Treatment_Category=="28"),]
data_29<-data[(data$Treatment_Category=="29"),]
data_30<-data[(data$Treatment_Category=="30"),]
data_31<-data[(data$Treatment_Category=="31"),]
data_32<-data[(data$Treatment_Category=="32"),]

CM_compiled<-rbind(data_1,data_5,data_15,data_16,data_27,data_28,data_21,data_22)
CM_compiled_graph<-ggplot(CM_compiled,aes(Hour, Cells_per_mL,colour=as.factor(Treatment_Category),shape=as.factor(Treatment_Category),fill=Bacteria))+
  geom_jitter(size =3,width=4)+
  xlim(-5,105)+  #geom_point(size=3)+
  # geom_line(aes(y= prediction), color="red")+
  scale_y_log10(limits = c(900,1e6))+
  theme(plot.title = element_text(hjust = 0.5,size = 20),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 18),axis.title=element_text(size=18,face="plain"),legend.title=element_text(size=18),legend.text=element_text(size=18))+
  xlab("\nHours")+
  stat_smooth(method="lm", se=TRUE, fill=NA,formula=y ~ poly(x, 3, raw=TRUE))+
  ylab("Cells per mL\n(log10)\n")+
  guides(size=FALSE,fill=FALSE)+
  ggtitle(expression(paste(italic("Coelastrum microporum"))))+
  scale_shape_manual(name="Treatment",labels=c("1"="Axenic Monoculture","5"="Xenic Monoculture","15"="Axenic Invading S.a.","16"="Xenic Invading S.a.","21"="Axenic Invading M.m.","22"="Xenic Invading M.m.","27"="Axenic Invading S.c.","28"="Xenic Invading S.c."),values=c("5"=16,"1"=21,"15"=22,"16"=15,"27"=23,"28"=18,"21"=24,"22"=17))+
  scale_fill_manual(values=c("5"="gray","1"="gray","15"="black","16"="black","27"="red","28"="red","21"="chocolate4","22"="chocolate4"))+
  scale_color_manual("Treatment",labels=c("1"="Axenic Monoculture","5"="Xenic Monoculture","15"="Axenic Invading S.a.","16"="Xenic Invading S.a.","21"="Axenic Invading M.m.","22"="Xenic Invading M.m.","27"="Axenic Invading S.c.","28"="Xenic Invading S.c."),values=c("5"="gray","1"="gray","15"="black","16"="black","27"="red", "28"="red","21"="chocolate4","22"="chocolate4"))

MM_compiled<-rbind(data_3,data_7,data_17,data_18,data_31,data_32,data_11,data_12)
MM_compiled_graph<-ggplot(MM_compiled,aes(Hour, Cells_per_mL,colour=as.factor(Treatment_Category),shape=as.factor(Treatment_Category),fill=Bacteria))+
  geom_jitter(size =3,width=4)+
  xlim(-5,105)+  #geom_point(size=3)+
  # geom_line(aes(y= prediction), color="red")+
  scale_y_log10(limits = c(900,1e8))+
  theme(plot.title = element_text(hjust = 0.5,size = 20),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 18),axis.title=element_text(size=18,face="plain"),legend.title=element_text(size=18),legend.text=element_text(size=18))+
  xlab("\nHours")+
  stat_smooth(method="lm", se=TRUE, fill=NA,formula=y ~ poly(x, 3, raw=TRUE))+
  ylab("Cells per mL\n(log10)\n")+
  guides(size=FALSE,fill=FALSE)+
  ggtitle(expression(paste(italic("Monoraphidium minutum"))))+
  scale_shape_manual(name="Treatment",labels=c("3"="Axenic Monoculture","7"="Xenic Monoculture","17"="Axenic Invading S.a.","18"="Xenic Invading S.a.","11"="Axenic Invading C.m.","12"="Xenic Invading C.m.","31"="Axenic Invading S.c.","32"="Xenic Invading S.c."),values=c("3"=16,"7"=21,"17"=22,"18"=15,"31"=23,"32"=18,"11"=24,"2"=17))+
  scale_fill_manual(values=c("3"="gray","7"="gray","17"="black","18"="black","31"="red","32"="red","11"="chocolate4","12"="chocolate4"))+
  scale_color_manual("Treatment",labels=c("3"="Axenic Monoculture","7"="Xenic Monoculture","17"="Axenic Invading S.a.","18"="Xenic Invading S.a.","11"="Axenic Invading C.m.","12"="Xenic Invading C.m.","31"="Axenic Invading S.c.","32"="Xenic Invading S.c."),values=c("3"="gray","7"="gray","17"="black","18"="black","31"="red", "32"="red","11"="chocolate4","12"="chocolate4"))

Sa_compiled<-rbind(data_2,data_6,data_9,data_10,data_23,data_24,data_29,data_30)
Sa_compiled_graph<-ggplot(Sa_compiled,aes(Hour, Cells_per_mL,colour=as.factor(Treatment_Category),shape=as.factor(Treatment_Category),fill=Bacteria))+
  geom_jitter(size =3,width=4)+
  xlim(-5,105)+  #geom_point(size=3)+
  # geom_line(aes(y= prediction), color="red")+
  scale_y_log10(limits = c(900,1e6))+
  theme(plot.title = element_text(hjust = 0.5,size = 20),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 18),axis.title=element_text(size=18,face="plain"),legend.title=element_text(size=18),legend.text=element_text(size=18))+
  xlab("\nHours")+
  stat_smooth(method="lm", se=TRUE, fill=NA,formula=y ~ poly(x, 3, raw=TRUE))+
  ylab("Cells per mL\n(log10)\n")+
  guides(size=FALSE,fill=FALSE)+
  ggtitle(expression(paste(italic("Scenedesmus accuminatus"))))+
  scale_shape_manual(name="Treatment",labels=c("2"="Axenic Monoculture","6"="Xenic Monoculture","9"="Axenic Invading C.m.","10"="Xenic Invading C.m.","23"="Axenic Invading M.m.","24"="Xenic Invading M.m.","29"="Axenic Invading S.c.","30"="Xenic Invading S.c."),values=c("2"=16,"6"=21,"9"=22,"10"=15,"23"=23,"24"=18,"29"=24,"30"=17))+
  scale_fill_manual(values=c("2"="gray","6"="gray","9"="black","10"="black","23"="red","24"="red","29"="chocolate4","30"="chocolate4"))+
  scale_color_manual("Treatment",labels=c("2"="Axenic Monoculture","6"="Xenic Monoculture","9"="Axenic Invading C.m.","10"="Xenic Invading C.m.","23"="Axenic Invading M.m.","24"="Xenic Invading M.m.","29"="Axenic Invading S.c.","30"="Xenic Invading S.c."),values=c("2"="gray","6"="gray","9"="black","10"="black","23"="red", "24"="red","29"="chocolate4","30"="chocolate4"))

Sc_compiled<-rbind(data_4,data_8,data_13,data_14,data_19,data_20,data_25,data_26)
Sc_compiled_graph<-ggplot(Sc_compiled,aes(Hour, Cells_per_mL,colour=as.factor(Treatment_Category),shape=as.factor(Treatment_Category),fill=Bacteria))+
  geom_jitter(size =3,width=4)+
  xlim(-5,105)+  #geom_point(size=3)+
  # geom_line(aes(y= prediction), color="red")+
  scale_y_log10(limits = c(900,1e6))+
  theme(plot.title = element_text(hjust = 0.5,size = 20),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 18),axis.title=element_text(size=18,face="plain"),legend.title=element_text(size=18),legend.text=element_text(size=18))+
  xlab("\nHours")+
  stat_smooth(method="lm", se=TRUE, fill=NA,formula=y ~ poly(x, 3, raw=TRUE))+
  ylab("Cells per mL\n(log10)\n")+
  guides(size=FALSE,fill=FALSE)+
  ggtitle(expression(paste(italic("Selenastrum capricornutum"))))+
  scale_shape_manual(name="Treatment",labels=c("4"="Axenic Monoculture","8"="Xenic Monoculture","19"="Axenic Invading S.a.","20"="Xenic Invading S.a.","13"="Axenic Invading C.m.","14"="Xenic Invading C.m.","25"="Axenic Invading M.m.","26"="Xenic Invading M.m."),values=c("4"=16,"8"=21,"13"=22,"14"=15,"19"=23,"20"=18,"25"=24,"26"=17))+
  scale_fill_manual(values=c("4"="gray","8"="gray","13"="black","14"="black","19"="red","20"="red","25"="chocolate4","26"="chocolate4"))+
  scale_color_manual("Treatment", labels=c("4"="Axenic Monoculture","8"="Xenic Monoculture","19"="Axenic Invading S.a.","20"="Xenic Invading S.a.","13"="Axenic Invading C.m.","14"="Xenic Invading C.m.","25"="Axenic Invading M.m.","26"="Xenic Invading M.m."),values=c("4"="gray","8"="gray","13"="black","14"="black","19"="red", "20"="red","25"="chocolate4","26"="chocolate4"))

grid.arrange(CM_compiled_graph,MM_compiled_graph,Sa_compiled_graph,Sc_compiled_graph)

# Supplementary Fig. S7. Growth rates and CV over time of cell count data.  

data<-read.csv("C:/Users/sjackrel/Desktop/Lenovo_Backup/Mutual-Invasibility-master/Growth Rate Deceleration.csv")
data<-read.csv("./Growth Rate Deceleration.csv")
summary(aov(Growth.Rate~Stage,data=data))
data<-data[!is.na(data$Growth.Rate),]

deceleration<-ggplot(data, aes(Stage,Growth.Rate,colour=Bacteria,shape=Bacteria))+
  geom_jitter(aes(size=3))+
  scale_shape_manual(name="Axenic Status",values=c(15,16))+
  scale_colour_manual(name="Axenic Status",values=c("black","gray"))+
  theme(axis.text.x = element_text(hjust = 1,size=18),plot.title = element_text(hjust = 0.5),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 16),axis.title=element_text(size=18,face="plain"),legend.title=element_text(size=16),legend.text=element_text(size=16))+
  xlab("\nTime Span")+
  annotate("text",x=1.05,y=7,label="Growth Rate by Time:",size=5)+
  annotate("text",x=0.52,y=6.5,label=paste("list(ANOVA:~F['2,194']==516.8", ",p==2*e^-{16})"),parse=TRUE,hjust=0.0,size=5)+
  guides(size=FALSE, colour = guide_legend(override.aes = list(size=5)),shape = guide_legend(override.aes = list(size=5)))+
  ylab("Growth Rate\n")+
  guides(size=FALSE)
deceleration

#data<-read.csv("C:/Users/sjackrel/Documents/UMich Projects/Mutual Invasibility/CV_output.csv")
data<-read.csv("./CV_output.csv")
data<-data[!(data$Hour=="0"),]
data<-data[(data$Monoculture_or_Biculture=="Biculture"),]
data$Treatment_.<-NULL
data$Cells_per_mL.x<-NULL
data<-unique(data)
result<-aov(asin(sqrt(Cells_per_mL_CV/100))~Hour, data=data)
ggplot(data,aes(Day, Cells_per_mL_CV,color=Bacteria,shape=Bacteria,alpha=0.45))+
  geom_point(position = position_dodge(width = 0.35),size=6)+
  ylab("Coefficient of Variation\n(cell density counts)\n")+
  xlab("\nDay")+
  theme(plot.title = element_text(hjust = 0.5,size = 20),legend.text.align = 0,legend.key=element_rect(fill=NA),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size = 18),axis.title=element_text(size=18,face="plain"),legend.title=element_text(size=18),legend.text=element_text(size=18))+
  annotate("text", x=0.3, y=80,label="CV by Time:",size=5)+
  annotate("text", x=-0.11, y=75,label=paste("list(ANOVA:~F['1,70']==0.149", ", p==0.701)"),parse=TRUE,hjust=0.0,size=5)+
  scale_colour_manual(name="Axenic Status",values=c("black","gray"))+
  scale_shape_manual(name="Axenic Status",values=c(15,16))+
  guides(alpha=FALSE)


