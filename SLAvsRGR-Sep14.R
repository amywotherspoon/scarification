##### Get data #####

read.csv("/Users/Amy/Desktop/R/SLAvsRGR.csv") #laptop
data<-read.csv("/Users/Amy/Desktop/R/SLAvsRGR.csv") #laptop

#read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/R/SLAvsRGR-byyear.csv") #desktop
#data<-read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/R/SLAvsRGR-byyear.csv") #desktop

#data<-na.omit(data2)

str(data)
data$Block<-factor(data$Block)
#data$Tree<-factor(data$Tree)
data$Year<-factor(data$Year)
#data$X2007<-as.integer(data$X2007)
#data$X2015<-as.integer(data$X2015)
data$X2017<-as.integer(data$X2017)
sapply(data,class)

##### Subsets #####

#RGR= ((log(data$X2017))-(log(data$X2017))/(2017-2007))

abitibi<-subset(data,Climate=="Abitibi")
cn<-subset(data,Climate=="Cote Nord")
#RGR.ab= ((log(abitibi$X2017))-(log(abitibi$X2017))/(2017-2007))
#RGR.cn= ((log(cn$X2017))-(log(cn$X2017))/(2017-2007))

tr<-subset(data,Trt=="TR")
te<-subset(data,Trt=="TE")
#RGR.tr= ((log(tr$X2017))-(log(tr$X2017))/(2017-2007))
#RGR.te= ((log(te$X2017))-(log(te$X2017))/(2017-2007))

##### Exploring Data #####

hist(data$SLA)
shapiro.test(data$SLA)
boxplot(data=data,SLA~Climate*Trt,las=2)

#Data Transformations
logSLA<-log(data$SLA) 
log10SLA<-log10(data$SLA)
sqrtSLA<-sqrt(data$SLA)
hist(sqrtSLA)
shapiro.test(data$SLA)

#GGPlot
library(ggplot2)
ggplot(data=data,aes(x=Trt,y=SLA,fill=Trt))+
  geom_boxplot()+
#  scale_colour_hue(l=50)+
  geom_smooth(method="lm",se=FALSE)+
  scale_fill_grey(start=0.4,end=0.8)+ theme_classic()+
  theme(legend.position="none")+
  facet_grid(~Climate)+  #labeller = as_labeller(Trt_names
  scale_x_discrete(labels=c("TR" = "Scarified", "TE" = "Non-scarified"))+
  xlab("Treatment")+
  ylab("Specific leaf area (cm2/g)")+
  ggtitle("SLA x Treatment x Climate")

##### GLM #####

library(lme4)
library(arm)
library(lmerTest)
library(multcomp)
library(nlme) #for lme

#To compare multipled models with same mixed effects use method="REML". Otherwise use method="ML"
model.sla<-lme(data=data, SLA~Climate*Trt,random=~1|Block/Year/Tree,method="ML")
summary(model.sla)
glht(model.sla,linfct=mcp(Climate="Tukey"))

model.lmer<-lmer(data=data,SLA~Climate*Trt*Block*Year*Tree+(1|Block)+(1|Year)+(1|Tree))
summary(model.lmer)

summary(model.ab<-lme(data=abitibi,RGR.ab~SLA*Trt,random=~1|Block/Year/Tree))
anova(model.ab)
glht(model.ab,linfct=mcp(Trt="Tukey"))

summary(model.cn<-lme(data=cn,RGR.cn~SLA*Trt,random=~1|Block/Year/Tree))
anova(model.cn)
glht(model.cn,linfct=mcp(Trt="Tukey"))

anova(model.ab,model.cn)

##### Aggregates & Tukey Test #####

aggregate(SLA~Climate*Trt*Year,data=data,mean)
aggregate(SLA~Climate*Trt*Year,data=data,sd)

