setwd("C:/Users/63139/Documents/GitHub/Yuchen-Creative-Component")
data<- read.csv(file="General.csv",header=T)
data.clean<-na.omit(data)
data.part<-data.clean[c(data.clean[,2]==1|data.clean[,2]==2),c(1,2,12:18)]
##--------pca regression----------
data.pca<- princomp(data.part[,2:8],cor=T)
data.r<-data.frame(data.part[,c(1,2)],predict(data.pca))
data.r$Veteran<-as.factor(data.r$Veteran)
data.r$HeartAttack<-data.r$HeartAttack-1
set.seed(2018)
samp1<-nrow(data.r)
a<-sample(samp1,0.7*samp1)
train<-data.r[a,]
test<-data.r[-a,]
m1<-glm(HeartAttack~Veteran+Comp.1+Comp.2+Comp.2+Comp.4+Comp.5+Comp.6,family=binomial(link='logit'),data=train)
fitted.results1<-predict(m1,newdata=subset(test),type='response')
r1<-data.frame(D=test$HeartAttack,M=fitted.results1,mod=rep("M1",length(fitted.results1)))
##------logistic regression--------
data_original<-data.part
MStatus2<-rep(1,365315)
data_original<-data.frame(data_original,MStatus2)
data_original[data_original$MStatus==2|data_original$MStatus==2|data_original$MStatus==4,9]<-2
data_original[data_original$MStatus==5|data_original$MStatus==6,9]<-3
data_original$HeartAttack<-data_original$HeartAttack-1
data_original$Veteran<-as.factor(data_original$Veteran)
data_original$Education<-as.factor(data_original$Education)
data_original$Age<-as.factor(data_original$Age)
data_original$Income<-as.factor(data_original$Income)
data_original$Sex<-as.factor(data_original$Sex)
data_original$MStatus<-as.factor(data_original$MStatus)
data_original$Employment<-as.factor(data_original$Employment)
data_original$MStatus2<-as.factor(data_original$MStatus2)
set.seed(2018)
samp1<-nrow(data_original)
a<-sample(samp1,0.7*samp1)
train<-data_original[a,]
test<-data_original[-a,]
##base model
m0<-glm(HeartAttack~Age+Education+Employment+Income+MStatus+Sex,family= binomial(link='logit'),data=train)
fitted.results0<-predict(m0,newdata=subset(test),type='response')
r0<-data.frame(D=test$HeartAttack,M=fitted.results0,mod=rep("M0",length(fitted.results0)))
##base model + veteran
m1<-glm(HeartAttack~Veteran+Age+Education+Employment+Income+MStatus+Sex,family= binomial(link='logit'),data=train)
fitted.results1<-predict(m1,newdata=subset(test),type='response')
r1<-data.frame(D=test$HeartAttack,M=fitted.results1,mod=rep("M1",length(fitted.results1)))
##base model * veteran
m2<-glm(HeartAttack ~ Veteran*(Age+Education+Employment+Income+MStatus+Sex), family =  binomial(link = "logit"), data = test)
fitted.results2<-predict(m2,newdata=subset(test),type='response')
r2<-data.frame(D=test$HeartAttack,M=fitted.results2,mod=rep("M2.1",length(fitted.results2)))
##base model*veteran (but with new version feature)
m3<-glm(HeartAttack~Veteran*(Employment+Income+MStatus2+Sex)+Age,family= binomial(link='logit'),data=train)
fitted.results3<-predict(m3,newdata=subset(test),type='response')
r3<-data.frame(D=test$HeartAttack,M=fitted.results3,mod=rep("M2.2",length(fitted.results3)))
##base model*veteran + sex* others
m4<-glm(HeartAttack ~Veteran*(Age+Education+Employment+Income+MStatus+Sex)+Sex*(Age+Education+Employment+Income+MStatus), family =  binomial(link = "logit"), data = train)
fitted.results4<-predict(m4,newdata=subset(test),type='response')
r4<-data.frame(D=test$HeartAttack,M=fitted.results4,mod=rep("M3.1",length(fitted.results4)))
##base model*veteran + sex* others (but with new version feature)
m5<-glm(HeartAttack ~Age+Veteran*(Employment+Income+MStatus2+Sex)+Sex*(Age+Employment+Income+MStatus2), family =  binomial(link = "logit"), data = train)
fitted.results5<-predict(m5,newdata=subset(test),type='response')
r5<-data.frame(D=test$HeartAttack,M=fitted.results5,mod=rep("M3.2",length(fitted.results5)))

##---------ROC curve-----------
library(ggplot2)
library(plotROC)
R<-rbind(r0,r1)
##the whole plot
ggplot(data=R2,aes(d=D,m=M,color=mod))+geom_roc()
##enlarged plot
ggplot(data=R,aes(d=D,m=M,color=mod))+geom_roc()+scale_x_continuous(limits=c(0,0.5))+scale_y_continuous(limits=c(0.25,0.875))
