##################################################################
##
##    Version for estimates of oocyst intensity
##     Different treatments
##
##################################################################
setwd("C:\\Users\\Ellie\\Documents\\")

require(plotrix)
require(glmmADMB)

intd=read.table("~\\Data Malaria\\UCT OZPIP FER DSM Feb2015\\M2M OocystIntensity\\OocystIntensity.txt",header=T)

tapply(intd$oocysts,intd$treat,mean)
tapply(intd$oocysts,intd$treat,se)
intd$prev<-ifelse(intd$oocysts<1,0,1)
prev<-tapply(intd$prev,intd$treat,sum)/100

blank<-subset(intd,treat=="Blank")
UCT<-subset(intd,treat=="UCT")
OZPIP<-subset(intd,treat=="OZPIP")
OZFER<-subset(intd,treat=="OZFER")
OZDSM<-subset(intd,treat=="OZDSM")

a<-numeric(10000)
for (i in 1:10000)  a[i]<- mean(sample((blank$oocysts),replace=T),na.rm=T)
quantile(a,c(0.025,0.975)) 
uct<-numeric(10000)
for (i in 1:10000)  uct[i]<- mean(sample((UCT$oocysts),replace=T),na.rm=T)
quantile(uct,c(0.025,0.975)) 
ozpip<-numeric(10000)
for (i in 1:10000)  ozpip[i]<- mean(sample((OZPIP$oocysts),replace=T),na.rm=T)
quantile(ozpip,c(0.025,0.975)) 
ozfer<-numeric(10000)
for (i in 1:10000)  ozfer[i]<- mean(sample((OZFER$oocysts),replace=T),na.rm=T)
quantile(ozfer,c(0.025,0.975)) 
ozdsm<-numeric(10000)
for (i in 1:10000)  ozdsm[i]<- mean(sample((OZDSM$oocysts),replace=T),na.rm=T)
quantile(ozdsm,c(0.025,0.975)) 

oocysts<-c(mean(uct),mean(ozpip),mean(ozfer),mean(ozdsm))
oocystsQL<-c(quantile(uct,0.025),quantile(ozpip,0.025),quantile(ozfer,0.025),quantile(ozdsm,0.025))
oocystsQU<-c(quantile(uct,0.975),quantile(ozpip,0.975),quantile(ozfer,0.975),quantile(ozdsm,0.975))

oocystsblank<-c(rep(mean(a),4))
oocystsQLblank<-c(rep(quantile(a,0.025),4))
oocystsQUblank<-c(rep(quantile(a,0.975),4))

REDUCTIONoocysts<-(1 - (oocysts/oocystsblank))*100
REDUCTIONLower<-(1-(oocystsQL/oocystsQLblank))*100
REDUCTIONUpper<-(1-(oocystsQU/oocystsQUblank))*100






rps<-unique(intd$rep)
intd$treat<-as.factor(intd$treat)
treatments<-unique(intd$treat)


dsn1<-expand.grid("treatments"=treatments)
dsnb<-subset(dsn1,dsn1$treatments=="Blank")
dsn<-subset(dsn1,dsn1$treatments!="Blank")

dsn$eff.i<-NA
dsn$i.upp<-NA
dsn$i.low<-NA

for(i in 1:length(dsn$treatments)){
  
  temp<-subset(intd,intd$treat==dsn$treatments[i])
  tempb1<-subset(intd,intd$treat=="Blank")
  tempb<-subset(tempb1,match(tempb1$rep,temp$rep)!="NA")
  dtemp<-rbind(tempb,temp)
  dtemp$rpfc<-as.factor(dtemp$rep)
  dtemp$treatment<-relevel(dtemp$treat, ref = "Blank")
  
  if(length(unique(dtemp$rep))>3.5){
    
    i.glm<-glmmadmb(oocysts~treat+(1|rpfc), data=dtemp,zeroInflation=TRUE,family="nbinom")
    dsn$eff.i[i]  <- round((1-exp(coef(i.glm)[2]))  *100,3)
    i.ci<-confint(i.glm)
    dsn$i.low[i] <- round((1-exp(i.ci[2,2]))  *100,3)
    dsn$i.upp[i] <- round((1-exp(i.ci[2,1]))  *100,3)
    
  }else{
    
    i.glm<-glmmadmb(oocysts~treat, data=dtemp,zeroInflation=TRUE,family="nbinom")
    dsn$eff.i[i]  <- round((1-exp(coef(i.glm)[2]))  *100,3)
    i.ci<-confint(i.glm)
    dsn$i.low[i] <- round((1-exp(i.ci[2,2]))  *100,3)
    dsn$i.upp[i] <- round((1-exp(i.ci[2,1]))  *100,3)
  }
}



dsn$cs<-ifelse(dsn$treatments=="SD",1,ifelse(dsn$treatments=="ATV",2,ifelse(dsn$treatments=="UCT",3,ifelse(dsn$treatments=="OZPIP",4,ifelse(dsn$treatments=="OZFER",5,6)))))
plot(dsn$eff.i[3:6]~dsn$cs[3:6],pch=16,cex=1.25,col="chartreuse4",bty="n",main="GLMM estimation",las=1,ylim=c(0,100),xlab="Treatment type",ylab="Oocyst intensity efficacy (%)",axes=F)

axis(side=1,at=c(3:6),labels=c("UCT","OZ PIP","OZ FER","OZ DSM"))
axis(side=2,at=c(0,20,40,60,80,100),labels=c(0,20,40,60,80,100),las=2)
points(c(100)~c(2),pch=16,cex=1.5,col="chartreuse4")

for(i in 1:length(dsn$cs[3:6])){
segments(dsn$cs[3:6][i], dsn$i.low[3:6][i], x1 = dsn$cs[3:6][i], y1 = dsn$i.upp[3:6][i],
         col = "black", lty = 1, lwd = 2)}


#####Prevalence
blank<-subset(intd,treat=="Blank")
UCT<-subset(intd,treat=="UCT")
OZPIP<-subset(intd,treat=="OZPIP")
OZFER<-subset(intd,treat=="OZFER")
OZDSM<-subset(intd,treat=="OZDSM")
    
a<-numeric(10000)
for (i in 1:10000)  a[i]<- mean(sample((1-blank$prev),replace=T),na.rm=T)
                quantile(a,c(0.025,0.975)) 
uct<-numeric(10000)
for (i in 1:10000)  uct[i]<- mean(sample((1-UCT$prev),replace=T),na.rm=T)
quantile(uct,c(0.025,0.975)) 
ozpip<-numeric(10000)
for (i in 1:10000)  ozpip[i]<- mean(sample((1-OZPIP$prev),replace=T),na.rm=T)
quantile(ozpip,c(0.025,0.975)) 
ozfer<-numeric(10000)
for (i in 1:10000)  ozfer[i]<- mean(sample((1-OZFER$prev),replace=T),na.rm=T)
quantile(ozfer,c(0.025,0.975)) 
ozdsm<-numeric(10000)
for (i in 1:10000)  ozdsm[i]<- mean(sample((1-OZDSM$prev),replace=T),na.rm=T)
quantile(ozdsm,c(0.025,0.975)) 
    
prevs<-c(mean(uct),mean(ozpip),mean(ozfer),mean(ozdsm))
prevsQL<-c(quantile(uct,0.025),quantile(ozpip,0.025),quantile(ozfer,0.025),quantile(ozdsm,0.025))
prevsQU<-c(quantile(uct,0.975),quantile(ozpip,0.975),quantile(ozfer,0.975),quantile(ozdsm,0.975))

prevsblank<-c(rep(mean(a),4))
prevsQLblank<-c(rep(quantile(a,0.025),4))
prevsQUblank<-c(rep(quantile(a,0.975),4))

REDUCTIONprevs<-(prevs-prevsblank)*100
REDUCTIONLower<-(prevsQL-prevsQLblank)*100
REDUCTIONUpper<-(prevsQU-prevsQUblank)*100

    dsn1<-expand.grid("treatments"=treatments)
    dsnb<-subset(dsn1,dsn1$treatments=="Blank")
    dsnp<-subset(dsn1,dsn1$treatments!="Blank")
    
    dsnp$eff.i<-NA
    dsnp$i.upp<-NA
    dsnp$i.low<-NA
    
    for(i in 1:length(dsnp$treatments)){
      
      temp<-subset(intd,intd$treat==dsnp$treatments[i])
      tempb1<-subset(intd,intd$treat=="Blank")
      tempb<-subset(tempb1,match(tempb1$rep,temp$rep)!="NA")
      dtemp<-rbind(tempb,temp)
      dtemp$rpfc<-as.factor(dtemp$rep)
      dtemp$treatment<-relevel(dtemp$treat, ref = "Blank")
      
      if(length(unique(dtemp$rep))>3.5){
        
        i.glm<-glmmadmb(prev~treat+(1|rpfc), data=dtemp,family="binomial")
        dsnp$eff.i[i]  <- round((1-exp(coef(i.glm)[2]))  *100,3)
        i.ci<-confint(i.glm)
        dsnp$i.low[i] <- round((1-exp(i.ci[2,2]))  *100,3)
        dsnp$i.upp[i] <- round((1-exp(i.ci[2,1]))  *100,3)
        
      }else{
        
        i.glm<-glmmadmb(prev~treat, data=dtemp,family="binomial")
        dsnp$eff.i[i]  <- round((1-exp(coef(i.glm)[2]))  *100,3)
        i.ci<-confint(i.glm)
        dsnp$i.low[i] <- round((1-exp(i.ci[2,2]))  *100,3)
        dsnp$i.upp[i] <- round((1-exp(i.ci[2,1]))  *100,3)
      }
    }

dsnp$cs<-ifelse(dsnp$treatments=="SD",1,ifelse(dsnp$treatments=="ATV",2,ifelse(dsnp$treatments=="UCT",3,ifelse(dsnp$treatments=="OZPIP",4,ifelse(dsnp$treatments=="OZFER",5,6)))))
plot(dsnp$eff.i[3:6]~dsnp$cs[3:6],pch=16,cex=1.25,col="chartreuse4",bty="n",main="GLMM estimation",las=1,ylim=c(0,100),xlab="Treatment type",ylab="Prevalence efficacy (%)",axes=F)

axis(side=1,at=c(3:6),labels=c("UCT","OZ PIP","OZ FER","OZ DSM"))
axis(side=2,at=c(0,20,40,60,80,100),labels=c(0,20,40,60,80,100),las=2)
points(c(100)~c(2),pch=16,cex=1.5,col="chartreuse4")

for(i in 1:length(dsn$cs[3:6])){
  segments(dsnp$cs[3:6][i], dsnp$i.low[3:6][i], x1 = dsnp$cs[3:6][i], y1 = dsnp$i.upp[3:6][i],
           col = "black", lty = 1, lwd = 2)}


#######################################################
## Sporozoites ########################################
#######################################################

#data.mouse=read.table("D:\\IMPERIAL Nov 2014\\Andrew Blagborough\\12Feb2015_4DrugTreatments\\M2M SporozoiteScores\\SporoParaIntensity.txt",header=TRUE)
#data.mouse$SporPrevtemp<-NA
#  for (i in 1:nrow(data.mouse)){
#   data.mouse$SporPrevtemp[i]<-sum(data.mouse[i,5:14],na.rm=TRUE)}
#data.mouse$meanSporoScore<-ifelse(data.mouse$Bites==2,data.mouse$SporPrevtemp/2,ifelse(data.mouse$Bites==5,data.mouse$SporPrevtemp/5,data.mouse$SporPrevtemp/10))
#data.mouse$prevSpScore<-ifelse(data.mouse$meanSporoScore==0,0,1)
#data.mouse<-data.mouse[data.mouse$Treatment!="SD" & data.mouse$Treatment!="ATV",]

##OR!!
data.mouse=read.table("D:\\IMPERIAL Nov 2014\\Andrew Blagborough\\12Feb2015_4DrugTreatments\\M2M SporozoiteScores\\StackedSporozoites.txt",header=TRUE)

data.mouse<-data.mouse[complete.cases(data.mouse),]
data.mouse<-data.mouse[data.mouse$Treatment!="SD" & data.mouse$Treatment!="ATV",]
data.mouse$prevSpScore<-ifelse(data.mouse$Sporozoite1==0,0,1)

#####Prevalence


rps<-unique(data.mouse$Round)
data.mouse$rep<-ifelse(data.mouse$Round=="slot3",3,4)
data.mouse$treat<-as.factor(data.mouse$Treatment)
treatments<-unique(data.mouse$treat)



dsn1<-expand.grid("treatments"=treatments)
dsnb<-subset(dsn1,dsn1$treatments=="Blank")
dsnpS<-subset(dsn1,dsn1$treatments!="Blank")

dsnpS$eff.i<-NA
dsnpS$i.upp<-NA
dsnpS$i.low<-NA

for(i in 1:length(dsnpS$treatments)){
  
  temp<-subset(data.mouse,data.mouse$treat==dsnpS$treatments[i])
  tempb1<-subset(data.mouse,data.mouse$treat=="Blank")
  tempb<-subset(tempb1,match(tempb1$rep,temp$rep)!="NA")
  dtemp<-rbind(tempb,temp)
  dtemp$rpfc<-as.factor(dtemp$rep)
  dtemp$treatment<-relevel(dtemp$treat, ref = "Blank")
  
  if(length(unique(dtemp$rep))>3.5){
    
    i.glm<-glmmadmb(prevSpScore~treat+(1|Bites), data=dtemp,family="binomial")
    dsnpS$eff.i[i]  <- round((1-exp(coef(i.glm)[2]))  *100,3)
    i.ci<-confint(i.glm)
    dsnpS$i.low[i] <- round((1-exp(i.ci[2,2]))  *100,3)
    dsnpS$i.upp[i] <- round((1-exp(i.ci[2,1]))  *100,3)
    
  }else{
    
    i.glm<-glmmadmb(prevSpScore~treat, data=dtemp,family="binomial")
    dsnpS$eff.i[i]  <- round((1-exp(coef(i.glm)[2]))  *100,3)
    i.ci<-confint(i.glm)
    dsnpS$i.low[i] <- round((1-exp(i.ci[2,2]))  *100,3)
    dsnpS$i.upp[i] <- round((1-exp(i.ci[2,1]))  *100,3)
  }
}
dsnpS




dsn1<-expand.grid("treatments"=treatments)
dsnb<-subset(dsn1,dsn1$treatments=="Blank")
dsnpSc<-subset(dsn1,dsn1$treatments!="Blank")

dsnpSc$eff.i<-NA
dsnpSc$i.upp<-NA
dsnpSc$i.low<-NA

for(i in 1:length(dsnpSc$treatments)){
  
  temp<-subset(data.mouse,data.mouse$treat==dsnpSc$treatments[i])
  tempb1<-subset(data.mouse,data.mouse$treat=="Blank")
  tempb<-subset(tempb1,match(tempb1$rep,temp$rep)!="NA")
  dtemp<-rbind(tempb,temp)
  dtemp$rpfc<-as.factor(dtemp$rep)
  dtemp$treatment<-relevel(dtemp$treat, ref = "Blank")
  
  if(length(unique(dtemp$rep))>3.5){
    
    i.glm<-glmmadmb(Sporozoite1~treat+(1|Bites), data=dtemp,family="Poisson")
    dsnpSc$eff.i[i]  <- round((1-exp(coef(i.glm)[2]))  *100,3)
    i.ci<-confint(i.glm)
    dsnpSc$i.low[i] <- round((1-exp(i.ci[2,2]))  *100,3)
    dsnpSc$i.upp[i] <- round((1-exp(i.ci[2,1]))  *100,3)
    
  }else{
    
    i.glm<-glmmadmb(Sporozoite1~treat, data=dtemp,family="Poisson")
    dsnpSc$eff.i[i]  <- round((1-exp(coef(i.glm)[2]))  *100,3)
    i.ci<-confint(i.glm)
    dsnpSc$i.low[i] <- round((1-exp(i.ci[2,2]))  *100,3)
    dsnpSc$i.upp[i] <- round((1-exp(i.ci[2,1]))  *100,3)
  }
}
dsnpSc


#########################################################
##
##  Figure to mirror Upton 2015 Figure 2
##
#########################################################

dsn$cs<-ifelse(dsnp$treatments=="SD",1,ifelse(dsnp$treatments=="ATV",2,ifelse(dsnp$treatments=="UCT",3,ifelse(dsnp$treatments=="OZPIP",4,ifelse(dsnp$treatments=="OZFER",5,6)))))
dsnpS$cs<-ifelse(dsnpS$treatments=="UCT",1,ifelse(dsnpS$treatments=="OZPIP",2,ifelse(dsnpS$treatments=="OZFER",3,4)))
par(mfrow=c(1,1))
barplot(c(dsn$eff.i[3],dsnp$eff.i[3],dsnpS$eff.i[1],dsnpSc$eff.i[1],0,
          dsn$eff.i[4],dsnp$eff.i[4],dsnpS$eff.i[2],dsnpSc$eff.i[2],0,
          dsn$eff.i[5],dsnp$eff.i[5],dsnpS$eff.i[3],dsnpSc$eff.i[3],0,
          dsn$eff.i[6],dsnp$eff.i[6],dsnpS$eff.i[4],dsnpSc$eff.i[4]),
        col=c("blue4","darkseagreen3","firebrick4","darksalmon","white"),
        ylim=c(-20,100),ylab="% Inhibition",main="GLMM estimation",xlab="Treatment type",axes=F)
axis(side=1,at=c(2,8,14,20),labels=c("UCT","OZ PIP","OZ FER","OZ DSM"))
axis(side=2,at=c(-20,0,20,40,60,80,100),labels=c(-20,0,20,40,60,80,100),las=2)

valu<-c(6,12,18)
  for (i in 1:3){
    segments((0.7), dsn$i.low[3], x1 = (0.7), y1 = dsn$i.upp[3],
             col = "dimgrey", lty = 2, lwd = 1)
    segments((1.9), dsnp$i.low[3], x1 = (1.9), y1 = dsnp$i.upp[3],
             col = "dimgrey", lty = 2, lwd = 1)
    segments((3.2), dsnpS$i.low[1], x1 = (3.2), y1 = dsnpS$i.upp[1],
             col = "dimgrey", lty = 2, lwd = 1)
    segments((4.3), dsnpSc$i.low[1], x1 = (4.3), y1 = dsnpSc$i.upp[1],
             col = "dimgrey", lty = 2, lwd = 1)
  segments((0.7+valu[i]), dsn$i.low[3+i], x1 = (0.7+valu[i]), y1 = dsn$i.upp[3+i],
           col = "dimgrey", lty = 2, lwd = 1)
  segments((1.9+valu[i]), dsnp$i.low[3+i], x1 = (1.9+valu[i]), y1 = dsnp$i.upp[3+i],
           col = "dimgrey", lty = 2, lwd = 1)
  segments((3.2+valu[i]), dsnpS$i.low[1+i], x1 = (3.2+valu[i]), y1 = dsnpS$i.upp[1+i],
           col = "dimgrey", lty = 2, lwd = 1)
  segments((4.3+valu[i]), dsnpSc$i.low[1+i], x1 = (4.3+valu[i]), y1 = dsnpSc$i.upp[1+i],
           col = "dimgrey", lty = 2, lwd = 1)
           }
text(2.5,-10, "Oocyst intensity")
text(7.5,-10, "Oocyst prevalence")
text(14.2,-10, "Sporozoite intensity")
text(21,-10, "Sporozoite prevalence")
points(c(-10,-10,-10,-10)~c(0.2,5,11.5,18),col=c("blue4","darkseagreen3","firebrick4","darksalmon"),pch=15)
