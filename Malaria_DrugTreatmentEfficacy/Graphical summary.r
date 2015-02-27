#########################################################
##                                                     ##
##  Graphical illustration of drug treatment trials    ##
##                                                     ## 
##                                                     ##
##                                                     ##
##                                                     ##
#########################################################
std <- function(x) sd(x)/sqrt(length(x))

##Required packages
rm(list = ls())
require(bbmle)
require(hacks)
require(VGAM)
require(emdbook)
require(lme4)
require(boot)
library(reshape)
library("glmmADMB")
require(binom)
require(RColorBrewer)


##Define the data
is.mosi<-0
n.rounds=2
n.bites=3
t.bites<-c(2,5,10)


#
##
###
####Load the appropriate data
###
##
#
data.mosi=read.table("D:\\IMPERIAL Nov 2014\\Andrew Blagborough\\12Feb2015_4DrugTreatments\\M2M SporozoiteScores\\OocystIntensity.txt",header=TRUE)
data.mouse=read.table("D:\\IMPERIAL Nov 2014\\Andrew Blagborough\\12Feb2015_4DrugTreatments\\M2M SporozoiteScores\\SporoParaIntensity.txt",header=TRUE)
data.mosi$Round<-ifelse(data.mosi$rep==3,"slot3","slot4")

data.all<-if(is.mosi==1) data.mosi else data.mouse
Parasite<-if(is.mosi==0) data.all$Parasitemia else data.all$oocysts
prev<-as.factor(ifelse(Parasite>0,1,0))
Treatment<-as.factor(data.all$Treatment)
Bites<-as.factor(abs(data.all$Bites))
Rond<-as.factor(data.all$Round)
my.id<-seq(1,length(Rond),1)
inf.data<-data.frame(my.id,prev,Parasite,Treatment,Bites,Rond)
summary(inf.data)
levels(inf.data$Treatment)

###
#### PARACETEMIA PREVALENCE
###

##OVERALL
results.table<-binom.confint(x = as.numeric(table(prev[Rond=="slot3"],Treatment[Rond=="slot3"])[2,]), 
                             n = as.numeric(table(Rond,Treatment)[1,]),methods="wilson")

results.table$bitesperc<-as.numeric(results.table3[,4])*100
results.table$bitespercLower<-as.numeric(results.table3[,5])*100
results.table$bitespercUpper<-as.numeric(results.table3[,6])*100

##combined impact on prevalence
mous<-expand.grid("treat"=unique(inf.data$Treatment))
mous$inf_mouse<-NA
inf.data$prev2<-as.numeric(inf.data$prev)
inf.data$prev2<-ifelse(inf.data$prev2==1,0,1)
mous$inf_mouse[1]<-sum(inf.data$prev2[inf.data$Treatment=="Blank"])
mous$inf_mouse[2]<-sum(inf.data$prev2[inf.data$Treatment=="SD"])
mous$inf_mouse[3]<-sum(inf.data$prev2[inf.data$Treatment=="ATV"])
mous$inf_mouse[4]<-sum(inf.data$prev2[inf.data$Treatment=="UCT"])
mous$inf_mouse[5]<-sum(inf.data$prev2[inf.data$Treatment=="OZPIP"])
mous$inf_mouse[6]<-sum(inf.data$prev2[inf.data$Treatment=="OZFER"])
mous$inf_mouse[7]<-sum(inf.data$prev2[inf.data$Treatment=="OZDSM"])

mous$tot_mouse[1]<-length(inf.data$prev2[inf.data$Treatment=="Blank"])
mous$tot_mouse[2]<-length(inf.data$prev2[inf.data$Treatment=="SD"])
mous$tot_mouse[3]<-length(inf.data$prev2[inf.data$Treatment=="ATV"])
mous$tot_mouse[4]<-length(inf.data$prev2[inf.data$Treatment=="UCT"])
mous$tot_mouse[5]<-length(inf.data$prev2[inf.data$Treatment=="OZPIP"])
mous$tot_mouse[6]<-length(inf.data$prev2[inf.data$Treatment=="OZFER"])
mous$tot_mouse[7]<-length(inf.data$prev2[inf.data$Treatment=="OZDSM"])

mous$uninf_mos<-mous$tot_mouse-mous$inf_mouse
mous

combinedPREV<-expand.grid("treat"=unique(mous$treat))
combinedPREV$fisher<-NA
combinedPREV$P<-NA
combinedPREV$CI95LOW<-NA
combinedPREV$CI95UPP<-NA
for (i in 2:7){
  tab3<-matrix(nrow=2,ncol=2,data=NA);colnames(tab3)<-c("inf","un_inf")
  tab3[1,]<-c(mous[1,2],mous[1,4]);tab3[2,]<-c(mous[i,2],mous[i,4])
  test2b<-fisher.test(tab3)
  combinedPREV[i,2]<-test2b[3];combinedPREV[i,3]<-test2b[1]
  combinedPREV[i,4]<-test2b$conf.int[1];combinedPREV[i,5]<-test2b$conf.int[2]
}

##RENAME AND GROUP
mousr3<-expand.grid("treat"=unique(inf.data$Treatment))
mousr3$inf_mouse<-NA
inf.data$prev2<-as.numeric(inf.data$prev)
inf.data$prev2<-ifelse(inf.data$prev2==1,0,1)
mousr3$inf_mouse[1]<-sum(inf.data$prev2[inf.data$Treatment=="Blank" & inf.data$rep == 3])
mousr3$inf_mouse[2]<-sum(inf.data$prev2[inf.data$Treatment=="SD"& inf.data$rep == 3])
mousr3$inf_mouse[3]<-sum(inf.data$prev2[inf.data$Treatment=="ATV"& inf.data$rep == 3])
mousr3$inf_mouse[4]<-sum(inf.data$prev2[inf.data$Treatment=="UCT"& inf.data$rep == 3])
mousr3$inf_mouse[5]<-sum(inf.data$prev2[inf.data$Treatment=="OZPIP"& inf.data$rep == 3])
mousr3$inf_mouse[6]<-sum(inf.data$prev2[inf.data$Treatment=="OZFER"& inf.data$rep == 3])
mousr3$inf_mouse[7]<-sum(inf.data$prev2[inf.data$Treatment=="OZDSM"& inf.data$rep == 3])

mousr3$tot_mouse[1]<-length(inf.data$prev2[inf.data$Treatment=="Blank"& inf.data$rep == 3])
mousr3$tot_mouse[2]<-length(inf.data$prev2[inf.data$Treatment=="SD"& inf.data$rep == 3])
mousr3$tot_mouse[3]<-length(inf.data$prev2[inf.data$Treatment=="ATV"& inf.data$rep == 3])
mousr3$tot_mouse[4]<-length(inf.data$prev2[inf.data$Treatment=="UCT"& inf.data$rep == 3])
mousr3$tot_mouse[5]<-length(inf.data$prev2[inf.data$Treatment=="OZPIP"& inf.data$rep == 3])
mousr3$tot_mouse[6]<-length(inf.data$prev2[inf.data$Treatment=="OZFER"& inf.data$rep == 3])
mousr3$tot_mouse[7]<-length(inf.data$prev2[inf.data$Treatment=="OZDSM"& inf.data$rep == 3])

mousr3$uninf_mos<-mousr3$tot_mouse-mousr3$inf_mouse
mousr3

mousr4<-expand.grid("treat"=unique(inf.data$Treatment))
mousr4$inf_mouse<-NA
inf.data$prev2<-as.numeric(inf.data$prev)
inf.data$prev2<-ifelse(inf.data$prev2==1,0,1)
mousr4$inf_mouse[1]<-sum(inf.data$prev2[inf.data$Treatment=="Blank" & inf.data$rep == 4])
mousr4$inf_mouse[2]<-sum(inf.data$prev2[inf.data$Treatment=="SD"& inf.data$rep == 4])
mousr4$inf_mouse[3]<-sum(inf.data$prev2[inf.data$Treatment=="ATV"& inf.data$rep == 4])
mousr4$inf_mouse[4]<-sum(inf.data$prev2[inf.data$Treatment=="UCT"& inf.data$rep == 4])
mousr4$inf_mouse[5]<-sum(inf.data$prev2[inf.data$Treatment=="OZPIP"& inf.data$rep == 4])
mousr4$inf_mouse[6]<-sum(inf.data$prev2[inf.data$Treatment=="OZFER"& inf.data$rep == 4])
mousr4$inf_mouse[7]<-sum(inf.data$prev2[inf.data$Treatment=="OZDSM"& inf.data$rep == 4])

mousr4$tot_mouse[1]<-length(inf.data$prev2[inf.data$Treatment=="Blank"& inf.data$rep == 4])
mousr4$tot_mouse[2]<-length(inf.data$prev2[inf.data$Treatment=="SD"& inf.data$rep == 4])
mousr4$tot_mouse[3]<-length(inf.data$prev2[inf.data$Treatment=="ATV"& inf.data$rep == 4])
mousr4$tot_mouse[4]<-length(inf.data$prev2[inf.data$Treatment=="UCT"& inf.data$rep == 4])
mousr4$tot_mouse[5]<-length(inf.data$prev2[inf.data$Treatment=="OZPIP"& inf.data$rep == 4])
mousr4$tot_mouse[6]<-length(inf.data$prev2[inf.data$Treatment=="OZFER"& inf.data$rep == 4])
mousr4$tot_mouse[7]<-length(inf.data$prev2[inf.data$Treatment=="OZDSM"& inf.data$rep == 4])

mousr4$uninf_mos<-mousr4$tot_mouse-mousr4$inf_mouse
mousr4


prevd<-expand.grid("treat"=unique(mous$treat))
prevd$rep1_uninf<-mousr3$uninf_mos
prevd$rep1_inf<-mousr3$inf_mouse
prevd$rep2_uninf<-mousr4$uninf_mos
prevd$rep2_inf<-mousr4$inf_mouse

newc<-rep(prevd$treat,2)
uninf<-c(prevd$rep1_uninf,prevd$rep2_uninf)
inf<-c(prevd$rep1_inf,prevd$rep2_inf)
prev<-inf/(inf+uninf)

pdn<-prevd

pinf<-c(pdn$rep1_inf[2:7]/(pdn$rep1_inf[2:7]+pdn$rep1_uninf[2:7]),
        pdn$rep2_inf[2:7]/(pdn$rep2_inf[2:7]+pdn$rep2_uninf[2:7]))


pconinf<-c(rep((pdn$rep1_inf[1]/(pdn$rep1_inf[1]+pdn$rep1_uninf[1])),6),
           rep((pdn$rep2_inf[1]/(pdn$rep2_inf[1]+pdn$rep2_uninf[1])),6))


conc.vec<-rep(pdn$treat[2:7],2)
plot(conc.vec,1-(pinf/pconinf),ylim=c(0,1),bty="n",xlim=c(0,8),las=1,xlab="Treatment",ylab="Prevalence efficacy",cex=1.25,col="chartreuse4",pch=16)

((1-(pinf/pconinf))[1:6]+(1-(pinf/pconinf))[7:12])/2


  results.table1<-binom.confint(x = as.numeric(table(prev[Rond=="slot3"&Bites==2],Treatment[Rond=="slot3"&Bites==2])[2,]), 
                                n = as.numeric(table(Rond[Bites==2],Treatment[Bites==2])[1,]),methods="wilson")

  results.table1$bites2perc<-as.numeric(results.table1[,4])*100
  results.table1$bites2percLower<-as.numeric(results.table1[,5])*100
  results.table1$bites2percUpper<-as.numeric(results.table1[,6])*100

              results.table2<-binom.confint(x = as.numeric(table(prev[Rond=="slot3"&Bites==5],Treatment[Rond=="slot3"&Bites==5])[2,]), 
                              n = as.numeric(table(Rond[Bites==5],Treatment[Bites==5])[1,]),methods="wilson")

              results.table2$bites5perc<-as.numeric(results.table2[,4])*100
              results.table2$bites5percLower<-as.numeric(results.table2[,5])*100
              results.table2$bites5percUpper<-as.numeric(results.table2[,6])*100

results.table3<-binom.confint(x = as.numeric(table(prev[Rond=="slot3"&Bites==10],Treatment[Rond=="slot3"&Bites==10])[2,]), 
                              n = as.numeric(table(Rond[Bites==10],Treatment[Bites==10])[1,]),methods="wilson")

results.table3$bites10perc<-as.numeric(results.table3[,4])*100
results.table3$bites10percLower<-as.numeric(results.table3[,5])*100
results.table3$bites10percUpper<-as.numeric(results.table3[,6])*100


 par(mfrow=c(1,3)) 
  hh=rbind(results.table1[,7])
  hh2<-100-hh
  hh.max<-100-(apply(hh, 2, max)+6)
  ciL<-rbind(100-results.table1[,8])
  ciU<-rbind(100-results.table1[,9])
  colnames(hh2)<-c("ATV","Blank","OZDSM","OZFER","OZPIP","SD","UCT")
  par(las=1,col.axis="black")
  
  mybarcol <- "gray20"
  mp <- barplot(hh2, 
                col = c("dodgerblue4"),
                ylim= c(0,100),
                        ylab = "Impact on Parasitemia", font.main = 4, cex.lab = 2,
                        main = "2 Bites",
                cex.names = 1.4)
  segments(mp, ciL, mp, ciU , col = mybarcol, lwd = 1.5)



hh=rbind(results.table2[,7])
hh2<-100-hh
hh.max<-100-(apply(hh2, 2, max)+6)
ciL<-rbind(100-results.table2[,8])
ciU<-rbind(100-results.table2[,9])
colnames(hh2)<-c("ATV","Blank","OZDSM","OZFER","OZPIP","SD","UCT")
par(las=1,col.axis="black")

mybarcol <- "gray20"
mp <- barplot(hh2, 
              col = c("dodgerblue4"),
              ylim= c(0,100),
                      main = "5 Bites", font.main = 4,
              #        sub = "Transmission cycle", col.sub = mybarcol,
              cex.names = 1.4)
segments(mp, ciL, mp, ciU , col = mybarcol, lwd = 1.5)

hh2=rbind(100-results.table3[,7])
hh.max<-apply(hh2, 2, max)+6
ciL<-rbind(100-results.table3[,8])
ciU<-rbind(100-results.table3[,9])
colnames(hh2)<-c("ATV","NC","OZDSM","OZFER","OZPIP","SD","UCT")

par(las=1,col.axis="black")

mybarcol <- "gray20"
mp <- barplot(hh2, 
              col = c("dodgerblue4","dodgerblue4","red","red","red","red","red"),
              ylim= c(0,100),
                      main = "10 Bites", font.main = 4,
              #        sub = "Transmission cycle", col.sub = mybarcol,
              cex.names = 1.4)
segments(mp, ciL, mp, ciU , col = mybarcol, lwd = 1.5)


#######################################
#########################################
########################################### Parasitemia Day 10
#########################################
#######################################

data.mosi=read.table("D:\\IMPERIAL Nov 2014\\Andrew Blagborough\\12Feb2015_4DrugTreatments\\M2M SporozoiteScores\\OocystIntensity.txt",header=TRUE)
data.mouse=read.table("D:\\IMPERIAL Nov 2014\\Andrew Blagborough\\12Feb2015_4DrugTreatments\\M2M SporozoiteScores\\SporoParaIntensity.txt",header=TRUE)
data.mosi$Round<-ifelse(data.mosi$rep==3,"slot3","slot4")
data.mosi$Bites<-rep(1,length(data.mosi$rep))

para3<-mean(data.mouse$Parasitemia[data.mouse$Treatment=="Blank" & data.mouse$Round=="slot3"])
para4<-mean(data.mouse$Parasitemia[data.mouse$Treatment=="Blank" & data.mouse$Round=="slot4"])

para3UCT<-mean(data.mouse$Parasitemia[data.mouse$Treatment=="UCT" & data.mouse$Round=="slot3"])
para4UCT<-mean(data.mouse$Parasitemia[data.mouse$Treatment=="UCT" & data.mouse$Round=="slot4"])

para3PIP<-mean(data.mouse$Parasitemia[data.mouse$Treatment=="OZPIP" & data.mouse$Round=="slot3"])
para4PIP<-mean(data.mouse$Parasitemia[data.mouse$Treatment=="OZPIP" & data.mouse$Round=="slot4"])

para3FER<-mean(data.mouse$Parasitemia[data.mouse$Treatment=="OZFER" & data.mouse$Round=="slot3"])
para4FER<-mean(data.mouse$Parasitemia[data.mouse$Treatment=="OZFER" & data.mouse$Round=="slot4"])

para3DSM<-mean(data.mouse$Parasitemia[data.mouse$Treatment=="OZDSM" & data.mouse$Round=="slot3"])
para4DSM<-mean(data.mouse$Parasitemia[data.mouse$Treatment=="OZDSM" & data.mouse$Round=="slot4"])

parasH<-matrix(ncol=7,nrow=3)

data.all<-if(is.mosi==1) data.mosi else data.mouse
Parasite<-if(is.mosi==0) data.all$Parasitemia else data.all$oocysts
prev<-as.factor(ifelse(Parasite>0,1,0))
Treatment<-as.factor(data.all$Treatment)
Treatment2<-as.numeric(Treatment)
Bites<-as.factor(abs(data.all$Bites))
Rond<-as.factor(data.all$Round)
my.id<-seq(1,length(Rond),1)
inf.data<-data.frame(my.id,prev,Parasite,Treatment,Bites,Rond)
summary(inf.data)
n.boots<-1000
boot.fun1<-numeric(n.boots)

Roundnum<-as.numeric(Rond)
for (j in 1:7){
for (i in 1:2){
    model.values1<-Parasite[Treatment2==j&Roundnum==i] 
    for(b in 1:n.boots){
      boot.fun1[b]<-mean(sample(model.values1,size=length(model.values1),replace = TRUE))}
    parasH[1,j]<-sort(boot.fun1)[0.025*n.boots]
    parasH[2,j]<-sort(boot.fun1)[0.975*n.boots]
    
    parasH[3,j]<-mean(model.values1)
  }

}
parasH
PARAh<-100-parasH
colnames(PARAh)<-c("ATV","BLANK","OZDSM","OZFER","OZPIP","SD","UCT")
##% INHIBITION 


hh=rbind(parasH[3,])
  hh.max<-apply(hh, 2, max)+5
  ciL<-rbind(parasH[1,])
  ciU<-rbind(parasH[2,])
  colnames(hh)<-seq(1,7,1)
  par(las=1,col.axis="black")
  
  
  mybarcol <- "gray20"
  mp <- barplot(hh,
                col = c("firebrick3"),
                ylim= c(0,100),
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL, mp, ciU , col = mybarcol, lwd = 1.5)
  #text(colMeans(mp)+0.2,hh.max,  labels =less.than.001[mb,], col = "red")
  
  #mtext(side = 1, at = colMeans(mp), line = 2,
  #      text = paste("efficacy", formatC(colMeans(hh))), col = "red")
  
}
################################################
################################################
################################################







##Sporozoites intensity and prevalence
head(data.mouse)



n.boots<-1000
sporo.intensity<-matrix(ncol=3,nrow=n.bites)
colnames(sporo.intensity)=c("mean","upper","lower")
rownames(sporo.intensity)=c("2Bites","5Bites","10Bites")

sporo.prev<-matrix(ncol=3,nrow=n.bites)
colnames(sporo.prev)=c("mean","upper","lower")
rownames(sporo.prev)=c("2Bites","5Bites","10Bites")

##CONTROL
data.mouseC<-subset(data.mouse,Treatment=="OZDSM")
  bites2<-subset(data.mouseC,Bites==2)
  
  sporo.data<-bites2[5:14]
    sporos<-as.numeric(na.omit(unlist(stack(sporo.data)[1])))
    prev.sporos<-ifelse(sporos>0,1,0)

    sporo.intensity[1,1]<-mean(sporos,na.action=na.omit)
    sporo.prev[1,1]<-mean(prev.sporos)
    
    my.boots<-numeric(n.boots)
    for(b in 1:n.boots){
      my.boots[b]<-mean(sample(sporos,length(sporos), replace = TRUE))}
    
    sporo.intensity[1,2]<-as.numeric(quantile(my.boots,prob=c(0.025)))
    sporo.intensity[1,3]<-as.numeric(quantile(my.boots,prob=c(0.975)))
    
    for(b in 1:n.boots){
      my.boots[b]<-mean(sample(prev.sporos,length(sporos), replace = TRUE))}
    
    sporo.prev[1,2]<-as.numeric(quantile(my.boots,prob=c(0.025)))
    sporo.prev[1,3]<-as.numeric(quantile(my.boots,prob=c(0.975)))

bites<-subset(data.mouseC,Bites==5)

sporo.data<-bites[5:14]
sporos<-as.numeric(na.omit(unlist(stack(sporo.data)[1])))
prev.sporos<-ifelse(sporos>0,1,0)

sporo.intensity[2,1]<-mean(sporos,na.action=na.omit)
sporo.prev[2,1]<-mean(prev.sporos)

my.boots<-numeric(n.boots)
for(b in 1:n.boots){
  my.boots[b]<-mean(sample(sporos,length(sporos), replace = TRUE))}

sporo.intensity[2,2]<-as.numeric(quantile(my.boots,prob=c(0.025)))
sporo.intensity[2,3]<-as.numeric(quantile(my.boots,prob=c(0.975)))

for(b in 1:n.boots){
  my.boots[b]<-mean(sample(prev.sporos,length(sporos), replace = TRUE))}

sporo.prev[2,2]<-as.numeric(quantile(my.boots,prob=c(0.025)))
sporo.prev[2,3]<-as.numeric(quantile(my.boots,prob=c(0.975)))

bites<-subset(data.mouseC,Bites==10)

sporo.data<-bites[5:14]
sporos<-as.numeric(na.omit(unlist(stack(sporo.data)[1])))
prev.sporos<-ifelse(sporos>0,1,0)

sporo.intensity[3,1]<-mean(sporos,na.action=na.omit)
sporo.prev[3,1]<-mean(prev.sporos)

my.boots<-numeric(n.boots)
for(b in 1:n.boots){
  my.boots[b]<-mean(sample(sporos,length(sporos), replace = TRUE))}

sporo.intensity[3,2]<-as.numeric(quantile(my.boots,prob=c(0.025)))
sporo.intensity[3,3]<-as.numeric(quantile(my.boots,prob=c(0.975)))

for(b in 1:n.boots){
  my.boots[b]<-mean(sample(prev.sporos,length(sporos), replace = TRUE))}

sporo.prev[3,2]<-as.numeric(quantile(my.boots,prob=c(0.025)))
sporo.prev[3,3]<-as.numeric(quantile(my.boots,prob=c(0.975)))

sporo.prev
sporo.intensity

sporo.prevSum<-expand.grid(c(rep("blank",3),rep("UCT",3),rep("OZPIP",3),rep("OZFER",3),rep("OZDSM",3)))
sporo.prevSum$mbr<-c(2,5,10,2,5,10,2,5,10,2,5,10,2,5,10)
sporo.prevSum$meanINTENSITY<-c(1.65,1.7,1.49,0.85,1.14,0.93,1,1.42,1.1,1.05,1.08,0.9,0.95,0.9,1.24)
sporo.prevSum$lowINT<-c(0.9,1.19,1.17,0.3,0.74,0.65,0.4,0.96,0.83,0.35,0.72,0.64,0.35,0.52,0.93)
sporo.prevSum$uppINT<-c(2.3,2.23,1.81,1.45,1.6,1.21,1.6,1.88,1.41,1.8,1.5,1.16,1.6,1.3,1.55)
sporo.prevSum$prev<-c(55,51,49,35,44,31,40,46,39,30,44,34,35,32,41)
sporo.prevSum$lowprev<-c(30,35,40,15,30,21,20,32,30,10,30,25,15,20,32)
sporo.prevSum$uppprev<-c(75,65,58,55,58,41,60,60,49,50,58,44,55,44,52)

sporo.prevSum$ImpactSpScore<-NA
t.bites<-c(2,5,10)
#for(i in 1:3){
  sporo.prevSum$ImpactSpScore[0+i]<-sporo.prevSum$meanINTENSITY[sporo.prevSum$Var1=="blank" & sporo.prevSum$mbr == 10]-
  sporo.prevSum$meanINTENSITY[sporo.prevSum$Var1=="blank" & sporo.prevSum$mbr == 10]
sporo.prevSum$ImpactSpScore[2+i]<-sporo.prevSum$meanINTENSITY[sporo.prevSum$Var1=="blank" & sporo.prevSum$mbr == 10]-
  sporo.prevSum$meanINTENSITY[sporo.prevSum$Var1=="UCT" & sporo.prevSum$mbr == 10]
sporo.prevSum$ImpactSpScore[5+i]<-sporo.prevSum$meanINTENSITY[sporo.prevSum$Var1=="blank" & sporo.prevSum$mbr == 10]-
  sporo.prevSum$meanINTENSITY[sporo.prevSum$Var1=="OZPIP" & sporo.prevSum$mbr == 10]
sporo.prevSum$ImpactSpScore[8+i]<-sporo.prevSum$meanINTENSITY[sporo.prevSum$Var1=="blank" & sporo.prevSum$mbr == 10]-
  sporo.prevSum$meanINTENSITY[sporo.prevSum$Var1=="OZFER" & sporo.prevSum$mbr == 10]
sporo.prevSum$ImpactSpScore[11+i]<-sporo.prevSum$meanINTENSITY[sporo.prevSum$Var1=="blank" & sporo.prevSum$mbr == 10]-
  sporo.prevSum$meanINTENSITY[sporo.prevSum$Var1=="OZDSM" & sporo.prevSum$mbr == 10]

valu<-c(1,4,7,10,13)
par(mfrow=c(1,2))
mp <- barplot(c(sporo.prevSum[1,6],sporo.prevSum[4,6],sporo.prevSum[7,6],sporo.prevSum[10,6],sporo.prevSum[13,6]),
                    col = "orange",cex.names = 1.4,ylim=c(0,100),ylab="Prevalence")
  par(las=1,col.axis="black")
  for (i in 1:5){
  segments(mp[i], sporo.prevSum[valu[i],7],
           mp[i], sporo.prevSum[valu[i],8],
              col = "black", lwd = 1.5)}
axis(1,at=c(1:5),labels=c("Blank","UCT","OZPIP","OZFER","OZDSM"))

  mp2 <- barplot(c(sporo.prevSum[1,3],sporo.prevSum[4,3],sporo.prevSum[7,3],sporo.prevSum[10,3],sporo.prevSum[13,3]),
                col = "orange",cex.names = 1.4,ylim=c(0,4),ylab="Mean Sporozoite score")
  par(las=1,col.axis="black")
  for (i in 1:5){
  segments(mp2[i], sporo.prevSum[valu[i],4], mp2[i], sporo.prevSum[valu[i],5] , col = "black", lwd = 1.5)
  }
axis(1,at=c(1:5),labels=c("Blank","UCT","OZPIP","OZFER","OZDSM"))
