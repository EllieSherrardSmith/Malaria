#######################################
##  Impact on oocyst intensity       ##
##  Impact on oocyst prevalence      ##
##  Impact on sporozoite intensity   ##
##  Impact on sporozoite prevalence  ##
#######################################

mosqOocystSlot3 = read.table("D:\\IMPERIAL Nov 2014\\Andrew Blagborough\\12Feb2015_4DrugTreatments\\M2M ChainBinomial Effect Size\\Mosq Oocysts Slot 3.txt",header=TRUE)
mosqOocystSlot4 = read.table("D:\\IMPERIAL Nov 2014\\Andrew Blagborough\\12Feb2015_4DrugTreatments\\M2M ChainBinomial Effect Size\\Mosq Oocysts Slot 4.txt",header=TRUE)

mosqOocystSlot3$prev = ifelse(mosqOocystSlot3$oocysts < 1, 0, 1)
treat<-unique(mosqOocystSlot3$treat)

data.mosqu3=expand.grid("treat"=treat)
data.mosqu3$uninf_mosq = NA
data.mosqu3$inf_mosq = NA
data.mosqu3$tot_mosq = NA

data.mosqu3[1,3]<-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="Blank"])
data.mosqu3[2,3]<-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="SD"])
data.mosqu3[3,3]<-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="ATV"])
data.mosqu3[4,3]<-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="UCT"])
data.mosqu3[5,3]<-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZPIP"])
data.mosqu3[6,3]<-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZFER"])
data.mosqu3[7,3]<-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZDSM"])

data.mosqu3[1,4]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="Blank"])
data.mosqu3[2,4]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="SD"])
data.mosqu3[3,4]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="ATV"])
data.mosqu3[4,4]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="UCT"])
data.mosqu3[5,4]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZPIP"])
data.mosqu3[6,4]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZFER"])
data.mosqu3[7,4]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZDSM"])

data.mosqu3[1,2]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="Blank"])-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="Blank"])
data.mosqu3[2,2]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="SD"])-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="SD"])
data.mosqu3[3,2]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="ATV"])-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="ATV"])
data.mosqu3[4,2]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="UCT"])-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="UCT"])
data.mosqu3[5,2]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZPIP"])-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZPIP"])
data.mosqu3[6,2]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZFER"])-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZFER"])
data.mosqu3[7,2]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZDSM"])-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZDSM"])


mosqOocystSlot4$prev = ifelse(mosqOocystSlot4$oocysts < 1, 0, 1)
treat<-unique(mosqOocystSlot4$treat)

data.mosqu4=expand.grid("treat"=treat)
data.mosqu4$uninf_mosq = NA
data.mosqu4$inf_mosq = NA
data.mosqu4$tot_mosq = NA

data.mosqu4[1,3]<-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="Blank"])
data.mosqu4[2,3]<-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="SD"])
data.mosqu4[3,3]<-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="ATV"])
data.mosqu4[4,3]<-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="UCT"])
data.mosqu4[5,3]<-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZPIP"])
data.mosqu4[6,3]<-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZFER"])
data.mosqu4[7,3]<-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZDSM"])

data.mosqu4[1,4]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="Blank"])
data.mosqu4[2,4]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="SD"])
data.mosqu4[3,4]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="ATV"])
data.mosqu4[4,4]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="UCT"])
data.mosqu4[5,4]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZPIP"])
data.mosqu4[6,4]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZFER"])
data.mosqu4[7,4]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZDSM"])

data.mosqu4[1,2]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="Blank"])-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="Blank"])
data.mosqu4[2,2]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="SD"])-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="SD"])
data.mosqu4[3,2]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="ATV"])-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="ATV"])
data.mosqu4[4,2]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="UCT"])-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="UCT"])
data.mosqu4[5,2]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZPIP"])-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZPIP"])
data.mosqu4[6,2]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZFER"])-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZFER"])
data.mosqu4[7,2]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZDSM"])-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZDSM"])

#########################################
##                                     ##
##  Impact of drugs on the oocyst      ##
##     prevalence and intensity        ##
##                                     ##
##                                     ##
#########################################

oocystimpact<-expand.grid("treat"=treat)
oocystimpact$W_MannWhitney3<-NA
oocystimpact$P_VALUE3<-NA
oocystimpact$W_MannWhitney4<-NA
oocystimpact$P_VALUE4<-NA


oocystimpactPREV<-expand.grid("treat"=treat)
oocystimpactPREV$OddsRatioFishers3<-NA
oocystimpactPREV$P_VALUE3<-NA
oocystimpactPREV$CI95Low3<-NA
oocystimpactPREV$CI95Hi3<-NA

oocystimpactPREV$OddsRatioFishers4<-NA
oocystimpactPREV$P_VALUE4<-NA
oocystimpactPREV$CI95Low4<-NA
oocystimpactPREV$CI95Hi4<-NA

for (i in 2:7){
  tab<-matrix(nrow=2,ncol=2,data=NA);colnames(tab)<-c("inf","un_inf")
  tab[1,]<-c(data.mosqu3[1,3],data.mosqu3[1,2]);tab[2,]<-c(data.mosqu3[i,3],data.mosqu3[i,2])
  test2a<-fisher.test(tab)
  oocystimpactPREV[i,2]<-test2a[3];oocystimpactPREV[i,3]<-test2a[1]
  oocystimpactPREV[i,4]<-test2a$conf.int[1];oocystimpactPREV[i,5]<-test2a$conf.int[2]
  
  tab2<-matrix(nrow=2,ncol=2,data=NA);colnames(tab2)<-c("inf","un_inf")
  tab2[1,]<-c(data.mosqu4[1,3],data.mosqu4[1,2]);tab2[2,]<-c(data.mosqu4[i,3],data.mosqu4[i,2])
  test2b<-fisher.test(tab2)
  oocystimpactPREV[i,6]<-test2b[3];oocystimpactPREV[i,7]<-test2b[1]
  oocystimpactPREV[i,8]<-test2b$conf.int[1];oocystimpactPREV[i,9]<-test2b$conf.int[2]
}

##combined impact on prevalence
data.mosqu<-expand.grid("treat"=unique(data.mosqu3$treat))
data.mosqu$inf_mosq<-data.mosqu3$inf_mosq+data.mosqu4$inf_mosq
data.mosqu$uninf_mosq<-data.mosqu3$uninf_mosq+data.mosqu4$uninf_mosq
data.mosqu$tot<-data.mosqu3$tot_mosq+data.mosqu4$tot_mosq

combinedPREV<-expand.grid("treat"=unique(data.mosqu3$treat))
combinedPREV$fisher<-NA
combinedPREV$P<-NA
combinedPREV$CI95LOW<-NA
combinedPREV$CI95UPP<-NA
for (i in 2:7){
tab3<-matrix(nrow=2,ncol=2,data=NA);colnames(tab3)<-c("inf","un_inf")
tab3[1,]<-c(data.mosqu[1,3],data.mosqu[1,2]);tab3[2,]<-c(data.mosqu[i,3],data.mosqu[i,2])
test2b<-fisher.test(tab3)
combinedPREV[i,2]<-test2b[3];combinedPREV[i,3]<-test2b[1]
combinedPREV[i,4]<-test2b$conf.int[1];combinedPREV[i,5]<-test2b$conf.int[2]
}

##SD

dataSD3<-subset(mosqOocystSlot3,treat=="Blank" | treat=="SD")
dataSD4<-subset(mosqOocystSlot4,treat=="Blank" | treat=="SD")

test1a<-wilcox.test(oocysts~treat,data=dataSD3);oocystimpact[2,2]=test1a[1];oocystimpact[2,3]=test1a[3]
test1b<-wilcox.test(oocysts~treat,data=dataSD4);oocystimpact[2,4]=test1b[1];oocystimpact[2,5]=test1b[3]


##ATV

dataATV3<-subset(mosqOocystSlot3,treat=="Blank" | treat=="ATV")
dataATV4<-subset(mosqOocystSlot4,treat=="Blank" | treat=="ATV")

test1a<-wilcox.test(oocysts~treat,data=dataATV3);oocystimpact[3,2]=test1a[1];oocystimpact[3,3]=test1a[3]
test1b<-wilcox.test(oocysts~treat,data=dataATV4);oocystimpact[3,4]=test1b[1];oocystimpact[3,5]=test1b[3]

##UCT

dataUCT3<-subset(mosqOocystSlot3,treat=="Blank" | treat=="UCT")
dataUCT4<-subset(mosqOocystSlot4,treat=="Blank" | treat=="UCT")

test1a<-wilcox.test(oocysts~treat,data=dataUCT3);oocystimpact[4,2]=test1a[1];oocystimpact[4,3]=test1a[3]
test1b<-wilcox.test(oocysts~treat,data=dataUCT4);oocystimpact[4,4]=test1b[1];oocystimpact[4,5]=test1b[3]


##OZPIP

dataPIP3<-subset(mosqOocystSlot3,treat=="Blank" | treat=="OZPIP")
dataPIP4<-subset(mosqOocystSlot4,treat=="Blank" | treat=="OZPIP")

test1a<-wilcox.test(oocysts~treat,data=dataPIP3);oocystimpact[5,2]=test1a[1];oocystimpact[5,3]=test1a[3]
test1b<-wilcox.test(oocysts~treat,data=dataPIP4);oocystimpact[5,4]=test1b[1];oocystimpact[5,5]=test1b[3]

##OZFER

dataFER3<-subset(mosqOocystSlot3,treat=="Blank" | treat=="OZFER")
dataFER4<-subset(mosqOocystSlot4,treat=="Blank" | treat=="OZFER")

test1a<-wilcox.test(oocysts~treat,data=dataFER3);oocystimpact[6,2]=test1a[1];oocystimpact[6,3]=test1a[3]
test1b<-wilcox.test(oocysts~treat,data=dataFER4);oocystimpact[6,4]=test1b[1];oocystimpact[6,5]=test1b[3]

##OZDSM

dataDSM3<-subset(mosqOocystSlot3,treat=="Blank" | treat=="OZDSM")
dataDSM4<-subset(mosqOocystSlot4,treat=="Blank" | treat=="OZDSM")

test1a<-wilcox.test(oocysts~treat,data=dataDSM3);oocystimpact[7,2]=test1a[1];oocystimpact[7,3]=test1a[3]
test1b<-wilcox.test(oocysts~treat,data=dataDSM4);oocystimpact[7,4]=test1b[1];oocystimpact[7,5]=test1b[3]

oocystimpactINTENSITY<-oocystimpact
oocystimpactPREV
oocystimpactINTENSITY