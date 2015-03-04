##########################################################################
## 
##  ##       ##     #########        ##         ##       ##
##  ###     #  #       ##          ##  ##     ##  ##    #  #
##  ## #   #    #      ##         ##    ##   ##    ##  ## 
##  ##  #  ######      ##         #      #   #      #  #
##  ##  #  #    #      ##         ##    ##   ##    ##  ##
##  ## #   #    #      ##          ##  ##     ##  ##    #  #
##  ###   ##    ##     ##   ##       ##         ##       ##
##  
##########################################################################


oocysts<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\UCT OZPIP FER DSM Feb2015\\M2M SporozoiteScores\\OocystIntensity.txt",header=TRUE)
blanks<-subset(oocysts,Treatment=="Blank")

oocdata1<-sort(blanks$oocysts[blanks$oocysts > 0])
length(oocdata1)/5

ooc<-c(0,mean(oocdata1[1:19]),mean(oocdata1[20:38]),mean(oocdata1[38:57]),mean(oocdata1[58:77]),mean(oocdata1[78:96]))
b1<-numeric(10000)
b2<-numeric(10000)
b3<-numeric(10000)
b4<-numeric(10000)
b5<-numeric(10000)
for (i in 1:10000) b1[i] <-sample(oocdata1[1:19],replace=TRUE)
bo1<-quantile(b1,c(0.025,0.975))
for (i in 1:10000) b2[i] <-sample(oocdata1[20:38],replace=TRUE)
bo2<-quantile(b2,c(0.025,0.975))
for (i in 1:10000) b3[i] <-sample(oocdata1[38:57],replace=TRUE)
bo3<-quantile(b3,c(0.025,0.975))
for (i in 1:10000) b4[i] <-sample(oocdata1[58:77],replace=TRUE)
bo4<-quantile(b4,c(0.025,0.975))
for (i in 1:10000) b5[i] <-sample(oocdata1[78:96],replace=TRUE)
bo5<-quantile(b5,c(0.025,0.975))
oocL<-c(0,bo1[1],bo2[1],bo3[1],bo4[1],bo5[1])
oocU<-c(0,bo1[2],bo2[2],bo3[2],bo4[2],bo5[2])


OZFER<-subset(oocysts,Treatment=="OZFER")

oocdataD1FER<-sort(OZFER$oocysts[OZFER$oocysts > 0])
length(oocdataD1FER)/5

oocFER<-c(0,mean(oocdataD1FER[1:12]),mean(oocdataD1FER[13:24]),mean(oocdataD1FER[25:36]),mean(oocdataD1FER[37:48]),mean(oocdataD1FER[49:60]))
b1<-numeric(10000)
b2<-numeric(10000)
b3<-numeric(10000)
b4<-numeric(10000)
b5<-numeric(10000)
for (i in 1:10000) b1[i] <-sample(oocdataD1FER[1:12],replace=TRUE)
bo1<-quantile(b1,c(0.025,0.975))
for (i in 1:10000) b2[i] <-sample(oocdataD1FER[13:24],replace=TRUE)
bo2<-quantile(b2,c(0.025,0.975))
for (i in 1:10000) b3[i] <-sample(oocdataD1FER[25:36],replace=TRUE)
bo3<-quantile(b3,c(0.025,0.975))
for (i in 1:10000) b4[i] <-sample(oocdataD1FER[37:48],replace=TRUE)
bo4<-quantile(b4,c(0.025,0.975))
for (i in 1:10000) b5[i] <-sample(oocdataD1FER[49:60],replace=TRUE)
bo5<-quantile(b5,c(0.025,0.975))
oocFERL<-c(0,bo1[1],bo2[1],bo3[1],bo4[1],bo5[1])
oocFERU<-c(0,bo1[2],bo2[2],bo3[2],bo4[2],bo5[2])

OZDSM<-subset(oocysts,Treatment=="OZDSM")

oocdataD1DSM<-sort(OZDSM$oocysts[OZDSM$oocysts > 0])
length(oocdataD1DSM)/5

oocDSM<-c(0,mean(oocdataD1DSM[1:15]),mean(oocdataD1DSM[16:30]),mean(oocdataD1DSM[31:44]),mean(oocdataD1DSM[45:60]),mean(oocdataD1DSM[61:74]))
b1<-numeric(10000)
b2<-numeric(10000)
b3<-numeric(10000)
b4<-numeric(10000)
b5<-numeric(10000)
for (i in 1:10000) b1[i] <-sample(oocdataD1DSM[1:15],replace=TRUE)
bo1<-quantile(b1,c(0.025,0.975))
for (i in 1:10000) b2[i] <-sample(oocdataD1DSM[16:30],replace=TRUE)
bo2<-quantile(b2,c(0.025,0.975))
for (i in 1:10000) b3[i] <-sample(oocdataD1DSM[31:44],replace=TRUE)
bo3<-quantile(b3,c(0.025,0.975))
for (i in 1:10000) b4[i] <-sample(oocdataD1DSM[45:60],replace=TRUE)
bo4<-quantile(b4,c(0.025,0.975))
for (i in 1:10000) b5[i] <-sample(oocdataD1DSM[61:74],replace=TRUE)
bo5<-quantile(b5,c(0.025,0.975))
oocDSML<-c(0,bo1[1],bo2[1],bo3[1],bo4[1],bo5[1])
oocDSMU<-c(0,bo1[2],bo2[2],bo3[2],bo4[2],bo5[2])


OZPIP<-subset(oocysts,Treatment=="OZPIP")

oocdataD1PIP<-sort(OZPIP$oocysts[OZPIP$oocysts > 0])
length(oocdataD1PIP)/5

oocPIP<-c(0,mean(oocdataD1PIP[1:15]),mean(oocdataD1PIP[16:30]),mean(oocdataD1PIP[31:44]),mean(oocdataD1PIP[45:60]),mean(oocdataD1PIP[61:73]))
b1<-numeric(10000)
b2<-numeric(10000)
b3<-numeric(10000)
b4<-numeric(10000)
b5<-numeric(10000)
for (i in 1:10000) b1[i] <-sample(oocdataD1PIP[1:15],replace=TRUE)
bo1<-quantile(b1,c(0.025,0.975))
for (i in 1:10000) b2[i] <-sample(oocdataD1PIP[16:30],replace=TRUE)
bo2<-quantile(b2,c(0.025,0.975))
for (i in 1:10000) b3[i] <-sample(oocdataD1PIP[31:44],replace=TRUE)
bo3<-quantile(b3,c(0.025,0.975))
for (i in 1:10000) b4[i] <-sample(oocdataD1PIP[45:60],replace=TRUE)
bo4<-quantile(b4,c(0.025,0.975))
for (i in 1:10000) b5[i] <-sample(oocdataD1PIP[61:73],replace=TRUE)
bo5<-quantile(b5,c(0.025,0.975))
oocPIPL<-c(0,bo1[1],bo2[1],bo3[1],bo4[1],bo5[1])
oocPIPU<-c(0,bo1[2],bo2[2],bo3[2],bo4[2],bo5[2])


UCT<-subset(oocysts,Treatment=="UCT")

oocdataD1UCT<-sort(UCT$oocysts[UCT$oocysts > 0])
length(oocdataD1UCT)/5

oocUCT<-c(0,mean(oocdataD1UCT[1:13]),mean(oocdataD1UCT[14:26]),mean(oocdataD1UCT[27:40]),mean(oocdataD1UCT[41:54]),mean(oocdataD1UCT[55:67]))
b1<-numeric(10000)
b2<-numeric(10000)
b3<-numeric(10000)
b4<-numeric(10000)
b5<-numeric(10000)
for (i in 1:10000) b1[i] <-sample(oocdataD1UCT[1:13],replace=TRUE)
bo1<-quantile(b1,c(0.025,0.975))
for (i in 1:10000) b2[i] <-sample(oocdataD1UCT[14:26],replace=TRUE)
bo2<-quantile(b2,c(0.025,0.975))
for (i in 1:10000) b3[i] <-sample(oocdataD1UCT[27:40],replace=TRUE)
bo3<-quantile(b3,c(0.025,0.975))
for (i in 1:10000) b4[i] <-sample(oocdataD1UCT[41:54],replace=TRUE)
bo4<-quantile(b4,c(0.025,0.975))
for (i in 1:10000) b5[i] <-sample(oocdataD1UCT[55:67],replace=TRUE)
bo5<-quantile(b5,c(0.025,0.975))
oocUCTL<-c(0,bo1[1],bo2[1],bo3[1],bo4[1],bo5[1])
oocUCTU<-c(0,bo1[2],bo2[2],bo3[2],bo4[2],bo5[2])


oocATV25<-read.table("")


##########################################################################
## 
##  ##       ##     #########        ##    ###       ##      ###
##  ###     #  #       ##          ##  ##  ## ##   ##  ##    ## ##
##  ## #   #    #      ##          ##      ##  #  ##    ##   ##  #
##  ##  #  ######      ##            ##    ####   #      #   ####    
##  ##  #  #    #      ##              ##  ##     ##    ##   ## #
##  ## #   #    #      ##          ##  ##  ##      ##  ##    ##  #
##  ###   ##    ##     ##   ##       ##    ##        ##      ##  ##  
##  
##########################################################################

spors<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\UCT OZPIP FER DSM Feb2015\\M2M SporozoiteScores\\SporoParaIntensity.txt",header=TRUE)
head(spors)
spors$bsprev<-ifelse(spors$Parasitemia == 0,0,1)

spors$sporozoites<-numeric(length(spors$Round))
for (i in 1:length(spors$Round)){
  spors$sporozoites[i]<-sum(spors[i,5:14],na.rm=TRUE)}

spors$ScorePerBite<-spors$sporozoites/spors$Bites

score0<- spors$ScorePerBite[spors$ScorePerBite == 0 & spors$Treatment == "Blank"]
score1<- spors$ScorePerBite[spors$ScorePerBite > 0 & spors$ScorePerBite<1 & spors$Treatment == "Blank"]
score2<- spors$ScorePerBite[spors$ScorePerBite >= 1 & spors$ScorePerBite<2 & spors$Treatment == "Blank"]
score3<- spors$ScorePerBite[spors$ScorePerBite >= 2 & spors$ScorePerBite<3 & spors$Treatment == "Blank"]
score4<- spors$ScorePerBite[spors$ScorePerBite >= 3 & spors$ScorePerBite<4 & spors$Treatment == "Blank"]
score5<- spors$ScorePerBite[spors$ScorePerBite >= 4 & spors$Treatment == "Blank"]

score<-c(score0,score1,score2,score3,score4,score5)
logScore<-c(mean(score0),mean(score1),mean(score2),mean(score3),mean(score4),max(score4))
a1<-numeric(10000)
a2<-numeric(10000)
a3<-numeric(10000)
a4<-numeric(10000)
for (i in 1:10000) a1[i] <-sample(score1,replace=TRUE)
ao1<-quantile(a1,c(0.025,0.975))
for (i in 1:10000) a2[i] <-sample(score2,replace=TRUE)
ao2<-quantile(a2,c(0.025,0.975))
for (i in 1:10000) a3[i] <-sample(score3,replace=TRUE)
ao3<-quantile(a3,c(0.025,0.975))
for (i in 1:10000) a4[i] <-sample(score4,replace=TRUE)
ao4<-quantile(a4,c(0.025,0.975))
logScoreL<-c(0,ao1[1],ao2[1],ao3[1],ao4[1],max(score4))
logScoreU<-c(0,ao1[2],ao2[2],ao3[2],ao4[2],max(score4))


score0<- spors$ScorePerBite[spors$ScorePerBite == 0 & spors$Treatment == "OZFER"]
score1<- spors$ScorePerBite[spors$ScorePerBite > 0 & spors$ScorePerBite<1 & spors$Treatment == "OZFER"]
score2<- spors$ScorePerBite[spors$ScorePerBite >= 1 & spors$ScorePerBite<2 & spors$Treatment == "OZFER"]
score3<- spors$ScorePerBite[spors$ScorePerBite >= 2 & spors$ScorePerBite<3 & spors$Treatment == "OZFER"]
score4<- spors$ScorePerBite[spors$ScorePerBite >= 3 & spors$ScorePerBite<4 & spors$Treatment == "OZFER"]
score5<- spors$ScorePerBite[spors$ScorePerBite >= 4 & spors$Treatment == "OZFER"]

FERscore<-c(score0,score1,score2,score3,score4,score5)
logScoreFER<-c(mean(score0),mean(score1),mean(score2),mean(score3),mean(score4),max(score4))
a1<-numeric(10000)
a2<-numeric(10000)
a3<-numeric(10000)
a4<-numeric(10000)
for (i in 1:10000) a1[i] <-sample(score1,replace=TRUE)
ao1<-quantile(a1,c(0.025,0.975))
for (i in 1:10000) a2[i] <-sample(score2,replace=TRUE)
ao2<-quantile(a2,c(0.025,0.975))
for (i in 1:10000) a3[i] <-sample(score3,replace=TRUE)
ao3<-quantile(a3,c(0.025,0.975))
for (i in 1:10000) a4[i] <-sample(score4,replace=TRUE)
ao4<-quantile(a4,c(0.025,0.975))
logScoreFERL<-c(0,ao1[1],ao2[1],ao3[1],ao4[1],max(score4))
logScoreFERU<-c(0,ao1[2],ao2[2],ao3[2],ao4[2],max(score4))


score0<- spors$ScorePerBite[spors$ScorePerBite == 0 & spors$Treatment == "OZDSM"]
score1<- spors$ScorePerBite[spors$ScorePerBite > 0 & spors$ScorePerBite<1 & spors$Treatment == "OZDSM"]
score2<- spors$ScorePerBite[spors$ScorePerBite >= 1 & spors$ScorePerBite<2 & spors$Treatment == "OZDSM"]
score3<- spors$ScorePerBite[spors$ScorePerBite >= 2 & spors$ScorePerBite<3 & spors$Treatment == "OZDSM"]
score4<- spors$ScorePerBite[spors$ScorePerBite >= 3 & spors$ScorePerBite<4 & spors$Treatment == "OZDSM"]
score5<- spors$ScorePerBite[spors$ScorePerBite >= 4 & spors$Treatment == "OZDSM"]

DSMscore<-c(score0,score1,score2,score3,score4,score5)
logScoreDSM<-c(mean(score0),mean(score1),mean(score2),mean(score3),mean(score4),max(score4))
a1<-numeric(10000)
a2<-numeric(10000)
a3<-numeric(10000)
a4<-numeric(10000)
for (i in 1:10000) a1[i] <-sample(score1,replace=TRUE)
ao1<-quantile(a1,c(0.025,0.975))
for (i in 1:10000) a2[i] <-sample(score2,replace=TRUE)
ao2<-quantile(a2,c(0.025,0.975))
for (i in 1:10000) a3[i] <-sample(score3,replace=TRUE)
ao3<-quantile(a3,c(0.025,0.975))
for (i in 1:10000) a4[i] <-sample(score4,replace=TRUE)
ao4<-quantile(a4,c(0.025,0.975))
logScoreDSML<-c(0,ao1[1],ao2[1],ao3[1],ao4[1],max(score4))
logScoreDSMU<-c(0,ao1[2],ao2[2],ao3[2],ao4[2],max(score4))

score0<- spors$ScorePerBite[spors$ScorePerBite == 0 & spors$Treatment == "OZPIP"]
score1<- spors$ScorePerBite[spors$ScorePerBite > 0 & spors$ScorePerBite<1 & spors$Treatment == "OZPIP"]
score2<- spors$ScorePerBite[spors$ScorePerBite >= 1 & spors$ScorePerBite<2 & spors$Treatment == "OZPIP"]
score3<- spors$ScorePerBite[spors$ScorePerBite >= 2 & spors$ScorePerBite<3 & spors$Treatment == "OZPIP"]
score4<- spors$ScorePerBite[spors$ScorePerBite >= 3 & spors$ScorePerBite<4 & spors$Treatment == "OZPIP"]
score5<- spors$ScorePerBite[spors$ScorePerBite >= 4 & spors$Treatment == "OZPIP"]

PIPscore<-c(score0,score1,score2,score3,score4,score5)
logScorePIP<-c(mean(score0),mean(score1),mean(score2),mean(score3),mean(score4),max(score4))
a1<-numeric(10000)
a2<-numeric(10000)
a3<-numeric(10000)
a4<-numeric(10000)
for (i in 1:10000) a1[i] <-sample(score1,replace=TRUE)
ao1<-quantile(a1,c(0.025,0.975))
for (i in 1:10000) a2[i] <-sample(score2,replace=TRUE)
ao2<-quantile(a2,c(0.025,0.975))
for (i in 1:10000) a3[i] <-sample(score3,replace=TRUE)
ao3<-quantile(a3,c(0.025,0.975))
for (i in 1:10000) a4[i] <-sample(score4,replace=TRUE)
ao4<-quantile(a4,c(0.025,0.975))
logScorePIPL<-c(0,ao1[1],ao2[1],ao3[1],ao4[1],max(score4))
logScorePIPU<-c(0,ao1[2],ao2[2],ao3[2],ao4[2],max(score4))


score0<- spors$ScorePerBite[spors$ScorePerBite == 0 & spors$Treatment == "UCT"]
score1<- spors$ScorePerBite[spors$ScorePerBite > 0 & spors$ScorePerBite<1 & spors$Treatment == "UCT"]
score2<- spors$ScorePerBite[spors$ScorePerBite >= 1 & spors$ScorePerBite<2 & spors$Treatment == "UCT"]
score3<- spors$ScorePerBite[spors$ScorePerBite >= 2 & spors$ScorePerBite<3 & spors$Treatment == "UCT"]
score4<- spors$ScorePerBite[spors$ScorePerBite >= 3 & spors$ScorePerBite<4 & spors$Treatment == "UCT"]
score5<- spors$ScorePerBite[spors$ScorePerBite >= 4 & spors$Treatment == "UCT"]

UCTscore<-c(score0,score1,score2,score3,score4,score5)
logScoreUCT<-c(mean(score0),mean(score1),mean(score2),mean(score3),mean(score4),max(score4))
a1<-numeric(10000)
a2<-numeric(10000)
a3<-numeric(10000)
a4<-numeric(10000)
for (i in 1:10000) a1[i] <-sample(score1,replace=TRUE)
ao1<-quantile(a1,c(0.025,0.975))
for (i in 1:10000) a2[i] <-sample(score2,replace=TRUE)
ao2<-quantile(a2,c(0.025,0.975))
for (i in 1:10000) a3[i] <-sample(score3,replace=TRUE)
ao3<-quantile(a3,c(0.025,0.975))
for (i in 1:10000) a4[i] <-sample(score4,replace=TRUE)
ao4<-quantile(a4,c(0.025,0.975))
logScoreUCTL<-c(0,ao1[1],ao2[1],ao3[1],ao4[1],max(score4))
logScoreUCTU<-c(0,ao1[2],ao2[2],ao3[2],ao4[2],max(score4))

##########################################################################
## 
##  ##       ##     #########      ###    ###    ######  ##    ##
##  ###     #  #       ##          ## ##  ## ##  ##       #    #
##  ## #   #    #      ##          ##  #  ##  #  ##       ##  ##
##  ##  #  ######      ##          ####   ####   ####      #  #
##  ##  #  #    #      ##          ##     ## #   ##        ####
##  ## #   #    #      ##          ##     ##  #  ##         ##
##  ###   ##    ##     ##   ##     ##     ##  ## ######     ##
##  
##########################################################################

sum(spors$bsprev[spors$Treatment == "Blank"])/length(spors$bsprev[spors$Treatment == "Blank"])

sum(spors$bsprev[spors$Treatment == "OZFER"])/length(spors$bsprev[spors$Treatment == "OZFER"])
sum(spors$bsprev[spors$Treatment == "OZDSM"])/length(spors$bsprev[spors$Treatment == "OZDSM"])
sum(spors$bsprev[spors$Treatment == "OZPIP"])/length(spors$bsprev[spors$Treatment == "OZPIP"])
sum(spors$bsprev[spors$Treatment == "UCT"])/length(spors$bsprev[spors$Treatment == "UCT"])

BLANKbsprev0<-sum(spors$bsprev[spors$ScorePerBite==0 & spors$Treatment == "Blank"])/length(spors$bsprev[spors$ScorePerBite==0 & spors$Treatment == "Blank"])
BLANKbsprev1<-sum(spors$bsprev[spors$ScorePerBite > 0 & spors$ScorePerBite <= 1 & spors$Treatment == "Blank"])/length(spors$bsprev[spors$ScorePerBite > 0 & spors$ScorePerBite <= 1 & spors$Treatment == "Blank"])
BLANKbsprev2<-sum(spors$bsprev[spors$ScorePerBite > 1 & spors$ScorePerBite <= 2 & spors$Treatment == "Blank"])/length(spors$bsprev[spors$ScorePerBite > 1 & spors$ScorePerBite <= 2 & spors$Treatment == "Blank"])
BLANKbsprev3<-sum(spors$bsprev[spors$ScorePerBite > 2 & spors$ScorePerBite <= 3 & spors$Treatment == "Blank"])/length(spors$bsprev[spors$ScorePerBite > 2 & spors$ScorePerBite <= 3 & spors$Treatment == "Blank"])
BLANKbsprev4<-sum(spors$bsprev[spors$ScorePerBite > 3 & spors$ScorePerBite <= 4 & spors$Treatment == "Blank"])/length(spors$bsprev[spors$ScorePerBite > 3 & spors$ScorePerBite <= 4 & spors$Treatment == "Blank"])
BLANKbsprev5<-sum(spors$bsprev[spors$ScorePerBite > 4 & spors$Treatment == "Blank"])/length(spors$bsprev[spors$ScorePerBite > 4 & spors$Treatment == "Blank"])
prevBLANK<-c(BLANKbsprev0,BLANKbsprev1,BLANKbsprev2,BLANKbsprev3,NA,NA)



#& spors$Treatment == "UCT"

FERbsprev0<-sum(spors$bsprev[spors$ScorePerBite==0 & spors$Treatment == "OZFER"])/length(spors$bsprev[spors$ScorePerBite==0 & spors$Treatment == "OZFER"])
FERbsprev1<-sum(spors$bsprev[spors$ScorePerBite > 0 & spors$ScorePerBite <= 1 & spors$Treatment == "OZFER"])/length(spors$bsprev[spors$ScorePerBite > 0 & spors$ScorePerBite <= 1 & spors$Treatment == "OZFER"])
FERbsprev2<-sum(spors$bsprev[spors$ScorePerBite > 1 & spors$ScorePerBite <= 2 & spors$Treatment == "OZFER"])/length(spors$bsprev[spors$ScorePerBite > 1 & spors$ScorePerBite <= 2 & spors$Treatment == "OZFER"])
FERbsprev3<-sum(spors$bsprev[spors$ScorePerBite > 2 & spors$ScorePerBite <= 3 & spors$Treatment == "OZFER"])/length(spors$bsprev[spors$ScorePerBite > 2 & spors$ScorePerBite <= 3 & spors$Treatment == "OZFER"])
FERbsprev4<-sum(spors$bsprev[spors$ScorePerBite > 3 & spors$ScorePerBite <= 4 & spors$Treatment == "OZFER"])/length(spors$bsprev[spors$ScorePerBite > 3 & spors$ScorePerBite <= 4 & spors$Treatment == "OZFER"])
FERbsprev5<-sum(spors$bsprev[spors$ScorePerBite > 4 & spors$Treatment == "OZFER"])/length(spors$bsprev[spors$ScorePerBite > 4 & spors$Treatment == "OZFER"])
prevFER<-c(FERbsprev0,FERbsprev1,FERbsprev2,FERbsprev3,FERbsprev4,NA)

PIPbsprev0<-sum(spors$bsprev[spors$ScorePerBite==0 & spors$Treatment == "OZPIP"])/length(spors$bsprev[spors$ScorePerBite==0 & spors$Treatment == "OZPIP"])
PIPbsprev1<-sum(spors$bsprev[spors$ScorePerBite > 0 & spors$ScorePerBite <= 1 & spors$Treatment == "OZPIP"])/length(spors$bsprev[spors$ScorePerBite > 0 & spors$ScorePerBite <= 1 & spors$Treatment == "OZPIP"])
PIPbsprev2<-sum(spors$bsprev[spors$ScorePerBite > 1 & spors$ScorePerBite <= 2 & spors$Treatment == "OZPIP"])/length(spors$bsprev[spors$ScorePerBite > 1 & spors$ScorePerBite <= 2 & spors$Treatment == "OZPIP"])
PIPbsprev3<-sum(spors$bsprev[spors$ScorePerBite > 2 & spors$ScorePerBite <= 3 & spors$Treatment == "OZPIP"])/length(spors$bsprev[spors$ScorePerBite > 2 & spors$ScorePerBite <= 3 & spors$Treatment == "OZPIP"])
PIPbsprev4<-sum(spors$bsprev[spors$ScorePerBite > 3 & spors$ScorePerBite <= 4 & spors$Treatment == "OZPIP"])/length(spors$bsprev[spors$ScorePerBite > 3 & spors$ScorePerBite <= 4 & spors$Treatment == "OZPIP"])
PIPbsprev5<-sum(spors$bsprev[spors$ScorePerBite > 4 & spors$Treatment == "OZPIP"])/length(spors$bsprev[spors$ScorePerBite > 4 & spors$Treatment == "OZPIP"])
prevPIP<-c(PIPbsprev0,PIPbsprev1,PIPbsprev2,PIPbsprev3,PIPbsprev4,NA)

DSMbsprev0<-sum(spors$bsprev[spors$ScorePerBite==0 & spors$Treatment == "OZDSM"])/length(spors$bsprev[spors$ScorePerBite==0 & spors$Treatment == "OZDSM"])
DSMbsprev1<-sum(spors$bsprev[spors$ScorePerBite > 0 & spors$ScorePerBite <= 1 & spors$Treatment == "OZDSM"])/length(spors$bsprev[spors$ScorePerBite > 0 & spors$ScorePerBite <= 1 & spors$Treatment == "OZDSM"])
DSMbsprev2<-sum(spors$bsprev[spors$ScorePerBite > 1 & spors$ScorePerBite <= 2 & spors$Treatment == "OZDSM"])/length(spors$bsprev[spors$ScorePerBite > 1 & spors$ScorePerBite <= 2 & spors$Treatment == "OZDSM"])
DSMbsprev3<-sum(spors$bsprev[spors$ScorePerBite > 2 & spors$ScorePerBite <= 3 & spors$Treatment == "OZDSM"])/length(spors$bsprev[spors$ScorePerBite > 2 & spors$ScorePerBite <= 3 & spors$Treatment == "OZDSM"])
DSMbsprev4<-sum(spors$bsprev[spors$ScorePerBite > 3 & spors$ScorePerBite <= 4 & spors$Treatment == "OZDSM"])/length(spors$bsprev[spors$ScorePerBite > 3 & spors$ScorePerBite <= 4 & spors$Treatment == "OZDSM"])
DSMbsprev5<-sum(spors$bsprev[spors$ScorePerBite > 4 & spors$Treatment == "OZDSM"])/length(spors$bsprev[spors$ScorePerBite > 4 & spors$Treatment == "OZDSM"])
prevDSM<-c(DSMbsprev0,DSMbsprev1,DSMbsprev2,DSMbsprev3,NA,NA)

UCTbsprev0<-sum(spors$bsprev[spors$ScorePerBite==0 & spors$Treatment == "UCT"])/length(spors$bsprev[spors$ScorePerBite==0 & spors$Treatment == "UCT"])
UCTbsprev1<-sum(spors$bsprev[spors$ScorePerBite > 0 & spors$ScorePerBite <= 1 & spors$Treatment == "UCT"])/length(spors$bsprev[spors$ScorePerBite > 0 & spors$ScorePerBite <= 1 & spors$Treatment == "UCT"])
UCTbsprev2<-sum(spors$bsprev[spors$ScorePerBite > 1 & spors$ScorePerBite <= 2 & spors$Treatment == "UCT"])/length(spors$bsprev[spors$ScorePerBite > 1 & spors$ScorePerBite <= 2 & spors$Treatment == "UCT"])
UCTbsprev3<-sum(spors$bsprev[spors$ScorePerBite > 2 & spors$ScorePerBite <= 3 & spors$Treatment == "UCT"])/length(spors$bsprev[spors$ScorePerBite > 2 & spors$ScorePerBite <= 3 & spors$Treatment == "UCT"])
UCTbsprev4<-sum(spors$bsprev[spors$ScorePerBite > 3 & spors$ScorePerBite <= 4 & spors$Treatment == "UCT"])/length(spors$bsprev[spors$ScorePerBite > 3 & spors$ScorePerBite <= 4 & spors$Treatment == "UCT"])
UCTbsprev5<-sum(spors$bsprev[spors$ScorePerBite > 4 & spors$Treatment == "UCT"])/length(spors$bsprev[spors$ScorePerBite > 4 & spors$Treatment == "UCT"])
prevUCT<-c(UCTbsprev0,UCTbsprev1,UCTbsprev2,UCTbsprev3,NA,NA)



##########################################################################
## 
##  ##       ##     #########      ###    ###        ##     ####  
##  ###     #  #       ##          ## ##  ## ##    ##  ##   ## ## 
##  ## #   #    #      ##          ##  #  ##  #   #      #  ##  #
##  ##  #  ######      ##          ####   ####   ##      ## ####
##  ##  #  #    #      ##          ##     ## #    #      #  ##
##  ## #   #    #      ##          ##     ##  #    ##  ##   ##
##  ###   ##    ##     ##   ##     ##     ##  ##     ##     ##
##  
##########################################################################

BLANKbspr0<-length(spors$bsprev[spors$ScorePerBite==0 & spors$Treatment == "Blank"])
BLANKbspr1<-length(spors$bsprev[spors$ScorePerBite > 0 & spors$ScorePerBite <= 1 & spors$Treatment == "Blank"])
BLANKbspr2<-length(spors$bsprev[spors$ScorePerBite > 1 & spors$ScorePerBite <= 2 & spors$Treatment == "Blank"])
BLANKbspr3<-length(spors$bsprev[spors$ScorePerBite > 2 & spors$ScorePerBite <= 3 & spors$Treatment == "Blank"])
BLANKbspr4<-length(spors$bsprev[spors$ScorePerBite > 3 & spors$ScorePerBite <= 4 & spors$Treatment == "Blank"])
BLANKbspr5<-length(spors$bsprev[spors$ScorePerBite > 4 & spors$Treatment == "Blank"])
propBLANK1<-c(BLANKbspr0,BLANKbspr1,BLANKbspr2,BLANKbspr3,BLANKbspr4,BLANKbspr5)
propBLANK<-c(BLANKbspr0,BLANKbspr1,BLANKbspr2,BLANKbspr3,BLANKbspr4,BLANKbspr5)/length(spors$bsprev[spors$Treatment == "Blank"])

& spors$Treatment == "UCT"

FERbspr0<-length(spors$bsprev[spors$ScorePerBite==0 & spors$Treatment == "OZFER"])
FERbspr1<-length(spors$bsprev[spors$ScorePerBite > 0 & spors$ScorePerBite <= 1 & spors$Treatment == "OZFER"])
FERbspr2<-length(spors$bsprev[spors$ScorePerBite > 1 & spors$ScorePerBite <= 2 & spors$Treatment == "OZFER"])
FERbspr3<-length(spors$bsprev[spors$ScorePerBite > 2 & spors$ScorePerBite <= 3 & spors$Treatment == "OZFER"])
FERbspr4<-length(spors$bsprev[spors$ScorePerBite > 3 & spors$ScorePerBite <= 4 & spors$Treatment == "OZFER"])
FERbspr5<-length(spors$bsprev[spors$ScorePerBite > 4 & spors$Treatment == "OZFER"])
propFER1<-c(FERbspr0,FERbspr1,FERbspr2,FERbspr3,FERbspr4,FERbspr5)
propFER<-c(FERbspr0,FERbspr1,FERbspr2,FERbspr3,FERbspr4,FERbspr5)/length(spors$bsprev[spors$Treatment == "OZFER"])


PIPbspr0<-length(spors$bsprev[spors$ScorePerBite==0 & spors$Treatment == "OZPIP"])
PIPbspr1<-length(spors$bsprev[spors$ScorePerBite > 0 & spors$ScorePerBite <= 1 & spors$Treatment == "OZPIP"])
PIPbspr2<-length(spors$bsprev[spors$ScorePerBite > 1 & spors$ScorePerBite <= 2 & spors$Treatment == "OZPIP"])
PIPbspr3<-length(spors$bsprev[spors$ScorePerBite > 2 & spors$ScorePerBite <= 3 & spors$Treatment == "OZPIP"])
PIPbspr4<-length(spors$bsprev[spors$ScorePerBite > 3 & spors$ScorePerBite <= 4 & spors$Treatment == "OZPIP"])
PIPbspr5<-length(spors$bsprev[spors$ScorePerBite > 4 & spors$Treatment == "OZPIP"])
propPIP1<-c(PIPbspr0,PIPbspr1,PIPbspr2,PIPbspr3,PIPbspr4,PIPbspr5)
propPIP<-c(PIPbspr0,PIPbspr1,PIPbspr2,PIPbspr3,PIPbspr4,PIPbspr5)/length(spors$bsprev[spors$Treatment == "OZPIP"])

DSMbspr0<-length(spors$bsprev[spors$ScorePerBite==0 & spors$Treatment == "OZDSM"])
DSMbspr1<-length(spors$bsprev[spors$ScorePerBite > 0 & spors$ScorePerBite <= 1 & spors$Treatment == "OZDSM"])
DSMbspr2<-length(spors$bsprev[spors$ScorePerBite > 1 & spors$ScorePerBite <= 2 & spors$Treatment == "OZDSM"])
DSMbspr3<-length(spors$bsprev[spors$ScorePerBite > 2 & spors$ScorePerBite <= 3 & spors$Treatment == "OZDSM"])
DSMbspr4<-length(spors$bsprev[spors$ScorePerBite > 3 & spors$ScorePerBite <= 4 & spors$Treatment == "OZDSM"])
DSMbspr5<-length(spors$bsprev[spors$ScorePerBite > 4 & spors$Treatment == "OZDSM"])
propDSM1<-c(DSMbspr0,DSMbspr1,DSMbspr2,DSMbspr3,DSMbspr4,DSMbspr5)
propDSM<-c(DSMbspr0,DSMbspr1,DSMbspr2,DSMbspr3,DSMbspr4,DSMbspr5)/length(spors$bsprev[spors$Treatment == "OZDSM"])

UCTbspr0<-length(spors$bsprev[spors$ScorePerBite==0 & spors$Treatment == "UCT"])
UCTbspr1<-length(spors$bsprev[spors$ScorePerBite > 0 & spors$ScorePerBite <= 1 & spors$Treatment == "UCT"])
UCTbspr2<-length(spors$bsprev[spors$ScorePerBite > 1 & spors$ScorePerBite <= 2 & spors$Treatment == "UCT"])
UCTbspr3<-length(spors$bsprev[spors$ScorePerBite > 2 & spors$ScorePerBite <= 3 & spors$Treatment == "UCT"])
UCTbspr4<-length(spors$bsprev[spors$ScorePerBite > 3 & spors$ScorePerBite <= 4 & spors$Treatment == "UCT"])
UCTbspr5<-length(spors$bsprev[spors$ScorePerBite > 4 & spors$Treatment == "UCT"])
propUCT1<-c(UCTbspr0,UCTbspr1,UCTbspr2,UCTbspr3,UCTbspr4,UCTbspr5)
propUCT<-c(UCTbspr0,UCTbspr1,UCTbspr2,UCTbspr3,UCTbspr4,UCTbspr5)/length(spors$bsprev[spors$Treatment == "UCT"])





#########################################
##
###
#### Simple plots
###
##
########################################
ooc #mean number of oocysts per 20% ranked data
prooc<-ooc/max(ooc)        #mean proportion of oocysts per 20% of ranked data, least to most infected
logScore   #mean sporozoite score post-bite for each mice
prevBLANK  #prevalence of blood-stage infection per mouse
propBLANK  #proportion of mice in this category of sporozoite score

plot(ooc/max(ooc),prevBLANK)
lines(ooc/max(ooc),prevBLANK)
plot(propBLANK~prooc,col="blue",lty=2)
lines(propBLANK~prooc,col="blue",lty=2)

plot(prooc,prevBLANK*propBLANK)
lines(prooc,prevBLANK*propBLANK)

lines(oocFER/max(oocFER),prevFER*propFER,col="blue",lty=2)
lines(oocPIP/max(oocPIP),prevPIP*propPIP,col="red",lty=2)
lines(oocDSM/max(oocDSM),prevDSM*propDSM,col="green",lty=2)
lines(oocUCT/max(oocUCT),prevUCT*propUCT,col="orange",lty=2)


#######################################
##
###
#### OOCYSTS TO sporos score
###
##
########################################
par(mfrow=c(1,2))
#
## Saturating fit
#
sat.binom<-function(p.vec){
  
  alpha  <- p.vec[1]
  beta  <- p.vec[2]
  delta <- p.vec[3]
  gamma <- p.vec[4]
  
  pred1a<- (alpha * sort(sample(blanks$oocysts,30))^beta)/(delta + gamma * sort(sample(blanks$oocysts,30))^beta)
    pred1b<- (alpha * sort(sample(blanks$oocysts,30))^beta)/(delta + gamma * sort(sample(blanks$oocysts,30))^beta)
    pred1c<- (alpha * sort(sample(blanks$oocysts,30))^beta)/(delta + gamma * sort(sample(blanks$oocysts,30))^beta)
    pred1d<- (alpha * sort(sample(blanks$oocysts,30))^beta)/(delta + gamma * sort(sample(blanks$oocysts,30))^beta)
    pred1e<- (alpha * sort(sample(blanks$oocysts,30))^beta)/(delta + gamma * sort(sample(blanks$oocysts,30))^beta)
  
  pred2a<- (alpha * sort(sample(OZFER$oocysts,30))^beta)/(delta + gamma * sort(sample(OZFER$oocysts,30))^beta)
    pred2b<- (alpha * sort(sample(OZFER$oocysts,30))^beta)/(delta + gamma * sort(sample(OZFER$oocysts,30))^beta)
    pred2c<- (alpha * sort(sample(OZFER$oocysts,30))^beta)/(delta + gamma * sort(sample(OZFER$oocysts,30))^beta)
    pred2d<- (alpha * sort(sample(OZFER$oocysts,30))^beta)/(delta + gamma * sort(sample(OZFER$oocysts,30))^beta)
    pred2e<- (alpha * sort(sample(OZFER$oocysts,30))^beta)/(delta + gamma * sort(sample(OZFER$oocysts,30))^beta)
  
  pred3a<- (alpha * sort(sample(OZDSM$oocysts,30))^beta)/(delta + gamma * sort(sample(OZDSM$oocysts,30))^beta)
    pred3b<- (alpha * sort(sample(OZDSM$oocysts,30))^beta)/(delta + gamma * sort(sample(OZDSM$oocysts,30))^beta)
    pred3c<- (alpha * sort(sample(OZDSM$oocysts,30))^beta)/(delta + gamma * sort(sample(OZDSM$oocysts,30))^beta)
    pred3d<- (alpha * sort(sample(OZDSM$oocysts,30))^beta)/(delta + gamma * sort(sample(OZDSM$oocysts,30))^beta)
    pred3e<- (alpha * sort(sample(OZDSM$oocysts,30))^beta)/(delta + gamma * sort(sample(OZDSM$oocysts,30))^beta)
  
  pred4a<- (alpha * sort(sample(OZPIP$oocysts,30))^beta)/(delta + gamma * sort(sample(OZPIP$oocysts,30))^beta)
    pred4b<- (alpha * sort(sample(OZPIP$oocysts,30))^beta)/(delta + gamma * sort(sample(OZPIP$oocysts,30))^beta)
    pred4c<- (alpha * sort(sample(OZPIP$oocysts,30))^beta)/(delta + gamma * sort(sample(OZPIP$oocysts,30))^beta)
    pred4d<- (alpha * sort(sample(OZPIP$oocysts,30))^beta)/(delta + gamma * sort(sample(OZPIP$oocysts,30))^beta)
    pred4e<- (alpha * sort(sample(OZPIP$oocysts,30))^beta)/(delta + gamma * sort(sample(OZPIP$oocysts,30))^beta)
  
  pred5a<- (alpha * sort(sample(UCT$oocysts,30))^beta)/(delta + gamma * sort(sample(UCT$oocysts,30))^beta)
    pred5b<- (alpha * sort(sample(UCT$oocysts,30))^beta)/(delta + gamma * sort(sample(UCT$oocysts,30))^beta)
    pred5c<- (alpha * sort(sample(UCT$oocysts,30))^beta)/(delta + gamma * sort(sample(UCT$oocysts,30))^beta)
    pred5d<- (alpha * sort(sample(UCT$oocysts,30))^beta)/(delta + gamma * sort(sample(UCT$oocysts,30))^beta)
    pred5e<- (alpha * sort(sample(UCT$oocysts,30))^beta)/(delta + gamma * sort(sample(UCT$oocysts,30))^beta)
  
  spors1<-score
  spors2<-FERscore
  spors3<-DSMscore
  spors4<-PIPscore
  spors5<-UCTscore
  
  loglik1a<- spors1* log((pred1a)+0.00001)+(1-spors1)*log(1-((pred1a)-0.00001))
    loglik1b<- spors1* log((pred1b)+0.00001)+(1-spors1)*log(1-((pred1b)-0.00001))
    loglik1c<- spors1* log((pred1c)+0.00001)+(1-spors1)*log(1-((pred1c)-0.00001))
    loglik1d<- spors1* log((pred1d)+0.00001)+(1-spors1)*log(1-((pred1d)-0.00001))
    loglik1e<- spors1* log((pred1e)+0.00001)+(1-spors1)*log(1-((pred1e)-0.00001))
  
  loglik2a<- spors2* log((pred2a)+0.00001)+(1-spors2)*log(1-((pred2a)-0.00001))
    loglik2b<- spors2* log((pred2b)+0.00001)+(1-spors2)*log(1-((pred2b)-0.00001))
    loglik2c<- spors2* log((pred2c)+0.00001)+(1-spors2)*log(1-((pred2c)-0.00001))
    loglik2d<- spors2* log((pred2d)+0.00001)+(1-spors2)*log(1-((pred2d)-0.00001))
    loglik2e<- spors2* log((pred2e)+0.00001)+(1-spors2)*log(1-((pred2e)-0.00001))
  
  loglik3a<- spors3* log((pred3a)+0.00001)+(1-spors3)*log(1-((pred3a)-0.00001))
    loglik3b<- spors3* log((pred3b)+0.00001)+(1-spors3)*log(1-((pred3b)-0.00001))
    loglik3c<- spors3* log((pred3c)+0.00001)+(1-spors3)*log(1-((pred3c)-0.00001))
    loglik3d<- spors3* log((pred3d)+0.00001)+(1-spors3)*log(1-((pred3d)-0.00001))
    loglik3e<- spors3* log((pred3e)+0.00001)+(1-spors3)*log(1-((pred3e)-0.00001))
  
  loglik4a<- spors4* log((pred4a)+0.00001)+(1-spors4)*log(1-((pred4a)-0.00001))
    loglik4b<- spors4* log((pred4b)+0.00001)+(1-spors4)*log(1-((pred4b)-0.00001))
    loglik4c<- spors4* log((pred4c)+0.00001)+(1-spors4)*log(1-((pred4c)-0.00001))
    loglik4d<- spors4* log((pred4d)+0.00001)+(1-spors4)*log(1-((pred4d)-0.00001))
    loglik4e<- spors4* log((pred4e)+0.00001)+(1-spors4)*log(1-((pred4e)-0.00001))
  
  loglik5a<- spors5* log((pred5a)+0.00001)+(1-spors5)*log(1-((pred5a)-0.00001))
    loglik5b<- spors5* log((pred5b)+0.00001)+(1-spors5)*log(1-((pred5b)-0.00001))
    loglik5c<- spors5* log((pred5c)+0.00001)+(1-spors5)*log(1-((pred5c)-0.00001))
    loglik5d<- spors5* log((pred5d)+0.00001)+(1-spors5)*log(1-((pred5d)-0.00001))
    loglik5e<- spors5* log((pred5e)+0.00001)+(1-spors5)*log(1-((pred5e)-0.00001))
  
  -sum(
      loglik1a,loglik1b,loglik1c,loglik1d,loglik1e,
       loglik2a,loglik2b,loglik2c,loglik2d,loglik2e,
       loglik3a,loglik3b,loglik3c,loglik3d,loglik3e,
       loglik4a,loglik4b,loglik4c,loglik4d,loglik4e,
       loglik5a,loglik5b,loglik5c,loglik5d,loglik5e,
       na.rm=T)
}
n.param<-4

nc<-seq(0,max(blanks$oocysts),1)
satmod<-optim(c(3,0.9999,35,0.9),sat.binom,method="L-BFGS-B",lower=c(1,0.4,15,0.4),upper=c(4,0.999999,100,0.9999))
satmod
predCON<-(satmod$par[1] * nc^satmod$par[2])/(satmod$par[3] + satmod$par[4] * nc^satmod$par[2])

ncFER<-seq(0,max(OZFER$oocysts),1)
satmodFER<-optim(c(3,0.9999,15,0.9),sat.binom,method="L-BFGS-B",lower=c(1,0.4,1,0.4),upper=c(3.5,0.999999,100,0.9999))
satmodFER
predFER<-(satmodFER$par[1] * ncFER^satmodFER$par[2])/(satmodFER$par[3] + satmodFER$par[4] * ncFER^satmodFER$par[2])

ncDSM<-seq(0,max(OZDSM$oocysts),1)
satmodDSM<-optim(c(2,0.9999,15,0.9),sat.binom,method="L-BFGS-B",lower=c(1,0.4,10,0.4),upper=c(3.5,0.999999,100,0.9999))
satmodDSM
predDSM<-(satmodDSM$par[1] * ncDSM^satmodDSM$par[2])/(satmodDSM$par[3] + satmodDSM$par[4] * ncDSM^satmodDSM$par[2])

ncPIP<-seq(0,max(OZPIP$oocysts),1)
satmodPIP<-optim(c(3,0.9999,15,0.9),sat.binom,method="L-BFGS-B",lower=c(1,0.4,15,0.9),upper=c(3.5,0.999999,100,0.9999))
satmodPIP
predPIP<-(satmodPIP$par[1] * ncPIP^satmodPIP$par[2])/(satmodPIP$par[3] + satmodPIP$par[4] * ncPIP^satmodPIP$par[2])

ncUCT<-seq(0,max(UCT$oocysts),1)
satmodUCT<-optim(c(3.5,0.9999,25,0.9),sat.binom,method="L-BFGS-B",lower=c(1,0.4,15,0.9),upper=c(2.2,0.999999,100,0.9999))
satmodUCT
predUCT<-(satmodUCT$par[1] * ncUCT^satmodUCT$par[2])/(satmodUCT$par[3] + satmodUCT$par[4] * ncUCT^satmodUCT$par[2])


plot(sort(sample(blanks$oocysts,30)),score,ylim=c(0,5),bty="n",xlim=c(0,300),
     las=1,xlab="Oocysts",ylab="Sporozoites",cex=1.25,col="chartreuse4",pch=16)
points(sort(sample(blanks$oocysts,30)),score,col="chartreuse4",pch=16)
points(sort(sample(OZFER$oocysts,30)),FERscore,col="red",pch=20);
points(sort(sample(OZDSM$oocysts,30)),DSMscore,col="blue",pch=20);
points(sort(sample(OZPIP$oocysts,30)),PIPscore,col="blueviolet",pch=20);
points(sort(sample(UCT$oocysts,30)),UCTscore,col="orange",pch=20)

ncFER<-seq(1,max(OZFER$oocysts),3.36)## 101/30
ncDSM<-seq(1,max(OZDSM$oocysts),6.26)## 188/30
ncPIP<-seq(1,max(OZPIP$oocysts),6.53)## 196/30
ncUCT<-seq(1,max(UCT$oocysts),5.93)## 178/30
  
  predCON<-(2.8885427 * nc^0.9665085)/(33.8804145 + 0.8720424 * nc^0.9665085)
  predFER<-(2.9633314 * ncFER^0.9753062)/(16.1740491 + 0.9119043 * ncFER^0.9753062)
  predDSM<-(1.9888327 * ncDSM^0.9999011)/(15.9492211 + 0.9011156 * ncDSM^0.9999011)
  predPIP<-(2.9875322 * ncPIP^0.9962143)/(15.5401245 + 0.9000538 * ncPIP^0.9962143)
  predUCT<-(2.2 * ncUCT^0.9999)/(24.9996919 + 0.9000031 * ncUCT^0.9999)


lines(nc,predCON,lwd=2,col="chartreuse4")
    lines(ncFER,predFER,lwd=2,col="red")
        lines(ncDSM,predDSM,lwd=2,col="blue")
            lines(ncPIP,predPIP,lwd=2,col="blueviolet")
                lines(ncUCT,predUCT,lwd=2,col="orange")


####********Including the data from TC's work*********#######

#################################################
##
###
#### Sporozoites to BS infection (BLANKS)
###
##
#################################################

############################################
##
###
#### Create distributions of the data
###
##
#################################################


blanksdistprev<-c(rnorm(propBLANK1[1], mean = prevBLANK[1], sd = (prevBLANK[2]-prevBLANK[1])/2),
                  rnorm(propBLANK1[2], mean = prevBLANK[2], sd = (prevBLANK[3]-prevBLANK[2])/2),
                  rnorm(propBLANK1[3], mean = prevBLANK[3], sd = (prevBLANK[4]-prevBLANK[3])/2),
                  rnorm(propBLANK1[4], mean = prevBLANK[4], sd = (prevBLANK[5]-prevBLANK[4])/2),
                  rnorm(propBLANK1[5], mean = prevBLANK[5], sd = (prevBLANK[6]-prevBLANK[5])/2))

FERdistprev<-c(rnorm(propFER1[1], mean = prevFER[1], sd = (prevFER[2]-prevFER[1])/2),
               rnorm(propFER1[2], mean = prevFER[2], sd = (prevFER[3]-prevFER[2])/2),
               rnorm(propFER1[3], mean = prevFER[3], sd = (prevFER[4]-prevFER[3])/2),
               rnorm(propFER1[4], mean = prevFER[4], sd = (prevFER[5]-prevFER[4])/2),
               rnorm(propFER1[5], mean = prevFER[5], sd = (prevFER[6]-prevFER[5])/2))

DSMdistprev<-c(rnorm(propDSM1[1], mean = prevDSM[1], sd = (prevDSM[2]-prevDSM[1])/2),
               rnorm(propDSM1[2], mean = prevDSM[2], sd = (prevDSM[3]-prevDSM[2])/2),
               rnorm(propDSM1[3], mean = prevDSM[3], sd = (prevDSM[4]-prevDSM[3])/2),
               rnorm(propDSM1[4], mean = prevDSM[4], sd = (prevDSM[5]-prevDSM[4])/2),
               rnorm(propDSM1[5], mean = prevDSM[5], sd = (prevDSM[6]-prevDSM[5])/2))

PIPdistprev<-c(rnorm(propPIP1[1], mean = prevPIP[1], sd = (prevPIP[2]-prevPIP[1])/2),
               rnorm(propPIP1[2], mean = prevPIP[2], sd = (prevPIP[3]-prevPIP[2])/2),
               rnorm(propPIP1[3], mean = prevPIP[3], sd = (prevPIP[4]-prevPIP[3])/2),
               rnorm(propPIP1[4], mean = prevPIP[4], sd = (prevPIP[5]-prevPIP[4])/2),
               rnorm(propPIP1[5], mean = prevPIP[5], sd = (prevPIP[6]-prevPIP[5])/2))

UCTdistprev<-c(rnorm(propUCT1[1], mean = prevUCT[1], sd = (prevUCT[2]-prevUCT[1])/2),
               rnorm(propUCT1[2], mean = prevUCT[2], sd = (prevUCT[3]-prevUCT[2])/2),
               rnorm(propUCT1[3], mean = prevUCT[3], sd = (prevUCT[4]-prevUCT[3])/2),
               rnorm(propUCT1[4], mean = prevUCT[4], sd = (prevUCT[5]-prevUCT[4])/2),
               rnorm(propUCT1[5], mean = prevUCT[5], sd = (prevUCT[6]-prevUCT[5])/2))


prev1temp<-c(0,0,0,0,sort(blanksdistprev[1:23])) ##match up the number of zeros in score because 0 sporozoites cannot cause blood infection
prev1<-ifelse(prev1temp<=1,prev1temp,1)      
prevFER1<-c(0,0,0,0,0,0,sort(FERdistprev[2:25]))
prevDSM1<-c(0,0,0,0,sort(DSMdistprev[3:28]))
prevPIP1<-c(0,0,0,sort(PIPdistprev[1:27]))
prevUCT1<-c(0,0,0,0,0,0,sort(UCTdistprev[1:29]))


sort(score)
#
## Logistic fit
#

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * sort(score)[1:27])) / (1 + exp(a + b * sort(score)[1:27])) ) + 0
        predFER<- ((exp(a + b * sort(FERscore))) / (1 + exp(a + b * sort(FERscore))) )  
              predDSM<- ((exp(a + b * sort(DSMscore))) / (1 + exp(a + b * sort(DSMscore))) )  
                    predPIP<-  ((exp(a + b * sort(PIPscore))) / (1 + exp(a + b * sort(PIPscore))) )  
                          predUCT<-  ((exp(a + b * c(sort(UCTscore),rep(max(UCTscore),5)))) / (1 + exp(a + b * c(sort(UCTscore),rep(max(UCTscore),5)))) )  
  
  prev1<-prev1   
        prevFER1<-prevFER1
              prevDSM1<-prevDSM1
                    prevPIP1<-prevPIP1
                          prevUCT1<-prevUCT1
  
  loglik1a<- prev1* log((pred1a)+0.0000001)+(1-prev1)*log(1-((pred1a)-0.0000001))
         loglikf<- prevFER1* log((predFER)+0.00001)+(1-prevFER1)*log(1-((predFER)-0.00001))
              loglikd<- prevDSM1* log((predDSM)+0.00001)+(1-prevDSM1)*log(1-((predDSM)-0.00001))
                    loglikp<- prevPIP1* log((predPIP)+0.00001)+(1-prevPIP1)*log(1-((predPIP)-0.00001))
                          logliku<- prevUCT1* log((predUCT)+0.00001)+(1-prevUCT1)*log(1-((predUCT)-0.00001))
  
  
  -sum(
    loglik1a,
          loglikf,
                loglikd,
                      loglikp,
                            logliku,
    na.rm=T)
}


n.param<-2

nc<-seq(0,4,0.01)
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmod
pred<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) ) 
pred[1]


logmodfer<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmodfer
predfer<-(exp(logmodfer$par[1] + logmodfer$par[2] * nc)) / (1 + exp(logmodfer$par[1] + logmodfer$par[2] * nc))

logmoddsm<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-100,-100),upper=c(-4,100))
logmoddsm
preddsm<-(exp(logmoddsm$par[1] + logmoddsm$par[2] * nc)) / (1 + exp(logmoddsm$par[1] + logmoddsm$par[2] * nc))

logmodpip<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,0),upper=c(10,10))
logmodpip
predpip<-(exp(logmodpip$par[1] + logmodpip$par[2] * nc)) / (1 + exp(logmodpip$par[1] + logmodpip$par[2] * nc))

logmoduct<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmoduct
preduct<-(exp(logmoduct$par[1] + logmoduct$par[2] * nc)) / (1 + exp(logmoduct$par[1] + logmoduct$par[2] * nc))
preduct[1]


plot(sort(score)[1:27],prev1,ylim=c(0,1),bty="n",xlim=c(0,4),las=1,xlab="Sporozoite Score",ylab="Prevalence blood stage infection",cex=1.25,col="chartreuse4",pch=16)

points(sort(FERscore),sort(prevFER1),pch=20,col="red")
points(sort(DSMscore),sort(prevDSM1),pch=20,col="blue")
points(sort(PIPscore),sort(prevPIP1),pch=20,col="blueviolet")
points(c(sort(UCTscore),rep(max(UCTscore),5)),sort(prevUCT1),pch=20,col="orange")


      PREDcon<-(exp(-6.966544 + 6.401925 * nc)) / (1 + exp(-6.966544 + 6.401925 * nc))
            PREDfer<-(exp(-2.607061 + 2.242994 * nc)) / (1 + exp(-2.607061 + 2.242994 * nc))
                  PREDdsm<-(exp(-4 + 4.3438 * nc)) / (1 + exp(-4 + 4.3438 * nc))
                        PREDpip<-(exp(-4 + 4.062189 * nc)) / (1 + exp(-4 + 4.062189 * nc))
                              PREDuct<-(exp(-1.921384 + 2.536381 * nc)) / (1 + exp(-1.921384 + 2.536381 * nc))

lines(nc,pred,lwd=2)

lines(nc,predfer,lwd=2,col="red")
lines(nc,preddsm,lwd=2,col="blue")
lines(nc,predpip,lwd=2,col="blueviolet")
lines(nc,preduct,lwd=2,col="orange")

########################################################################
##
###
####
#####  Ooocysts to Blood stage infection
####
###
##
#######################################################################


##BLANKS
a=-6.966544
b=6.401925
alpha=2.8885427
beta=0.9665085
delta=33.8804145
gamma=0.9665085


X1<-max(blanks$oocysts)

BLANKfitooc <- ((exp(a + b * ((alpha * seq(0,X1,1)^beta)/(delta + gamma * seq(0,X1,1)^beta)))) / 
                  (1 + exp(a + b * ((alpha * seq(0,X1,1)^beta)/(delta + gamma * seq(0,X1,1)^beta)))) )

plot(seq(0,X1,1),BLANKfitooc,col="aquamarine4")

##FER
a=-2.607061
b=2.242994
alpha=2.9633314
beta=0.9753062
delta=16.1740491
gamma=0.9119043


X1<-max(OZFER$oocysts)

FERfitooc <- ((exp(a + b * ((alpha * seq(0,X1,1)^beta)/(delta + gamma * seq(0,X1,1)^beta)))) / 
                  (1 + exp(a + b * ((alpha * seq(0,X1,1)^beta)/(delta + gamma * seq(0,X1,1)^beta)))) )

points(seq(0,X1,1),FERfitooc,col="chocolate1")


predCON<-(2.8885427 * nc^0.9665085)/(33.8804145 + 0.8720424 * nc^0.9665085)
predFER<-(2.9633314 * ncFER^0.9753062)/(16.1740491 + 0.9119043 * ncFER^0.9753062)



PREDcon<-(exp(-6.966544 + 6.401925 * nc)) / (1 + exp(-6.966544 + 6.401925 * nc))
PREDfer<-(exp(-2.607061 + 2.242994 * nc)) / (1 + exp(-2.607061 + 2.242994 * nc))



















## Logistic fit
#

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1<- ((exp(a + b * logScore[1:6])) / (1 + exp(a + b * logScore[1:6])) )  
  pred2<- ((exp(a + b * logScoreTC[1:6])) / (1 + exp(a + b * logScoreTC[1:6])) )  
  #pred3<- ((exp(a + b * data.mouse.a$ScorePerBite[data.mouse.a$rm.prev > 0])) / (1 + exp(a + b * data.mouse.a$ScorePerBite[data.mouse.a$rm.prev > 0])) )  
  predFER<- ((exp(a + b * logScoreFER[1:6])) / (1 + exp(a + b * logScoreFER[1:6])) )  
  predDSM<- ((exp(a + b * c(logScoreDSM[1:4],2.075,2.075))) / (1 + exp(a + b * c(logScoreDSM[1:4],2.075,2.075))) )  
  predPIP<- ((exp(a + b * logScorePIP[1:6])) / (1 + exp(a + b * logScorePIP[1:6])) )  
  predUCT<- ((exp(a + b * c(logScoreUCT[1:4],2.04,2.04))) / (1 + exp(a + b * c(logScoreUCT[1:4],2.04,2.04))) )  
  
  
  prev1<-prevBLANK[1:6]
  prev2<-prevMouseDataTC[1:6]
  #prev3<-data.mouse.a$meanPar[data.mouse.a$rm.prev > 0]
  prevFER1<-prevFER
  prevDSM1<-prevDSM
  prevPIP1<-prevPIP
  prevUCT1<-prevUCT
  
  loglik1<- prev1* log((pred1)+0.00001)+(1-prev1)*log(1-((pred1)-0.00001))
  loglik2<- prev2* log((pred2)+0.00001)+(1-prev2)*log(1-((pred2)-0.00001))
  #loglik3<- prev3* log((pred3)+0.00001)+(1-prev3)*log(1-((pred3)-0.00001))
  loglikf<- prevFER1* log((predFER)+0.00001)+(1-prevFER1)*log(1-((predFER)-0.00001))
  loglikd<- prevDSM1* log((predDSM)+0.00001)+(1-prevDSM1)*log(1-((predDSM)-0.00001))
  loglikp<- prevPIP1* log((predPIP)+0.00001)+(1-prevPIP1)*log(1-((predPIP)-0.00001))
  logliku<- prevUCT1* log((predUCT)+0.00001)+(1-prevUCT1)*log(1-((predUCT)-0.00001))
  
  
  -sum(
    loglik1,loglik2,
       #loglik3,
       loglikf,
       loglikd,
       loglikp,
       logliku,
       na.rm=T)
}
nc<-seq(0,4,0.01)

n.param<-2


logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,0),upper=c(0,10))
logmod
pred<-(exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc))

logmodfer<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,0),upper=c(0,10))
logmodfer
predfer<-(exp(logmodfer$par[1] + logmodfer$par[2] * nc)) / (1 + exp(logmodfer$par[1] + logmodfer$par[2] * nc))

logmoddsm<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-100,-100),upper=c(100,100))
logmoddsm
preddsm<-(exp(logmoddsm$par[1] + logmoddsm$par[2] * nc)) / (1 + exp(logmoddsm$par[1] + logmoddsm$par[2] * nc))

logmodpip<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,0),upper=c(10,10))
logmodpip
predpip<-(exp(logmodpip$par[1] + logmodpip$par[2] * nc)) / (1 + exp(logmodpip$par[1] + logmodpip$par[2] * nc))

logmoduct<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmoduct
preduct<-(exp(logmoduct$par[1] + logmoduct$par[2] * nc)) / (1 + exp(logmoduct$par[1] + logmoduct$par[2] * nc))



plot(logScore[1:5],prevBLANK,ylim=c(0,1),bty="n",xlim=c(0,4),las=1,xlab="Sporozoite Score",ylab="Prevalence blood stage infection",cex=1.25,col="chartreuse4",pch=16)
points(logScoreTC,prevMouseDataTC)
#points(data.mouse.a$ScorePerBite[data.mouse.a$rm.prev > 0],data.mouse.a$meanPar[data.mouse.a$rm.prev > 0],pch=20)

points(logScoreFER[1:6],prevFER,pch=20,col="blue")
points(c(logScoreDSM[1:4],2.075,2.075),prevDSM,pch=20,col="red")
points(logScorePIP[1:6],prevPIP,pch=20,col="green")
points(c(logScoreUCT[1:4],2.04,2.04),prevUCT,pch=20,col="orange")


lines(nc,pred,lwd=2)

lines(nc,predfer,lwd=2,col="red")
lines(nc,preddsm,lwd=2,col="blue")
lines(nc,predpip,lwd=2,col="green")
lines(nc,preduct,lwd=2,col="orange")


log.binomL<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1L<- ((exp(a + b * logScoreL[1:6])) / (1 + exp(a + b * logScoreL[1:6])) )  
  pred2L<- ((exp(a + b * logScoreTCL[1:6])) / (1 + exp(a + b * logScoreTCL[1:6])) )  
  #pred3L<- ((exp(a + b * data.mouse.a$ScorePerBite[data.mouse.a$rm.prev > 0])) / (1 + exp(a + b * data.mouse.a$ScorePerBite[data.mouse.a$rm.prev > 0])) )  
  
  prev1L<-prevMouseData[1:6]
  prev2L<-prevMouseDataTC[1:6]
  #prev3L<-data.mouse.a$meanPar[data.mouse.a$rm.prev > 0]
  
  loglik1L<- prev1L* log((pred1L)+0.00001)+(1-prev1L)*log(1-((pred1L)-0.00001))
  loglik2L<- prev2L* log((pred2L)+0.00001)+(1-prev2L)*log(1-((pred2L)-0.00001))
  #loglik3L<- prev3L* log((pred3L)+0.00001)+(1-prev3L)*log(1-((pred3L)-0.00001))
  
  -sum(loglik1L,loglik2L,na.rm=T)
}
n.param<-2
logmodL<-optim(c(0,0),log.binomL,method="L-BFGS-B",lower=c(-10,0),upper=c(0,10))
logmodL

nc<-seq(0,4,0.01)
predL<-(exp(logmodL$par[1] + logmodL$par[2] * nc)) / (1 + exp(logmodL$par[1] + logmodL$par[2] * nc))
lines(nc,predL,lwd=2,lty=2)

log.binomU<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1U<- ((exp(a + b * logScoreU[1:6])) / (1 + exp(a + b * logScoreU[1:6])) )  
  pred2U<- ((exp(a + b * logScoreTCU[1:6])) / (1 + exp(a + b * logScoreTCU[1:6])) )  
  #pred3U<- ((exp(a + b * data.mouse.a$ScorePerBite[data.mouse.a$rm.prev > 0])) / (1 + exp(a + b * data.mouse.a$ScorePerBite[data.mouse.a$rm.prev > 0])) )  
  
  prev1U<-prevMouseData[1:6]
  prev2U<-prevMouseDataTC[1:6]
  #prev3U<-data.mouse.a$meanPar[data.mouse.a$rm.prev > 0]
  
  loglik1U<- prev1U* log((pred1U)+0.00001)+(1-prev1U)*log(1-((pred1U)-0.00001))
  loglik2U<- prev2U* log((pred2U)+0.00001)+(1-prev2U)*log(1-((pred2U)-0.00001))
  #loglik3U<- prev3U* log((pred3U)+0.00001)+(1-prev3U)*log(1-((pred3U)-0.00001))
  
  -sum(loglik1U,loglik2U,na.rm=T)
}
n.param<-2
logmodU<-optim(c(0,0),log.binomU,method="L-BFGS-B",lower=c(-10,0),upper=c(0,10))
logmodU

nc<-seq(0,4,0.01)
predU<-(exp(logmodU$par[1] + logmodU$par[2] * nc)) / (1 + exp(logmodU$par[1] + logmodU$par[2] * nc))
lines(nc,predU,lwd=2,lty=2)


#######################################
##
###
#### OOCYSTS TO BLOODSTAGE PREVALENCE
###
##
########################################


ooc.binom<-function(p.vec){
  
  alpha <- p.vec[1]
  beta  <- p.vec[2]
  delta <- p.vec[3]
  gamma <- p.vec[4]
  a <- p.vec[5]
  b <- p.vec[6]
  
  #a<-p.vec[1]
  #b<-p.vec[2]
  fitooc1 <- ((exp(a + b * ((alpha * ooc[1:6]^beta)/(delta + gamma * ooc[1:6]^beta)))) / (1 + exp(a + b * ((alpha * ooc[1:6]^beta)/(delta + gamma * ooc[1:6]^beta)))) )
  #fitooc2<-((exp(a + b * ooc[1:6])) / (1 + exp(a + b * ooc[1:6])) ) 
  fitooc2 <- ((exp(a + b * ((alpha * oocFER[1:6]^beta)/(delta + gamma * oocFER[1:6]^beta)))) / (1 + exp(a + b * ((alpha * oocFER[1:6]^beta)/(delta + gamma * oocFER[1:6]^beta)))) )
  fitooc3 <- ((exp(a + b * ((alpha * oocDSM[1:6]^beta)/(delta + gamma * oocDSM[1:6]^beta)))) / (1 + exp(a + b * ((alpha * oocDSM[1:6]^beta)/(delta + gamma * oocDSM[1:6]^beta)))) )
  fitooc4 <- ((exp(a + b * ((alpha * oocPIP[1:6]^beta)/(delta + gamma * oocPIP[1:6]^beta)))) / (1 + exp(a + b * ((alpha * oocPIP[1:6]^beta)/(delta + gamma * oocPIP[1:6]^beta)))) )
  fitooc5 <- ((exp(a + b * ((alpha * oocUCT[1:6]^beta)/(delta + gamma * oocUCT[1:6]^beta)))) / (1 + exp(a + b * ((alpha * oocUCT[1:6]^beta)/(delta + gamma * oocUCT[1:6]^beta)))) )
  
    
  prev1<-prevMouseData[1:6]
  prevFER1<-prevFER
  prevDSM1<-prevDSM
  prevPIP1<-prevPIP
  prevUCT1<-prevUCT
  
  loglik1<- fitooc1* log((prev1)+0.001)+(1-fitooc1)*log(1-((prev1)-0.001))
  #loglik2<- fitooc2* log((prev1)+0.001)+(1-fitooc2)*log(1-((prev1)-0.001))
  loglik2<- fitooc2* log((prevFER1)+0.001)+(1-fitooc2)*log(1-((prevFER1)-0.001))
  loglik3<- fitooc3* log((prevDSM1)+0.001)+(1-fitooc3)*log(1-((prevDSM1)-0.001))
  loglik4<- fitooc4* log((prevPIP1)+0.001)+(1-fitooc4)*log(1-((prevPIP1)-0.001))
  loglik5<- fitooc5* log((prevUCT1)+0.001)+(1-fitooc5)*log(1-((prevUCT1)-0.001))
  
  -sum(
    loglik1,
    loglik2,
    loglik3,
    loglik4,
    loglik5,
    na.rm=T)
}
n.param<-6
oocmod<-optim(c(3.4999292,0.99999,23.6197325,0.9000661,-2.991983,2.603804),ooc.binom,method="L-BFGS-B", ###These are for the BLANK data
              lower=c(3.49,0.9,23,0.9,-3,2.6),
              upper=c(3.5,0.999999,26,0.92,-2.98,2.61))

oocmodFER<-optim(c(3.5,0.99999318,24.7097148,0.9,-1.423906,2.294481),ooc.binom,method="L-BFGS-B", ###These are for the FER data
              lower=c(3.4,0.9,24,0.9,-2,2.2),
              upper=c(3.6,0.9999999,26,0.92,-1,2.3))

oocmodDSM<-optim(c(3.4938832,0.9796118,15.0012052,0.9039049,-2.184231,3.322265),ooc.binom,method="L-BFGS-B", ###These are for the DSM data
              lower=c(3.4,0.9,14,0.9,-3,3.2),
              upper=c(3.8,0.98,16,0.92,-2,3.61))

oocmodPIP<-optim(c(3.4999387,0.999999,22.3078003,0.9000538,0.2597978,1.2328052),ooc.binom,method="L-BFGS-B", ###These are for the PIP data
              lower=c(3.4,0.9,22,0.9,0,1.2),
              upper=c(3.8,0.99999999,23,0.92,0.98,2))

oocmodUCT<-optim(c(3.4909695,0.9490303,25.0010236,0.9062446,-0.2114807,2.2849596),ooc.binom,method="L-BFGS-B", ###These are for the UCT data
              lower=c(3.4,0.9,24,0.9,-1,2.2),
              upper=c(3.8,0.95,26,0.92,0,2.3))
#n.param<-2
#oocmod2<-optim(c(-4.5,0.185),ooc.binom,method="L-BFGS-B",lower=c(-2.5,0.1),upper=c(0,1))

oocmod

fitdat<-seq(0,max(oocdata1),1)
pred<-((exp(oocmod$par[5] + oocmod$par[6] * ((oocmod$par[1] * fitdat^oocmod$par[2])/(oocmod$par[3] + oocmod$par[4] * fitdat^oocmod$par[2])))) / 
         (1 + exp(oocmod$par[5] + oocmod$par[6] * ((oocmod$par[1] * fitdat^oocmod$par[2])/(oocmod$par[3] + oocmod$par[4] * fitdat^oocmod$par[2])))) )

predFERc<-((exp(oocmodFER$par[5] + oocmodFER$par[6] * ((oocmodFER$par[1] * fitdat^oocmodFER$par[2])/(oocmodFER$par[3] + oocmodFER$par[4] * fitdat^oocmodFER$par[2])))) / 
         (1 + exp(oocmodFER$par[5] + oocmodFER$par[6] * ((oocmodFER$par[1] * fitdat^oocmodFER$par[2])/(oocmodFER$par[3] + oocmodFER$par[4] * fitdat^oocmodFER$par[2])))) )

predDSMc<-((exp(oocmodDSM$par[5] + oocmodDSM$par[6] * ((oocmodDSM$par[1] * fitdat^oocmodDSM$par[2])/(oocmodDSM$par[3] + oocmodDSM$par[4] * fitdat^oocmodDSM$par[2])))) / 
             (1 + exp(oocmodDSM$par[5] + oocmodDSM$par[6] * ((oocmodDSM$par[1] * fitdat^oocmodDSM$par[2])/(oocmodDSM$par[3] + oocmodDSM$par[4] * fitdat^oocmodDSM$par[2])))) )

predPIPc<-((exp(oocmodPIP$par[5] + oocmodPIP$par[6] * ((oocmodPIP$par[1] * fitdat^oocmodPIP$par[2])/(oocmodPIP$par[3] + oocmodPIP$par[4] * fitdat^oocmodPIP$par[2])))) / 
             (1 + exp(oocmodPIP$par[5] + oocmodPIP$par[6] * ((oocmodPIP$par[1] * fitdat^oocmodPIP$par[2])/(oocmodPIP$par[3] + oocmodPIP$par[4] * fitdat^oocmodPIP$par[2])))) )

predUCTc<-((exp(oocmodUCT$par[5] + oocmodUCT$par[6] * ((oocmodUCT$par[1] * fitdat^oocmodUCT$par[2])/(oocmodUCT$par[3] + oocmodUCT$par[4] * fitdat^oocmodUCT$par[2])))) / 
             (1 + exp(oocmodUCT$par[5] + oocmodUCT$par[6] * ((oocmodUCT$par[1] * fitdat^oocmodUCT$par[2])/(oocmodUCT$par[3] + oocmodUCT$par[4] * fitdat^oocmodUCT$par[2])))) )

par(mfrow=c(1,1))
plot(ooc[1:6],prevMouseData,ylim=c(0,1),bty="n",xlim=c(0,200),las=1,xlab="Oocyst intensity",ylab="Prevalence blood stage infection",cex=1.25,col="chartreuse4",pch=16)
lines(fitdat,pred,lwd=2)

lines(fitdat,predFERc,lwd=2,col="red")
lines(fitdat,predDSMc,lwd=2,col="blue")
lines(fitdat,predPIPc,lwd=2,col="green")
lines(fitdat,predUCTc,lwd=2,col="orange")


############################################################################
##
###
####
##### Create data with the proportion of cases at each level... 
####
###
###
################################################################################

##e.g. for blank
##fewer sporozoites cause infection...
##for example, after logScore = 2 then all sporozoites cause infection. 



sort(blanks$oocysts)
predblank2<-numeric(length(blanks$oocysts))
for (i in 1:length(blanks$oocysts))
{
  predblank2[i]<-pred[blanks$oocysts][i]
}
plot(sort(predblank2))
lines(sort(predblank2))

sort(OZFER$oocysts)
OZFER2<-numeric(length(OZFER$oocysts))
for (i in 1:length(OZFER$oocysts))
{
  OZFER2[i]<-predFERc[OZFER$oocysts][i]
}
lines(sort(OZFER2),col="red")
