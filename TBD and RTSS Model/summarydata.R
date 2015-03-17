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
mean(blanks$oocysts)
blanks$oocprev<-ifelse(blanks$oocysts==0,0,1)
sum(blanks$oocprev)/length(blanks$oocprev)

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
OZFER$oocprevfer<-ifelse(OZFER$oocysts==0,0,1)
sum(OZFER$oocprevfer)/length(OZFER$oocprevfer)
mean(OZFER$oocysts)
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
OZDSM$oocprevdsm<-ifelse(OZDSM$oocysts==0,0,1)
sum(OZDSM$oocprevdsm)/length(OZDSM$oocprevdsm)
mean(OZDSM$oocysts)
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
OZPIP$oocprevpip<-ifelse(OZPIP$oocysts==0,0,1)
sum(OZPIP$oocprevpip)/length(OZPIP$oocprevpip)
mean(OZPIP$oocysts)
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
UCT$oocprevuct<-ifelse(UCT$oocysts==0,0,1)
sum(UCT$oocprevuct)/length(UCT$oocprevuct)
mean(UCT$oocysts)
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


##### And the ATV25 data
####
###
#######

oocATV<-read.table("D:\\IMPERIAL Nov 2014\\Andrew Blagborough\\ATV-25 Example\\mosquito.txt",header=TRUE)

oocATV$oocprev<-ifelse(oocATV$Oocyst==0,0,1)

sum(oocATV$oocprev[oocATV$Round==3])/length(oocATV$oocprev[oocATV$Round==3])
oocATV1temp<-subset(oocATV,Round == 3);mean(oocATV1temp$Oocyst)
oocATV1<-sort(oocATV1temp$Oocyst[oocATV1temp$Oocyst > 0])
length(oocATV1)/5

ATV1<-c(0,mean(oocATV1[1:17]),mean(oocATV1[18:35]),mean(oocATV1[36:52]),mean(oocATV1[53:70]),mean(oocATV1[71:88]))

mean(oocATV$Oocyst[oocATV$Round==3]);sd(oocATV$Oocyst[oocATV$Round==3]);length(oocATV$Oocyst[oocATV$Round==3])
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

mean(spors$ScorePerBite[spors$Treatment == "Blank"])
sd(spors$ScorePerBite[spors$Treatment == "Blank"])
length(spors$ScorePerBite[spors$Treatment == "Blank"])

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

mean(spors$ScorePerBite[spors$Treatment == "OZFER"])
sd(spors$ScorePerBite[spors$Treatment == "OZFER"])
length(spors$ScorePerBite[spors$Treatment == "OZFER"])

mean(spors$ScorePerBite[spors$Treatment == "OZDSM"])
sd(spors$ScorePerBite[spors$Treatment == "OZDSM"])
length(spors$ScorePerBite[spors$Treatment == "OZDSM"])

mean(spors$ScorePerBite[spors$Treatment == "OZPIP"])
sd(spors$ScorePerBite[spors$Treatment == "OZPIP"])
length(spors$ScorePerBite[spors$Treatment == "OZPIP"])

mean(spors$ScorePerBite[spors$Treatment == "UCT"])
sd(spors$ScorePerBite[spors$Treatment == "UCT"])
length(spors$ScorePerBite[spors$Treatment == "UCT"])


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


###
## And for ATV25
##
####
spors2<-read.table("D:\\IMPERIAL Nov 2014\\Andrew Blagborough\\ATV-25 Example\\mouse.txt",header=TRUE)

head(spors2)
spors2$bsprev<-ifelse(spors2$Parasitemia == 0,0,1)

spors2$sporozoites<-numeric(length(spors2$Round))
for (i in 1:length(spors2$Round)){
  spors2$sporozoites[i]<-sum(spors2[i,6:15],na.rm=TRUE)}

spors2$ScorePerBite<-spors2$sporozoites/spors2$Bites

mean(spors2$ScorePerBite);sd(spors2$ScorePerBite);length(spors2$ScorePerBite);
score0<- spors2$ScorePerBite[spors2$ScorePerBite == 0]
score1<- spors2$ScorePerBite[spors2$ScorePerBite > 0 & spors2$ScorePerBite<1]
score2<- spors2$ScorePerBite[spors2$ScorePerBite >= 1 & spors2$ScorePerBite<2]
score3<- spors2$ScorePerBite[spors2$ScorePerBite >= 2 & spors2$ScorePerBite<3]
score4<- spors2$ScorePerBite[spors2$ScorePerBite >= 3 & spors2$ScorePerBite<4]
score5<- spors2$ScorePerBite[spors2$ScorePerBite >= 4]

ATVscore<-c(score0,score1,score2,score3,score4,score5)
logScoreATV<-c(mean(score0),mean(score1),mean(score2),mean(score3),mean(score4),max(score4))
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
logScoreLATV<-c(0,ao1[1],ao2[1],ao3[1],ao4[1],max(score4))
logScoreUATV<-c(0,ao1[2],ao2[2],ao3[2],ao4[2],max(score4))


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

sum(spors2$bsprev)/length(spors2$bsprev)

ATVbsprev0<-sum(spors2$bsprev[spors2$ScorePerBite==0 ])/length(spors2$bsprev[spors2$ScorePerBite==0 ])
ATVbsprev1<-sum(spors2$bsprev[spors2$ScorePerBite > 0 & spors2$ScorePerBite <= 1 ])/length(spors2$bsprev[spors2$ScorePerBite > 0 & spors2$ScorePerBite <= 1 ])
ATVbsprev2<-sum(spors2$bsprev[spors2$ScorePerBite > 1 & spors2$ScorePerBite <= 2 ])/length(spors2$bsprev[spors2$ScorePerBite > 1 & spors2$ScorePerBite <= 2 ])
ATVbsprev3<-sum(spors2$bsprev[spors2$ScorePerBite > 2 & spors2$ScorePerBite <= 3 ])/length(spors2$bsprev[spors2$ScorePerBite > 2 & spors2$ScorePerBite <= 3 ])
ATVbsprev4<-sum(spors2$bsprev[spors2$ScorePerBite > 3 & spors2$ScorePerBite <= 4 ])/length(spors2$bsprev[spors2$ScorePerBite > 3 & spors2$ScorePerBite <= 4 ])
ATVbsprev5<-sum(spors2$bsprev[spors2$ScorePerBite > 4 ])/length(spors2$bsprev[spors2$ScorePerBite > 4 ])
prevATV<-c(ATVbsprev0,ATVbsprev1,ATVbsprev2,ATVbsprev3,NA,NA)


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


#######
### And for ATV 25
####
ATVbspr0<-length(spors2$bsprev[spors2$ScorePerBite==0])
ATVbspr1<-length(spors2$bsprev[spors2$ScorePerBite > 0 & spors2$ScorePerBite <= 1])
ATVbspr2<-length(spors2$bsprev[spors2$ScorePerBite > 1 & spors2$ScorePerBite <= 2])
ATVbspr3<-length(spors2$bsprev[spors2$ScorePerBite > 2 & spors2$ScorePerBite <= 3])
ATVbspr4<-length(spors2$bsprev[spors2$ScorePerBite > 3 & spors2$ScorePerBite <= 4])
ATVbspr5<-length(spors2$bsprev[spors2$ScorePerBite > 4])
propATV1<-c(ATVbspr0,ATVbspr1,ATVbspr2,ATVbspr3,ATVbspr4,ATVbspr5)
propATV<-c(ATVbspr0,ATVbspr1,ATVbspr2,ATVbspr3,ATVbspr4,ATVbspr5)/length(spors2$bsprev)



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
  
  pred1a<- (alpha * sort(sample(oocdata1,30))^beta)/(delta + gamma * sort(sample(oocdata1,30))^beta)
    pred1b<- (alpha * sort(sample(oocdata1,30))^beta)/(delta + gamma * sort(sample(oocdata1,30))^beta)
    pred1c<- (alpha * sort(sample(oocdata1,30))^beta)/(delta + gamma * sort(sample(oocdata1,30))^beta)
    pred1d<- (alpha * sort(sample(oocdata1,30))^beta)/(delta + gamma * sort(sample(oocdata1,30))^beta)
    pred1e<- (alpha * sort(sample(oocdata1,30))^beta)/(delta + gamma * sort(sample(oocdata1,30))^beta)
  
  pred2a<- (alpha * sort(sample(oocdataD1FER,30))^beta)/(delta + gamma * sort(sample(oocdataD1FER,30))^beta)
    pred2b<- (alpha * sort(sample(oocdataD1FER,30))^beta)/(delta + gamma * sort(sample(oocdataD1FER,30))^beta)
    pred2c<- (alpha * sort(sample(oocdataD1FER,30))^beta)/(delta + gamma * sort(sample(oocdataD1FER,30))^beta)
    pred2d<- (alpha * sort(sample(oocdataD1FER,30))^beta)/(delta + gamma * sort(sample(oocdataD1FER,30))^beta)
    pred2e<- (alpha * sort(sample(oocdataD1FER,30))^beta)/(delta + gamma * sort(sample(oocdataD1FER,30))^beta)
  
  pred3a<- (alpha * sort(sample(oocdataD1DSM,30))^beta)/(delta + gamma * sort(sample(oocdataD1DSM,30))^beta)
    pred3b<- (alpha * sort(sample(oocdataD1DSM,30))^beta)/(delta + gamma * sort(sample(oocdataD1DSM,30))^beta)
    pred3c<- (alpha * sort(sample(oocdataD1DSM,30))^beta)/(delta + gamma * sort(sample(oocdataD1DSM,30))^beta)
    pred3d<- (alpha * sort(sample(oocdataD1DSM,30))^beta)/(delta + gamma * sort(sample(oocdataD1DSM,30))^beta)
    pred3e<- (alpha * sort(sample(oocdataD1DSM,30))^beta)/(delta + gamma * sort(sample(oocdataD1DSM,30))^beta)
  
  pred4a<- (alpha * sort(sample(oocdataD1PIP,30))^beta)/(delta + gamma * sort(sample(oocdataD1PIP,30))^beta)
    pred4b<- (alpha * sort(sample(oocdataD1PIP,30))^beta)/(delta + gamma * sort(sample(oocdataD1PIP,30))^beta)
    pred4c<- (alpha * sort(sample(oocdataD1PIP,30))^beta)/(delta + gamma * sort(sample(oocdataD1PIP,30))^beta)
    pred4d<- (alpha * sort(sample(oocdataD1PIP,30))^beta)/(delta + gamma * sort(sample(oocdataD1PIP,30))^beta)
    pred4e<- (alpha * sort(sample(oocdataD1PIP,30))^beta)/(delta + gamma * sort(sample(oocdataD1PIP,30))^beta)
  
  pred5a<- (alpha * sort(sample(oocdataD1UCT,30))^beta)/(delta + gamma * sort(sample(oocdataD1UCT,30))^beta)
    pred5b<- (alpha * sort(sample(oocdataD1UCT,30))^beta)/(delta + gamma * sort(sample(oocdataD1UCT,30))^beta)
    pred5c<- (alpha * sort(sample(oocdataD1UCT,30))^beta)/(delta + gamma * sort(sample(oocdataD1UCT,30))^beta)
    pred5d<- (alpha * sort(sample(oocdataD1UCT,30))^beta)/(delta + gamma * sort(sample(oocdataD1UCT,30))^beta)
    Pred5e<- (alpha * sort(sample(oocdataD1UCT,30))^beta)/(delta + gamma * sort(sample(oocdataD1UCT,30))^beta)
 
  #a<-p.vec[1]
  #b<-p.vec[2]
  #pred5a<-  ((exp(a + b * sort(sample(oocdataD1UCT,30)))) / (1 + exp(a + b * sort(sample(oocdataD1UCT,30)))) )  
  #pred5b<-  ((exp(a + b * sort(sample(oocdataD1UCT,30)))) / (1 + exp(a + b * sort(sample(oocdataD1UCT,30)))) )  
  #pred5c<-  ((exp(a + b * sort(sample(oocdataD1UCT,30)))) / (1 + exp(a + b * sort(sample(oocdataD1UCT,30)))) )  
  #pred5d<-  ((exp(a + b * sort(sample(oocdataD1UCT,30)))) / (1 + exp(a + b * sort(sample(oocdataD1UCT,30)))) )  
  #pred5e<-  ((exp(a + b * sort(sample(oocdataD1UCT,30)))) / (1 + exp(a + b * sort(sample(oocdataD1UCT,30)))) )  
  
  
  pred6a<- (alpha * sort(sample(oocATV1,80))^beta)/(delta + gamma * sort(sample(oocATV1,80))^beta)
  pred6b<- (alpha * sort(sample(oocATV1,80))^beta)/(delta + gamma * sort(sample(oocATV1,80))^beta)
  pred6c<- (alpha * sort(sample(oocATV1,80))^beta)/(delta + gamma * sort(sample(oocATV1,80))^beta)
  pred6d<- (alpha * sort(sample(oocATV1,80))^beta)/(delta + gamma * sort(sample(oocATV1,80))^beta)
  pred6e<- (alpha * sort(sample(oocATV1,80))^beta)/(delta + gamma * sort(sample(oocATV1,80))^beta)
  
  spors1<-score
  spors2<-FERscore
  spors3<-DSMscore
  spors4<-PIPscore
  spors5<-UCTscore
  spors6<-ATVscore
  
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

  loglik6a<- spors6* log((pred6a)+0.00001)+(1-spors6)*log(1-((pred6a)-0.00001))
    loglik6b<- spors6* log((pred6b)+0.00001)+(1-spors6)*log(1-((pred6b)-0.00001))
    loglik6c<- spors6* log((pred6c)+0.00001)+(1-spors6)*log(1-((pred6c)-0.00001))
    loglik6d<- spors6* log((pred6d)+0.00001)+(1-spors6)*log(1-((pred6d)-0.00001))
    loglik6e<- spors6* log((pred6e)+0.00001)+(1-spors6)*log(1-((pred6e)-0.00001))
  
  -sum(
      loglik1a,loglik1b,loglik1c,loglik1d,loglik1e,
       loglik2a,loglik2b,loglik2c,loglik2d,loglik2e,
       loglik3a,loglik3b,loglik3c,loglik3d,loglik3e,
       loglik4a,loglik4b,loglik4c,loglik4d,loglik4e,
       loglik5a,loglik5b,loglik5c,loglik5d,loglik5e,
       loglik6a,loglik6b,loglik6c,loglik6d,loglik6e,
       na.rm=T)
}
n.param<-4

nc<-seq(0,max(blanks$oocysts),1)
satmod<-optim(c(3,0.9999,40,0.9),sat.binom,method="L-BFGS-B",lower=c(2,0.4,15,0.4),upper=c(4,0.999999,100,0.9999))
satmod
predCON<-(satmod$par[1] * nc^satmod$par[2])/(satmod$par[3] + satmod$par[4] * nc^satmod$par[2])

ncFER<-seq(0,max(OZFER$oocysts),1)
satmodFER<-optim(c(3,0.9,22,0.9),sat.binom,method="L-BFGS-B",lower=c(1,0.4,15,0.8),upper=c(3.5,0.999999,100,0.9999))
satmodFER
predFER<-(satmodFER$par[1] * ncFER^satmodFER$par[2])/(satmodFER$par[3] + satmodFER$par[4] * ncFER^satmodFER$par[2])

ncDSM<-seq(0,max(OZDSM$oocysts),1)
satmodDSM<-optim(c(2,0.9999,18,0.9),sat.binom,method="L-BFGS-B",lower=c(1,0.4,10,0.4),upper=c(3.5,0.999999,100,0.9999))
satmodDSM
predDSM<-(satmodDSM$par[1] * ncDSM^satmodDSM$par[2])/(satmodDSM$par[3] + satmodDSM$par[4] * ncDSM^satmodDSM$par[2])

ncPIP<-seq(0,max(OZPIP$oocysts),1)
satmodPIP<-optim(c(3,0.9999,25,0.9),sat.binom,method="L-BFGS-B",lower=c(1,0.4,15,0.9),upper=c(3.5,0.999999,100,0.9999))
satmodPIP
predPIP<-(satmodPIP$par[1] * ncPIP^satmodPIP$par[2])/(satmodPIP$par[3] + satmodPIP$par[4] * ncPIP^satmodPIP$par[2])

ncUCT<-seq(0,max(UCT$oocysts),1)
satmodUCT<-optim(c(3.5,0.9999,30,0.9),sat.binom,method="L-BFGS-B",lower=c(1,0.4,15,0.9),upper=c(2.2,0.999999,100,0.9999))
satmodUCT
predUCT<-(satmodUCT$par[1] * ncUCT^satmodUCT$par[2])/(satmodUCT$par[3] + satmodUCT$par[4] * ncUCT^satmodUCT$par[2])

ncATV<-seq(0,max(oocATV1),1)
satmodATV<-optim(c(3,0.9999,40,0.9),sat.binom,method="L-BFGS-B",lower=c(2,0.4,15,0.4),upper=c(3.5,0.999999,100,0.9999))
satmodATV
predATV<-(satmodATV$par[1] * ncATV^satmodATV$par[2])/(satmodATV$par[3] + satmodATV$par[4] * ncATV^satmodATV$par[2])

plot(sort(sample(oocdata1,30)),score,ylim=c(0,5),bty="n",xlim=c(0,300),
     las=1,xlab="Oocysts",ylab="Sporozoites",cex=2,cex.lab=2,col="chartreuse4",pch=16)
points(sort(sample(oocdata1,30)),score,col="chartreuse4",pch=16)
points(sort(sample(oocdataD1FER,30)),FERscore,col="red",pch=20);
points(sort(sample(oocdataD1DSM,30)),DSMscore,col="blue",pch=20);
points(sort(sample(oocdataD1PIP,30)),PIPscore,col="blueviolet",pch=20);
points(sort(sample(oocdataD1UCT,30)),UCTscore,col="orange",pch=20)
points(sort(sample(oocATV1,80)),ATVscore,col="chocolate",pch=20,cex=2,cex.lab=2)

ncFER<-seq(0,max(oocdataD1FER))##
ncDSM<-seq(0,max(oocdataD1DSM))##
ncPIP<-seq(0,max(oocdataD1PIP))##
ncUCT<-seq(0,max(oocdataD1UCT))##
ncATV<-seq(0,max(oocATV1))##

  predCON<-(2.9693194 * nc^0.9721653)/(39.8925127 + 0.9270415 * nc^0.9721653)
  predFER<-(2.976640 * ncFER^0.901168)/(22.911030 + 0.898832 * ncFER^0.901168)
  predDSM<-(2 * ncDSM^0.9999)/(18 + 0.899999 * ncDSM^0.9999)
  predPIP<-(3.0057840 * ncPIP^0.9929603)/(25.8676069 + 0.9011557 * ncPIP^0.9929603)
  predUCT<-(2.2 * ncUCT^0.9928586)/(29.9303881 + 0.9400415 * ncUCT^0.9928586)
  predATV<-(3 * nc^0.9999)/(40 + 0.9 * nc^0.9999)


lines(nc,predCON,lwd=2,col="chartreuse4")
    lines(ncFER,predFER,lwd=2,col="red")
        lines(ncDSM,predDSM,lwd=2,col="blue")
            lines(ncPIP,predPIP,lwd=2,col="blueviolet")
                lines(ncUCT,predUCT,lwd=2,col="orange")
                      lines(nc,predATV,lwd=2,col="chocolate")
legend(150,4.5,legend=c("Control","ATV-25%"),lty=c(1,1),col=c("chartreuse4","chocolate"),cex=2)
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

ATVdistprev<-c(rnorm(propATV1[1], mean = prevATV[1], sd = (prevATV[2]-prevATV[1])/2),
               rnorm(propATV1[2], mean = prevATV[2], sd = (prevATV[3]-prevATV[2])/2),
               rnorm(propATV1[3], mean = prevATV[3], sd = (prevATV[4]-prevATV[3])/2),
               rnorm(propATV1[4], mean = prevATV[4], sd = (prevATV[5]-prevATV[4])/2),
               rnorm(propATV1[5], mean = prevATV[5], sd = (prevATV[6]-prevATV[5])/2))


  prev1temp<-c(0,0,0,0,sort(blanksdistprev[1:23])) ##match up the number of zeros in score because 0 sporozoites cannot cause blood infection
prev1<-ifelse(prev1temp<=1,prev1temp,1)  
  prevfertemp<-sort(FERdistprev)
  prevfertemp1<-prevfertemp[2:29]
prevFER1<-c(rep(0,6),prevfertemp1[1:6],prevfertemp1[8:13],prevfertemp1[15:21],prevfertemp1[23:26],prevfertemp1[27])
length(prevFER1)
  prevdsmtemp<-sort(DSMdistprev)
  prevdsmtemp1<-prevdsmtemp[2:28]
prevDSM1<-c(rep(0,4),prevdsmtemp1[2:27])
length(prevDSM1)
  prevpiptemp<-sort(PIPdistprev)
prevPIP1<-c(0,0,0,sort(PIPdistprev[1:27]))
prevUCT1<-c(0,0,0,0,0,0,sort(UCTdistprev[1:29]))
atvtemp<-sort(ATVdistprev[1:70])
atvtemp2<-c(atvtemp[1:4],atvtemp[7:11],atvtemp[14:18],atvtemp[21:24],atvtemp[27:30],atvtemp[33:36],
            atvtemp[39:42],atvtemp[45:48],atvtemp[51:53],atvtemp[57:60],atvtemp[63:66],atvtemp[68])
prevATV1<-c(rep(0,34),sort(atvtemp2))
length(prevATV1)

#
## Logistic fit
#

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * sort(score)[1:27])) / (1 + exp(a + b * sort(score)[1:27])) ) + 0
        predFER<- ((exp(a + b * FERscore)) / (1 + exp(a + b * FERscore)) )  
              predDSM<- ((exp(a + b * sort(DSMscore))) / (1 + exp(a + b * sort(DSMscore))) )  
                    predPIP<-  ((exp(a + b * sort(PIPscore))) / (1 + exp(a + b * sort(PIPscore))) )  
                          predUCT<-  ((exp(a + b * c(sort(UCTscore),rep(max(UCTscore),5)))) / (1 + exp(a + b * c(sort(UCTscore),rep(max(UCTscore),5)))) )  
                                predUCT<-  ((exp(a + b * sort(ATVscore))) / (1 + exp(a + b * sort(ATVscore))) )  
  
  prev1<-prev1   
        prevFER1<-prevFER1
              prevDSM1<-prevDSM1
                    prevPIP1<-prevPIP1
                          prevUCT1<-prevUCT1
                                prevATV1<-prevATV1
  
  loglik1a<- prev1* log((pred1a)+0.0000001)+(1-prev1)*log(1-((pred1a)-0.0000001))
         loglikf<- prevFER1* log((predFER)+0.00001)+(1-prevFER1)*log(1-((predFER)-0.00001))
              loglikd<- prevDSM1* log((predDSM)+0.00001)+(1-prevDSM1)*log(1-((predDSM)-0.00001))
                    loglikp<- prevPIP1* log((predPIP)+0.00001)+(1-prevPIP1)*log(1-((predPIP)-0.00001))
                          logliku<- prevUCT1* log((predUCT)+0.00001)+(1-prevUCT1)*log(1-((predUCT)-0.00001))
                                loglikatv<- prevATV1* log((predATV)+0.00001)+(1-prevATV1)*log(1-((predATV)-0.00001))
  
  
  -sum(
    loglik1a,
          loglikf,
                loglikd,
                      loglikp,
                            logliku,
                                  loglikatv,
    na.rm=T)
}


n.param<-2

nc<-seq(0,4,0.01)
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmod
pred<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) ) 
pred[1]


logmodfer<-optim(c(-3,3),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmodfer
predfer<-(exp(logmodfer$par[1] + logmodfer$par[2] * nc)) / (1 + exp(logmodfer$par[1] + logmodfer$par[2] * nc))
predfer[1]

logmoddsm<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-100,-100),upper=c(-4,100))
logmoddsm
preddsm<-(exp(logmoddsm$par[1] + logmoddsm$par[2] * nc)) / (1 + exp(logmoddsm$par[1] + logmoddsm$par[2] * nc))
preddsm[1]

logmodpip<-optim(c(-6,2),log.binom,method="L-BFGS-B",lower=c(-10,0),upper=c(-4,10))
logmodpip
predpip<-(exp(logmodpip$par[1] + logmodpip$par[2] * nc)) / (1 + exp(logmodpip$par[1] + logmodpip$par[2] * nc))
predpip[1]

logmoduct<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(-4,10))
logmoduct
preduct<-(exp(logmoduct$par[1] + logmoduct$par[2] * nc)) / (1 + exp(logmoduct$par[1] + logmoduct$par[2] * nc))
preduct[1]

logmodatv<-optim(c(-6,4.5),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmodatv
predatv<-(exp(logmodatv$par[1] + logmodatv$par[2] * nc)) / (1 + exp(logmodatv$par[1] + logmodatv$par[2] * nc))

plot(sort(score)[1:27],prev1,ylim=c(0,1),bty="n",xlim=c(0,4),las=1,
     xlab="Sporozoite Score",ylab="Prevalence blood stage infection",cex=1.25,col="chartreuse4",pch=16,cex.lab=2)

points(sort(FERscore),sort(prevFER1),pch=20,col="red")
points(sort(DSMscore),sort(prevDSM1),pch=20,col="blue")
points(sort(PIPscore),sort(prevPIP1),pch=20,col="blueviolet")
points(c(sort(UCTscore),rep(max(UCTscore),5)),sort(prevUCT1),pch=20,col="orange")
points(sort(ATVscore),sort(prevATV1))

      PREDcon<-(exp(-6.618501 + 6.021603 * nc)) / (1 + exp(-6.618501 + 6.021603 * nc))
            PREDfer<-(exp(-5.402039 + 4.233812 * nc)) / (1 + exp(-5.402039 + 4.233812 * nc))
                  PREDdsm<-(exp(-4 + 3.983218 * nc)) / (1 + exp(-4 + 3.983218 * nc))
                        PREDpip<-(exp(-4 + 3.818461 * nc)) / (1 + exp(-4 + 3.818461 * nc))
                              PREDuct<-(exp(-1.921384 + 2.536381 * nc)) / (1 + exp(-1.921384 + 2.536381 * nc))
                                    PREDatv<-(exp(-6.6 + 5* nc)) / (1 + exp(-6.6 + 5 * nc))
lines(nc,PREDcon,lwd=2,col="chartreuse4")

lines(nc,PREDfer,lwd=2,col="red")
lines(nc,PREDdsm,lwd=2,col="blue")
lines(nc,PREDpip,lwd=2,col="blueviolet")
lines(nc,PREDuct,lwd=2,col="orange")
lines(nc,PREDatv,lwd=2,col="chocolate")
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
a<- -6.618501 
b<-6.021603
alpha=2.9693194
beta=0.9721653
delta=39.8925127
gamma=0.9270415

X1<-max(blanks$oocysts)

BLANKfitooc <- ((exp(a + b * ((alpha * seq(0,X1,1)^beta)/(delta + gamma * seq(0,X1,1)^beta)))) / 
                  (1 + exp(a + b * ((alpha * seq(0,X1,1)^beta)/(delta + gamma * seq(0,X1,1)^beta)))) )

BLANKfitooc2<-BLANKfitooc*(sum(spors$bsprev[spors$Treatment == "Blank"])/length(spors$bsprev[spors$Treatment == "Blank"]))

plot(seq(0,X1,1),BLANKfitooc2,col="aquamarine4", log="x",
     xlab="Number of oocysts per mosquito", ylab="Probability of blood stage infection",cex.lab=2)
lines(seq(0,X1,1),BLANKfitooc2)

####FER
a=-5.402039  
b=4.233812
alpha<-2.976640  
  beta<-0.901168
  delta<-22.911030
  gamma<-0.898832


X2<-max(OZFER$oocysts)

FERfitooc <- ((exp(a + b * ((alpha * seq(0,X2,1)^beta)/(delta + gamma * seq(0,X2,1)^beta)))) / 
                  (1 + exp(a + b * ((alpha * seq(0,X2,1)^beta)/(delta + gamma * seq(0,X2,1)^beta)))) )
FERfitooc2<-FERfitooc * (sum(spors$bsprev[spors$Treatment == "OZFER"])/length(spors$bsprev[spors$Treatment == "OZFER"]))
lines(seq(0,X2,1),FERfitooc2,col="chocolate1")


##ATV
a=-6.6 
b=5
alpha=3
beta=0.9999
delta=40
gamma=0.9



ATVfitooc <- ((exp(a + b * ((alpha * seq(0,X1,1)^beta)/(delta + gamma * seq(0,X1,1)^beta)))) / 
                (1 + exp(a + b * ((alpha * seq(0,X1,1)^beta)/(delta + gamma * seq(0,X1,1)^beta)))) )
ATVfitooc2<-ATVfitooc*(sum(spors2$bsprev)/length(spors2$bsprev))
points(seq(0,X1,1),ATVfitooc2,col="chocolate",pch=20)
lines(seq(0,X1,1),ATVfitooc2,col="chocolate")



#################################
#################################
## And using the mean gompertz sporoscores approach...
  a=0.7945516
  b=-8.3516861
  c=-0.4617241
  alpha=-6.618501 
  beta=6.021603
  nc<-seq(0,100,1)
spsc<-a * exp (b * exp(c * nc))
PREDcon<-(exp(alpha + beta * spsc)) / (1 + exp(alpha + beta * spsc))

plot(PREDcon~nc,log="x")
