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

oocdataD1FER<-sort(OZFER$oocysts[OZFER$oocysts > 0],na.rm=TRUE)
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
  
  pred1<- (alpha * ooc[1:6]^beta)/(delta + gamma * ooc[1:6]^beta)
  pred2<- (alpha * oocFER[1:6]^beta)/(delta + gamma * oocFER[1:6]^beta)
  pred3<- (alpha * oocDSM[1:6]^beta)/(delta + gamma * oocDSM[1:6]^beta)
  pred4<- (alpha * oocPIP[1:6]^beta)/(delta + gamma * oocPIP[1:6]^beta)
  pred5<- (alpha * oocUCT[1:6]^beta)/(delta + gamma * oocUCT[1:6]^beta)
  
  spors1<-logScore[1:6]
  spors2<-logScoreFER[1:6]
  spors3<-logScoreDSM[1:6]
  spors4<-logScorePIP[1:6]
  spors5<-logScoreUCT[1:6]
  
  loglik1<- spors1* log((pred1)+0.00001)+(1-spors1)*log(1-((pred1)-0.00001))
  loglik2<- spors2* log((pred2)+0.00001)+(1-spors2)*log(1-((pred2)-0.00001))
  loglik3<- spors3* log((pred3)+0.00001)+(1-spors3)*log(1-((pred3)-0.00001))
  loglik4<- spors4* log((pred4)+0.00001)+(1-spors4)*log(1-((pred4)-0.00001))
  loglik5<- spors5* log((pred5)+0.00001)+(1-spors5)*log(1-((pred5)-0.00001))
  
  -sum(
      loglik1,
       loglik2,
       loglik3,
       loglik4,
       loglik5,
       na.rm=T)
}
n.param<-4
satmod<-optim(c(3.5,0.9999,25,0.9),sat.binom,method="L-BFGS-B",lower=c(1,0.4,15,0.9),upper=c(3.5,0.999999,100,0.9999))
satmod
predCON<-(satmod$par[1] * nc^satmod$par[2])/(satmod$par[3] + satmod$par[4] * nc^satmod$par[2])

satmodFER<-optim(c(3.5,0.9999,25,0.9),sat.binom,method="L-BFGS-B",lower=c(1,0.4,15,0.9),upper=c(3.5,0.999999,100,0.9999))
satmodFER
predFER<-(satmodFER$par[1] * nc^satmodFER$par[2])/(satmodFER$par[3] + satmodFER$par[4] * nc^satmodFER$par[2])

satmodDSM<-optim(c(3.5,0.9999,25,0.9),sat.binom,method="L-BFGS-B",lower=c(1,0.4,15,0.9),upper=c(3.5,0.999999,100,0.9999))
satmodDSM
predDSM<-(satmodDSM$par[1] * nc^satmodDSM$par[2])/(satmodDSM$par[3] + satmodDSM$par[4] * nc^satmodDSM$par[2])

satmodPIP<-optim(c(3.5,0.9999,25,0.9),sat.binom,method="L-BFGS-B",lower=c(1,0.4,15,0.9),upper=c(3.5,0.999999,100,0.9999))
satmodPIP
predPIP<-(satmodPIP$par[1] * nc^satmodPIP$par[2])/(satmodPIP$par[3] + satmodPIP$par[4] * nc^satmodPIP$par[2])

satmodUCT<-optim(c(3.5,0.9999,25,0.9),sat.binom,method="L-BFGS-B",lower=c(1,0.4,15,0.9),upper=c(3.5,0.999999,100,0.9999))
satmodUCT
predUCT<-(satmodUCT$par[1] * nc^satmodUCT$par[2])/(satmodUCT$par[3] + satmodUCT$par[4] * nc^satmodUCT$par[2])

nc<-seq(0,300,1)

plot(ooc,logScore,ylim=c(0,5),bty="n",xlim=c(0,300),
     las=1,xlab="Oocysts",ylab="Sporozoites",cex=1.25,col="chartreuse4",pch=16)
points(oocFER,logScoreFER,col="red",pch=20);points(oocDSM,logScoreDSM,col="blue",pch=20);
points(oocPIP,logScorePIP,col="green",pch=20);points(oocPIP,logScorePIP,col="orange",pch=20)


  
  predCON<-(3.4999292 * nc^0.999999)/(23.6197325 + 0.9000661 * nc^0.99999)
  predFER<-(3.5 * nc^0.9999318)/(24.7097148 + 0.9 * nc^0.9999318)
  predDSM<-(3.4938832 * nc^0.9796118)/(15.0012052 + 0.9039049 * nc^0.9796118)
  predPIP<-(3.4999387 * NC^0.9999990)/(22.3078003 + 0.9000538 * NC^0.9000538)
  predUCT<-(3.4909695 * nc^0.9490303)/(25.0010236 + 0.9062446 * nc^0.9490303)

lines(nc,predCON,lwd=2,col="black")
    lines(nc,predFER,lwd=2,col="red")
        lines(nc,predDSM,lwd=2,col="blue")
            lines(nc,predPIP,lwd=2,col="green")
                lines(nc,predUCT,lwd=2,col="orange")
#
## Logistic fit
#
satU.binom<-function(p.vec){
  
  alpha  <- p.vec[1]
  beta  <- p.vec[2]
  delta <- p.vec[3]
  gamma <- p.vec[4]
  
  
  pred1<- (alpha * oocU[1:6]^beta)/(delta + gamma * oocU[1:6]^beta)
  pred2<- (alpha * oocFERU[1:6]^beta)/(delta + gamma * oocFERU[1:6]^beta)
  pred3<- (alpha * oocDSMU[1:6]^beta)/(delta + gamma * oocDSMU[1:6]^beta)
  pred4<- (alpha * oocPIPU[1:6]^beta)/(delta + gamma * oocPIPU[1:6]^beta)
  pred5<- (alpha * oocUCTU[1:6]^beta)/(delta + gamma * oocUCTU[1:6]^beta)
  
  spors1<-logScoreU[1:6]
  spors2<-logScoreFERU[1:6]
  spors3<-logScoreDSMU[1:6]
  spors4<-logScorePIPU[1:6]
  spors5<-logScoreUCTU[1:6]
  
  loglik1<- spors1* log((pred1)+0.00001)+(1-spors1)*log(1-((pred1)-0.00001))
  loglik2<- spors1* log((pred2)+0.00001)+(1-spors2)*log(1-((pred2)-0.00001))
  loglik3<- spors1* log((pred3)+0.00001)+(1-spors3)*log(1-((pred3)-0.00001))
  loglik4<- spors1* log((pred4)+0.00001)+(1-spors4)*log(1-((pred4)-0.00001))
  loglik5<- spors1* log((pred5)+0.00001)+(1-spors5)*log(1-((pred5)-0.00001))
  
  -sum(loglik1,loglik2,loglik3,loglik4,loglik5,na.rm=T)
}
n.param<-4
satmodU<-optim(c(4,0.99,20,0.99),satU.binom,method="L-BFGS-B",lower=c(1,0.8,10,0.750),upper=c(8,0.9999,200,0.9999))
satmodU


pred<-(satmodU$par[1] * nc^satmodU$par[2])/(satmodU$par[3] + satmodU$par[4] * nc^satmodU$par[2])
points(oocU,logScoreU);points(oocL,logScoreL)
lines(nc,pred,lwd=2,col="red",lty=2)

satL.binom<-function(p.vec){
  
  alpha  <- p.vec[1]
  beta  <- p.vec[2]
  delta <- p.vec[3]
  gamma <- p.vec[4]
  
  
  pred1<- (alpha * oocL[1:6]^beta)/(delta + gamma * oocL[1:6]^beta)
  pred2<- (alpha * oocFERL[1:6]^beta)/(delta + gamma * oocFERL[1:6]^beta)
  pred3<- (alpha * oocDSML[1:6]^beta)/(delta + gamma * oocDSML[1:6]^beta)
  pred4<- (alpha * oocPIPL[1:6]^beta)/(delta + gamma * oocPIPL[1:6]^beta)
  pred5<- (alpha * oocUCTL[1:6]^beta)/(delta + gamma * oocUCTL[1:6]^beta)
  
  spors1<-logScoreL[1:6]
  spors2<-logScoreFERL[1:6]
  spors3<-logScoreDSML[1:6]
  spors4<-logScorePIPL[1:6]
  spors5<-logScoreUCTL[1:6]
  
  loglik1<- spors1* log((pred1)+0.00001)+(1-spors1)*log(1-((pred1)-0.00001))
  loglik2<- spors1* log((pred2)+0.00001)+(1-spors2)*log(1-((pred2)-0.00001))
  loglik3<- spors1* log((pred3)+0.00001)+(1-spors3)*log(1-((pred3)-0.00001))
  loglik4<- spors1* log((pred4)+0.00001)+(1-spors4)*log(1-((pred4)-0.00001))
  loglik5<- spors1* log((pred5)+0.00001)+(1-spors5)*log(1-((pred5)-0.00001))
  
  -sum(loglik1,loglik2,loglik3,loglik4,loglik5,na.rm=T)
}
n.param<-4
satmodL<-optim(c(5,0.97,45,0.99),satL.binom,method="L-BFGS-B",lower=c(3.5,0.8,0,0.8),upper=c(20,0.9999,200,0.9999))
satmodL

pred<-(satmodL$par[1] * nc^satmodL$par[2])/(satmodL$par[3] + satmodL$par[4] * nc^satmodL$par[2])
lines(nc,pred,lwd=2,col="red",lty=2)
