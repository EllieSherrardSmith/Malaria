###########################################################################################
##
## DRUG MODEL: OZDSM
##
##
###########################################################################################


spors<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\UCT OZPIP FER DSM Feb2015\\M2M SporozoiteScores\\SporoParaIntensity.txt",header=TRUE)
head(spors)
spors$bsprev<-ifelse(spors$Parasitemia == 0,0,1)

spors$sporozoites<-numeric(length(spors$Round))
for (i in 1:length(spors$Round)){
  spors$sporozoites[i]<-sum(spors[i,5:14],na.rm=TRUE)}

spors$ScorePerBite<-spors$sporozoites/spors$Bites

score0<- spors$ScorePerBite[spors$ScorePerBite == 0 & spors$Treatment == "OZDSM"]
score1<- spors$ScorePerBite[spors$ScorePerBite > 0 & spors$ScorePerBite<1  & spors$Treatment == "OZDSM"]
score2<- spors$ScorePerBite[spors$ScorePerBite >= 1 & spors$ScorePerBite<2  & spors$Treatment == "OZDSM"]
score3<- spors$ScorePerBite[spors$ScorePerBite >= 2 & spors$ScorePerBite<3  & spors$Treatment == "OZDSM"]
score4<- spors$ScorePerBite[spors$ScorePerBite >= 3 & spors$ScorePerBite<4  & spors$Treatment == "OZDSM"]

logScoreD<-c(mean(score0),mean(score1),mean(score2),mean(score3),max(spors$ScorePerBite[spors$Treatment == "OZDSM"]),max(spors$ScorePerBite[spors$Treatment == "OZDSM"]))

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
logScoreLDSM<-c(0,ao1[1],ao2[1],ao3[1],max(score3),max(score3))
logScoreUDSM<-c(0,ao1[2],ao2[2],ao3[2],min(score3),min(score3))


oocysts<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\UCT OZPIP FER DSM Feb2015\\M2M SporozoiteScores\\OocystIntensity.txt",header=TRUE)
OZDSM<-subset(oocysts,Treatment=="OZDSM")

oocdataD1<-sort(OZDSM$oocysts[OZDSM$oocysts > 0])
length(oocdataD1)/5
oocD1<-c(0,mean(oocdataD1[1:12]),mean(oocdataD1[13:24]),mean(oocdataD1[25:36]),mean(oocdataD1[37:48]),mean(oocdataD1[49:60]))


#######################################
##
###
#### OOCYSTS TO sporos score
###
##
########################################


#
## Logistic fit
#
sat.binom<-function(p.vec){
  
  alpha  <- p.vec[1]
  beta  <- p.vec[2]
  delta <- p.vec[3]
  gamma <- p.vec[4]
  
  
  pred1<- (alpha * oocD1[1:6]^beta)/(delta + gamma * oocD1[1:6]^beta)
 
  
  #a <- p.vec[1]
  #b <- p.vec[2]
  
  #pred1<- ((exp(a + b * oocD1[1:6])) / (1 + exp(a + b * oocD1[1:6])) )  


  spors1<-logScoreD[1:6]

  
  loglik1<- spors1* log((pred1)+0.001)+(1-spors1)*log(1-((pred1)-0.001))
 
  
  -sum(loglik1,na.rm=T)
}
n.param<-4
satmod2<-optim(c(2.2,0.999,25,0.8),sat.binom,method="L-BFGS-B",lower=c(0,0.6,8,0.8),upper=c(2.2,0.999999,100,0.99))
satmod2

#n.param<-4
#satmod2<-optim(c(4,0.999,10,0.99),sat.binom,method="L-BFGS-B",lower=c(-10,0.6,1,0.8),upper=c(100,0.9999,100,0.9999))
#satmod2

par(mfrow=c(1,2))

nc<-seq(0,max(oocdataD1),1)
predDSM<-(satmod2$par[1] * nc^satmod2$par[2])/(satmod2$par[3] + satmod2$par[4] * nc^satmod2$par[2])
#pred2<-((exp(satmod3$par[1]  + satmod3$par[2]  * nc)) / (1 + exp(satmod3$par[1]  + satmod3$par[2]  * nc)) ) 
plot(oocD1,logScoreD,ylim=c(0,5),bty="n",xlim=c(0,300),main="DSM",
     las=1,xlab="Oocysts",ylab="Sporozoites",cex=1.25,col="chartreuse4",pch=16)
lines(nc,predDSM,lwd=2,col="red")


sat.binomL<-function(p.vec){
  
  alpha  <- p.vec[1]
  beta  <- p.vec[2]
  delta <- p.vec[3]
  gamma <- p.vec[4]
  
   pred1<- (alpha * oocD1[1:6]^beta)/(delta + gamma * oocD1[1:6]^beta)
   spors1<-logScoreLDSM[1:6]
  
   loglik1<- spors1* log((pred1)+0.001)+(1-spors1)*log(1-((pred1)-0.001))
  
    -sum(loglik1,na.rm=T)
}
n.param<-4
satmod2L<-optim(c(2.2,0.999,25,0.8),sat.binomL,method="L-BFGS-B",lower=c(0,0.5,5,0.8),upper=c(2.2,0.999999,100,0.99))
satmod2L
predDSML<-(satmod2L$par[1] * nc^satmod2L$par[2])/(satmod2L$par[3] + satmod2L$par[4] * nc^satmod2L$par[2])
lines(nc,predDSML,lwd=2,col="red",lty=2)

sat.binomU<-function(p.vec){
  
  alpha  <- p.vec[1]
  beta  <- p.vec[2]
  delta <- p.vec[3]
  gamma <- p.vec[4]
  
  pred1<- (alpha * oocD1[1:6]^beta)/(delta + gamma * oocD1[1:6]^beta)
  spors1<-logScoreUDSM[1:6]
  
  loglik1<- spors1* log((pred1)+0.001)+(1-spors1)*log(1-((pred1)-0.001))
  
  -sum(loglik1,na.rm=T)
}
n.param<-4
satmod2U<-optim(c(2.2,0.999,5,0.8),sat.binomU,method="L-BFGS-B",lower=c(0,0.5,5,0.8),upper=c(2.2,0.999999,100,0.99))
satmod2U
predDSMU<-(satmod2U$par[1] * nc^satmod2U$par[2])/(satmod2U$par[3] + satmod2U$par[4] * nc^satmod2U$par[2])
lines(nc,predDSMU,lwd=2,col="red",lty=2)
########################################
##
###
#### sPOROS TO BLOODSTAGE PREVALENCE
###
##
########################################


prev1<-sum(spors$bsprev[spors$Treatment=="OZDSM" & spors$ScorePerBite == "0"])/length(spors$bsprev[spors$Treatment=="OZDSM" & spors$ScorePerBite == "0"])
prev2<-sum(spors$bsprev[spors$Treatment=="OZDSM" & spors$ScorePerBite == "1"])/length(spors$bsprev[spors$Treatment=="OZDSM" & spors$ScorePerBite == "1"])
prev3<-sum(spors$bsprev[spors$Treatment=="OZDSM" & spors$ScorePerBite == "2"])/length(spors$bsprev[spors$Treatment=="OZDSM" & spors$ScorePerBite == "2"])
prev4<-sum(spors$bsprev[spors$Treatment=="OZDSM" & spors$ScorePerBite == "3"])/length(spors$bsprev[spors$Treatment=="OZDSM" & spors$ScorePerBite == "3"])
prev5<-sum(spors$bsprev[spors$Treatment=="OZDSM" & spors$ScorePerBite == "4"])/length(spors$bsprev[spors$Treatment=="OZDSM" & spors$ScorePerBite == "4"])
prevMouseDataDSM<-c(0,prev1,prev2,prev3,NA,1)


#
## Saturating fit
#


sat.binom<-function(p.vec){
  
  
  #b<-p.vec[1]
  #umb<-p.vec[2]
  #charlie<-p.vec[3]
  
  #pred0<- (b * logScoreD[1]^1)/(charlie + umb * logScoreD[1]^1)
  #pred1<- (b * logScoreD[2]^1)/(charlie + umb * logScoreD[2]^1)  
  #pred2<- (b * logScoreD[3]^1)/(charlie + umb * logScoreD[3]^1) 
  #pred3<- (b * logScoreD[4]^1)/(charlie + umb * logScoreD[4]^1)  
  #pred4<- (b * logScoreD[5]^1)/(charlie + umb * logScoreD[5]^1)  
  #pred5<- (b * logScoreD[6]^1)/(charlie + umb * logScoreD[6]^1)  
  
  a <- p.vec[1]
  b <- p.vec[2]
  #pred0<- ((exp(a + b * 0) / (1 + exp(a + b * 0)) ) )
  pred1<- ((exp(a + b * logScoreD[1:6])) / (1 + exp(a + b * logScoreD[1:6])) )  
  
  prev1<-prevMouseDataDSM[1:6]

  loglik1<- prev1* log((pred1)+0.001)+(1-prev1)*log(1-((pred1)-0.001))
  
  -sum(loglik1,na.rm=T)
}
n.param<-2
satmod<-optim(c(0,0),sat.binom,method="L-BFGS-B",lower=c(-100,-10),upper=c(10,10))
satmod

nc<-seq(0,4,0.01)
pred1DSM<- ((exp(satmod$par[1] + satmod$par[2] * nc)) / (1 + exp(satmod$par[1] + satmod$par[2] * nc)) ) 
#pred<-(0 + satmod$par[1] * nc^1)/(satmod$par[3] + satmod$par[2] * nc^1) 
plot(logScoreD,prevMouseData,ylim=c(0,1),bty="n",xlim=c(0,4),las=1,xlab="Sporozoite Score",ylab="Prevalence blood stage infection",cex=1.25,col="chartreuse4",pch=16)
lines(nc,pred1DSM,lwd=2,col="red")

sat.binomL2<-function(p.vec){

  a <- p.vec[1]
  b <- p.vec[2]

  pred1L<- ((exp(a + b * logScoreLDSM[1:6])) / (1 + exp(a + b * logScoreLDSM[1:6])) )  
  
  prev1L<-prevMouseDataDSM[1:6]
  
  loglik1<- prev1L* log((pred1L)+0.001)+(1-prev1L)*log(1-((pred1L)-0.001))
  
  -sum(loglik1,na.rm=T)
}
n.param<-2
satmod<-optim(c(0,0),sat.binomL2,method="L-BFGS-B",lower=c(-100,-10),upper=c(10,100))
satmod

nc<-seq(0,4,0.01)
pred1DSML<- ((exp(satmod$par[1] + satmod$par[2] * nc)) / (1 + exp(satmod$par[1] + satmod$par[2] * nc)) ) 
lines(nc,pred1DSML,lwd=2,lty=2,col="red")


sat.binomU2<-function(p.vec){
  
  a <- p.vec[1]
  b <- p.vec[2]
  
  pred1U<- ((exp(a + b * logScoreUDSM[1:6])) / (1 + exp(a + b * logScoreUDSM[1:6])) )  
  
  prev1U<-prevMouseDataDSM[1:6]
  
  loglik1<- prev1U* log((pred1U)+0.001)+(1-prev1U)*log(1-((pred1U)-0.001))
  
  -sum(loglik1,na.rm=T)
}
n.param<-2
satmodU<-optim(c(0,0),sat.binomU2,method="L-BFGS-B",lower=c(-100,-10),upper=c(10,100))
satmodU

nc<-seq(0,4,0.01)
pred1DSMU<- ((exp(satmodU$par[1] + satmodU$par[2] * nc)) / (1 + exp(satmodU$par[1] + satmodU$par[2] * nc)) ) 
lines(nc,pred1DSMU,lwd=2,lty=2,col="red")
#######################################
##
###
#### OOCYSTS TO BLOODSTAGE PREVALENCE
###
##
########################################

oocysts<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\UCT OZPIP FER DSM Feb2015\\M2M SporozoiteScores\\OocystIntensity.txt",header=TRUE)
OZDSMs<-subset(oocysts,Treatment=="OZDSM")

oocdataD1<-sort(OZDSMs$oocysts[OZDSMs$oocysts > 0])
length(oocdataD1)/5
ooc

###Mean model

fitdat<-seq(0,max(oocdata1),1)
alpha = 2.2
beta = 0.99999999
delta = 8
gamma = 0.8
a <- -12.24466
b <- 10


fitooc1DSM <- ((exp(a + b * ((alpha * fitdat^beta)/(delta + gamma * fitdat^beta)))) / 
              (1 + exp(a + b * ((alpha * fitdat^beta)/(delta + gamma * fitdat^beta)))) )

points(fitooc1DSM~fitdat,xlim=c(0,120),
       pch=20,col="red")
lines(fitdat,fitooc1DSM,lwd=2)



###Upper model

alpha = 2.2
beta = 0.9999
delta = 5
gamma = 0.8

a <- -33.93591
b <- 34.34221
fitooc1UDSM <- ((exp(a + b * ((alpha * fitdat^beta)/(delta + gamma * fitdat^beta)))) / 
               (1 + exp(a + b * ((alpha * fitdat^beta)/(delta + gamma * fitdat^beta)))) )

lines(fitdat,fitooc1UDSM,lwd=2,lty=2,col="red")



###Lower model

alpha = 2.1758484
beta = 0.9810398
delta = 12.1133842
gamma = 0.8145564

a <- -100
b <- 62.75


fitooc1LDSM <- ((exp(a + b * ((alpha * fitdat^beta)/(delta + gamma * fitdat^beta)))) / 
               (1 + exp(a + b * ((alpha * fitdat^beta)/(delta + gamma * fitdat^beta)))) )

lines(fitdat,fitooc1LDSM,lwd=2,lty=2,col="red")