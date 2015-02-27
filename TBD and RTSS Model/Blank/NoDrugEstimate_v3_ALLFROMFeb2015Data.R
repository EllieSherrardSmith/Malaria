###########################################################################################
##
## BLANK MODEL
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

#######################################
##
###
#### OOCYSTS TO sporos score
###
##
########################################
par(mfrow=c(1,2))

#
## Logistic fit
#
sat.binom<-function(p.vec){
  
  alpha  <- p.vec[1]
  beta  <- p.vec[2]
  delta <- p.vec[3]
  gamma <- p.vec[4]
  
  pred0<- (alpha * ooc[1:6]^beta)/(delta + gamma * ooc[1:6]^beta)
  spors0<-logScore[1:6]

  loglik0<- spors0* log((pred0)+0.001)+(1-spors0)*log(1-((pred0)-0.001))

  -sum(loglik0,na.rm=T)
}
n.param<-4
satmod<-optim(c(3.8,0.9999,25,0.9),sat.binom,method="L-BFGS-B",lower=c(1,0.8,25,0.85),upper=c(8,0.9999,200,0.9999))
satmod



nc<-seq(0,300,1)
pred<-(satmod$par[1] * nc^satmod$par[4])/(satmod$par[3] + satmod$par[2] * nc^satmod$par[4])

plot(ooc,logScore,ylim=c(0,5),bty="n",xlim=c(0,300),
     las=1,xlab="Oocysts",ylab="Sporozoites",cex=1.25,col="chartreuse4",pch=16)
points(ooc,logScore);points(ooc,logScoreU);points(ooc,logScoreL)
lines(nc,pred,lwd=2,col="red")

#
## Logistic fit
#
satU.binom<-function(p.vec){
  
  alpha  <- p.vec[1]
  beta  <- p.vec[2]
  delta <- p.vec[3]
  gamma <- p.vec[4]
  
  
  pred0<- (alpha * ooc[1:6]^beta)/(delta + gamma * ooc[1:6]^beta)
  spors0<-logScoreU[1:6]
  
  loglik0<- spors0* log((pred0)+0.001)+(1-spors0)*log(1-((pred0)-0.001))
  
  -sum(loglik0,na.rm=T)
}
n.param<-4
satmodU<-optim(c(4,0.99,20,0.99),satU.binom,method="L-BFGS-B",lower=c(1,0.8,10,0.750),upper=c(8,0.9999,200,0.9999))
satmodU


pred<-(satmodU$par[1] * nc^satmodU$par[4])/(satmodU$par[3] + satmodU$par[2] * nc^satmodU$par[4])
points(oocU,logScoreU);points(oocL,logScoreL)
lines(nc,pred,lwd=2,col="red",lty=2)

satL.binom<-function(p.vec){
  
  alpha  <- p.vec[1]
  beta  <- p.vec[2]
  delta <- p.vec[3]
  gamma <- p.vec[4]
  
  
  pred0<- (alpha * ooc[1:6]^beta)/(delta + gamma * ooc[1:6]^beta)
  spors0<-logScoreL[1:6]
    
  loglik0<- spors0* log((pred0)+0.001)+(1-spors0)*log(1-((pred0)-0.001))
 
  -sum(loglik0,na.rm=T)
}
n.param<-4
satmodL<-optim(c(5,0.97,45,0.99),satL.binom,method="L-BFGS-B",lower=c(1,0.8,0,0.00001),upper=c(20,0.9999,200,0.9999))
satmodL

pred<-(satmodL$par[1] * nc^satmodL$par[4])/(satmodL$par[3] + satmodL$par[2] * nc^satmodL$par[4])
lines(nc,pred,lwd=2,col="red",lty=2)

########################################
##
###
#### sPOROS TO BLOODSTAGE PREVALENCE
###
##
########################################


prev1<-sum(spors$bsprev[spors$Treatment=="Blank" & spors$ScorePerBite == "0"])/length(spors$bsprev[spors$Treatment=="Blank" & spors$ScorePerBite == "0"])
prev2<-sum(spors$bsprev[spors$Treatment=="Blank" & spors$ScorePerBite == "1"])/length(spors$bsprev[spors$Treatment=="Blank" & spors$ScorePerBite == "1"])
prev3<-sum(spors$bsprev[spors$Treatment=="Blank" & spors$ScorePerBite == "2"])/length(spors$bsprev[spors$Treatment=="Blank" & spors$ScorePerBite == "2"])
prev4<-sum(spors$bsprev[spors$Treatment=="Blank" & spors$ScorePerBite == "3"])/length(spors$bsprev[spors$Treatment=="Blank" & spors$ScorePerBite == "3"])
prev5<-sum(spors$bsprev[spors$Treatment=="Blank" & spors$ScorePerBite == "4"])/length(spors$bsprev[spors$Treatment=="Blank" & spors$ScorePerBite == "4"])
prevMouseData<-c(0,prev1,prev2,prev3,prev4,1)


###################################################
##                                               ## 
##     ####       #     ##########     #         ##
##     ## ##     ###        ##        ###        ##
##     ##  ##   ## ##       ##       ## ##       ##
##     ##  ##  ##   ##      ##      ##   ##      ## 
##     ##  ##  #######      ##      #######      ## 
##     ## ##  ##     ##     ##     ##     ##     ##
##     ####  ##       ##    ##    ##       ##    ##
##                                               ## 
###################################################
setwd("C:\\Users\\Ellie\\Documents\\Data Malaria\\")
## Formatting

data.mouse.a=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\Mouse Data All\\ParasiteAllMASTER5.txt",header=TRUE)
summary(data.mouse.a)
data.mouse.a[14:25,]
data.mouse.a$Studyf<-as.factor(data.mouse.a$Study)
data.mouse.a$Mosiesf<-as.factor(data.mouse.a$Mosies)
tapply(data.mouse.a$Studyf,data.mouse.a$Mosiesf,summary)

rm.infbites<-data.mouse.a$TotalInf
rm.parasum.a<-data.mouse.a[,14:20]
rm.parasum<-numeric(length(rm.infbites))
for(i in 1:length(rm.infbites)){
  rm.parasum[i]<-sum(rm.parasum.a[i,], na.rm = TRUE)}
data.mouse.a$rm.prev<-ifelse(rm.parasum>0,1,0) ##This is the prevalence for the infected mice


rm.p14<-ifelse(data.mouse.a[,14]>=0,1,0)
rm.p15<-ifelse(data.mouse.a[,15]>=0,1,0)
rm.p16<-ifelse(data.mouse.a[,16]>=0,1,0)
rm.p17<-ifelse(data.mouse.a[,17]>=0,1,0)
rm.p18<-ifelse(data.mouse.a[,18]>=0,1,0)
rm.p19<-ifelse(data.mouse.a[,19]>=0,1,0)
rm.p20<-ifelse(data.mouse.a[,20]>=0,1,0)

for (i in 1:length(data.mouse.a$rm.prev)){
  data.mouse.a$SUMrm.paracount[i]<-sum(rm.p14[i],rm.p15[i],rm.p16[i],rm.p17[i],rm.p18[i],rm.p19[i],rm.p20[i],na.rm=T)
} ##This gives the number of parasitemia counts recorded for each row of the data

data.mouse.a$meanPara<-rm.parasum/data.mouse.a$SUMrm.paracount
data.mouse.a$ScorePerBite<-data.mouse.a$Sum/data.mouse.a$Mosies

score0<- data.mouse.a$ScorePerBite[data.mouse.a$ScorePerBite == 0]
score1<- data.mouse.a$ScorePerBite[data.mouse.a$ScorePerBite > 0 & data.mouse.a$ScorePerBite<1]
score2<- data.mouse.a$ScorePerBite[data.mouse.a$ScorePerBite >= 1 & data.mouse.a$ScorePerBite<2]
score3<- data.mouse.a$ScorePerBite[data.mouse.a$ScorePerBite >= 2 & data.mouse.a$ScorePerBite<3]
score4<- data.mouse.a$ScorePerBite[data.mouse.a$ScorePerBite >= 3 & data.mouse.a$ScorePerBite<4]
score5<- data.mouse.a$ScorePerBite[data.mouse.a$ScorePerBite >= 4]
logScoreTC<-c(mean(score0),mean(score1),mean(score2),mean(score3),mean(score4),mean(score5))

a1<-numeric(10000)
a2<-numeric(10000)
a3<-numeric(10000)
a4<-numeric(10000)
a5<-numeric(10000)
for (i in 1:10000) a1[i] <-sample(score1,replace=TRUE)
ao1<-quantile(a1,c(0.025,0.975))
for (i in 1:10000) a2[i] <-sample(score2,replace=TRUE)
ao2<-quantile(a2,c(0.025,0.975))
for (i in 1:10000) a3[i] <-sample(score3,replace=TRUE)
ao3<-quantile(a3,c(0.025,0.975))
for (i in 1:10000) a4[i] <-sample(score4,replace=TRUE)
ao4<-quantile(a4,c(0.025,0.975))
for (i in 1:10000) a5[i] <-sample(score5,replace=TRUE)
ao5<-quantile(a5,c(0.025,0.975))
logScoreTCL<-c(0,ao1[1],ao2[1],ao3[1],ao4[1],ao5[1])
logScoreTCU<-c(0,ao1[2],ao2[2],ao3[2],ao4[2],ao5[2])

length(score1);length(score2);length(score3);length(score4);length(score5)
var(score1);var(score2);var(score3);var(score4);var(score5)

data.mouse.a$ScoreType<-ifelse(data.mouse.a$ScorePerBite < 1,0,ifelse(data.mouse.a$ScorePerBite >=1 & data.mouse.a$ScorePerBite < 2, 1,
                                                                      ifelse(data.mouse.a$ScorePerBite >=2 & data.mouse.a$ScorePerBite < 3, 2,
                                                                             ifelse(data.mouse.a$ScorePerBite >=3 & data.mouse.a$ScorePerBite < 4, 3,4))))
parasit1<-sum(data.mouse.a$meanPara[data.mouse.a$ScoreType==0])/length(data.mouse.a$meanPara[data.mouse.a$ScoreType==0])
parasit2<-sum(data.mouse.a$meanPara[data.mouse.a$ScoreType==1])/length(data.mouse.a$meanPara[data.mouse.a$ScoreType==1])
parasit3<-sum(data.mouse.a$meanPara[data.mouse.a$ScoreType==2])/length(data.mouse.a$meanPara[data.mouse.a$ScoreType==2])
parasit4<-sum(data.mouse.a$meanPara[data.mouse.a$ScoreType==3])/length(data.mouse.a$meanPara[data.mouse.a$ScoreType==3])
parasit5<-sum(data.mouse.a$meanPara[data.mouse.a$ScoreType==4])/length(data.mouse.a$meanPara[data.mouse.a$ScoreType==4])
parasitMouseDataTC<-c(0,parasit1,parasit2,parasit3,parasit4,parasit5)

prev1<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==0])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==0])
prev2<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==1])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==1])
prev3<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==2])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==2])
prev4<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==3])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==3])
prev5<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==4])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==4])
prevMouseDataTC<-c(0,prev1,prev2,prev3,prev4,prev5)
#
## Logistic fit
#

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1<- ((exp(a + b * logScore[1:6])) / (1 + exp(a + b * logScore[1:6])) )  
  pred2<- ((exp(a + b * logScoreTC[1:6])) / (1 + exp(a + b * logScoreTC[1:6])) )  
  #pred3<- ((exp(a + b * data.mouse.a$ScorePerBite[data.mouse.a$rm.prev > 0])) / (1 + exp(a + b * data.mouse.a$ScorePerBite[data.mouse.a$rm.prev > 0])) )  
  
  prev1<-prevMouseData[1:6]
  prev2<-prevMouseDataTC[1:6]
  #prev3<-data.mouse.a$meanPar[data.mouse.a$rm.prev > 0]
  
  loglik1<- prev1* log((pred1)+0.00001)+(1-prev1)*log(1-((pred1)-0.00001))
  loglik2<- prev2* log((pred2)+0.00001)+(1-prev2)*log(1-((pred2)-0.00001))
  #loglik3<- prev3* log((pred3)+0.00001)+(1-prev3)*log(1-((pred3)-0.00001))
  
  -sum(loglik1,loglik2,na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,0),upper=c(0,10))
logmod

nc<-seq(0,4,0.01)
pred<-(exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc))
plot(logScore,prevMouseData,ylim=c(0,1),bty="n",xlim=c(0,4),las=1,xlab="Sporozoite Score",ylab="Prevalence blood stage infection",cex=1.25,col="chartreuse4",pch=16)
points(logScoreTC,prevMouseDataTC)
#points(data.mouse.a$ScorePerBite[data.mouse.a$rm.prev > 0],data.mouse.a$meanPar[data.mouse.a$rm.prev > 0],pch=20)

lines(nc,pred,lwd=2)



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

oocysts<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\UCT OZPIP FER DSM Feb2015\\M2M SporozoiteScores\\OocystIntensity.txt",header=TRUE)
blanks<-subset(oocysts,Treatment=="Blank")

oocdata1<-sort(blanks$oocysts[blanks$oocysts > 0])
length(oocdata1)/5
ooc<-c(0,mean(oocdata1[1:19]),mean(oocdata1[20:38]),mean(oocdata1[38:57]),mean(oocdata1[58:77]),mean(oocdata1[78:96]))

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
  prev1<-prevMouseData[1:6]
  
  
  loglik1<- fitooc1* log((prev1)+0.001)+(1-fitooc1)*log(1-((prev1)-0.001))
  #loglik2<- fitooc2* log((prev1)+0.001)+(1-fitooc2)*log(1-((prev1)-0.001))
  
  -sum(loglik1,na.rm=T)
}
n.param<-6
oocmod<-optim(c(3.7912483,0.9793911,25.0010451,0.9079316,-2.991983,2.603804),ooc.binom,method="L-BFGS-B",
              lower=c(3.7,0.9,25,0.9,-3,2.6),
              upper=c(3.8,0.98,26,0.92,-2.98,2.61))
#n.param<-2
#oocmod2<-optim(c(-4.5,0.185),ooc.binom,method="L-BFGS-B",lower=c(-2.5,0.1),upper=c(0,1))

oocmod

fitdat<-seq(0,max(oocdata1),1)
pred<-((exp(oocmod$par[5] + oocmod$par[6] * ((oocmod$par[1] * fitdat^oocmod$par[2])/(oocmod$par[3] + oocmod$par[4] * fitdat^oocmod$par[2])))) / 
         (1 + exp(oocmod$par[5] + oocmod$par[6] * ((oocmod$par[1] * fitdat^oocmod$par[2])/(oocmod$par[3] + oocmod$par[4] * fitdat^oocmod$par[2])))) )

pred2<-((exp(oocmod2$par[1] + oocmod2$par[2] * fitdat)) / (1 + exp(oocmod2$par[1] + oocmod2$par[2] * fitdat)) ) 
par(mfrow=c(1,1))
plot(ooc[1:6],prevMouseData,ylim=c(0,1),bty="n",xlim=c(0,200),las=1,xlab="Oocyst intensity",ylab="Prevalence blood stage infection",cex=1.25,col="chartreuse4",pch=16)
lines(fitdat,pred,lwd=2)


###Mean model

alpha = 3.7912483
beta = 0.9793911
delta = 25.0010451
gamma = 0.9079316
a <- -2.991983
b <- 2.603804


fitooc1 <- ((exp(a + b * ((alpha * fitdat^beta)/(delta + gamma * fitdat^beta)))) / 
              (1 + exp(a + b * ((alpha * fitdat^beta)/(delta + gamma * fitdat^beta)))) )

plot(fitooc1~fitdat,xlim=c(0,120),xlab="Number of oocysts",ylab="Prevalence of blood stage infection")
lines(fitdat,fitooc1,lwd=2)



###Upper model

alpha = 4
beta = 0.99
delta = 20
gamma = 0.999
a <- -3.317141
b <- 2.264785


fitooc1U <- ((exp(a + b * ((alpha * fitdat^beta)/(delta + gamma * fitdat^beta)))) / 
              (1 + exp(a + b * ((alpha * fitdat^beta)/(delta + gamma * fitdat^beta)))) )

lines(fitdat,fitooc1U,lwd=2,lty=2)



###Lower model

alpha = 5.3717306
beta = 0.9999
delta = 44.9641637
gamma = 0.7341659
a <- -2.307983
b <- 2.544483


fitooc1L <- ((exp(a + b * ((alpha * fitdat^beta)/(delta + gamma * fitdat^beta)))) / 
               (1 + exp(a + b * ((alpha * fitdat^beta)/(delta + gamma * fitdat^beta)))) )

lines(fitdat,fitooc1L,lwd=2,lty=2)