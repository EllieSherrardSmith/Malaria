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
  
  -sum(loglik1,loglik2,
       #loglik3,
       na.rm=T)
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
#
## Saturating fit
#


sat.binom<-function(p.vec){
  
  #a<-p.vec[1]
  b<-p.vec[1]
  d<-p.vec[2]
  ch<-p.vec[3]
  
  pred0<- (b * logScore[1]^1)/(ch + d * logScore[1]^1)
  pred1<- (b * logScore[2]^1)/(ch + d * logScore[2]^1)  
  pred2<- (b * logScore[3]^1)/(ch + d * logScore[3]^1) 
  pred3<- (b * logScore[4]^1)/(ch + d * logScore[4]^1)  
  pred4<- (b * logScore[5]^1)/(ch + d * logScore[5]^1)  
  pred5<- (b * logScore[6]^1)/(ch + d * logScore[6]^1)  
  
  
  prev0=sum(0)/length(0)
  prev1<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==0])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==0])
  prev2<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==1])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==1])
  prev3<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==2])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==2])
  prev4<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==3])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==3])
  prev5<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==4])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==4])
  
  loglik0<- prev0* log((pred0)+0.001)+(1-prev0)*log(1-((pred0)-0.001))
  loglik1<- prev1* log((pred1)+0.001)+(1-prev1)*log(1-((pred1)-0.001))
  loglik2<- prev2* log((pred2)+0.001)+(1-prev2)*log(1-((pred2)-0.001))
  loglik3<- prev3* log((pred3)+0.001)+(1-prev3)*log(1-((pred3)-0.001))
  loglik4<- prev4* log((pred4)+0.001)+(1-prev4)*log(1-((pred4)-0.001))
  loglik5<- prev5* log((pred5)+0.001)+(1-prev5)*log(1-((pred5)-0.001))
  
  -sum(loglik0,loglik1,loglik2,loglik3,loglik4,loglik5,na.rm=T)
}
n.param<-3
satmod<-optim(c(0.9,0.88,0.8),sat.binom,method="L-BFGS-B",lower=c(0,0.88,0.4),upper=c(1,1,1))
satmod

nc<-seq(0,4,0.01)
pred<-(0 + satmod$par[1] * nc^1)/(satmod$par[3] + satmod$par[2] * nc^1) 
plot(logScore,prevMouseData,ylim=c(0,1),bty="n",xlim=c(0,4),las=1,xlab="Sporozoite Score",ylab="Prevalence blood stage infection",cex=1.25,col="chartreuse4",pch=16)
lines(nc,pred,lwd=2,lty=2,col="red")




Realcount0<-round(score0)
Realcount1<-rep(1,length(score1))
Realcount2<-round((score2-1)*10)
Realcount2<-ifelse(Realcount2==0,10,Realcount2)
Realcount3<-round((score3-2)*100)
Realcount3<-ifelse(Realcount3==0,100,Realcount3)
Realcount4<-round((score4-3)*1000)
Realcount4<-ifelse(Realcount4==0,1000,Realcount4)
Realcount5<-rep(1001,length(score5))
real<-sort(c(Realcount0,Realcount1,Realcount2,Realcount3,Realcount4,Realcount5))


prevMouseData<-c(0,prev1,prev2,prev3,prev4,prev5)
logScore<-c(mean(score0),mean(score1),mean(score2),mean(score3),mean(score4),mean(score5))
realScore<-c(0,1,round((mean(score2)-1)*10),round((mean(score3)-2)*100),round((mean(score4)-3)*1000),1001)

plot(realScore,prevMouseData,ylim=c(0,1),bty="n",xlim=c(0,50),las=1,xlab="Sporozoite Score",ylab="Prevalence blood stage infection",cex=1.25,col="chartreuse4",pch=16)
#
## Logistic fit
#

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred0<- ((exp(a + b * realScore[1])) / (1 + exp(a + b * realScore[1])) ) 
  pred1<- ((exp(a + b * realScore[2])) / (1 + exp(a + b * realScore[2])) )  
  pred2<- ((exp(a + b * realScore[3])) / (1 + exp(a + b * realScore[3])) )  
  pred3<- ((exp(a + b * realScore[4])) / (1 + exp(a + b * realScore[4])) )  
  pred4<- ((exp(a + b * realScore[5])) / (1 + exp(a + b * realScore[5])) )  
  pred5<- ((exp(a + b * realScore[6])) / (1 + exp(a + b * realScore[6])) )  
  
  prev0<-prevMouseData[1]
  prev0<-prevMouseData[2]
  prev0<-prevMouseData[3]
  prev0<-prevMouseData[4]
  prev0<-prevMouseData[5]
  prev0<-prevMouseData[6]

  
  loglik0<- prev0* log((pred0)+0.00001)+(1-prev0)*log(1-((pred0)-0.00001))
  loglik1<- prev1* log((pred1)+0.00001)+(1-prev1)*log(1-((pred1)-0.00001))
  loglik2<- prev2* log((pred2)+0.00001)+(1-prev2)*log(1-((pred2)-0.00001))
  loglik3<- prev3* log((pred3)+0.00001)+(1-prev3)*log(1-((pred3)-0.00001))
  loglik4<- prev4* log((pred4)+0.00001)+(1-prev4)*log(1-((pred4)-0.00001))
  loglik5<- prev5* log((pred5)+0.00001)+(1-prev5)*log(1-((pred5)-0.00001))
  
  -sum(loglik0,loglik1,loglik2,loglik3,
       loglik4,loglik5,
       na.rm=T)
}
n.param<-2
logmod<-optim(c(-3,0.99),log.binom,method="L-BFGS-B",lower=c(-10,0),upper=c(10,0.99))
logmod

pred<-(exp(logmod$par[1] + logmod$par[2] * real)) / (1 + exp(logmod$par[1] + logmod$par[2] * real))
lines(real,pred,lwd=2)


#
## Saturating fit
#


sat.binom<-function(p.vec){
  
  #a<-p.vec[1]
  b<-p.vec[1]
  d<-p.vec[2]
  ch<-p.vec[3]
  
  pred0<- (b * realScore[1]^1)/(ch + d * realScore[1]^1)
  pred1<- (b * realScore[2]^1)/(ch + d * realScore[2]^1)  
  pred2<- (b * realScore[3]^1)/(ch + d * realScore[3]^1) 
  pred3<- (b * realScore[4]^1)/(ch + d * realScore[4]^1)  
  pred4<- (b * realScore[5]^1)/(ch + d * realScore[5]^1)  
  pred5<- (b * realScore[6]^1)/(ch + d * realScore[6]^1)  
  
  
  prev0=sum(0)/length(0)
  prev1<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==0])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==0])
  prev2<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==1])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==1])
  prev3<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==2])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==2])
  prev4<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==3])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==3])
  prev5<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==4])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==4])
  
  loglik0<- prev0* log((pred0)+0.001)+(1-prev0)*log(1-((pred0)-0.001))
  loglik1<- prev1* log((pred1)+0.001)+(1-prev1)*log(1-((pred1)-0.001))
  loglik2<- prev2* log((pred2)+0.001)+(1-prev2)*log(1-((pred2)-0.001))
  loglik3<- prev3* log((pred3)+0.001)+(1-prev3)*log(1-((pred3)-0.001))
  loglik4<- prev4* log((pred4)+0.001)+(1-prev4)*log(1-((pred4)-0.001))
  loglik5<- prev5* log((pred5)+0.001)+(1-prev5)*log(1-((pred5)-0.001))
  
  -sum(loglik0,loglik1,loglik2,loglik3,loglik4,loglik5,na.rm=T)
}
n.param<-3
satmod<-optim(c(0.9,0.88,0.57),sat.binom,method="L-BFGS-B",lower=c(0.89,0.87,0.56),upper=c(0.91,0.89,0.58))
satmod

pred<-(0 + satmod$par[1] * real^1)/(satmod$par[3] + satmod$par[2] * real^1) 
lines(real,pred,lwd=2,lty=2,col="blue")

predfrmloggeddataparameters<-(0 + 0.9 * real^1)/(0.57 + 0.88 * real^1)
lines(real,predfrmloggeddataparameters,lwd=2,lty=2,col="green")
