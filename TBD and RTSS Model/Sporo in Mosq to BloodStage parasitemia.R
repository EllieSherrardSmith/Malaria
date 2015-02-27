###########################################################
##
###
####  Parasitemia data
###
##
###########################################################

## Formatting

data.mouse.a=read.table("D:\\IMPERIAL Nov 2014\\Malaria Modelling\\DATA\\ParasiteAllMASTER5.txt",header=TRUE)
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

score1<- data.mouse.a$ScorePerBite[data.mouse.a$ScorePerBite >= 0 & data.mouse.a$ScorePerBite<1]
score2<- data.mouse.a$ScorePerBite[data.mouse.a$ScorePerBite >= 1 & data.mouse.a$ScorePerBite<2]
score3<- data.mouse.a$ScorePerBite[data.mouse.a$ScorePerBite >= 2 & data.mouse.a$ScorePerBite<3]
score4<- data.mouse.a$ScorePerBite[data.mouse.a$ScorePerBite >= 3 & data.mouse.a$ScorePerBite<4]
score5<- data.mouse.a$ScorePerBite[data.mouse.a$ScorePerBite >= 4]
logScore<-c(0,mean(score1),mean(score2),mean(score3),mean(score4),mean(score5))

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
parasitMouseData<-c(0,parasit1,parasit2,parasit3,parasit4,parasit5)

#
## Logistic fit
#

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred0<- ((exp(a + b * 0) / (1 + exp(a + b * 0)) ) )
  pred1<- ((exp(a + b * logScore[2])) / (1 + exp(a + b * logScore[2])) )  
  pred2<- ((exp(a + b * logScore[3])) / (1 + exp(a + b * logScore[3])) )  
  pred3<- ((exp(a + b * logScore[4])) / (1 + exp(a + b * logScore[4])) )  
  pred4<- ((exp(a + b * logScore[5])) / (1 + exp(a + b * logScore[5])) )  
  pred5<- ((exp(a + b * logScore[6])) / (1 + exp(a + b * logScore[6])) )  
  
  
  prev0=sum(0)/length(0)
  prev1<-sum(data.mouse.a$parasitMouseData[data.mouse.a$ScoreType==0])/length(data.mouse.a$parasitMouseData[data.mouse.a$ScoreType==0])
  prev2<-sum(data.mouse.a$parasitMouseData[data.mouse.a$ScoreType==1])/length(data.mouse.a$parasitMouseData[data.mouse.a$ScoreType==1])
  prev3<-sum(data.mouse.a$parasitMouseData[data.mouse.a$ScoreType==2])/length(data.mouse.a$parasitMouseData[data.mouse.a$ScoreType==2])
  prev4<-sum(data.mouse.a$parasitMouseData[data.mouse.a$ScoreType==3])/length(data.mouse.a$parasitMouseData[data.mouse.a$ScoreType==3])
  prev5<-sum(data.mouse.a$parasitMouseData[data.mouse.a$ScoreType==4])/length(data.mouse.a$parasitMouseData[data.mouse.a$ScoreType==4])
  
  loglik0<- prev0* log((pred0)+0.00001)+(1-prev0)*log(1-((pred0)-0.00001))
  loglik1<- prev1* log((pred1)+0.00001)+(1-prev1)*log(1-((pred1)-0.00001))
  loglik2<- prev2* log((pred2)+0.00001)+(1-prev2)*log(1-((pred2)-0.00001))
  loglik3<- prev3* log((pred3)+0.00001)+(1-prev3)*log(1-((pred3)-0.00001))
  loglik4<- prev4* log((pred4)+0.00001)+(1-prev4)*log(1-((pred4)-0.00001))
  loglik5<- prev5* log((pred5)+0.00001)+(1-prev5)*log(1-((pred5)-0.00001))
  
  -sum(loglik0,loglik1,loglik2,loglik3,loglik4,loglik5,na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-100,0.5),upper=c(0,10))
logmod

nc<-seq(0,4,0.01)
pred<-(exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc))
plot(logScore,parasitMouseData,ylim=c(0,1),bty="n",xlim=c(0,4),las=1,xlab="Sporozoite Score",ylab="Parasitemia",cex=1.25,col="chartreuse4",pch=16)
lines(nc,pred,lwd=2)


#
## Saturating fit
#


sat.binom<-function(p.vec){
  
  #a<-p.vec[1]
  b<-p.vec[1]
  d<-p.vec[2]
  ch<-p.vec[3]
  
  pred0<- (b * logScore[1]^ch)/(1 + d * logScore[1]^ch)
  pred1<- (b * logScore[2]^ch)/(1 + d * logScore[2]^ch)  
  pred2<- (b * logScore[3]^ch)/(1 + d * logScore[3]^ch) 
  pred3<- (b * logScore[4]^ch)/(1 + d * logScore[4]^ch)  
  pred4<- (b * logScore[5]^ch)/(1 + d * logScore[5]^ch)  
  pred5<- (b * logScore[6]^ch)/(1 + d * logScore[6]^ch)  
  
  
  prev0=sum(0)/length(0)
  prev1<-sum(data.mouse.a$parasitMouseData[data.mouse.a$ScoreType==0])/length(data.mouse.a$parasitMouseData[data.mouse.a$ScoreType==0])
  prev2<-sum(data.mouse.a$parasitMouseData[data.mouse.a$ScoreType==1])/length(data.mouse.a$parasitMouseData[data.mouse.a$ScoreType==1])
  prev3<-sum(data.mouse.a$parasitMouseData[data.mouse.a$ScoreType==2])/length(data.mouse.a$parasitMouseData[data.mouse.a$ScoreType==2])
  prev4<-sum(data.mouse.a$parasitMouseData[data.mouse.a$ScoreType==3])/length(data.mouse.a$parasitMouseData[data.mouse.a$ScoreType==3])
  prev5<-sum(data.mouse.a$parasitMouseData[data.mouse.a$ScoreType==4])/length(data.mouse.a$parasitMouseData[data.mouse.a$ScoreType==4])
  
  loglik0<- prev0* log((pred0)+0.001)+(1-prev0)*log(1-((pred0)-0.001))
  loglik1<- prev1* log((pred1)+0.001)+(1-prev1)*log(1-((pred1)-0.001))
  loglik2<- prev2* log((pred2)+0.001)+(1-prev2)*log(1-((pred2)-0.001))
  loglik3<- prev3* log((pred3)+0.001)+(1-prev3)*log(1-((pred3)-0.001))
  loglik4<- prev4* log((pred4)+0.001)+(1-prev4)*log(1-((pred4)-0.001))
  loglik5<- prev5* log((pred5)+0.001)+(1-prev5)*log(1-((pred5)-0.001))
  
  -sum(loglik0,loglik1,loglik2,loglik3,loglik4,loglik5,na.rm=T)
}
n.param<-3
satmod<-optim(c(0.3,0.8,1),sat.binom,method="L-BFGS-B",lower=c(0,0.1,0),upper=c(0.99,1,1.8))
satmod

nc<-seq(0,4,0.01)
pred<-(0 + satmod$par[1] * nc^satmod$par[3])/(1 + satmod$par[2] * nc^satmod$par[3]) 
plot(logScore,parasitMouseData,ylim=c(0,1),bty="n",xlim=c(0,4),las=1,xlab="Sporozoite Score",ylab="Parasitemia",cex=1.25,col="chartreuse4",pch=16)
lines(nc,pred,lwd=2,lty=2,col="red")
