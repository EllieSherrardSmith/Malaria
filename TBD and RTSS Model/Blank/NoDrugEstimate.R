###########################################################################################
##
## BLANK MODEL
##
##
###########################################################################################


spors<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\UCT OZPIP FER DSM Feb2015\\M2M SporozoiteScores\\SporoParaIntensity.txt",header=TRUE)
head(spors)

spors$sporozoites<-numeric(length(spors$Round))
for (i in 1:length(spors$Round)){
  spors$sporozoites[i]<-sum(spors[i,5:14],na.rm=TRUE)}

spors$ScorePerBite<-spors$sporozoites/spors$Bites

score0<- spors$ScorePerBite[spors$ScorePerBite == 0 & spors$Treatment == "Blank"]
score1<- spors$ScorePerBite[spors$ScorePerBite > 0 & spors$ScorePerBite<1 & spors$Treatment == "Blank"]
score2<- spors$ScorePerBite[spors$ScorePerBite >= 1 & spors$ScorePerBite<2 & spors$Treatment == "Blank"]
score3<- spors$ScorePerBite[spors$ScorePerBite >= 2 & spors$ScorePerBite<3 & spors$Treatment == "Blank"]
score4<- spors$ScorePerBite[spors$ScorePerBite >= 3 & spors$ScorePerBite<4 & spors$Treatment == "Blank"]

logScore<-c(mean(score0),mean(score1),mean(score2),mean(score3),mean(score4),max(score4))


oocysts<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\UCT OZPIP FER DSM Feb2015\\M2M SporozoiteScores\\OocystIntensity.txt",header=TRUE)
blanks<-subset(oocysts,Treatment=="Blank")

oocdata1<-sort(blanks$oocysts[blanks$oocysts > 0])
length(oocdata1)/5
ooc<-c(0,mean(oocdata1[1:19]),mean(oocdata1[20:38]),mean(oocdata1[38:57]),mean(oocdata1[58:77]),mean(oocdata1[78:96]))


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

  
  pred0<- (alpha * ooc[1]^beta)/(delta + gamma * ooc[1]^beta)
  pred1<- (alpha * ooc[2]^beta)/(delta + gamma * ooc[2]^beta)
  pred2<- (alpha * ooc[3]^beta)/(delta + gamma * ooc[3]^beta)  
  pred3<- (alpha * ooc[4]^beta)/(delta + gamma * ooc[4]^beta)  
  pred4<- (alpha * ooc[5]^beta)/(delta + gamma * ooc[5]^beta)  
  pred5<- (alpha * ooc[6]^beta)/(delta + gamma * ooc[6]^beta)
  
  spors0<-logScore[1]
  spors1<-logScore[2]
  spors2<-logScore[3]
  spors3<-logScore[4]
  spors4<-logScore[5]
  spors5<-logScore[6]
  
  loglik0<- spors0* log((pred0)+0.001)+(1-spors0)*log(1-((pred0)-0.001))
  loglik1<- spors1* log((pred1)+0.001)+(1-spors1)*log(1-((pred1)-0.001))
  loglik2<- spors2* log((pred2)+0.001)+(1-spors2)*log(1-((pred2)-0.001))
  loglik3<- spors3* log((pred3)+0.001)+(1-spors3)*log(1-((pred3)-0.001))
  loglik4<- spors4* log((pred4)+0.001)+(1-spors4)*log(1-((pred4)-0.001))
  loglik5<- spors5* log((pred5)+0.001)+(1-spors5)*log(1-((pred5)-0.001))
  
  -sum(loglik0,loglik1,loglik2,loglik3,loglik4,loglik5,na.rm=T)
}
n.param<-4
satmod<-optim(c(4,0.99,50,0.99),sat.binom,method="L-BFGS-B",lower=c(1,0.8,30,0.750),upper=c(8,0.9999,200,0.9999))
satmod

par(mfrow=c(1,1))

nc<-seq(0,max(oocdata1),1)
pred<-(satmod$par[1] * nc^satmod$par[4])/(satmod$par[3] + satmod$par[2] * nc^satmod$par[4])

plot(ooc,logScore,ylim=c(0,5),bty="n",xlim=c(0,300),
     las=1,xlab="Oocysts",ylab="Sporozoites",cex=1.25,col="chartreuse4",pch=16)
points(ooc,logScore)
lines(nc,pred,lwd=2,col="red")

########################################
##
###
#### sPOROS TO BLOODSTAGE PREVALENCE
###
##
########################################

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
logScore<-c(mean(score0),mean(score1),mean(score2),mean(score3),mean(score4),mean(score5))


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

prev1<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==0])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==0])
prev2<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==1])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==1])
prev3<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==2])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==2])
prev4<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==3])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==3])
prev5<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==4])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==4])
prevMouseData<-c(0,prev1,prev2,prev3,prev4,prev5)


#
## Saturating fit
#


sat.binom<-function(p.vec){
  
  #a<-p.vec[1]
  b<-p.vec[1]
  umb<-p.vec[2]
  charlie<-p.vec[3]
  
  pred0<- (b * logScore[1]^1)/(charlie + umb * logScore[1]^1)
  pred1<- (b * logScore[2]^1)/(charlie + umb * logScore[2]^1)  
  pred2<- (b * logScore[3]^1)/(charlie + umb * logScore[3]^1) 
  pred3<- (b * logScore[4]^1)/(charlie + umb * logScore[4]^1)  
  pred4<- (b * logScore[5]^1)/(charlie + umb * logScore[5]^1)  
  pred5<- (b * logScore[6]^1)/(charlie + umb * logScore[6]^1)  
  
  
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


#######################################
##
###
#### OOCYSTS TO BLOODSTAGE PREVALENCE
###
##
########################################

oocysts<-read.table("D:\\IMPERIAL Nov 2014\\Andrew Blagborough\\12Feb2015_4DrugTreatments\\M2M SporozoiteScores\\OocystIntensity.txt",header=TRUE)
blanks<-subset(oocysts,Treatment=="Blank")

oocdata1<-sort(blanks$oocysts[blanks$oocysts > 0])
length(oocdata1)/5
ooc<-c(0,mean(oocdata1[1:19]),mean(oocdata1[20:38]),mean(oocdata1[38:57]),mean(oocdata1[58:77]),mean(oocdata1[78:96]))

ooc.binom<-function(p.vec){
  
  alpha <- p.vec[1]
  beta  <- p.vec[2]
  delta <- p.vec[3]
  gamma <- p.vec[4]
  charlie <- p.vec[5]
  umb <- p.vec[6]
  
  
  
  fitooc1 <- ((alpha * ooc[1]^beta)/(delta + gamma * ooc[1]^beta))/(charlie + umb * ((alpha * ooc[1]^beta)/(delta + gamma * ooc[1]^beta)))
  fitooc2 <- ((alpha * ooc[2]^beta)/(delta + gamma * ooc[2]^beta))/(charlie + umb * ((alpha * ooc[2]^beta)/(delta + gamma * ooc[2]^beta)))
  fitooc3 <- ((alpha * ooc[3]^beta)/(delta + gamma * ooc[3]^beta))/(charlie + umb * ((alpha * ooc[3]^beta)/(delta + gamma * ooc[3]^beta)))
  fitooc4 <- ((alpha * ooc[4]^beta)/(delta + gamma * ooc[4]^beta))/(charlie + umb * ((alpha * ooc[4]^beta)/(delta + gamma * ooc[4]^beta)))
  fitooc5 <- ((alpha * ooc[5]^beta)/(delta + gamma * ooc[5]^beta))/(charlie + umb * ((alpha * ooc[5]^beta)/(delta + gamma * ooc[5]^beta)))
  fitooc6 <- ((alpha * ooc[6]^beta)/(delta + gamma * ooc[6]^beta))/(charlie + umb * ((alpha * ooc[6]^beta)/(delta + gamma * ooc[6]^beta)))
  
  prev0=sum(0)/length(0)
  prev1<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==0])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==0])
  prev2<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==1])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==1])
  prev3<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==2])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==2])
  prev4<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==3])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==3])
  prev5<-sum(data.mouse.a$rm.prev[data.mouse.a$ScoreType==4])/length(data.mouse.a$rm.prev[data.mouse.a$ScoreType==4])
  
  
  
  loglik1<- fitooc1* log((prev0)+0.001)+(1-fitooc1)*log(1-((prev0)-0.001))
  loglik2<- fitooc1* log((prev1)+0.001)+(1-fitooc1)*log(1-((prev1)-0.001))
  loglik3<- fitooc1* log((prev2)+0.001)+(1-fitooc1)*log(1-((prev2)-0.001))
  loglik4<- fitooc1* log((prev3)+0.001)+(1-fitooc1)*log(1-((prev3)-0.001))
  loglik5<- fitooc1* log((prev4)+0.001)+(1-fitooc1)*log(1-((prev4)-0.001))
  loglik6<- fitooc1* log((prev5)+0.001)+(1-fitooc1)*log(1-((prev5)-0.001))
  
  
  -sum(loglik1,loglik2,loglik3,loglik4,loglik5,loglik6,na.rm=T)
}
n.param<-6
oocmod<-optim(c(6.416,0.95,69.806,0.72,0.65,0.9),ooc.binom,method="L-BFGS-B",
              lower=c(5,0.9,30,0.6,0.4,0.8),
              upper=c(9,0.99,150,0.999,0.8,0.95))
oocmod

fitdat<-seq(0,max(oocdata1),1)
pred<-((oocmod$par[1] * fitdat^oocmod$par[2])/(oocmod$par[3] + oocmod$par[4] * fitdat^oocmod$par[2]))/(oocmod$par[5] + oocmod$par[6] * ((oocmod$par[1] * fitdat^oocmod$par[2])/(oocmod$par[3] + oocmod$par[4] * fitdat^oocmod$par[2])))
plot(ooc,prevMouseData,ylim=c(0,1),bty="n",xlim=c(0,200),las=1,xlab="Oocyst intensity",ylab="Prevalence blood stage infection",cex=1.25,col="chartreuse4",pch=16)
lines(fitdat,pred,lwd=2)
