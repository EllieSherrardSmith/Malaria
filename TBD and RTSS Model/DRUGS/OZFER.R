###########################################################################################
##
## DRUG MODEL: OZFER
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

score0<- spors$ScorePerBite[spors$ScorePerBite == 0 & spors$Treatment == "OZFER"]
score1<- spors$ScorePerBite[spors$ScorePerBite > 0 & spors$ScorePerBite<1  & spors$Treatment == "OZFER"]
score2<- spors$ScorePerBite[spors$ScorePerBite >= 1 & spors$ScorePerBite<2  & spors$Treatment == "OZFER"]
score3<- spors$ScorePerBite[spors$ScorePerBite >= 2 & spors$ScorePerBite<3  & spors$Treatment == "OZFER"]
score4<- spors$ScorePerBite[spors$ScorePerBite >= 3 & spors$ScorePerBite<4  & spors$Treatment == "OZFER"]

logScoreD<-c(mean(score0),mean(score1),mean(score2),mean(score3),mean(score4),max(score4))


oocysts<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\UCT OZPIP FER DSM Feb2015\\M2M SporozoiteScores\\OocystIntensity.txt",header=TRUE)
OZFER<-subset(oocysts,Treatment=="OZFER")

oocdataD1<-sort(OZFER$oocysts[OZFER$oocysts > 0])
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
## 
Logistic fit
#
sat.binom<-function(p.vec){
 
###alpha  <- p.vec[1]
###beta  <- p.vec[2]
###delta <- p.vec[3]
###gamma <- p.vec[4]
  
  
 ###pred1<- (alpha * oocD1[1:6]^beta)/(delta + gamma * oocD1[1:6]^beta)
 
  
  a <- p.vec[1]
  b <- p.vec[2]
  
  pred1<- 3.5 * ((exp(a + b * oocD1[1:6])) / (1 + exp(a + b * oocD1[1:6])) )
  
  spors1<-logScoreD[1:6]
  
  loglik1<- spors1* log((pred1)+0.001)+(1-spors1)*log(1-((pred1)-0.001))
  
  -sum(loglik1,na.rm=T)
}
###n.param<-4
###satmod2<-optim(c(3.5,0.8,15,0.95),sat.binom,method="L-BFGS-B",
###              lower=c(0,0.6,10,0.1),
###              upper=c(10,0.999999,100,0.99))
###satmod2

n.param<-2
satmod2<-optim(c(-2,0.15),sat.binom,method="L-BFGS-B",lower=c(-10,0),upper=c(10,0.15))
satmod2

par(mfrow=c(1,1))

nc<-seq(0,max(oocdata1),1)
###pred<-(satmod2$par[1] * nc^satmod2$par[2])/(satmod2$par[3] + satmod2$par[4] * nc^satmod2$par[2])
pred2<-3.5*((exp(satmod2$par[1]  + satmod2$par[2]  * nc)) / (1 + exp(satmod2$par[1]  + satmod2$par[2]  * nc)) ) 
plot(oocD1,logScoreD,ylim=c(0,5),bty="n",xlim=c(0,300),
     las=1,xlab="Oocysts",ylab="Sporozoites",cex=1.25,col="chartreuse4",pch=16)
lines(nc,pred2,lwd=2,col="red")


a = -2
  b = 0.15
pred1<- 3.5*((exp(a + b * nc)) / (1 + exp(a + b * nc)) )
lines(nc,pred1,lwd=2,col="red")

########################################
##
###
#### sPOROS TO BLOODSTAGE PREVALENCE
###
##
########################################


prev1<-sum(spors$bsprev[spors$Treatment=="OZFER" & spors$ScorePerBite == "0"])/length(spors$bsprev[spors$Treatment=="OZFER" & spors$ScorePerBite == "0"])
prev2<-sum(spors$bsprev[spors$Treatment=="OZFER" & spors$ScorePerBite == "1"])/length(spors$bsprev[spors$Treatment=="OZFER" & spors$ScorePerBite == "1"])
prev3<-sum(spors$bsprev[spors$Treatment=="OZFER" & spors$ScorePerBite == "2"])/length(spors$bsprev[spors$Treatment=="OZFER" & spors$ScorePerBite == "2"])
prev4<-sum(spors$bsprev[spors$Treatment=="OZFER" & spors$ScorePerBite == "3"])/length(spors$bsprev[spors$Treatment=="OZFER" & spors$ScorePerBite == "3"])
prev5<-sum(spors$bsprev[spors$Treatment=="OZFER" & spors$ScorePerBite == "4"])/length(spors$bsprev[spors$Treatment=="OZFER" & spors$ScorePerBite == "4"])
prevMouseData<-c(0,prev1,prev2,prev3,NA,1)


#
## Saturating fit
#


sat.binom<-function(p.vec){
  
  
  #b<-p.vec[1]
  #umb<-p.vec[2]
  #charlie<-p.vec[3]
  
  #pred0<- (b * logScoreD[1:6]^1)/(charlie + umb * logScoreD[1:6]^1)
 
  
  a <- p.vec[1]
  b <- p.vec[2]

  pred1<- ((exp(a + b * logScoreD[1:6])) / (1 + exp(a + b * logScoreD[1:6])) )    
  prev1<-prevMouseData[1:6]

  loglik1<- prev1* log((pred1)+0.001)+(1-prev1)*log(1-((pred1)-0.001))
  
  -sum(loglik1,na.rm=T)
}
n.param<-2
satmod<-optim(c(0,0),sat.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
satmod

nc<-seq(0,4,0.01)
pred1<- ((exp(satmod$par[1] + satmod$par[2] * nc)) / (1 + exp(satmod$par[1] + satmod$par[2] * nc)) ) 
#pred<-(0 + satmod$par[1] * nc^1)/(satmod$par[3] + satmod$par[2] * nc^1) 
plot(logScoreD,prevMouseData,ylim=c(0,1),bty="n",xlim=c(0,4),las=1,xlab="Sporozoite Score",ylab="Prevalence blood stage infection",cex=1.25,col="chartreuse4",pch=16)
lines(nc,pred1,lwd=2,lty=2,col="red")


#######################################
##
###
#### OOCYSTS TO BLOODSTAGE PREVALENCE
###
##
########################################

oocysts<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\UCT OZPIP FER DSM Feb2015\\M2M SporozoiteScores\\OocystIntensity.txt",header=TRUE)
OZFERs<-subset(oocysts,Treatment=="OZFER")

oocdata1<-sort(OZFERs$oocysts[OZFERs$oocysts > 0])
length(oocdata1)/5
oocd<-c(0,mean(oocdata1[1:19]),mean(oocdata1[20:38]),mean(oocdata1[38:57]),mean(oocdata1[58:77]),mean(oocdata1[78:96]))

ooc.binom<-function(p.vec){
  
  beta)
  #a <- p.vec[1]
  #b <- p.vec[2]
  #ac<- p.vec[3]
  #bc<- p.vec[4]
  #fitooc1 <- ((exp(a + b * (3.5 * ((exp(ac + bc * oocD1[1:6])) / (1 + exp(ac + bc * oocD1[1:6])) )))) / 
  #              (1 + exp(a + b * (3.5 * ((exp(ac + bc * oocD1[1:6])) / (1 + exp(ac + bc * oocD1[1:6])) )))) )
  
  
  
alpha  <- p.vec[1]
beta  <- p.vec[2]
delta <- p.vec[3]
gamma <- p.vec[4]
  
  
  fitooc2<- (alpha * oocD1[1:6]^beta)/(delta + gamma * oocD1[1:6]^beta)

  prev1<-prevMouseData[1:6]
  loglik2<- fitooc2* log((prev1)+0.001)+(1-fitooc2)*log(1-((prev1)-0.001))
 
  -sum(loglik2,na.rm=T)
}
n.param<-4
#oocmod<-optim(c(-2.288,0.1444,-3.010623,2.045771),ooc.binom,method="L-BFGS-B",
#              lower=c(-3,0.14,-4,2),
#              upper=c(-2,0.15,-3,2.1))
#oocmod

oocmod<-optim(c(1,0.4,10,0.5),ooc.binom,method="L-BFGS-B",
                lower=c(0,0.4,1,0.1),
                upper=c(1,0.999,100,0.99))
oocmod
fitdatD1<-seq(0,max(oocdata1),1)
#predD1<-((exp(oocmod$par[1] + oocmod$par[2] * (3.5 * ((exp(oocmod$par[3] + oocmod$par[4] * fitdatD1)) / (1 + exp(oocmod$par[3] + oocmod$par[4] * fitdatD1)) )))) / 
#           (1 + exp(oocmod$par[1] + oocmod$par[2] * (3.5 * ((exp(oocmod$par[3] + oocmod$par[4] * fitdatD1)) / (1 + exp(oocmod$par[3] + oocmod$par[4] * fitdatD1)) )))) )
predD2<-(oocmod$par[1] * fitdatD1^oocmod$par[2])/(oocmod$par[3] + oocmod$par[4] * fitdatD1^oocmod$par[2])

plot(ooc,prevMouseData,ylim=c(0,1),bty="n",xlim=c(0,200),las=1,xlab="Oocyst intensity",ylab="Prevalence blood stage infection",cex=1.25,col="chartreuse4",pch=16)
lines(fitdatD1,predD2,lwd=2,col="red")
