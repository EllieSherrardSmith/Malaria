###########################################################################################
##
## DRUG MODEL: OZPIP
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

score0<- spors$ScorePerBite[spors$ScorePerBite == 0 & spors$Treatment == "OZPIP"]
score1<- spors$ScorePerBite[spors$ScorePerBite > 0 & spors$ScorePerBite<1  & spors$Treatment == "OZPIP"]
score2<- spors$ScorePerBite[spors$ScorePerBite >= 1 & spors$ScorePerBite<2  & spors$Treatment == "OZPIP"]
score3<- spors$ScorePerBite[spors$ScorePerBite >= 2 & spors$ScorePerBite<3  & spors$Treatment == "OZPIP"]
score4<- spors$ScorePerBite[spors$ScorePerBite >= 3 & spors$ScorePerBite<4  & spors$Treatment == "OZPIP"]

logScoreD<-c(mean(score0),mean(score1),mean(score2),mean(score3),max(spors$ScorePerBite[spors$Treatment == "OZPIP"]),max(spors$ScorePerBite[spors$Treatment == "OZPIP"]))


oocysts<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\UCT OZPIP FER DSM Feb2015\\M2M SporozoiteScores\\OocystIntensity.txt",header=TRUE)
OZPIP<-subset(oocysts,Treatment=="OZPIP")

oocdataD1<-sort(OZPIP$oocysts[OZPIP$oocysts > 0])
length(oocdataD1)/5
oocD1<-c(0,mean(oocdataD1[1:15]),mean(oocdataD1[16:30]),mean(oocdataD1[31:45]),mean(oocdataD1[46:59]),mean(oocdataD1[60:73]))


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
  
  
  pred1<- (alpha * oocD1[1]^beta)/(delta + gamma * oocD1[1]^beta)
  pred2<- (alpha * oocD1[2]^beta)/(delta + gamma * oocD1[2]^beta)
  pred3<- (alpha * oocD1[3]^beta)/(delta + gamma * oocD1[3]^beta)  
  pred4<- (alpha * oocD1[4]^beta)/(delta + gamma * oocD1[4]^beta)  
  pred5<- (alpha * oocD1[5]^beta)/(delta + gamma * oocD1[5]^beta)  
  pred6<- (alpha * oocD1[6]^beta)/(delta + gamma * oocD1[6]^beta)  
  
  #a <- p.vec[1]
  #b <- p.vec[2]
  
  #pred1<- ((exp(a + b * oocD1[1])) / (1 + exp(a + b * oocD1[1])) )  
  #pred2<- ((exp(a + b * oocD1[2])) / (1 + exp(a + b * oocD1[2])) )  
  #pred3<- ((exp(a + b * oocD1[3])) / (1 + exp(a + b * oocD1[3])) )  
  #pred4<- ((exp(a + b * oocD1[4])) / (1 + exp(a + b * oocD1[4])) )  
  #pred5<- ((exp(a + b * oocD1[5])) / (1 + exp(a + b * oocD1[5])) ) 
  #pred6<- ((exp(a + b * oocD1[6])) / (1 + exp(a + b * oocD1[6])) ) 
  
  ##a<-p.vec[1]
  ##b<-p.vec[2]
  ##ch<-p.vec[3]
  
  ##pred1<- (a * exp (b * exp(ch * (oocD1[1:6]))))  
  ##spors1<-logScoreD[1:6]
  
  spors1<-logScoreD[1]
  spors2<-logScoreD[2]
  spors3<-logScoreD[3]
  spors4<-logScoreD[4]
  spors5<-logScoreD[5]
  
  
  loglik1<- spors1* log((pred1)+0.001)+(1-spors1)*log(1-((pred1)-0.001))
  loglik2<- spors2* log((pred2)+0.001)+(1-spors2)*log(1-((pred2)-0.001))
  loglik3<- spors3* log((pred3)+0.001)+(1-spors3)*log(1-((pred3)-0.001))
  loglik4<- spors4* log((pred4)+0.001)+(1-spors4)*log(1-((pred4)-0.001))
  loglik5<- spors5* log((pred5)+0.001)+(1-spors5)*log(1-((pred5)-0.001))
  
  -sum(loglik1,loglik2,loglik3,loglik4,loglik5,na.rm=T)
  #-loglik1
}
#n.param<-2
#satmod2<-optim(c(0,0),sat.binom,method="L-BFGS-B",lower=c(-100,-100),upper=c(100,100))
#satmod2

##n.param<-3
##satmod2<-optim(c(0,0,0),sat.binom,method="L-BFGS-B",lower=c(-10,-10,-10),upper=c(10,10,10))
##satmod2

n.param<-4
satmod2<-optim(c(4,0.999,10,0.99),sat.binom,method="L-BFGS-B",lower=c(-10,0.6,1,0.8),upper=c(100,0.9999,100,0.9999))
satmod2

par(mfrow=c(1,1))

nc<-seq(0,max(oocdataD1),1)
pred<-(satmod2$par[1] * nc^satmod2$par[2])/(satmod2$par[3] + satmod2$par[4] * nc^satmod2$par[2])
#pred2<-((exp(satmod2$par[1]  + satmod2$par[2]  * nc)) / (1 + exp(satmod2$par[1]  + satmod2$par[2]  * nc)) ) 
plot(oocD1,logScoreD,ylim=c(0,5),bty="n",xlim=c(0,300),
     las=1,xlab="Oocysts",ylab="Sporozoites",cex=1.25,col="chartreuse4",pch=16)
lines(nc,pred2,lwd=2,col="red")

########################################
##
###
#### sPOROS TO BLOODSTAGE PREVALENCE
###
##
########################################


prev1<-sum(spors$bsprev[spors$Treatment=="OZPIP" & spors$ScorePerBite == "0"])/length(spors$bsprev[spors$Treatment=="OZPIP" & spors$ScorePerBite == "0"])
prev2<-sum(spors$bsprev[spors$Treatment=="OZPIP" & spors$ScorePerBite == "1"])/length(spors$bsprev[spors$Treatment=="OZPIP" & spors$ScorePerBite == "1"])
prev3<-sum(spors$bsprev[spors$Treatment=="OZPIP" & spors$ScorePerBite == "2"])/length(spors$bsprev[spors$Treatment=="OZPIP" & spors$ScorePerBite == "2"])
prev4<-sum(spors$bsprev[spors$Treatment=="OZPIP" & spors$ScorePerBite == "3"])/length(spors$bsprev[spors$Treatment=="OZPIP" & spors$ScorePerBite == "3"])
prev5<-sum(spors$bsprev[spors$Treatment=="OZPIP" & spors$ScorePerBite == "4"])/length(spors$bsprev[spors$Treatment=="OZPIP" & spors$ScorePerBite == "4"])
prevMouseData<-c(0,prev1,prev2,prev3,prev4,1)


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
  pred1<- ((exp(a + b * logScoreD[1])) / (1 + exp(a + b * logScoreD[1])) )  
  pred2<- ((exp(a + b * logScoreD[2])) / (1 + exp(a + b * logScoreD[2])) )  
  pred3<- ((exp(a + b * logScoreD[3])) / (1 + exp(a + b * logScoreD[3])) )  
  pred4<- ((exp(a + b * logScoreD[4])) / (1 + exp(a + b * logScoreD[4])) )  
  pred5<- ((exp(a + b * logScoreD[5])) / (1 + exp(a + b * logScoreD[5])) )  
  pred6<- ((exp(a + b * logScoreD[6])) / (1 + exp(a + b * logScoreD[6])) ) 
  
  prev1<-prevMouseData[1]
  prev2<-prevMouseData[2]
  prev3<-prevMouseData[3]
  prev4<-prevMouseData[4]
  prev5<-prevMouseData[5]
  prev6<-prevMouseData[6]
  
  #loglik0<- prev0* log((pred0)+0.001)+(1-prev0)*log(1-((pred0)-0.001))
  loglik1<- prev1* log((pred1)+0.001)+(1-prev1)*log(1-((pred1)-0.001))
  loglik2<- prev2* log((pred2)+0.001)+(1-prev2)*log(1-((pred2)-0.001))
  loglik3<- prev3* log((pred3)+0.001)+(1-prev3)*log(1-((pred3)-0.001))
  loglik4<- prev4* log((pred4)+0.001)+(1-prev4)*log(1-((pred4)-0.001))
  loglik5<- prev5* log((pred5)+0.001)+(1-prev5)*log(1-((pred5)-0.001))
  loglik6<- prev6* log((pred6)+0.001)+(1-prev6)*log(1-((pred6)-0.001))
  
  -sum(loglik1,loglik2,loglik3,loglik4,loglik5,loglik6,na.rm=T)
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
OZPIPs<-subset(oocysts,Treatment=="OZPIP")

oocdataD1<-sort(OZPIPs$oocysts[OZPIPs$oocysts > 0])
length(oocdataD1)/5
ooc

ooc.binom<-function(p.vec){
  
  alpha <- p.vec[1]
  beta  <- p.vec[2]
  delta <- p.vec[3]
  gamma <- p.vec[4]
  a <- p.vec[5]
  b <- p.vec[6]
  
  
  fitooc1 <- ((exp(a + b * ((alpha * ooc[1]^beta)/(delta + gamma * ooc[1]^beta)))) / (1 + exp(a + b * ((alpha * ooc[1]^beta)/(delta + gamma * ooc[1]^beta)))) )
  fitooc2 <- ((exp(a + b * ((alpha * ooc[2]^beta)/(delta + gamma * ooc[2]^beta)))) / (1 + exp(a + b * ((alpha * ooc[2]^beta)/(delta + gamma * ooc[2]^beta)))) )
  fitooc3 <- ((exp(a + b * ((alpha * ooc[3]^beta)/(delta + gamma * ooc[3]^beta)))) / (1 + exp(a + b * ((alpha * ooc[3]^beta)/(delta + gamma * ooc[3]^beta)))) )
  fitooc4 <- ((exp(a + b * ((alpha * ooc[4]^beta)/(delta + gamma * ooc[4]^beta)))) / (1 + exp(a + b * ((alpha * ooc[4]^beta)/(delta + gamma * ooc[4]^beta)))) )
  fitooc5 <- ((exp(a + b * ((alpha * ooc[6]^beta)/(delta + gamma * ooc[6]^beta)))) / (1 + exp(a + b * ((alpha * ooc[6]^beta)/(delta + gamma * ooc[6]^beta)))) )
  
  prev1<-sum(spors$bsprev[spors$Treatment=="OZPIP" & spors$ScorePerBite == "0"])/length(spors$bsprev[spors$Treatment=="OZPIP" & spors$ScorePerBite == "0"])
  prev2<-sum(spors$bsprev[spors$Treatment=="OZPIP" & spors$ScorePerBite == "1"])/length(spors$bsprev[spors$Treatment=="OZPIP" & spors$ScorePerBite == "1"])
  prev3<-sum(spors$bsprev[spors$Treatment=="OZPIP" & spors$ScorePerBite == "2"])/length(spors$bsprev[spors$Treatment=="OZPIP" & spors$ScorePerBite == "2"])
  prev4<-sum(spors$bsprev[spors$Treatment=="OZPIP" & spors$ScorePerBite == "3"])/length(spors$bsprev[spors$Treatment=="OZPIP" & spors$ScorePerBite == "3"])
  prev5<-sum(spors$bsprev[spors$Treatment=="OZPIP" & spors$ScorePerBite == "4"])/length(spors$bsprev[spors$Treatment=="OZPIP" & spors$ScorePerBite == "4"])
  
  
  
  loglik2<- fitooc1* log((prev1)+0.001)+(1-fitooc1)*log(1-((prev1)-0.001))
  loglik3<- fitooc1* log((prev2)+0.001)+(1-fitooc1)*log(1-((prev2)-0.001))
  loglik4<- fitooc1* log((prev3)+0.001)+(1-fitooc1)*log(1-((prev3)-0.001))
  loglik5<- fitooc1* log((prev4)+0.001)+(1-fitooc1)*log(1-((prev4)-0.001))
  loglik6<- fitooc1* log((prev5)+0.001)+(1-fitooc1)*log(1-((prev5)-0.001))
  
  
  -sum(loglik2,loglik3,loglik4,loglik5,loglik6,na.rm=T)
}
n.param<-6
oocmod<-optim(c(2.5140557, 0.7033866,10.0908338,0.9997296,-10,8.2261),ooc.binom,method="L-BFGS-B",
              lower=c(1,0.5,5,0.5,-13,-10),
              upper=c(10,0.9999,50,0.999,-6,10))
oocmod

fitdatD2<-seq(0,max(oocdataD1),1)
predD2<-((exp(oocmod$par[5] + oocmod$par[6] * ((oocmod$par[1] * fitdatD2^oocmod$par[2])/(oocmod$par[3] + oocmod$par[4] * fitdatD2^oocmod$par[2])))) / 
           (1 + exp(oocmod$par[5] + oocmod$par[6] * ((oocmod$par[1] * fitdatD2^oocmod$par[2])/(oocmod$par[3] + oocmod$par[4] * fitdatD2^oocmod$par[2])))) )


plot(ooc,prevMouseData,ylim=c(0,1),bty="n",xlim=c(0,200),las=1,xlab="Oocyst intensity",ylab="Prevalence blood stage infection",cex=1.25,col="chartreuse4",pch=16)
lines(fitdatD2,predD2,lwd=2,col="red",lty=2)
