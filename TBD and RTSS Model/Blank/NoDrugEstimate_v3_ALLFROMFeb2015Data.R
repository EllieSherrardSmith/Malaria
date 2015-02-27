Mollydog29b
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

logScore<-c(mean(score0),mean(score1),mean(score2),mean(score3),mean(score4),max(score4),3,3,3)
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
logScoreL<-c(0,ao1[1],ao2[1],ao3[1],ao4[1],max(score4),3,3,3)
logScoreU<-c(0,ao1[2],ao2[2],ao3[2],ao4[2],max(score4),3,3,3)


oocysts<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\UCT OZPIP FER DSM Feb2015\\M2M SporozoiteScores\\OocystIntensity.txt",header=TRUE)
blanks<-subset(oocysts,Treatment=="Blank")

oocdata1<-sort(blanks$oocysts[blanks$oocysts > 0])
length(oocdata1)/5

ooc<-c(0,mean(oocdata1[1:19]),mean(oocdata1[20:38]),mean(oocdata1[38:57]),mean(oocdata1[58:77]),mean(oocdata1[78:96]),300,310,320)
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
  
  pred0<- (alpha * ooc[1:9]^beta)/(delta + gamma * ooc[1:9]^beta)
  spors0<-logScore[1:9]

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
  
  
  pred0<- (alpha * ooc[1:9]^beta)/(delta + gamma * ooc[1:9]^beta)
  spors0<-logScoreU[1:9]
  
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
  
  
  pred0<- (alpha * ooc[1:9]^beta)/(delta + gamma * ooc[1:9]^beta)
  spors0<-logScoreL[1:9]
    
  loglik0<- spors0* log((pred0)+0.001)+(1-spors0)*log(1-((pred0)-0.001))
 
  -sum(loglik0,na.rm=T)
}
n.param<-4
satmodL<-optim(c(5,0.99,45,0.99),satL.binom,method="L-BFGS-B",lower=c(1,0.8,0,0.00001),upper=c(20,0.9999,200,0.9999))
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


#
## Saturating fit
#


sat.binom<-function(p.vec){

  a <- p.vec[1]
  b <- p.vec[2]

  pred1<- ((exp(a + b * logScore[1:6])) / (1 + exp(a + b * logScore[1:6])) )  
  prev1<-prevMouseData[1:6]
  loglik1<- prev1* log((pred1)+0.001)+(1-prev1)*log(1-((pred1)-0.001))

  
  -sum(loglik1,na.rm=T)
}
n.param<-2
satmod<-optim(c(0,0),sat.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
satmod

nc<-seq(0,4,0.01)
pred1<- ((exp(satmod$par[1] + satmod$par[2] * nc)) / (1 + exp(satmod$par[1] + satmod$par[2] * nc)) ) 
plot(logScore[1:6],prevMouseData,ylim=c(0,1),bty="n",xlim=c(0,4),las=1,xlab="Sporozoite Score",ylab="Prevalence blood stage infection",cex=1.25,col="chartreuse4",pch=16)
lines(nc,pred1,lwd=2,col="red")

satU.binom<-function(p.vec){
  
  a <- p.vec[1]
  b <- p.vec[2]
  
  pred1<- ((exp(a + b * logScoreU[1:6])) / (1 + exp(a + b * logScoreU[1:6])) )  
  prev1<-prevMouseData[1:6]
  loglik1<- prev1* log((pred1)+0.001)+(1-prev1)*log(1-((pred1)-0.001))
  
  
  -sum(loglik1,na.rm=T)
}
n.param<-2
satmodU<-optim(c(0,0),satU.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
satmodU
predU<- ((exp(satmodU$par[1] + satmodU$par[2] * nc)) / (1 + exp(satmodU$par[1] + satmodU$par[2] * nc)) ) 
lines(nc,predU,lwd=2,lty=2,col="red")

satL.binom<-function(p.vec){
  
  a <- p.vec[1]
  b <- p.vec[2]
  
  pred1<- ((exp(a + b * logScoreL[1:6])) / (1 + exp(a + b * logScoreL[1:6])) )  
  prev1<-prevMouseData[1:6]
  loglik1<- prev1* log((pred1)+0.001)+(1-prev1)*log(1-((pred1)-0.001))
  
  
  -sum(loglik1,na.rm=T)
}
n.param<-2
satmodL<-optim(c(0,0),satL.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
satmodL
predL<- ((exp(satmodL$par[1] + satmodL$par[2] * nc)) / (1 + exp(satmodL$par[1] + satmodL$par[2] * nc)) ) 
lines(nc,predL,lwd=2,lty=2,col="red")

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
oocmod<-optim(c(3.7912483,0.9793911,25.0010451,0.9079316,-4.821661,3.695501),ooc.binom,method="L-BFGS-B",
              lower=c(3.7,0.9,25,0.9,-4.9,3.6),
              upper=c(3.8,0.98,26,0.92,-4.7,3.7))
#n.param<-2
#oocmod2<-optim(c(-4.5,0.185),ooc.binom2,method="L-BFGS-B",lower=c(-10,0),upper=c(0,100))

oocmod

fitdat<-seq(0,max(oocdata1),1)
pred<-((exp(oocmod$par[5] + oocmod$par[6] * ((oocmod$par[1] * fitdat^oocmod$par[2])/(oocmod$par[3] + oocmod$par[4] * fitdat^oocmod$par[2])))) / 
         (1 + exp(oocmod$par[5] + oocmod$par[6] * ((oocmod$par[1] * fitdat^oocmod$par[2])/(oocmod$par[3] + oocmod$par[4] * fitdat^oocmod$par[2])))) )

pred2<-((exp(oocmod2$par[1] + oocmod2$par[2] * fitdat)) / (1 + exp(oocmod2$par[1] + oocmod2$par[2] * fitdat)) ) 
par(mfrow=c(1,1))
plot(ooc[1:6],prevMouseData,ylim=c(0,1),bty="n",xlim=c(0,200),las=1,xlab="Oocyst intensity",ylab="Prevalence blood stage infection",cex=1.25,col="chartreuse4",pch=16)
lines(fitdat,pred,lwd=2)



a= -4.5
b=0.185
pred2<-((exp(a + b * fitdat)) / (1 + exp(a + b * fitdat)) ) 
lines(fitdat,pred2,lwd=2,col="red")

alpha = 0.99
beta = 0.8
delta = 15
gamma = 0.99

pred3<-(alpha * fitdat^beta)/(delta + gamma * fitdat^beta)
lines(fitdat,pred3,lwd=2)
