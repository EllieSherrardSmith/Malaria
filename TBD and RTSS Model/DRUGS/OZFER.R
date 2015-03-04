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

FERscore<-c(score0,score1,score2,score3,score4,score5)
logScoreDFER<-c(mean(score0),mean(score1),mean(score2),mean(score3),max(score4))
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
logScoreLFER<-c(0,ao1[1],ao2[1],ao3[1],ao4[1])
logScoreUFER<-c(0,ao1[2],ao2[2],ao3[2],ao4[2])


oocysts<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\UCT OZPIP FER DSM Feb2015\\M2M SporozoiteScores\\OocystIntensity.txt",header=TRUE)
OZFER<-subset(oocysts,Treatment=="OZFER")

oocdataD1<-sort(OZFER$oocysts[OZFER$oocysts > 0])
length(oocdataD1)/5
oocD1<-c(0,mean(oocdataD1[1:12]),mean(oocdataD1[13:24]),mean(oocdataD1[25:36]),mean(oocdataD1[49:60]))


#######################################
##
###
#### OOCYSTS TO sporos score
###
##
########################################


#
## 
###Logistic fit
#
sat.binomFER<-function(p.vec){
 
alpha  <- p.vec[1]
beta  <- p.vec[2]
delta <- p.vec[3]
gamma <- p.vec[4]
  
  
pred1<- (alpha * sort(sample(OZFER$oocysts,30))^beta)/(delta + gamma * sort(sample(OZFER$oocysts,30))^beta)
pred2<- (alpha * sort(sample(OZFER$oocysts,30))^beta)/(delta + gamma * sort(sample(OZFER$oocysts,30))^beta)
pred3<- (alpha * sort(sample(OZFER$oocysts,30))^beta)/(delta + gamma * sort(sample(OZFER$oocysts,30))^beta)
pred4<- (alpha * sort(sample(OZFER$oocysts,30))^beta)/(delta + gamma * sort(sample(OZFER$oocysts,30))^beta)
pred5<- (alpha * sort(sample(OZFER$oocysts,30))^beta)/(delta + gamma * sort(sample(OZFER$oocysts,30))^beta)

  
###  a <- p.vec[1]
###  b <- p.vec[2]
  
###  pred1<- 3.5 * ((exp(a + b * oocD1[1:6])) / (1 + exp(a + b * oocD1[1:6])) )
  
  spors1<-FERscore
  
  loglik1<- spors1* log((pred1)+0.00001)+(1-spors1)*log(1-((pred1)-0.00001))
loglik2<- spors1* log((pred2)+0.00001)+(1-spors1)*log(1-((pred2)-0.00001))
loglik3<- spors1* log((pred3)+0.00001)+(1-spors1)*log(1-((pred3)-0.00001))
loglik4<- spors1* log((pred4)+0.00001)+(1-spors1)*log(1-((pred4)-0.00001))
loglik5<- spors1* log((pred5)+0.00001)+(1-spors1)*log(1-((pred5)-0.00001))

  -sum(loglik1,loglik2,loglik3,loglik4,loglik5,na.rm=T)
}
n.param<-4
satmod2FER<-optim(c(3.5,0.8,15,0.95),sat.binomFER,method="L-BFGS-B",
            lower=c(0,0.6,10,0.1),
             upper=c(10,0.999999,100,0.99))
satmod2FER

###n.param<-2
###satmod2<-optim(c(-2,0.15),sat.binom,method="L-BFGS-B",lower=c(-10,0),upper=c(10,0.15))
###satmod2

par(mfrow=c(1,1))

nc<-seq(0,max(oocdata1),1)
predFER<-(satmod2FER$par[1] * nc^satmod2FER$par[2])/(satmod2FER$par[3] + satmod2FER$par[4] * nc^satmod2FER$par[2])
###pred2<-3.5*((exp(satmod2$par[1]  + satmod2$par[2]  * nc)) / (1 + exp(satmod2$par[1]  + satmod2$par[2]  * nc)) ) 
plot(sort(sample(OZFER$oocysts,30)),FERscore,ylim=c(0,5),bty="n",xlim=c(0,300),
     las=1,xlab="Oocysts",ylab="Sporozoites",cex=1.25,col="chartreuse4",pch=16)
points(sort(sample(OZFER$oocysts,30)),FERscore,col="chartreuse4",pch=16)


lines(nc,predFER,lwd=2,col="red")



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
prevMouseDataFER<-c(0,prev1,prev2,prev3,1)


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

  pred1<- ((exp(a + b * logScoreDFER[1:5])) / (1 + exp(a + b * logScoreDFER[1:5])) )    
  prev1<-prevMouseDataFER[1:5]

  loglik1<- prev1* log((pred1)+0.001)+(1-prev1)*log(1-((pred1)-0.001))
  
  -sum(loglik1,na.rm=T)
}
n.param<-2
satmod<-optim(c(0,0),sat.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
satmod

nc<-seq(0,4,0.01)
pred1FER<- ((exp(satmod$par[1] + satmod$par[2] * nc)) / (1 + exp(satmod$par[1] + satmod$par[2] * nc)) ) 
#pred<-(0 + satmod$par[1] * nc^1)/(satmod$par[3] + satmod$par[2] * nc^1) 
plot(logScoreDFER,prevMouseDataFER,ylim=c(0,1),bty="n",xlim=c(0,4),las=1,xlab="Sporozoite Score",ylab="Prevalence blood stage infection",cex=1.25,col="chartreuse4",pch=16)
lines(nc,pred1FER,lwd=2,lty=2,col="red")


#######################################
##
###
#### OOCYSTS TO BLOODSTAGE PREVALENCE
###
##
########################################


###Mean model

alpha = 3.5457676
beta = 0.9562184
delta = 14.9907515
gamma = 0.92777794
a <- -3.01062
b <- 2.045771


fitooc1FER <- ((exp(a + b * ((alpha * fitdat^beta)/(delta + gamma * fitdat^beta)))) / 
              (1 + exp(a + b * ((alpha * fitdat^beta)/(delta + gamma * fitdat^beta)))) )

points(fitooc1FER~fitdat,xlim=c(0,120),xlab="Number of oocysts",ylab="Prevalence of blood stage infection",col="red",pch=20)
lines(fitdat,fitooc1FER,lwd=2,col="red")



###Upper model

alpha = 
beta = 
delta = 
gamma = 
a <- 
b <- 

fitooc1Ufer <- ((exp(a + b * ((alpha * fitdat^beta)/(delta + gamma * fitdat^beta)))) / 
               (1 + exp(a + b * ((alpha * fitdat^beta)/(delta + gamma * fitdat^beta)))) )

lines(fitdat,fitooc1Ufer,lwd=2,lty=2)



###Lower model

alpha = 
beta = 
delta = 
gamma = 
a <- 
b <- 


fitooc1Lfer <- ((exp(a + b * ((alpha * fitdat^beta)/(delta + gamma * fitdat^beta)))) / 
               (1 + exp(a + b * ((alpha * fitdat^beta)/(delta + gamma * fitdat^beta)))) )

lines(fitdat,fitooc1Lfer,lwd=2,lty=2)