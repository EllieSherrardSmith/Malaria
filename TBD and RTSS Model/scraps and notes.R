

## Logistic fit
#

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1<- ((exp(a + b * logScore[1:6])) / (1 + exp(a + b * logScore[1:6])) )  
  pred2<- ((exp(a + b * logScoreTC[1:6])) / (1 + exp(a + b * logScoreTC[1:6])) )  
  #pred3<- ((exp(a + b * data.mouse.a$ScorePerBite[data.mouse.a$rm.prev > 0])) / (1 + exp(a + b * data.mouse.a$ScorePerBite[data.mouse.a$rm.prev > 0])) )  
  predFER<- ((exp(a + b * logScoreFER[1:6])) / (1 + exp(a + b * logScoreFER[1:6])) )  
  predDSM<- ((exp(a + b * c(logScoreDSM[1:4],2.075,2.075))) / (1 + exp(a + b * c(logScoreDSM[1:4],2.075,2.075))) )  
  predPIP<- ((exp(a + b * logScorePIP[1:6])) / (1 + exp(a + b * logScorePIP[1:6])) )  
  predUCT<- ((exp(a + b * c(logScoreUCT[1:4],2.04,2.04))) / (1 + exp(a + b * c(logScoreUCT[1:4],2.04,2.04))) )  
  
  
  prev1<-prevBLANK[1:6]
  prev2<-prevMouseDataTC[1:6]
  #prev3<-data.mouse.a$meanPar[data.mouse.a$rm.prev > 0]
  prevFER1<-prevFER
  prevDSM1<-prevDSM
  prevPIP1<-prevPIP
  prevUCT1<-prevUCT
  
  loglik1<- prev1* log((pred1)+0.00001)+(1-prev1)*log(1-((pred1)-0.00001))
  loglik2<- prev2* log((pred2)+0.00001)+(1-prev2)*log(1-((pred2)-0.00001))
  #loglik3<- prev3* log((pred3)+0.00001)+(1-prev3)*log(1-((pred3)-0.00001))
  loglikf<- prevFER1* log((predFER)+0.00001)+(1-prevFER1)*log(1-((predFER)-0.00001))
  loglikd<- prevDSM1* log((predDSM)+0.00001)+(1-prevDSM1)*log(1-((predDSM)-0.00001))
  loglikp<- prevPIP1* log((predPIP)+0.00001)+(1-prevPIP1)*log(1-((predPIP)-0.00001))
  logliku<- prevUCT1* log((predUCT)+0.00001)+(1-prevUCT1)*log(1-((predUCT)-0.00001))
  
  
  -sum(
    loglik1,loglik2,
    #loglik3,
    loglikf,
    loglikd,
    loglikp,
    logliku,
    na.rm=T)
}
nc<-seq(0,4,0.01)

n.param<-2


logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,0),upper=c(0,10))
logmod
pred<-(exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc))

logmodfer<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,0),upper=c(0,10))
logmodfer
predfer<-(exp(logmodfer$par[1] + logmodfer$par[2] * nc)) / (1 + exp(logmodfer$par[1] + logmodfer$par[2] * nc))

logmoddsm<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-100,-100),upper=c(100,100))
logmoddsm
preddsm<-(exp(logmoddsm$par[1] + logmoddsm$par[2] * nc)) / (1 + exp(logmoddsm$par[1] + logmoddsm$par[2] * nc))

logmodpip<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,0),upper=c(10,10))
logmodpip
predpip<-(exp(logmodpip$par[1] + logmodpip$par[2] * nc)) / (1 + exp(logmodpip$par[1] + logmodpip$par[2] * nc))

logmoduct<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmoduct
preduct<-(exp(logmoduct$par[1] + logmoduct$par[2] * nc)) / (1 + exp(logmoduct$par[1] + logmoduct$par[2] * nc))



plot(logScore[1:5],prevBLANK,ylim=c(0,1),bty="n",xlim=c(0,4),las=1,xlab="Sporozoite Score",ylab="Prevalence blood stage infection",cex=1.25,col="chartreuse4",pch=16)
points(logScoreTC,prevMouseDataTC)
#points(data.mouse.a$ScorePerBite[data.mouse.a$rm.prev > 0],data.mouse.a$meanPar[data.mouse.a$rm.prev > 0],pch=20)

points(logScoreFER[1:6],prevFER,pch=20,col="blue")
points(c(logScoreDSM[1:4],2.075,2.075),prevDSM,pch=20,col="red")
points(logScorePIP[1:6],prevPIP,pch=20,col="green")
points(c(logScoreUCT[1:4],2.04,2.04),prevUCT,pch=20,col="orange")


lines(nc,pred,lwd=2)

lines(nc,predfer,lwd=2,col="red")
lines(nc,preddsm,lwd=2,col="blue")
lines(nc,predpip,lwd=2,col="green")
lines(nc,preduct,lwd=2,col="orange")


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
  fitooc2 <- ((exp(a + b * ((alpha * oocFER[1:6]^beta)/(delta + gamma * oocFER[1:6]^beta)))) / (1 + exp(a + b * ((alpha * oocFER[1:6]^beta)/(delta + gamma * oocFER[1:6]^beta)))) )
  fitooc3 <- ((exp(a + b * ((alpha * oocDSM[1:6]^beta)/(delta + gamma * oocDSM[1:6]^beta)))) / (1 + exp(a + b * ((alpha * oocDSM[1:6]^beta)/(delta + gamma * oocDSM[1:6]^beta)))) )
  fitooc4 <- ((exp(a + b * ((alpha * oocPIP[1:6]^beta)/(delta + gamma * oocPIP[1:6]^beta)))) / (1 + exp(a + b * ((alpha * oocPIP[1:6]^beta)/(delta + gamma * oocPIP[1:6]^beta)))) )
  fitooc5 <- ((exp(a + b * ((alpha * oocUCT[1:6]^beta)/(delta + gamma * oocUCT[1:6]^beta)))) / (1 + exp(a + b * ((alpha * oocUCT[1:6]^beta)/(delta + gamma * oocUCT[1:6]^beta)))) )
  
  
  prev1<-prevMouseData[1:6]
  prevFER1<-prevFER
  prevDSM1<-prevDSM
  prevPIP1<-prevPIP
  prevUCT1<-prevUCT
  
  loglik1<- fitooc1* log((prev1)+0.001)+(1-fitooc1)*log(1-((prev1)-0.001))
  #loglik2<- fitooc2* log((prev1)+0.001)+(1-fitooc2)*log(1-((prev1)-0.001))
  loglik2<- fitooc2* log((prevFER1)+0.001)+(1-fitooc2)*log(1-((prevFER1)-0.001))
  loglik3<- fitooc3* log((prevDSM1)+0.001)+(1-fitooc3)*log(1-((prevDSM1)-0.001))
  loglik4<- fitooc4* log((prevPIP1)+0.001)+(1-fitooc4)*log(1-((prevPIP1)-0.001))
  loglik5<- fitooc5* log((prevUCT1)+0.001)+(1-fitooc5)*log(1-((prevUCT1)-0.001))
  
  -sum(
    loglik1,
    loglik2,
    loglik3,
    loglik4,
    loglik5,
    na.rm=T)
}
n.param<-6
oocmod<-optim(c(3.4999292,0.99999,23.6197325,0.9000661,-2.991983,2.603804),ooc.binom,method="L-BFGS-B", ###These are for the BLANK data
              lower=c(3.49,0.9,23,0.9,-3,2.6),
              upper=c(3.5,0.999999,26,0.92,-2.98,2.61))

oocmodFER<-optim(c(3.5,0.99999318,24.7097148,0.9,-1.423906,2.294481),ooc.binom,method="L-BFGS-B", ###These are for the FER data
                 lower=c(3.4,0.9,24,0.9,-2,2.2),
                 upper=c(3.6,0.9999999,26,0.92,-1,2.3))

oocmodDSM<-optim(c(3.4938832,0.9796118,15.0012052,0.9039049,-2.184231,3.322265),ooc.binom,method="L-BFGS-B", ###These are for the DSM data
                 lower=c(3.4,0.9,14,0.9,-3,3.2),
                 upper=c(3.8,0.98,16,0.92,-2,3.61))

oocmodPIP<-optim(c(3.4999387,0.999999,22.3078003,0.9000538,0.2597978,1.2328052),ooc.binom,method="L-BFGS-B", ###These are for the PIP data
                 lower=c(3.4,0.9,22,0.9,0,1.2),
                 upper=c(3.8,0.99999999,23,0.92,0.98,2))

oocmodUCT<-optim(c(3.4909695,0.9490303,25.0010236,0.9062446,-0.2114807,2.2849596),ooc.binom,method="L-BFGS-B", ###These are for the UCT data
                 lower=c(3.4,0.9,24,0.9,-1,2.2),
                 upper=c(3.8,0.95,26,0.92,0,2.3))
#n.param<-2
#oocmod2<-optim(c(-4.5,0.185),ooc.binom,method="L-BFGS-B",lower=c(-2.5,0.1),upper=c(0,1))

oocmod

fitdat<-seq(0,max(oocdata1),1)
pred<-((exp(oocmod$par[5] + oocmod$par[6] * ((oocmod$par[1] * fitdat^oocmod$par[2])/(oocmod$par[3] + oocmod$par[4] * fitdat^oocmod$par[2])))) / 
         (1 + exp(oocmod$par[5] + oocmod$par[6] * ((oocmod$par[1] * fitdat^oocmod$par[2])/(oocmod$par[3] + oocmod$par[4] * fitdat^oocmod$par[2])))) )

predFERc<-((exp(oocmodFER$par[5] + oocmodFER$par[6] * ((oocmodFER$par[1] * fitdat^oocmodFER$par[2])/(oocmodFER$par[3] + oocmodFER$par[4] * fitdat^oocmodFER$par[2])))) / 
             (1 + exp(oocmodFER$par[5] + oocmodFER$par[6] * ((oocmodFER$par[1] * fitdat^oocmodFER$par[2])/(oocmodFER$par[3] + oocmodFER$par[4] * fitdat^oocmodFER$par[2])))) )

predDSMc<-((exp(oocmodDSM$par[5] + oocmodDSM$par[6] * ((oocmodDSM$par[1] * fitdat^oocmodDSM$par[2])/(oocmodDSM$par[3] + oocmodDSM$par[4] * fitdat^oocmodDSM$par[2])))) / 
             (1 + exp(oocmodDSM$par[5] + oocmodDSM$par[6] * ((oocmodDSM$par[1] * fitdat^oocmodDSM$par[2])/(oocmodDSM$par[3] + oocmodDSM$par[4] * fitdat^oocmodDSM$par[2])))) )

predPIPc<-((exp(oocmodPIP$par[5] + oocmodPIP$par[6] * ((oocmodPIP$par[1] * fitdat^oocmodPIP$par[2])/(oocmodPIP$par[3] + oocmodPIP$par[4] * fitdat^oocmodPIP$par[2])))) / 
             (1 + exp(oocmodPIP$par[5] + oocmodPIP$par[6] * ((oocmodPIP$par[1] * fitdat^oocmodPIP$par[2])/(oocmodPIP$par[3] + oocmodPIP$par[4] * fitdat^oocmodPIP$par[2])))) )

predUCTc<-((exp(oocmodUCT$par[5] + oocmodUCT$par[6] * ((oocmodUCT$par[1] * fitdat^oocmodUCT$par[2])/(oocmodUCT$par[3] + oocmodUCT$par[4] * fitdat^oocmodUCT$par[2])))) / 
             (1 + exp(oocmodUCT$par[5] + oocmodUCT$par[6] * ((oocmodUCT$par[1] * fitdat^oocmodUCT$par[2])/(oocmodUCT$par[3] + oocmodUCT$par[4] * fitdat^oocmodUCT$par[2])))) )

par(mfrow=c(1,1))
plot(ooc[1:6],prevMouseData,ylim=c(0,1),bty="n",xlim=c(0,200),las=1,xlab="Oocyst intensity",ylab="Prevalence blood stage infection",cex=1.25,col="chartreuse4",pch=16)
lines(fitdat,pred,lwd=2)

lines(fitdat,predFERc,lwd=2,col="red")
lines(fitdat,predDSMc,lwd=2,col="blue")
lines(fitdat,predPIPc,lwd=2,col="green")
lines(fitdat,predUCTc,lwd=2,col="orange")


############################################################################
##
###
####
##### Create data with the proportion of cases at each level... 
####
###
###
################################################################################

##e.g. for blank
##fewer sporozoites cause infection...
##for example, after logScore = 2 then all sporozoites cause infection. 



sort(blanks$oocysts)
predblank2<-numeric(length(blanks$oocysts))
for (i in 1:length(blanks$oocysts))
{
  predblank2[i]<-pred[blanks$oocysts][i]
}
plot(sort(predblank2))
lines(sort(predblank2))

sort(OZFER$oocysts)
OZFER2<-numeric(length(OZFER$oocysts))
for (i in 1:length(OZFER$oocysts))
{
  OZFER2[i]<-predFERc[OZFER$oocysts][i]
}
lines(sort(OZFER2),col="red")
