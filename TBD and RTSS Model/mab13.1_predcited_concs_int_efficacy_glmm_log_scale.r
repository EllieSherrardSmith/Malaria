#
## Version for estimates just GLMM on log scale, log function (compare with gompertz? generalised logsitic?)
#

require(plotrix)
require(glmmADMB)

intd=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\hand over data and scripts\\predicting concs to achieve intensity efficacies\\mab_13.1_int_data.txt",header=T)

rps<-unique(intd$rep)
concs<-unique(intd$conc)

dsn1<-expand.grid("concs"=concs)
dsnb<-subset(dsn1,dsn1$concs=="blank")
dsn<-subset(dsn1,dsn1$concs!="blank")

dsn$eff.i<-NA
dsn$i.upp<-NA
dsn$i.low<-NA

for(i in 1:length(dsn$concs)){
  
  temp<-subset(intd,intd$conc==dsn$concs[i])
  tempb1<-subset(intd,intd$conc=="blank")
  tempb<-subset(tempb1,match(tempb1$rep,temp$rep)!="NA")
  dtemp<-rbind(tempb,temp)
  dtemp$rpfc<-as.factor(dtemp$rep)
  dtemp$conc<-relevel(dtemp$conc, ref = "blank")
  
  if(length(unique(dtemp$rep))>1.5){
    
    i.glm<-glmmadmb(ooc~conc+(1|rpfc), data=dtemp,zeroInflation=TRUE,family="nbinom")
    dsn$eff.i[i]  <- round((1-exp(coef(i.glm)[2]))  *100,3)
    i.ci<-confint(i.glm)
    dsn$i.low[i] <- round((1-exp(i.ci[2,2]))  *100,3)
    dsn$i.upp[i] <- round((1-exp(i.ci[2,1]))  *100,3)
    
  }else{
    
    i.glm<-glmmadmb(ooc~conc, data=dtemp,zeroInflation=TRUE,family="nbinom")
    dsn$eff.i[i]  <- round((1-exp(coef(i.glm)[2]))  *100,3)
    i.ci<-confint(i.glm)
    dsn$i.low[i] <- round((1-exp(i.ci[2,2]))  *100,3)
    dsn$i.upp[i] <- round((1-exp(i.ci[2,1]))  *100,3)
  }
}

# log scale

dsn$cs<-ifelse(dsn$concs=="25",25,ifelse(dsn$concs=="50",50,ifelse(dsn$concs=="100",100,ifelse(dsn$concs=="150",150,ifelse(dsn$concs=="200",200,ifelse(dsn$concs=="250",250,ifelse(dsn$concs=="350",350,500)))))))
plot(dsn$eff.i~log(dsn$cs),pch=16,cex=1.25,col="chartreuse4",bty="n",main="GLMM estimation",las=1,ylim=c(20,100),xlim=c(3,7),xlab=expression(paste("mAb 13.1 concentration (",mu,"g/mL)")),ylab="intensity efficacy (%)",axes=F)
for(i in 1:length(dsn$eff.i)){
  arrows(log(dsn$cs[i]),dsn$i.upp[i],log(dsn$cs[i]),dsn$i.low[i],length=0,lty=3,col="blue")
}
axis(side=1,at=c(log(20),log(50),log(100),log(200),log(400),log(800)),labels=c(20,50,100,200,400,800))
axis(side=2,at=c(20,40,60,80,100),labels=c(20,40,60,80,100),las=2)
points(dsn$eff.i~log(dsn$cs),pch=16,cex=1.5,col="chartreuse4")

#
## Fit curve
#

log.binom<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  pred1<- (exp(a + b * dsn$cs)) / (1 + exp(a + b * dsn$cs)) 
  data1<- dsn$eff.i/100
  loglik1<- (data1* log(pred1+0.00001)+(1-data1)*log(1-(pred1-0.00001))) *2.4444
  -sum(loglik1,na.rm=T) # the ratio of datapoints being used here and the independent number of unique concentration*replicate combinations
}
n.param<-2
logmod<-optim(c(-0.01,0.1),log.binom,method="L-BFGS-B",lower=c(-1000,0),upper=c(1000,1000))
logmod

nc<-seq(25,500,1)
pred<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)))*100
lines(log(nc),pred,lwd=2)

pd50<- ((log(0.50/(1-0.50)))-logmod$par[1])/logmod$par[2]
pd80<- ((log(0.80/(1-0.80)))-logmod$par[1])/logmod$par[2]
pd50
pd80

points(log(pd50),50,pch=15,col="red",cex=1.5)
points(log(pd80),80,pch=15,col="red",cex=1.5)

#
## repeat, replace data with lower cis
#

log.binom<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  pred1<- (exp(a + b * dsn$cs)) / (1 + exp(a + b * dsn$cs)) 
  data1<- dsn$i.low/100
  loglik1<- (data1* log(pred1+0.00001)+(1-data1)*log(1-(pred1-0.00001))) *2.4444
  -sum(loglik1,na.rm=T) # the ratio of datapoints being used here and the independent number of unique concentration*replicate combinations
}
n.param<-2
logmod<-optim(c(-0.01,0.1),log.binom,method="L-BFGS-B",lower=c(-1000,0),upper=c(1000,1000))
logmod

nc<-seq(25,500,1)
pred<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)))*100
lines(log(nc),pred,lwd=1,lty=3)

lpd50<- ((log(0.50/(1-0.50)))-logmod$par[1])/logmod$par[2]
lpd80<- ((log(0.80/(1-0.80)))-logmod$par[1])/logmod$par[2]
lpd50
lpd80

points(log(lpd50),50,pch=15,col="red",cex=1)
points(log(lpd80),80,pch=15,col="red",cex=1)

#
## repeat, replace data with upper cis
#

log.binom<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  pred1<- (exp(a + b * dsn$cs)) / (1 + exp(a + b * dsn$cs)) 
  data1<- dsn$i.upp/100
  loglik1<- (data1* log(pred1+0.00001)+(1-data1)*log(1-(pred1-0.00001))) *2.4444
  -sum(loglik1,na.rm=T) # the ratio of datapoints being used here and the independent number of unique concentration*replicate combinations
}
n.param<-2
logmod<-optim(c(-0.01,0.1),log.binom,method="L-BFGS-B",lower=c(-1000,0),upper=c(1000,1000))
logmod

nc<-seq(25,500,1)
pred<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)))*100
lines(log(nc),pred,lwd=1,lty=3)

upd50<- ((log(0.50/(1-0.50)))-logmod$par[1])/logmod$par[2]
upd80<- ((log(0.80/(1-0.80)))-logmod$par[1])/logmod$par[2]
upd50
upd80

points(log(upd50),50,pch=15,col="red",cex=1)
points(log(upd80),80,pch=15,col="red",cex=1)

#

arrows(log(20),50,log(lpd50),50,length=0,col="red",lty=3)
arrows(log(upd80),80,log(lpd80),80,length=0,col="red",lty=3)

#

upd50
pd50
lpd50

upd80
pd80
lpd80
