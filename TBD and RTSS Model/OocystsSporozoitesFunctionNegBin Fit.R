
oocysts<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\UCT OZPIP FER DSM Feb2015\\M2M SporozoiteScores\\OocystIntensity.txt",header=TRUE)
blanks<-subset(oocysts,Treatment=="Blank")
OZFER<-subset(oocysts,Treatment=="OZFER")
OZDSM<-subset(oocysts,Treatment=="OZDSM")
OZPIP<-subset(oocysts,Treatment=="OZPIP")
UCT<-subset(oocysts,Treatment=="UCT")

oocATV<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ATV25\\mosquito.txt",header=TRUE)
summary(oocATV)

oocATVB1<-c(oocATV$Oocyst[oocATV$Bites==1]);mean(oocATVB1)
oocATVB2<-c(oocATV$Oocyst[oocATV$Bites==2]);mean(oocATVB2)
oocATVB5<-c(oocATV$Oocyst[oocATV$Bites==5]);mean(oocATVB5)
oocATVB10<-c(oocATV$Oocyst[oocATV$Bites==10]);mean(oocATVB10)

OOC<-c(blanks$oocysts,OZFER$oocysts,OZDSM$oocysts,OZPIP$oocysts,UCT$oocysts,oocATV$Oocyst)


oocysts2<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ATV50 and Controls\\atv50_mosquito_final_data_missing10.txt",header=T)
head(oocysts2)
oocContB1<-c(oocysts2$Oocyst[oocysts2$Bites==1])
oocContB2<-c(oocysts2$Oocyst[oocysts2$Bites==2])
oocContB5<-c(oocysts2$Oocyst[oocysts2$Bites==5])

#spors<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\UCT OZPIP FER DSM Feb2015\\M2M SporozoiteScores\\SporoParaIntensity.txt",header=TRUE)
#head(spors)

#sporsB2<-c(spors$Sporozoite1[spors$Treatment=="Blank" & spors$Bites==2],spors$Sporozoite2[spors$Treatment=="Blank" & spors$Bites==2])
#sporsB5<-c(spors$Sporozoite1[spors$Treatment=="Blank" & spors$Bites==5],spors$Sporozoite2[spors$Treatment=="Blank" & spors$Bites==5])
#sporsB10<-c(spors$Sporozoite1[spors$Treatment=="Blank" & spors$Bites==10],spors$Sporozoite2[spors$Treatment=="Blank" & spors$Bites==10])

sporosALL<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\Mouse Data All\\ParasiteAllMASTER5.txt",header=T)
sporsBites1<-c(sporosALL$Sporozoite1[sporosALL$Mosies==1])
sporsBites2<-c(sporosALL$Sporozoite1[sporosALL$Mosies==2],sporosALL$Sporozoite2[sporosALL$Mosies==2])

sporsBites5<-c(sporosALL$Sporozoite1[sporosALL$Mosies==5],sporosALL$Sporozoite2[sporosALL$Mosies==5],
               sporosALL$Sporozoite3[sporosALL$Mosies==5],sporosALL$Sporozoite4[sporosALL$Mosies==5],
               sporosALL$Sporozoite5[sporosALL$Mosies==5])
sporsBites10<-c(sporosALL$Sporozoite1[sporosALL$Mosies==10],sporosALL$Sporozoite2[sporosALL$Mosies==10],
               sporosALL$Sporozoite3[sporosALL$Mosies==10],sporosALL$Sporozoite4[sporosALL$Mosies==10],
               sporosALL$Sporozoite3[sporosALL$Mosies==10],sporosALL$Sporozoite4[sporosALL$Mosies==10],
               sporosALL$Sporozoite3[sporosALL$Mosies==10],sporosALL$Sporozoite4[sporosALL$Mosies==10],
               sporosALL$Sporozoite3[sporosALL$Mosies==10],sporosALL$Sporozoite4[sporosALL$Mosies==10])

sporocounts<-c(0,1,2,3,4)
aBit1sp<-c(sum(ifelse(sporsBites1==0,1,0)),sum(ifelse(sporsBites1==1,1,0)),
          sum(ifelse(sporsBites1==2,1,0)),sum(ifelse(sporsBites1==3,1,0)),
          sum(ifelse(sporsBites1==4,1,0)))
bBit2sp<-c(sum(ifelse(sporsBites2==0,1,0)),sum(ifelse(sporsBites2==1,1,0)),
          sum(ifelse(sporsBites2==2,1,0)),sum(ifelse(sporsBites2==3,1,0)),
          sum(ifelse(sporsBites2==4,1,0)))
cBit5sp<-c(sum(ifelse(sporsBites5==0,1,0)),sum(ifelse(sporsBites5==1,1,0)),
          sum(ifelse(sporsBites5==2,1,0)),sum(ifelse(sporsBites5==3,1,0)),
          sum(ifelse(sporsBites5==4,1,0)))
dBit10sp<-c(sum(ifelse(sporsBites10==0,1,0)),sum(ifelse(sporsBites10==1,1,0)),
          sum(ifelse(sporsBites10==2,1,0)),sum(ifelse(sporsBites10==3,1,0)),
          sum(ifelse(sporsBites10==4,1,0)))
nobite<-c(100,0,0,0,0)
SPORtab<-data.frame(sporocounts,nobite,aBit1sp,bBit2sp,cBit5sp,dBit10sp)
scores<-rep(sporocounts,5);bites<-c(nobite,aBit1sp,bBit2sp,cBit5sp,dBit10sp);group<-c(rep("Nobite",5),rep("aBit1sp",5),rep("bBit2sp",5),rep("cBit5sp",5),rep("dBit10sp",5))
SPORtab2<-data.frame(scores,bites,group)
sportab<-cast(SPORtab2, group ~ scores, mean, value = 'bites')


meanooc<-c(rep(0,100),rep(mean(c(oocContB1,oocATVB1)),length(sporsBites1)),
            rep(mean(c(oocContB2,oocATVB2)),length(sporsBites2[sporsBites2<5])),
            rep(mean(c(oocContB5,oocATVB5)),length(sporsBites5)),
            rep(mean(oocATVB10),length(sporsBites10)))
sporscores<-c(rep(0,100),sporsBites1,sporsBites2[sporsBites2<5],sporsBites5,sporsBites10)
biteref<-c(rep(0,100),rep("Bite1",length(sporsBites1)),rep("Bite2",length(sporsBites2[sporsBites2<5])),rep("Bite5",length(sporsBites5)),
                                                                               rep("Bite10",length(sporsBites10)))
countdata<-data.frame(meanooc,sporscores,biteref)
head(countdata)


plot(sporscores~meanooc)
##spor score distribution is negbin
meanooc2<-c(unique(meanooc))
meanspor<-c(0,mean(sporsBites1),mean(sporsBites2[sporsBites2<5]),mean(sporsBites5),mean(sporsBites10))


plot(meanspor~meanooc2)

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * meanooc2)) / (1 + exp(a + b * meanooc2)) ) 
  prev1<-meanspor  
  loglik1a<- prev1* log((pred1a)+0.0000001)+(1-prev1)*log(1-((pred1a)-0.0000001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2

nc<-seq(0,40,1)
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmod
pred<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) ) 
lines(pred~nc)

#
## Gompertz
#

gom.binom<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  c<-p.vec[3]
  
  pred1<- (a * exp (b * exp(c *  meanooc2)))
  data1<- meanspor
  loglik1<- data1* log((pred1)+0.00001)+(1-data1)*log(1-((pred1)-0.00001))
  -sum(loglik1,na.rm=T)
}
n.param<-3
gommod<-optim(c(0.75,-2,-0.5),gom.binom,method="L-BFGS-B",lower=c(0.5,-10,-5),upper=c(0.99,-1,-0.0001))
gommod

pred2<-   gommod$par[1] * exp (gommod$par[2] * exp(gommod$par[3] * nc))
plot(meanspor~meanooc2)
lines(nc,pred2,col="red",lwd=2)

#####
#####

##############################################################
##
###
#### Fit to the distribution of sporozoites at each score for each biting rate
###
##
###################################################################################
gom.binom<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  c<-p.vec[3]
  
  pred1<- (a * exp (b * exp(c *  meanooc)))
  data1<- sporscores
  loglik1<- data1* log((pred1)+0.00001)+(1-data1)*log(1-((pred1)-0.00001))
  -sum(loglik1,na.rm=T)
}
n.param<-3
gommod<-optim(c(0.75,-2,-0.5),gom.binom,method="L-BFGS-B",lower=c(0.5,-10,-5),upper=c(0.99,-1,-0.0001))
gommod

pred2<-   gommod$par[1] * exp (gommod$par[2] * exp(gommod$par[3] * nc))
plot(meanspor~meanooc2)
lines(nc,pred2,col="red",lwd=2,lty=2)

#
## CIs for estimates from gompertz model
#

optim.model<-gom.binom(gommod$par)

size.of.grid<-100
a.range<-seq(0.5,0.9,length=size.of.grid)
b.range<-seq(-12,-5,length=size.of.grid)
c.range<-seq(-0.7,-0.2,length=size.of.grid)

pds<-expand.grid("a"=a.range,"b"=b.range,"c"=c.range)
pds$modcom<-NA

for(i in 1:length(pds$a)){
  p.vec<-c(pds$a[i],pds$b[i],pds$c[i])
  ci.n.param<-length(p.vec) 
  ci.fit<-gom.binom(p.vec)     
  pds$modcom[i]<-ifelse(1-pchisq(2*(max(ci.fit,optim.model)-min(ci.fit,optim.model)),1) < 0.05,"discard","keep")
  #print(i)
}
pds.new<-subset(pds,pds$fmodcom=="keep")

length(pds$a)
length(pds.new$a)
max(pds.new$a)
min(pds.new$a)
max(pds.new$b)
min(pds.new$b)
max(pds.new$c)
min(pds.new$c)

#
##

  q1a<-quantile(pds.new$a,0.025)
  q2a<-quantile(pds.new$a,0.975)
  q1b<-quantile(pds.new$b,0.025)
  q2b<-quantile(pds.new$b,0.975)
  q1c<-quantile(pds.new$c,0.025)
  q2c<-quantile(pds.new$c,0.975)

}
pred2upper<-   q2a * exp (q2b * exp(q2c * nc))
pred2lower<-   q1a * exp (q1b * exp(q1c * nc))
lines(pred2upper~nc)
lines(pred2lower~nc)


polygon(c(nc, rev(nc)),c(pred2upper,rev(pred2lower)),border=NA, col="chartreuse4")




######
#######Try in Rstan





sportab$oocystmn<-c(meanooc2[2:5],meanooc2[1])
#colnames(sportab)[2]<-"y1"
#colnames(sportab)[3]<-"y2"
#colnames(sportab)[4]<-"y3"
#colnames(sportab)[5]<-"y4"
#colnames(sportab)[6]<-"y5"

head(countdata);sportab

data1<-list(nMBR=5,
            nSporScore=5,
            Y = structure(.Data = c(100,0,0,0,0,71,10,10,5,4,
                      265,41,41,39,13,619,94,133,92,57,
                      1004,100,195,113,78),
            .Dim=c(5,5)),
            nOoc = structure(.Data = c(0.000000,  7.404022, 13.878788, 17.435662, 30.393333),
                            .Dim=c(1,5)))

inits<-list(alpha = -1, beta = -1, alpha2 = -1)

library(MASS)
fitdistr(dBit10sp, "negative binomial") 
fitdistr(dBit10sp, "geometric") 
fitdistr(dBit10sp, "poisson") 

std <- function(x) sd(x)/sqrt(length(x))

#dataDist<-list(
#  x=meanooc2,
#  I=meanspor,
#  sigma=c(std(rep(0,100)),std(sporsBites1),std(sporsBites2[sporsBites2<5]),std(sporsBites5),std(sporsBites10)))

library(rstan)
fit1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\rstanATTEMPTmodel.stan", data=dataDist,
        iter=100, chains=4)

