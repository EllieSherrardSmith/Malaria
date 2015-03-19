library(reshape)
oocysts<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\UCT OZPIP FER DSM Feb2015\\M2M SporozoiteScores\\OocystIntensity.txt",header=TRUE)
blanks<-subset(oocysts,Treatment=="Blank");mean(blanks$oocysts)
OZFER<-subset(oocysts,Treatment=="OZFER");mean(OZFER$oocysts)
OZDSM<-subset(oocysts,Treatment=="OZDSM");mean(OZDSM$oocysts)
OZPIP<-subset(oocysts,Treatment=="OZPIP");mean(OZPIP$oocysts)
UCT<-subset(oocysts,Treatment=="UCT");mean(UCT$oocysts)

oocATV<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ATV25\\mosquito.txt",header=TRUE)
summary(oocATV)

oocATVB1<-c(oocATV$Oocyst[oocATV$Bites==1]);mean(oocATVB1)
oocATVB2<-c(oocATV$Oocyst[oocATV$Bites==2]);mean(oocATVB2)
oocATVB5<-c(oocATV$Oocyst[oocATV$Bites==5]);mean(oocATVB5)
oocATVB10<-c(oocATV$Oocyst[oocATV$Bites==10]);mean(oocATVB10)

OOC<-c(blanks$oocysts,OZFER$oocysts,OZDSM$oocysts,OZPIP$oocysts,UCT$oocysts,oocATV$Oocyst)

mean(oocATVB10)

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


###Sporozoites
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

#####Parasitemia
sporosALL$paraprev<-NA
  for (i in 1:length(sporosALL[,1])){
    sporosALL$paraprev[i]<-ifelse(sum(sporosALL[i,14:20],na.rm=T)>0,1,0)}
sporosALL$paraprev
paraBites1<-c(sporosALL$paraprev[sporosALL$Mosies==1])
paraBites2<-c(sporosALL$paraprev[sporosALL$Mosies==2])
paraBites5<-c(sporosALL$paraprev[sporosALL$Mosies==5])
paraBites10<-c(sporosALL$paraprev[sporosALL$Mosies==10])
paras<-c(0,sum(paraBites1)/length(paraBites1),sum(paraBites2)/length(paraBites2),sum(paraBites5)/length(paraBites5),sum(paraBites10)/length(paraBites10))


###Oocysts
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
medianooc<-c(0,median(c(oocContB1,oocATVB1)),median(c(oocContB2,oocATVB2)),
             median(c(oocContB5,oocATVB5)),median(oocATVB10))
meanspor<-c(0,mean(sporsBites1),mean(sporsBites2[sporsBites2<5]),mean(sporsBites5),mean(sporsBites10))

library(beanplot)

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

nc<-seq(0,40,1)

pred2<-   gommod$par[1] * exp (gommod$par[2] * exp(gommod$par[3] * nc))
plot(meanspor~meanooc2,
     ylab="Mean sporozoite score per mosquito biting rate (MBR)",xlab="Mean oocyst count per MBR",
     cex.lab=1.4,cex=1.2)
lines(nc,pred2,col="yellow",lwd=2)

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
plot(meanspor~meanooc2, ylim=c(0,4))
lines(nc,pred2,col="chocolate2",lwd=3,lty=2)
points(meanspor~meanooc2,col="chocolate2",pty=16)
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
pds.new<-subset(pds,pds$modcom=="keep")

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

  q1a<-quantile(pds.new$a,0.025)##0.7868687
  q2a<-quantile(pds.new$a,0.975)##0.8111111
  q1b<-quantile(pds.new$b,0.025)##-11.85859 
  q2b<-quantile(pds.new$b,0.975)##-5.212121
  q1c<-quantile(pds.new$c,0.025)##-0.5686869 
  q2c<-quantile(pds.new$c,0.975)##-0.3818182


pred2upper<-   q1a * exp (q1b * exp(q2c * nc))
pred2lower<-   q2a * exp (q2b * exp(q1c * nc))

pred2upper<-   0.7868687 * exp (-11.85859 * exp(-0.3818182 * nc))
pred2lower<-   0.8111111 * exp (-5.212121* exp(-0.5686869 * nc))

lines(pred2upper~nc)
lines(pred2lower~nc)

beanplot(0,0,0,0,0,0,sporsBites1,0,0,0,0,0,sporsBites2[sporsBites2<5],0,0,0,sporsBites5,
         0,0,0,0,0,0,0,0,0,0,0,0,sporsBites10,xaxt="n",
         ylab="Sporozoite scores",xlab="Number of oocysts")


axis(1,at=seq(from=0, to=30,1),labels=seq(0,30,1))

polygon(c(nc, rev(nc)),c(pred2upper,rev(pred2lower)),border=NA, col="chartreuse4")
lines(nc,pred2,col="yellow",lwd=2)
points(meanspor~meanooc2,col="chocolate2",pch=16)

#############################################
####
#####
###### Try fitting mean oocysts to mean prevalence for each mouse populations 
#####
####
######################################
plot(meanooc2,paras)

log.binom2<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * meanooc2)) / (1 + exp(a + b * meanooc2)) ) 
  prev1<-paras  
  loglik1a<- prev1* log((pred1a)+0.0000001)+(1-prev1)*log(1-((pred1a)-0.0000001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2

nc<-seq(0,150,1)
logmod2<-optim(c(0,0),log.binom2,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmod2
pred<-((exp(logmod2$par[1] + logmod2$par[2] * nc)) / (1 + exp(logmod2$par[1] + logmod2$par[2] * nc)) ) 
lines(pred~nc)

gom.binom2<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  c<-p.vec[3]
  
  pred1<- (a * exp (b * exp(c *  meanooc2)))
  data1<- paras 
  loglik1<- data1* log((pred1)+0.00001)+(1-data1)*log(1-((pred1)-0.00001))
  -sum(loglik1,na.rm=T)
}
n.param<-3
gommod2<-optim(c(0.75,-2,-0.5),gom.binom2,method="L-BFGS-B",lower=c(0.5,-10,-5),upper=c(0.99,-1,-0.0001))
gommod2

pred2g<-   gommod2$par[1] * exp (gommod2$par[2] * exp(gommod2$par[3] * nc))
lines(pred2g~nc)

#
## CIs for estimates from gompertz model
#

optim.model2<-gom.binom2(gommod2$par)

size.of.grid<-100
a.range<-seq(0.5,0.9,length=size.of.grid)
b.range<-seq(-7,-3,length=size.of.grid)
c.range<-seq(-0.5,-0.01,length=size.of.grid)

pds2<-expand.grid("a"=a.range,"b"=b.range,"c"=c.range)
pds2$modcom<-NA

for(i in 1:length(pds2$a)){
  p.vec<-c(pds2$a[i],pds2$b[i],pds2$c[i])
  ci.n.param<-length(p.vec) 
  ci.fit<-gom.binom2(p.vec)     
  pds2$modcom[i]<-ifelse(1-pchisq(2*(max(ci.fit,optim.model2)-min(ci.fit,optim.model2)),1) < 0.05,"discard","keep")
  #print(i)
}
pds2.new<-subset(pds2,pds2$modcom=="keep")

length(pds2$a)
length(pds2.new$a)
max(pds2.new$a)
min(pds2.new$a)
max(pds2.new$b)
min(pds2.new$b)
max(pds2.new$c)
min(pds2.new$c)

#
##

q1a2<-quantile(pds2.new$a,0.025)##0.5080808
q2a2<-quantile(pds2.new$a,0.975)##0.8919192
q1b2<-quantile(pds2.new$b,0.025)##-6.919192 
q2b2<-quantile(pds2.new$b,0.975)##-3.080808
q1c2<-quantile(pds2.new$c,0.025)##-0.490101
q2c2<-quantile(pds2.new$c,0.975)##-0.05949495


pred2lower2<-   q1a2 * exp (q1b2 * exp(q2c2 * nc))
pred2upper2<-   q2a2 * exp (q2b2 * exp(q1c2 * nc))


pred2lower2<-   0.5080808 * exp (-6.919192 * exp(-0.05949495 * nc))
pred2upper2<-   0.8919192 * exp (-3.080808 * exp(-0.490101 * nc))

plot(meanooc2,paras,
     ylim=c(0,1),ylab="Prevalence in mouse population",
     xlim=c(0,30))
lines(pred2g~nc)
lines(pred2upper2~nc)
lines(pred2lower2~nc)


#############################################
##
###Try probability
###
##
#################
bit1ooc<-sort(c(oocContB1,oocATVB1))
bit1spor<-sort(sporsBites1)
length(bit1ooc)

##0 - 441 times
##1 - 13 times
##2 - 11 times
##3 - 12 times
##4 - 1 time
##5 - 2 times
##6 - 4 times

freqooc<-numeric(length(unique(bit1ooc)))
for (i in 1:length(unique(bit1ooc))){ 
  freqooc[i]<-sum(ifelse(bit1ooc==unique(bit1ooc)[i],1,0))}

freqspor<-numeric(length(unique(bit1spor)))
for (i in 1:length(unique(bit1spor))){
  freqspor[i]<-sum(ifelse(bit1spor==unique(bit1spor)[i],1,0))}

propbit1spor<-matrix(nrow=length(freqooc),ncol=length(freqspor),data=NA)
propbit1spor<-as.data.frame(propbit1spor)
colnames(propbit1spor)[1]<-"sporos0"
colnames(propbit1spor)[2]<-"sporos1"
colnames(propbit1spor)[3]<-"sporos2"
colnames(propbit1spor)[4]<-"sporos3"
colnames(propbit1spor)[5]<-"sporos4"
propbit1spor[1,1:5]<-c(freqooc[1],0,0,0,0)

for (j in 2:length(freqooc)){
for (i in 1:length(freqspor)){
propbit1spor[j,i]<-(freqooc[j]*(freqspor[i]/length(bit1spor)))
}}
sum(propbit1spor[1,])

propbit1spor$bit1ooc<-c(unique(bit1ooc))

propbit1spor$sumprob<-numeric(length(propbit1spor$bit1ooc))
for (i in 1:length(propbit1spor$bit1ooc)){
propbit1spor$sumprob[i]<-sum(propbit1spor[i,1:5])}

propbit1spor$MBR<-rep("Bite1",length(propbit1spor$bit1ooc))

propbit1spor$sporf0<-propbit1spor$sporos0/(length(bit1ooc)-freqooc[1]);propbit1spor$sporf0[1]<-NA
propbit1spor$sporf1<-propbit1spor$sporos1/(length(bit1ooc)-freqooc[1])
propbit1spor$sporf2<-propbit1spor$sporos2/(length(bit1ooc)-freqooc[1])
propbit1spor$sporf3<-propbit1spor$sporos3/(length(bit1ooc)-freqooc[1])
propbit1spor$sporf4<-propbit1spor$sporos4/(length(bit1ooc)-freqooc[1])
head(propbit1spor)



bit2ooc<-sort(c(oocContB2,oocATVB2))
bit2spor<-sort(sporsBites2)
length(bit2ooc)

freqooc<-numeric(length(unique(bit2ooc)))
for (i in 1:length(unique(bit2ooc))){ 
  freqooc[i]<-sum(ifelse(bit2ooc==unique(bit2ooc)[i],1,0))}

freqspor<-numeric(length(unique(bit2spor)))
for (i in 1:length(unique(bit2spor))){
  freqspor[i]<-sum(ifelse(bit2spor==unique(bit2spor)[i],1,0))}
freqspor<-freqspor[1:5]
propbit2spor<-matrix(nrow=length(freqooc),ncol=length(freqspor),data=NA)
propbit2spor<-as.data.frame(propbit2spor)
colnames(propbit2spor)[1]<-"sporos0"
colnames(propbit2spor)[2]<-"sporos1"
colnames(propbit2spor)[3]<-"sporos2"
colnames(propbit2spor)[4]<-"sporos3"
colnames(propbit2spor)[5]<-"sporos4"

propbit2spor[1,1:5]<-c(freqooc[1],0,0,0,0)

for (j in 2:length(freqooc)){
  for (i in 1:length(freqspor)){
    propbit2spor[j,i]<-(freqooc[j]*(freqspor[i]/length(bit2spor)))
  }}
sum(propbit2spor)

propbit2spor$bit2ooc<-c(unique(bit2ooc))

propbit2spor$sumprob2<-numeric(length(propbit2spor$bit2ooc))
for (i in 1:length(propbit2spor$bit2ooc)){
  propbit2spor$sumprob2[i]<-sum(propbit2spor[i,1:5])}

propbit2spor$MBR<-rep("Bite2",length(propbit2spor$bit2ooc))
colnames(propbit2spor)[6]<-"nooc"
colnames(propbit2spor)[7]<-"sumprob"
colnames(propbit1spor)[6]<-"nooc"

propbit2spor$sporf0<-propbit2spor$sporos0/(length(bit2ooc)-freqooc[1]);propbit2spor$sporf0[1]<-NA
propbit2spor$sporf1<-propbit2spor$sporos1/(length(bit2ooc)-freqooc[1])
propbit2spor$sporf2<-propbit2spor$sporos2/(length(bit2ooc)-freqooc[1])
propbit2spor$sporf3<-propbit2spor$sporos3/(length(bit2ooc)-freqooc[1])
propbit2spor$sporf4<-propbit2spor$sporos4/(length(bit2ooc)-freqooc[1])
head(propbit2spor)



bit5ooc<-sort(c(oocContB5,oocATVB5))
bit5spor<-sort(sporsBites5)
length(bit5ooc)

freqooc<-numeric(length(unique(bit5ooc)))
for (i in 1:length(unique(bit5ooc))){ 
  freqooc[i]<-sum(ifelse(bit5ooc==unique(bit5ooc)[i],1,0))}

freqspor<-numeric(length(unique(bit5spor)))
for (i in 1:length(unique(bit5spor))){
  freqspor[i]<-sum(ifelse(bit5spor==unique(bit5spor)[i],1,0))}
freqspor<-freqspor[1:5]
propbit5spor<-matrix(nrow=length(freqooc),ncol=length(freqspor),data=NA)
propbit5spor<-as.data.frame(propbit5spor)
colnames(propbit5spor)[1]<-"sporos0"
colnames(propbit5spor)[2]<-"sporos1"
colnames(propbit5spor)[3]<-"sporos2"
colnames(propbit5spor)[4]<-"sporos3"
colnames(propbit5spor)[5]<-"sporos4"

propbit5spor[1,1:5]<-c(freqooc[1],0,0,0,0)

for (j in 2:length(freqooc)){
  for (i in 1:length(freqspor)){
    propbit5spor[j,i]<-(freqooc[j]*(freqspor[i]/length(bit5spor)))
  }}
sum(propbit5spor)

propbit5spor$bit5ooc<-c(unique(bit5ooc))

propbit5spor$sumprob5<-numeric(length(propbit5spor$bit5ooc))
for (i in 1:length(propbit5spor$bit5ooc)){
  propbit5spor$sumprob5[i]<-sum(propbit5spor[i,1:5])}

propbit5spor$MBR<-rep("Bite5",length(propbit5spor$bit5ooc))
colnames(propbit5spor)[6]<-"nooc"
colnames(propbit5spor)[7]<-"sumprob"

propbit5spor$sporf0<-propbit5spor$sporos0/(length(bit5ooc)-freqooc[1]);propbit5spor$sporf0[1]<-NA
propbit5spor$sporf1<-propbit5spor$sporos1/(length(bit5ooc)-freqooc[1])
propbit5spor$sporf2<-propbit5spor$sporos2/(length(bit5ooc)-freqooc[1])
propbit5spor$sporf3<-propbit5spor$sporos3/(length(bit5ooc)-freqooc[1])
propbit5spor$sporf4<-propbit5spor$sporos4/(length(bit5ooc)-freqooc[1])
head(propbit5spor)


bit10ooc<-sort(c(oocATVB10))
bit10spor<-sort(sporsBites10)
length(bit10ooc)

freqooc<-numeric(length(unique(bit10ooc)))
for (i in 1:length(unique(bit10ooc))){ 
  freqooc[i]<-sum(ifelse(bit10ooc==unique(bit10ooc)[i],1,0))}

freqspor<-numeric(length(unique(bit10spor)))
for (i in 1:length(unique(bit10spor))){
  freqspor[i]<-sum(ifelse(bit10spor==unique(bit10spor)[i],1,0))}
freqspor<-freqspor[1:5]
propbit10spor<-matrix(nrow=length(freqooc),ncol=length(freqspor),data=NA)
propbit10spor<-as.data.frame(propbit10spor)
colnames(propbit10spor)[1]<-"sporos0"
colnames(propbit10spor)[2]<-"sporos1"
colnames(propbit10spor)[3]<-"sporos2"
colnames(propbit10spor)[4]<-"sporos3"
colnames(propbit10spor)[5]<-"sporos4"

propbit10spor[1,1:5]<-c(freqooc[1],0,0,0,0)

for (j in 2:length(freqooc)){
  for (i in 1:length(freqspor)){
    propbit10spor[j,i]<-(freqooc[j]*(freqspor[i]/length(bit10spor)))
  }}
sum(propbit10spor)

propbit10spor$bit10ooc<-c(unique(bit10ooc))
propbit10spor$sumprob10<-numeric(length(propbit10spor$bit10ooc))
for (i in 1:length(propbit10spor$bit10ooc)){
  propbit10spor$sumprob10[i]<-sum(propbit10spor[i,1:5])}


propbit10spor$MBR<-rep("Bites10",length(propbit10spor$bit10ooc))
colnames(propbit10spor)[6]<-"nooc"
colnames(propbit10spor)[7]<-"sumprob"

propbit10spor$sporf0<-propbit10spor$sporos0/(length(bit10ooc)-freqooc[1]);propbit10spor$sporf0[1]<-NA
propbit10spor$sporf1<-propbit10spor$sporos1/(length(bit10ooc)-freqooc[1])
propbit10spor$sporf2<-propbit10spor$sporos2/(length(bit10ooc)-freqooc[1])
propbit10spor$sporf3<-propbit10spor$sporos3/(length(bit10ooc)-freqooc[1])
propbit10spor$sporf4<-propbit10spor$sporos4/(length(bit10ooc)-freqooc[1])
head(propbit10spor)


require(plotrix)
require(glmmADMB)
Alldata<-rbind(propbit1spor,propbit2spor,propbit5spor,propbit10spor)
dim(Alldata)

sporozoites<-stack(Alldata[,1:5])
sporprobs<-stack(Alldata[,9:13])
Sporscores<-c(rep(0,length(propbit1spor[,1])),rep(0,length(propbit2spor[,1])),rep(0,length(propbit5spor[,1])),rep(0,length(propbit10spor[,1])),
              rep(1,length(propbit1spor[,2])),rep(1,length(propbit2spor[,2])),rep(1,length(propbit5spor[,2])),rep(1,length(propbit10spor[,2])),
              rep(2,length(propbit1spor[,3])),rep(2,length(propbit2spor[,3])),rep(2,length(propbit5spor[,3])),rep(2,length(propbit10spor[,3])),
              rep(3,length(propbit1spor[,4])),rep(3,length(propbit2spor[,4])),rep(3,length(propbit5spor[,4])),rep(3,length(propbit10spor[,4])),
              rep(4,length(propbit1spor[,5])),rep(4,length(propbit2spor[,5])),rep(4,length(propbit5spor[,5])), rep(4,length(propbit10spor[,5])))
                     
oocysts<-rep(Alldata$nooc,5)
MBR<-rep(Alldata$MBR,5)
data<-data.frame(sporozoites,sporprobs,Sporscores,oocysts,MBR)
data[301:320,]
colnames(data)[1]<-"sporozoites"
colnames(data)[3]<-"sporprobs"
data$roundspors<-round(data$sporozoites)
head(data)
data2<-subset(data,oocysts!=0)
 
plot(data2$sporprobs[data2$MBR=="Bite1"]~data2$oocysts[data2$MBR=="Bite1"])


glm1<-glmmadmb(cbind(roundspors,547)~oocysts+(1|MBR), data=data,zeroInflation=TRUE,family="nbinom")

#datatemp<-subset(data,ind=="sporos1")

MBRs<-unique(data$MBR)

dsn1<-expand.grid("MBR"=MBRs)
dsnb<-subset(dsn1,dsn1$MBR=="Bite1")
dsn<-subset(dsn1,dsn1$MBR!="Bite1")

dsn1$eff.i<-NA
dsn1$i.upp<-NA
dsn1$i.low<-NA

for(i in 1:4){
  
  temp<-subset(data,data$MBR==MBRs[i])
  tempb1<-subset(data,data$MBR=="Bite1")
  dtemp<-rbind(tempb1,temp)
  
  if(length(unique(dtemp$MBR))>1.5){
    
    i.glm<-glmmadmb(cbind(roundspors,547)~oocysts+(1|MBR), data=dtemp,zeroInflation=TRUE,family="nbinom")
    dsn1$eff.i[i]  <- round((1-exp(coef(i.glm)[2]))  *100,3)
    i.ci<-confint(i.glm)
    dsn1$i.low[i] <- round((1-exp(i.ci[2,2]))  *100,3)
    dsn1$i.upp[i] <- round((1-exp(i.ci[2,1]))  *100,3)
    
  }else{
    
    i.glm<-glmmadmb(cbind(roundspors,547)~oocysts, data=dtemp,zeroInflation=TRUE,family="nbinom")
    dsn1$eff.i[i]  <- round((1-exp(coef(i.glm)[2]))  *100,3)
    i.ci<-confint(i.glm)
    dsn1$i.low[i] <- round((1-exp(i.ci[2,2]))  *100,3)
    dsn1$i.upp[i] <- round((1-exp(i.ci[2,1]))  *100,3)
  }
}

dsn1$meanooc2<-ifelse(dsn1$MBR=="Bite1",7.404022,ifelse(dsn1$MBR=="Bite2",13.878788,ifelse(dsn1$MBR=="Bite5",17.435662,30.393333)))
dsn1

dsn0<-expand.grid("MBR"=0)
dsn0$eff.i<-dsn0$i.upp<-dsn0$i.low<-dsn0$meanooc2<-0
dsn<-rbind(dsn0,dsn1)

plot(dsn$eff.i~dsn$meanooc2,pch=16,cex=1.25,col="chartreuse4",
     bty="n",main="GLMM estimation",las=1,
     ylim=c(0,2),xlim=c(0,36),
     xlab="Number of Oocysts",ylab="Sporozoite score",axes=F)
axis(side=1,at=seq(from=0,to=36,by=1),labels=seq(from=0,to=36,by=1))
axis(side=2,at=seq(0,2,0.5),las=2)


gom.binom2<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  c<-p.vec[3]
  
  pred1<- (a * exp (b * exp(c *  dsn$meanooc2)))
  data1<- dsn$eff.i 
  loglik1<- data1* log((pred1)+0.00001)+(1-data1)*log(1-((pred1)-0.00001))
  -sum(loglik1,na.rm=T)
}
n.param<-3
gommod2<-optim(c(0.75,-2,-0.5),gom.binom2,method="L-BFGS-B",lower=c(0.5,-10,-5),upper=c(0.99,-0.1,-0.0001))
gommod2

pred2g<-   gommod2$par[1] * exp (gommod2$par[2] * exp(gommod2$par[3] * nc))
lines(pred2g~nc)

gom.binom2l<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  c<-p.vec[3]
  
  pred1<- (a * exp (b * exp(c *  dsn$meanooc2)))
  data1<- dsn$i.low 
  loglik1<- data1* log((pred1)+0.00001)+(1-data1)*log(1-((pred1)-0.00001))
  -sum(loglik1,na.rm=T)
}
n.param<-3
gommod2l<-optim(c(0.75,-2,-0.5),gom.binom2l,method="L-BFGS-B",lower=c(0.5,-10,-5),upper=c(0.99,-0.1,-0.0001))
gommod2l

pred2gl<-   gommod2l$par[1] * exp (gommod2l$par[2] * exp(gommod2l$par[3] * nc))
lines(pred2gl~nc)

gom.binom2u<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  c<-p.vec[3]
  
  pred1<- (a * exp (b * exp(c *  dsn$meanooc2)))
  data1<- dsn$i.upp 
  loglik1<- data1* log((pred1)+0.00001)+(1-data1)*log(1-((pred1)-0.00001))
  -sum(loglik1,na.rm=T)
}
n.param<-3
gommod2u<-optim(c(0.75,-2,-0.5),gom.binom2u,method="L-BFGS-B",lower=c(0.5,-10,-0.48),upper=c(0.95,-0.1,-0.46))
gommod2u

pred2gu<-   gommod2u$par[1] * exp (gommod2u$par[2] * exp(gommod2u$par[3] * nc))*1.4
lines(pred2gu~nc)

polygon(c(nc, rev(nc)),c(pred2gu,rev(pred2gl)),border=NA, col="aquamarine1")
lines(pred2g~nc)
arrows(1,pred2gl[2],1,pred2gu[2],length=0,col="chocolate4",lty=3)
arrows(dsn$meanooc2[2],dsn$i.low[2],dsn$meanooc2[2],dsn$i.upp[2],length=0,col="chocolate4",lty=3)
arrows(dsn$meanooc2[3],dsn$i.low[3],dsn$meanooc2[3],dsn$i.upp[3],length=0,col="chocolate4",lty=3)
arrows(dsn$meanooc2[4],dsn$i.low[4],dsn$meanooc2[4],dsn$i.upp[4],length=0,col="chocolate4",lty=3)
arrows(dsn$meanooc2[5],dsn$i.low[5],dsn$meanooc2[5],dsn$i.upp[5],length=0,col="chocolate4",lty=3)

points(dsn$eff.i~dsn$meanooc2,pch=16,cex=1.5,col="chocolate4")

##################################################################
##################################################################

data2$correctedprob<-data2$sporprobs/sum(data2$sporprobs)
head(data2)
plot(data2$correctedprob~data2$oocysts)

##prevalence of sporozoites per oocyst per MBR
##number != sporos0 / total at a given biting rate for 1 oocyst 
a1<-sum(data2$sporozoites[data2$ind!="sporos0" & data2$oocysts==1 & data2$MBR=="Bite1"])/sum(data2$sporozoites[data2$oocysts==1 & data2$MBR=="Bite1"])
a2<-sum(data2$sporozoites[data2$ind!="sporos0" & data2$oocysts==1 & data2$MBR=="Bite2"])/sum(data2$sporozoites[data2$oocysts==1 & data2$MBR=="Bite2"])
a3<-sum(data2$sporozoites[data2$ind!="sporos0" & data2$oocysts==1 & data2$MBR=="Bite5"])/sum(data2$sporozoites[data2$oocysts==1 & data2$MBR=="Bite5"])
a4<-sum(data2$sporozoites[data2$ind!="sporos0" & data2$oocysts==1 & data2$MBR=="Bites10"])/sum(data2$sporozoites[data2$oocysts==1 & data2$MBR=="Bites10"])
sdat<-c(a1,a2,a3,a4) ##proportion of sporozoite infection per oocysts given 1, 2, 5 or 10 MBR 

probdata<-matrix(nrow=length(unique(data2$oocysts)),ncol=4)
probdata<-as.data.frame(probdata)
probdata$oocysts<-unique(data2$oocysts)
colnames(probdata)[1]<-"Bite1";colnames(probdata)[2]<-"Bite2";colnames(probdata)[3]<-"Bite5";colnames(probdata)[4]<-"BiteS10"
for (i in 1:length(unique(data2$oocysts))){
  probdata[i,1]<-  1 - (1-sdat[1])^unique(data2$oocysts)[i]
  probdata[i,2]<-  1 - (1-sdat[2])^unique(data2$oocysts)[i]
  probdata[i,3]<-  1 - (1-sdat[3])^unique(data2$oocysts)[i]
  probdata[i,4]<-  1 - (1-sdat[4])^unique(data2$oocysts)[i]
}
head(probdata)


##prevalence of 1 sporozoites per oocyst per MBR
a1<-sum(data2$sporozoites[data2$ind=="sporos1" & data2$oocysts==1 & data2$MBR=="Bite1"])/sum(data2$sporozoites[data2$oocysts==1 & data2$MBR=="Bite1"])
a2<-sum(data2$sporozoites[data2$ind=="sporos1" & data2$oocysts==1 & data2$MBR=="Bite2"])/sum(data2$sporozoites[data2$oocysts==1 & data2$MBR=="Bite2"])
a3<-sum(data2$sporozoites[data2$ind=="sporos1" & data2$oocysts==1 & data2$MBR=="Bite5"])/sum(data2$sporozoites[data2$oocysts==1 & data2$MBR=="Bite5"])
a4<-sum(data2$sporozoites[data2$ind=="sporos1" & data2$oocysts==1 & data2$MBR=="Bites10"])/sum(data2$sporozoites[data2$oocysts==1 & data2$MBR=="Bites10"])
s1dat<-c(a1,a2,a3,a4)

a<-sum(data2$sporozoites[data2$ind=="sporos2" & data2$oocysts==1 & data2$MBR=="Bite1"])/sum(data2$sporozoites[data2$oocysts==1 & data2$MBR=="Bite1"])
b<-sum(data2$sporozoites[data2$ind=="sporos2" & data2$oocysts==1 & data2$MBR=="Bite2"])/sum(data2$sporozoites[data2$oocysts==1 & data2$MBR=="Bite2"])
c<-sum(data2$sporozoites[data2$ind=="sporos2" & data2$oocysts==1 & data2$MBR=="Bite5"])/sum(data2$sporozoites[data2$oocysts==1 & data2$MBR=="Bite5"])
d<-sum(data2$sporozoites[data2$ind=="sporos2" & data2$oocysts==1 & data2$MBR=="Bites10"])/sum(data2$sporozoites[data2$oocysts==1 & data2$MBR=="Bites10"])
s2dat<-c(a,b,c,d)
#s2datcum<-s1dat+s2dat

f<-sum(data2$sporozoites[data2$ind=="sporos3" & data2$oocysts==1 & data2$MBR=="Bite1"])/sum(data2$sporozoites[data2$oocysts==1 & data2$MBR=="Bite1"])
g<-sum(data2$sporozoites[data2$ind=="sporos3" & data2$oocysts==1 & data2$MBR=="Bite2"])/sum(data2$sporozoites[data2$oocysts==1 & data2$MBR=="Bite2"])
h<-sum(data2$sporozoites[data2$ind=="sporos3" & data2$oocysts==1 & data2$MBR=="Bite5"])/sum(data2$sporozoites[data2$oocysts==1 & data2$MBR=="Bite5"])
j<-sum(data2$sporozoites[data2$ind=="sporos3" & data2$oocysts==1 & data2$MBR=="Bites10"])/sum(data2$sporozoites[data2$oocysts==1 & data2$MBR=="Bites10"])
s3dat<-c(f,g,h,j)
#s3datcum<-s3dat+s2datcum

a<-sum(data2$sporozoites[data2$ind=="sporos4" & data2$oocysts==1 & data2$MBR=="Bite1"])/sum(data2$sporozoites[data2$oocysts==1 & data2$MBR=="Bite1"])
b<-sum(data2$sporozoites[data2$ind=="sporos4" & data2$oocysts==1 & data2$MBR=="Bite2"])/sum(data2$sporozoites[data2$oocysts==1 & data2$MBR=="Bite2"])
c<-sum(data2$sporozoites[data2$ind=="sporos4" & data2$oocysts==1 & data2$MBR=="Bite5"])/sum(data2$sporozoites[data2$oocysts==1 & data2$MBR=="Bite5"])
d<-sum(data2$sporozoites[data2$ind=="sporos4" & data2$oocysts==1 & data2$MBR=="Bites10"])/sum(data2$sporozoites[data2$oocysts==1 & data2$MBR=="Bites10"])
s4dat<-c(a,b,c,d)
#s4datcum<-s4dat+s3datcum;head(s4datcum);head(sdat)

max3ormore<-s4dat+s3dat
max2ormore<-max3ormore+s2dat
max1ormore<-max2ormore+s1dat

probdata4<-matrix(nrow=length(unique(data2$oocysts)),ncol=4)
probdata4<-as.data.frame(probdata4)
probdata4$oocysts<-unique(data2$oocysts)
colnames(probdata4)[1]<-"Bite1";colnames(probdata4)[2]<-"Bite2";colnames(probdata4)[3]<-"Bite5";colnames(probdata4)[4]<-"BiteS10"
for (i in 1:length(unique(data2$oocysts))){
  probdata4[i,1]<-  1 - (1-s4dat[1])^unique(data2$oocysts)[i]
  probdata4[i,2]<-  1 - (1-s4dat[2])^unique(data2$oocysts)[i]
  probdata4[i,3]<-  1 - (1-s4dat[3])^unique(data2$oocysts)[i]
  probdata4[i,4]<-  1 - (1-s4dat[4])^unique(data2$oocysts)[i]
}
head(probdata4)

probdata3<-matrix(nrow=length(unique(data2$oocysts)),ncol=4)
probdata3<-as.data.frame(probdata3)
probdata3$oocysts<-unique(data2$oocysts)
colnames(probdata3)[1]<-"Bite1";colnames(probdata3)[2]<-"Bite2";colnames(probdata3)[3]<-"Bite5";colnames(probdata3)[4]<-"BiteS10"
for (i in 1:length(unique(data2$oocysts))){
  probdata3[i,1]<-  1 - (1-max3ormore[1])^unique(data2$oocysts)[i]
  probdata3[i,2]<-  1 - (1-max3ormore[2])^unique(data2$oocysts)[i]
  probdata3[i,3]<-  1 - (1-max3ormore[3])^unique(data2$oocysts)[i]
  probdata3[i,4]<-  1 - (1-max3ormore[4])^unique(data2$oocysts)[i]
}
head(probdata3)

probdata2<-matrix(nrow=length(unique(data2$oocysts)),ncol=4)
probdata2<-as.data.frame(probdata2)
probdata2$oocysts<-unique(data2$oocysts)
colnames(probdata2)[1]<-"Bite1";colnames(probdata2)[2]<-"Bite2";colnames(probdata2)[3]<-"Bite5";colnames(probdata2)[4]<-"BiteS10"
for (i in 1:length(unique(data2$oocysts))){
  probdata2[i,1]<-  1 - (1-max2ormore[1])^unique(data2$oocysts)[i]
  probdata2[i,2]<-  1 - (1-max2ormore[2])^unique(data2$oocysts)[i]
  probdata2[i,3]<-  1 - (1-max2ormore[3])^unique(data2$oocysts)[i]
  probdata2[i,4]<-  1 - (1-max2ormore[4])^unique(data2$oocysts)[i]
}
head(probdata2)

probdata1<-matrix(nrow=length(unique(data2$oocysts)),ncol=4)
probdata1<-as.data.frame(probdata1)
probdata1$oocysts<-unique(data2$oocysts)
colnames(probdata1)[1]<-"Bite1";colnames(probdata1)[2]<-"Bite2";colnames(probdata1)[3]<-"Bite5";colnames(probdata1)[4]<-"BiteS10"
for (i in 1:length(unique(data2$oocysts))){
  probdata1[i,1]<-  1 - (1-max1ormore[1])^unique(data2$oocysts)[i]
  probdata1[i,2]<-  1 - (1-max1ormore[2])^unique(data2$oocysts)[i]
  probdata1[i,3]<-  1 - (1-max1ormore[3])^unique(data2$oocysts)[i]
  probdata1[i,4]<-  1 - (1-max1ormore[4])^unique(data2$oocysts)[i]
}
head(probdata1)

plot(c(0,probdata[1:20,1])~c(0,probdata[1:20,5]),xlab="Number of oocysts (logscale)",ylab="Probability of sporozoites")
lines(c(0,probdata[1:20,1])~c(0,probdata[1:20,5]))
lines(c(0,probdata[1:20,2])~c(0,probdata[1:20,5]))
lines(c(0,probdata[1:20,3])~c(0,probdata[1:20,5]))
lines(c(0,probdata[1:20,4])~c(0,probdata[1:20,5]))

points(c(0,probdata1[1:20,1])~c(0,probdata1[1:20,5]))
lines(c(0,probdata1[1:20,1])~c(0,probdata1[1:20,5]),lty=2)
lines(c(0,probdata1[1:20,2])~c(0,probdata1[1:20,5]),lty=2)
lines(c(0,probdata1[1:20,3])~c(0,probdata1[1:20,5]),lty=2)
lines(c(0,probdata1[1:20,4])~c(0,probdata1[1:20,5]),lty=2)

points(c(0,probdata2[1:20,1])~c(0,probdata2[1:20,5]))
lines(c(0,probdata2[1:20,1])~c(0,probdata2[1:20,5]),lty=3)
lines(c(0,probdata2[1:20,2])~c(0,probdata2[1:20,5]),lty=3)
lines(c(0,probdata2[1:20,3])~c(0,probdata2[1:20,5]),lty=3)
lines(c(0,probdata2[1:20,4])~c(0,probdata2[1:20,5]),lty=3)

points(c(0,probdata3[1:20,1])~c(0,probdata3[1:20,5]))
lines(c(0,probdata3[1:20,1])~c(0,probdata3[1:20,5]),lty=4)
lines(c(0,probdata3[1:20,2])~c(0,probdata3[1:20,5]),lty=4)
lines(c(0,probdata3[1:20,3])~c(0,probdata3[1:20,5]),lty=4)
lines(c(0,probdata3[1:20,4])~c(0,probdata3[1:20,5]),lty=4)

lines(c(0,probdata4[1:20,1])~c(0,probdata4[1:20,5]),lty=5)
lines(c(0,probdata4[1:20,2])~c(0,probdata4[1:20,5]),lty=5)
lines(c(0,probdata4[1:20,3])~c(0,probdata4[1:20,5]),lty=5)
lines(c(0,probdata4[1:20,4])~c(0,probdata4[1:20,5]),lty=5)

par(mfrow=c(2,2))
plot(c(0,probdata[1:20,1])~c(0,probdata[1:20,5]),
     main="MBR = 1 Bite",
     xlab="Number of oocysts",ylab="Probability of sporozoites")
lines(c(0,probdata[1:20,1])~c(0,probdata[1:20,5]))
polygon(c(c(0,probdata[1:20,5]), rev(c(0,probdata[1:20,5]))),c(c(0,probdata1[1:20,1]),rev(c(0,probdata2[1:20,1]))),border=NA, 
        col = terrain.colors(10,alpha = 0.25))
polygon(c(c(0,probdata[1:20,5]), rev(c(0,probdata[1:20,5]))),c(c(0,probdata1[1:20,1]),rev(c(0,probdata3[1:20,1]))),border=NA, 
        col = terrain.colors(15,alpha = 0.25))
polygon(c(c(0,probdata[1:20,5]), rev(c(0,probdata[1:20,5]))),c(c(0,probdata1[1:20,1]),rev(c(0,probdata4[1:20,1]))),border=NA, 
        col = terrain.colors(25,alpha = 0.25))

#arrows(5,probdata1[5,1],5,probdata4[5,1],length=0,col="chocolate4",lty=3)
#arrows(10,probdata1[10,1],10,probdata4[10,1],length=0,col="chocolate4",lty=3)
#arrows(20,probdata1[16,1],20,probdata4[20,1],length=0,col="chocolate4",lty=3)
#

plot(c(0,probdata[1:20,2])~c(0,probdata[1:20,5]),
     main="MBR = 2 Bites",
     xlab="Number of oocysts",ylab="Probability of sporozoites")
lines(c(0,probdata[1:20,2])~c(0,probdata[1:20,5]))
polygon(c(c(0,probdata[1:20,5]), rev(c(0,probdata[1:20,5]))),c(c(0,probdata1[1:20,2]),rev(c(0,probdata2[1:20,2]))),border=NA, 
        col = terrain.colors(10,alpha = 0.25))
polygon(c(c(0,probdata[1:20,5]), rev(c(0,probdata[1:20,5]))),c(c(0,probdata1[1:20,2]),rev(c(0,probdata3[1:20,2]))),border=NA, 
        col = terrain.colors(15,alpha = 0.25))
polygon(c(c(0,probdata[1:20,5]), rev(c(0,probdata[1:20,5]))),c(c(0,probdata1[1:20,2]),rev(c(0,probdata4[1:20,2]))),border=NA, 
        col = terrain.colors(25,alpha = 0.25))

plot(c(0,probdata[1:20,3])~c(0,probdata[1:20,5]),
     main="MBR = 5 Bites",
     xlab="Number of oocysts",ylab="Probability of sporozoites")
lines(c(0,probdata[1:20,3])~c(0,probdata[1:20,5]))
polygon(c(c(0,probdata[1:20,5]), rev(c(0,probdata[1:20,5]))),c(c(0,probdata1[1:20,3]),rev(c(0,probdata2[1:20,3]))),border=NA, 
        col = terrain.colors(10,alpha = 0.25))
polygon(c(c(0,probdata[1:20,5]), rev(c(0,probdata[1:20,5]))),c(c(0,probdata1[1:20,3]),rev(c(0,probdata3[1:20,3]))),border=NA, 
        col = terrain.colors(15,alpha = 0.25))
polygon(c(c(0,probdata[1:20,5]), rev(c(0,probdata[1:20,5]))),c(c(0,probdata1[1:20,3]),rev(c(0,probdata4[1:20,3]))),border=NA, 
        col = terrain.colors(25,alpha = 0.25))

plot(c(0,probdata[1:20,4])~c(0,probdata[1:20,5]),
     main="MBR = 10 Bites",
     xlab="Number of oocysts",ylab="Probability of sporozoites")
lines(c(0,probdata[1:20,4])~c(0,probdata[1:20,5]))
polygon(c(c(0,probdata[1:20,5]), rev(c(0,probdata[1:20,5]))),c(c(0,probdata1[1:20,4]),rev(c(0,probdata2[1:20,4]))),border=NA, 
        col = terrain.colors(10,alpha = 0.25))
polygon(c(c(0,probdata[1:20,5]), rev(c(0,probdata[1:20,5]))),c(c(0,probdata1[1:20,4]),rev(c(0,probdata3[1:20,4]))),border=NA, 
        col = terrain.colors(15,alpha = 0.25))
polygon(c(c(0,probdata[1:20,5]), rev(c(0,probdata[1:20,5]))),c(c(0,probdata1[1:20,4]),rev(c(0,probdata4[1:20,4]))),border=NA, 
        col = terrain.colors(25,alpha = 0.25))

probdata1[1:20,]
probdata4[1:20,]


####################################################################################################
###
####
###### Fit a function between sporozoite scores and parasitemia (using Tom's master copy)
#####
####
###
##
###################################################################################################


head(sporosALL)
for (i in 1:length(sporosALL$Sum)){
sporosALL$paramean[i]<-sum(sporosALL[i,14:20],na.rm=T)/length(which(sporosALL[i,14:20]!="NA"))
}


sporozoites<-stack(sporosALL[1:10])
sporozoites$paraint<-rep(sporosALL$paramean,10)
sporozoites$paraprev<-rep(sporosALL$paraprev,10)
sporozoites$mbr<-rep(sporosALL$Mosies)
head(sporozoites)
datBS<-subset(sporozoites,values!=5)
dataBS1<-subset(datBS,mbr==1 | mbr==2 | mbr==5 | mbr==10)
summary(dataBS1)

meanspsc<-c(0,mean(dataBS1$value[dataBS1$mbr==1]),
                 mean(dataBS1$value[dataBS1$mbr==2]),
                      mean(dataBS1$value[dataBS1$mbr==5]),
                           mean(dataBS1$value[dataBS1$mbr==10]))
meanparapr<-c(0,mean(dataBS1$paraprev[dataBS1$mbr==1]),
            mean(dataBS1$paraprev[dataBS1$mbr==2]),
            mean(dataBS1$paraprev[dataBS1$mbr==5]),
            mean(dataBS1$paraprev[dataBS1$mbr==10]))
meanparaint<-c(0,mean(dataBS1$paraint[dataBS1$mbr==1]),
              mean(dataBS1$paraint[dataBS1$mbr==2]),
              mean(dataBS1$paraint[dataBS1$mbr==5]),
              mean(dataBS1$paraint[dataBS1$mbr==10]))

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * meanspsc)) / (1 + exp(a + b * meanspsc)) ) 
  prev1<-meanparapr
  loglik1a<- prev1* log((pred1a)+0.0000001)+(1-prev1)*log(1-((pred1a)-0.0000001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2

nc<-seq(0,4,0.01)
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmod
plot(meanparapr~meanspsc,xlim=c(0,4),ylim=c(0,1))
pred<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) ) 
lines(pred~nc)

gom.binom<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  c<-p.vec[3]
  
  pred1<- (a * exp (b * exp(c *  meanspsc)))
  data1<- meanparapr
  loglik1<- data1* log((pred1)+0.00001)+(1-data1)*log(1-((pred1)-0.00001))
  -sum(loglik1,na.rm=T)
}
n.param<-3
gommod<-optim(c(0.75,-2,-0.5),gom.binom,method="L-BFGS-B",lower=c(0.5,-10,-5),upper=c(0.99,-1,-0.0001))
gommod

pred2<-   gommod$par[1] * exp (gommod$par[2] * exp(gommod$par[3] * nc))
plot(meanparapr~meanspsc, ylim=c(0,1),xlim=c(0,4))
points(meanparapr~meanspsc,col="chocolate2",pch=16)


#####
#####
#####
#####
#####
#####
#####
#####

##from (White et al. 2010) per MBR = 1
##salivary gland sporozoites follow geometric distribution
##mean 
n<-mean(sporsBites1)
    n2n<-mean(sporsBites2)
    n5n<-mean(sporsBites5)
    n10n<-mean(sporsBites10)


##probability of a single oocyst result in sporozoite scores > 1 from a single bite
s<-(100 - 71)/sum(aBit1sp) 
    s<-(399 - 265)/sum(bBit2sp) 
    s<-(995 - 619)/sum(cBit5sp) 
    s<-(1490 - 1004)/sum(dBit10sp) 

x<-seq(1,100,1)
probx<-1-(1-s)^x

##if distribution of sporozoite scores per oocyst follow a geometric distribution  with mean n
##then probability that x oocysts cause sporozoite scores of salivary glands is
x2<-x-1
n2<-1/n
    n2<-1/n2n
    n2<-1/n5n
    n2<-1/n10n

probsp<-((1 - n2)^x2)
probsp1<-probsp*n2

##probability that sporozoites will be in salivary gland
b<-sum(probsp1*probx)
b

prob1<-c(0,0.1994588,0.2729667,0.3453175,0.2704364)
plot(prob1~meanooc2,ylab="Probability that oocysts cause salivary gland infection",ylim=c(0,0.8))

gom.binom<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  c<-p.vec[3]
  
  pred1<- (a * exp (b * exp(c *  meanooc2)))
  data1<- prob1
  loglik1<- data1* log((pred1)+0.00001)+(1-data1)*log(1-((pred1)-0.00001))
  -sum(loglik1,na.rm=T)
}
n.param<-3
gommod<-optim(c(0.35,-2,-0.5),gom.binom,method="L-BFGS-B",lower=c(0.2,-10,-5),upper=c(0.4,-1,-0.0001))
gommod

nc<-seq(0,40,1)
pred2<-   gommod$par[1] * exp (gommod$par[2] * exp(gommod$par[3] * nc))
lines(pred2~nc,col="chocolate2",pch=16)

#
## CIs for estimates from gompertz model
#

optim.model2<-gom.binom(gommod$par)

size.of.grid<-100
a.range<-seq(0.1,0.6,length=size.of.grid)
b.range<-seq(-12,-7,length=size.of.grid)
c.range<-seq(-0.6,-0.2,length=size.of.grid)

pds2<-expand.grid("a"=a.range,"b"=b.range,"c"=c.range)
pds2$modcom<-NA

for(i in 1:length(pds2$a)){
  p.vec<-c(pds2$a[i],pds2$b[i],pds2$c[i])
  ci.n.param<-length(p.vec) 
  ci.fit<-gom.binom(p.vec)     
  pds2$modcom[i]<-ifelse(1-pchisq(2*(max(ci.fit,optim.model2)-min(ci.fit,optim.model2)),1) < 0.05,"discard","keep")
  #print(i)
}
pds2.new<-subset(pds2,pds2$modcom=="keep")

length(pds2$a)
length(pds2.new$a)
max(pds2.new$a)
min(pds2.new$a)
max(pds2.new$b)
min(pds2.new$b)
max(pds2.new$c)
min(pds2.new$c)

#
##

q1a<-quantile(pds2.new$a,0.025)##0.5080808
q2a<-quantile(pds2.new$a,0.975)##0.8919192
q1b<-quantile(pds2.new$b,0.025)##-6.919192 
q2b<-quantile(pds2.new$b,0.975)##-3.080808
q1c<-quantile(pds2.new$c,0.025)##-0.490101
q2c<-quantile(pds2.new$c,0.975)##-0.05949495
q1a;q2a;q1b;q2b;q1c;q2c

pred2lower2<-   q1a * exp (q1b * exp(q2c * nc))
pred2upper2<-   q2a * exp (q2b * exp(q1c * nc))

pred2lower2<-   q1a * exp (q1b * exp(q2c * nc))
pred2upper2<-   q2a * exp (q2b * exp(q1c * nc))

lines(pred2lower2~nc)
lines(pred2upper2~nc)
#####################################################
###################################################################################################
########################################################################################################################################

require(vcd)
require(MASS)

allfitdist(data2$correctedprob)
fit1 <- fitdistr(data2$correctedprob, "exponential")
ks.test(data2$correctedprob, "pexp", fit1$estimate)

exp.mod<-function(p.val){
  a<-p.val[1]
  
  predy<-(data2$oocysts/a^2)*exp(-data2$oocysts^2/(2*a^2))
  datay<-data2$correctedprob
  loglik1<- datay* log((predy)+0.00001)+(1-datay)*log(1-((predy)-0.00001))
  -sum(loglik1,na.rm=T)
}
n.param<-1
prob1<-optim(c(0.95),exp.mod,method="L-BFGS-B",lower=0.5,upper=1)
prob1

predyest<-(nc/prob1$par[1]^2)*exp(-nc^2/(2*prob1$par[1]^2))
lines(predyest~nc)




dataperbite<-matrix(nrow=4,ncol=length(propbit1spor$sumprob))
for(i in 1:length(propbit1spor$sumprob)){
  dataperbite[,i]<-c(propbit1spor$sumprob[i],propbit2spor$sumprob2[i],propbit5spor$sumprob5[i],propbit10spor$sumprob10[i])}

plot(dataperbite[1,2:55]~propbit1spor$bit1ooc[2:55],pch=20)
lower<-upper<-numeric(length(54));
for (i in 2:55){
  lower[i]<-min(dataperbite[1:4,i])
  upper[i]<-max(dataperbite[1:4,i])}
polygon(c(propbit1spor$bit1ooc, rev(propbit1spor$bit1ooc)),c(lower,rev(upper)),border=NA, col="chartreuse4")

plot(propbit1spor$bit1ooc,propbit1spor$sporos0,ylim=c(0,0.01),ylab="Probability of sporozoites")
points(propbit1spor$bit1ooc,propbit1spor$sporos1,pch=19)
points(propbit1spor$bit1ooc,propbit1spor$sporos2,pch=19,col="blue")
points(propbit1spor$bit1ooc,propbit1spor$sporos3,pch=19,col="chartreuse")
points(propbit1spor$bit1ooc,propbit1spor$sporos4,pch=19,col="chocolate")
par(mfrow=c(2,3))
colnames <- dimnames(propbit10spor)[[2]]
for (i in 1:5) {
  plot(propbit1spor[,i]~propbit1spor$bit1ooc,
       main=colnames[i], 
       col="red", pch=19,
       ylim=c(0,0.01),xlim=c(0,100),
       xlab="Proportion of sporozoite counts")
  points(propbit2spor[,i]~propbit2spor$bit2ooc,pch=19,col="orange")
  points(propbit5spor[,i]~propbit5spor$bit5ooc,pch=19,col="yellow")
  points(propbit10spor[,i]~propbit10spor$bit10ooc,pch=19,col="green")
  lines(propbit1spor[,i]~propbit1spor$bit1ooc,col="red",lty=1)
  lines(propbit2spor[,i]~propbit2spor$bit2ooc,col="orange",lty=1)
  lines(propbit5spor[,i]~propbit5spor$bit5ooc,col="yellow",lty=1)
  lines(propbit10spor[,i]~propbit10spor$bit10ooc,col="green",lty=1)
}

#Nstim=propbit1spor$sporo0
#geometric.loglikelihood <- function(p.vec) {
#  a<-p.vec[1]
#  b<-p.vec[2]
#  c<-p.vec[3]
 
#  pred1<- (a * exp (b * exp(c *  propbit1spor$bit1ooc)))
#  data1<- propbit1spor[,1]/sum(propbit1spor[,1])
#  data2<- propbit1spor[,2]/sum(propbit1spor[,2])
#  data3<- propbit1spor[,3]/sum(propbit1spor[,3])
#  data4<- propbit1spor[,4]/sum(propbit1spor[,4])
#  data5<- propbit1spor[,5]/sum(propbit1spor[,5])
  
#  loglik1<- data1* log((pred1)+0.00001)+(1-data1)*log(1-((pred1)-0.00001))
#  loglik2<- data2* log((pred1)+0.00001)+(1-data2)*log(1-((pred1)-0.00001))
#  loglik3<- data3* log((pred1)+0.00001)+(1-data3)*log(1-((pred1)-0.00001))
#  loglik4<- data4* log((pred1)+0.00001)+(1-data4)*log(1-((pred1)-0.00001))
#  loglik5<- data5* log((pred1)+0.00001)+(1-data5)*log(1-((pred1)-0.00001))
  
#  -sum(loglik1,loglik2,loglik3,loglik4,loglik5,na.rm=T) 
#}
### optimise the likelihood function
#test<-optim(c(0.5,0,0.8),geometric.loglikelihood,method="L-BFGS-B",lower=c(0,-10,-10),upper=c(0.99,10,10))

#x1<-seq(0,100,1)
#predbit1<-(test$par[1] * exp (test$par[2] * exp(test$par[3] *  x1)))
#plot(predbit1~x1)

##Try and fit the probability of 0 - 4 sporozoite scores 
##given n oocyst for the different biting rates
##

mod_base <- function(params){
  n        <- params[1] ##mean of the negbin probability function to be estimated
  sig_n    <- params[2] ##sd of the distribution of sporozoites probabilities
  mu       <- params[3] ##mean number of oocysts (gamma distribution)
  sig_mu   <- params[4] ##sd of oocysts (gamma distribution)
  SPRE       <- params[5] ##probability that a sporozoite is recovered
  
  ############################
  ## Secondary NegBin parameters
  
  p = (sig_n^2-n)/(sig_n^2)
  r = (n^2)/(sig_n^2-n)
  
  ############################
  ## Secondary Gamma parameters
  
  theta_g <- sig_mu*sig_mu/mu
  
  ##############################
  ## Sporozoites
  
  prop1 <- propbit1spor[,1]/sum(propbit1spor[,1])
  prop2 <- propbit1spor[,2]/sum(propbit1spor[,2])
  prop3 <- propbit1spor[,3]/sum(propbit1spor[,3])
  prop4 <- propbit1spor[,4]/sum(propbit1spor[,4])
  prop5 <- propbit1spor[,5]/sum(propbit1spor[,5])
  
  p_spz1 = n*prop1/(n*prop1 + r)
  p_spz2 = n*prop1/(n*prop1 + r)
  p_spz3 = n*prop1/(n*prop1 + r)
  p_spz4 = n*prop1/(n*prop1 + r)
  p_spz5 = n*prop1/(n*prop1 + r)
  
  II <- 1:1000
  
  
  logL1 <- logL2 <- logL3 <- logL4 <- logL5 <- 0
  for(j in 1:length(propbit1spor$bit1ooc)){
    if( propbit1spor$bit1ooc[j]>0 ){
      coeffs1 <- lgamma(II+r) - lgamma(II+1) - lgamma(r) + r*log(1-p_spz1[j]) + II*log(p_spz1[j])
      coeffs2 <- lgamma(II+r) - lgamma(II+1) - lgamma(r) + r*log(1-p_spz1[j]) + II*log(p_spz1[j])
      coeffs3 <- lgamma(II+r) - lgamma(II+1) - lgamma(r) + r*log(1-p_spz1[j]) + II*log(p_spz1[j])
      coeffs4 <- lgamma(II+r) - lgamma(II+1) - lgamma(r) + r*log(1-p_spz1[j]) + II*log(p_spz1[j])
      coeffs5 <- lgamma(II+r) - lgamma(II+1) - lgamma(r) + r*log(1-p_spz1[j]) + II*log(p_spz1[j])
      
      coeffs1 <- exp(coeffs1)
      coeffs2 <- exp(coeffs2)
      coeffs3 <- exp(coeffs3)
      coeffs4 <- exp(coeffs4)
      coeffs5 <- exp(coeffs5)
      
      logL1 <- logL1 + log(sum( coeffs1*dgamma(propbit1spor$bit1ooc[j], shape=mu*II/theta_g, scale=theta_g) ))
      logL2 <- logL2 + log(sum( coeffs1*dgamma(propbit1spor$bit1ooc[j], shape=mu*II/theta_g, scale=theta_g) ))
      logL3 <- logL3 + log(sum( coeffs1*dgamma(propbit1spor$bit1ooc[j], shape=mu*II/theta_g, scale=theta_g) ))
      logL4 <- logL4 + log(sum( coeffs1*dgamma(propbit1spor$bit1ooc[j], shape=mu*II/theta_g, scale=theta_g) ))
      logL5 <- logL5 + log(sum( coeffs1*dgamma(propbit1spor$bit1ooc[j], shape=mu*II/theta_g, scale=theta_g) ))
    }
    if( propbit1spor$bit1ooc[j]==0 ){
      logL1 <- logL1 + r*log(1-p_spz1[j])
      logL2 <- logL2 + r*log(1-p_spz1[j])
      logL3 <- logL3 + r*log(1-p_spz1[j])
      logL4 <- logL4 + r*log(1-p_spz1[j])
      logL5 <- logL5 + r*log(1-p_spz1[j])
    }
  }
  
  -sum(logL1,logL2,logL3,logL4,logL5,na.rm=T)
}
############################
## Define limits for parameters and perform 
## MLE model fitting

temp_mat <- matrix(NA, nrow=2, ncol=6)

lower <- c(10,  10,  200,   200,   0)
upper <- c(1000,1000,20000, 10000, 1)

ui <- rbind( diag(5), c(0,0,0,0,-1) )
ci <- c(lower, -1)

theta <- randomLHS(2,5)
theta <- t( lower +  t(theta)*(0.25*upper-lower) )


max_MLE <- 1e6
par_MLE <- rep(NA, 5)


for(j in 1:2){
  MLE_base <- constrOptim(theta=theta[j,], f=mod_base, grad=NULL, ui=ui, ci=ci,
                          outer.iterations = 100, outer.eps = 1e-06)
  
  temp_mat[j,1:5] <- MLE_base$par
  temp_mat[j,6]   <- -MLE_base$value
  
  if( MLE_base$value < max_MLE ){ 
    max_MLE <- -MLE_base$value
    best_MLE_base <- MLE_base 			
  }	
}



n_mle      <- best_MLE_base$par[1]
sig_n_mle  <- best_MLE_base$par[2]
mu_mle     <- best_MLE_base$par[3]
sig_mu_mle <- best_MLE_base$par[4]
VE_mle     <- best_MLE_base$par[5]

p_mle <- (sig_n_mle^2-n_mle)/(sig_n_mle^2)
r_mle <- (n_mle^2)/(sig_n_mle^2-n_mle) 

n_mle;sig_n_mle;mu_mle;sig_mu_mle;VE_mle

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

