###################################################
##                                               ## 
##     ####       #     ##########     #         ##
##     ## ##     ###        ##        ###        ##
##     ##  ##   ## ##       ##       ## ##       ##
##     ##  ##  ##   ##      ##      ##   ##      ## 
##     ##  ##  #######      ##      #######      ## 
##     ## ##  ##     ##     ##     ##     ##     ##
##     ####  ##       ##    ##    ##       ##    ##
##                                               ## 
###################################################

spors<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\UCT OZPIP FER DSM Feb2015\\M2M SporozoiteScores\\SporoParaIntensity.txt",header=TRUE)
head(spors)

spors$sporozoites<-numeric(length(spors$Round))
for (i in 1:length(spors$Round)){
  spors$sporozoites[i]<-sum(spors[i,5:14],na.rm=TRUE)}

spors$ScorePerBite<-spors$sporozoites/spors$Bites

score0<- spors$ScorePerBite[spors$ScorePerBite == 0]
score1<- spors$ScorePerBite[spors$ScorePerBite > 0 & spors$ScorePerBite<1]
score2<- spors$ScorePerBite[spors$ScorePerBite >= 1 & spors$ScorePerBite<2]
score3<- spors$ScorePerBite[spors$ScorePerBite >= 2 & spors$ScorePerBite<3]
score4<- spors$ScorePerBite[spors$ScorePerBite >= 3 & spors$ScorePerBite<4]

logScore<-c(mean(score0),mean(score1),mean(score2),mean(score3),mean(score4),4)

Real0<-round(score0)
Real1<-rep(1,length(score1))
Real2<-round((score2-1)*10)
Real2<-ifelse(Real2==0,10,Real2)
Real3<-round((score3-2)*100)
Real3<-ifelse(Real3==0,100,Real3)
Real4<-round((score4-3)*1000)
Real4<-ifelse(Real4==0,1000,Real4)
real<-c(Real0,Real1,Real2,Real3,Real4)
sort(real)

oocysts<-read.table("D:\\IMPERIAL Nov 2014\\Andrew Blagborough\\12Feb2015_4DrugTreatments\\M2M SporozoiteScores\\OocystIntensity.txt",header=TRUE)
blanks<-subset(oocysts,Treatment=="Blank")

oocdata1<-sort(blanks$oocysts[blanks$oocysts > 0])
length(oocdata1)/5
ooc<-c(0,mean(oocdata1[1:19]),mean(oocdata1[20:38]),mean(oocdata1[38:57]),mean(oocdata1[58:77]),mean(oocdata1[78:96]))



data1<-(sample(real[real > 0],50))
data2<-(sample(real[real > 0],50))
data3<-(sample(real[real > 0],50))
data4<-(sample(real[real > 0],50))
data5<-(sample(real[real > 0],50))



#
## Logistic fit
#
sat.binom<-function(p.vec){
  
  b  <- p.vec[1]
  d  <- p.vec[2]
  ch <- p.vec[3]
  a <- p.vec[4]
  
  #a <- p.vec[1]
  #b <- p.vec[2]
  #LOGpred0<-((exp(a + b * ooc[1])) / (1 + exp(a + b * ooc[1])) )
  #LOGpred1<-((exp(a + b * ooc[2])) / (1 + exp(a + b * ooc[2])) )
  #LOGpred2<-((exp(a + b * ooc[3])) / (1 + exp(a + b * ooc[3])) )
  #LOGpred3<-((exp(a + b * ooc[4])) / (1 + exp(a + b * ooc[4])) )
  #LOGpred4<-((exp(a + b * ooc[5])) / (1 + exp(a + b * ooc[5])) )
  #LOGpred5<-((exp(a + b * ooc[6])) / (1 + exp(a + b * ooc[6])) )
  
  pred0<- (b * ooc[1]^a)/(ch + d * ooc[1]^a)
  pred1<- (b * ooc[2]^a)/(ch + d * ooc[2]^a)
  pred2<- (b * ooc[3]^a)/(ch + d * ooc[3]^a)  
  pred3<- (b * ooc[4]^a)/(ch + d * ooc[4]^a)  
  pred4<- (b * ooc[5]^a)/(ch + d * ooc[5]^a)  
  pred5<- (b * ooc[6]^a)/(ch + d * ooc[6]^a)

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
satmod<-optim(c(8.5,0.999,128,0.92),sat.binom,method="L-BFGS-B",lower=c(5,0.9,120,0.8),upper=c(10,0.9999,0.99))
satmod



nc<-seq(0,max(oocdata1),1)
pred<-(satmod$par[1] * nc^satmod$par[4])/(satmod$par[3] + satmod$par[2] * nc^satmod$par[4])
#pred1<-((exp(satmod$par[1] + satmod$par[2] * nc)) / (1 + exp(satmod$par[1] + satmod$par[2] * nc)) )*10
plot(ooc,logScore,ylim=c(0,5),bty="n",xlim=c(0,300),
     las=1,xlab="Oocysts",ylab="Sporozoites",cex=1.25,col="chartreuse4",pch=16)
points(ooc,logScore)
lines(nc,pred,lwd=2)

b = 8.5   #1
ch = 128  #3
d = 0.999 #2
a = 0.92  #4
pred1<- (b * nc^a)/(ch + d * nc^a)
pred1
lines(nc,pred1,lwd=2,col="red")
