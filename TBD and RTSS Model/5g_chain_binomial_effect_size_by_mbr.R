#
## Fitted chain binomial - replicating Blagborough et al 2013, 5 mouse-mosquito rounds, by MBR
#

data.mouse=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\hand over data and scripts\\5g chain binomial effect size\\atv32_mouse_final_data.txt",header=T)
data.mosqu=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\hand over data and scripts\\5g chain binomial effect size\\atv32_mosquito_final_data.txt",header=T)

par(mfrow=c(1,1))
plot(1:1,col="white",xlim=c(1,5),ylim=c(-1,1),bty="n",xlab="MBR (esitmated individually)",ylab="effect size (95 % CI)")
for(k in 1:5){

vrs.binom<-function(p.vec){
  
v<-p.vec[1]
r<-p.vec[2]
s<-p.vec[3]
  
  # Round, Bites, Treatment, Species; number infected
  inf<-array(NA, dim = c(6,5,2,2))
  
  # Total number
  number<-array(NA, dim = c(6,5,2,2))
  
  # Enter p0
  inf[1,,1,2]<-5
  inf[1,,2,2]<-5
  number[1:6,,1,2]<-5
  number[1:6,,2,2]<-5

  # Pre-calc q0
  number[1,1:5,1,1] <- 50
  inf[1,1:5,1,1] <- s * 50

  number[1,1:5,2,1] <- 50
  inf[1,1:5,2,1] <- s * 50 * (1-v)
    
  # Main transmission calcs, loops for round and bites (p1-p5 and q1-q5)
  for(a in 1:5){
    for(b in 1:5){

      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==b & data.mosqu$Round==a & data.mosqu$Treatment==0)
      number[a+1,b,1,1] <- length(mosqu.temp$Oocyst)
    
      inf[a+1,b,1,2] <- (1 - (1 - r * (inf[a,b,1,1]/number[a,b,1,1])) ^ b) * number[a+1,b,1,2]                # mosquito to mouse
      inf[a+1,b,1,1] <- ((inf[a+1,b,1,2]/number[a+1,b,1,2]) * s) * length(mosqu.temp$Oocyst)                  # mouse to mosquito
      
      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==b & data.mosqu$Round==a & data.mosqu$Treatment==1)
      number[a+1,b,2,1] <- length(mosqu.temp$Oocyst)
      
      inf[a+1,b,2,2] <- (1 - (1 - r * (inf[a,b,2,1]/number[a,b,2,1]) ) ^ b) * number[a+1,b,2,2]               # mosquito to mouse
      inf[a+1,b,2,1] <- ((inf[a+1,b,2,2]/number[a+1,b,2,2]) * s * (1-v)) * length(mosqu.temp$Oocyst)          # mouse to mosquito
    }
  }
  
# Arrange data into appropriate format for fitting
  data.inf<-array(NA, dim = c(6,5,2,2))    
  data.inf[1,1:5,1,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment==0]>0)
  data.inf[1,1:5,2,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment==1]>0)
  for(a in 1:5){
    for(b in 1:5){
      data.inf[a+1,b,1,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==b & data.mouse$Round==a & data.mouse$Treatment==0]>0)
      data.inf[a+1,b,1,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==b & data.mosqu$Round==a & data.mosqu$Treatment==0]>0)

      data.inf[a+1,b,2,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==b & data.mouse$Round==a & data.mouse$Treatment==1]>0)
      data.inf[a+1,b,2,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==b & data.mosqu$Round==a & data.mosqu$Treatment==1]>0)
    }
  }
  
  # exclude q0
  inf[1,,,]<-NA  
  
  inf.new<-c(inf[,k,,])
  data.inf.new<-c(data.inf[,k,,])
  number.new<-c(number[,k,,])
  
  loglik<- data.inf.new * log((inf.new/number.new))+(number.new-data.inf.new)*log((1-(inf.new/number.new)))
  -sum(loglik,na.rm=T)  
}

n.param<-3
vrs.model<-optim(rep(0.05,n.param),vrs.binom,method="L-BFGS-B",lower=rep(0.01,n.param),upper=rep(0.99,n.param))
vrs.model

#print(vrs.model)

points(k,vrs.model$par[1],pch=16,cex=1.5)
optim.model<-vrs.binom(vrs.model$par)
size.of.grid<-100
v1.range<-seq(0.01,0.99,length=size.of.grid)
ci.grid.v1<-array(0, dim = c(size.of.grid))
for(a in 1:size.of.grid){
  p.vec<-c(v1.range[a],vrs.model$par[2:3])
  ci.n.param<-1
  ci.fit<-vrs.binom(p.vec)     
  ci.grid.v1[a]<-ifelse(ci.fit<=optim.model+qchisq(0.95, ci.n.param)/2,v1.range[a],NA)
#  print(a)
}
v1.ci<-range(ci.grid.v1, na.rm=T)
#print(v1.ci)
arrows(k,v1.ci[1],k,v1.ci[2],length=0.05,angle=90)
arrows(k,v1.ci[2],k,v1.ci[1],length=0.05,angle=90)

}

text(3.5,0.8,"Overall Effect Size = 8.4% ChainBinom")
text(3.5,0.6,"(excluding mbr = 1)")
