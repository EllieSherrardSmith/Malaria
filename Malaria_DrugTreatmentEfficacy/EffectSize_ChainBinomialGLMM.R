#####################################################################################################
##                                                                                                 ##                       
### Fitted chain binomial - replicating Blagborough et al 2013 Nat Comms, 5 mouse-mosquito rounds  ##  
##                                                                                                 ##  
#####################################################################################################


###New data with 2, 5 and 10 mosquito biting rates

data.mouse=read.table("E:\\IMPERIAL Nov 2014\\Andrew Blagborough\\ATV-65\\atv65_mouse_final_data_missing10.txt",header=T)
data.mosqu=read.table("E:\\IMPERIAL Nov 2014\\Andrew Blagborough\\ATV-65\\atv65_mosquito_final_data_missing10.txt",header=T)
summary(data.mouse);summary(data.mosqu)

data.mouse<-subset(data.mouse,Round==1)
data.mosqu<-subset(data.mosqu,Round==0 |Round==1) 


vrs.binom<-function(p.vec){
  
  v<-p.vec[1]
  r<-p.vec[2]
  s<-p.vec[3]
  
  # Round, Bites, Treatment, Species; predicted (except for p0) number infected
  inf<-array(NA, dim = c(2,3,2,2))
  
  # Total numbers of mice and mosquitoes
  number<-array(NA, dim = c(2,3,2,2))
  
  # Enter p0 data
  inf[1,,1,2]<-5
  inf[1,,2,2]<-5
  number[1:2,,1,2]<-5
  number[1:2,,2,2]<-5
  
  # Predict q0
  number[1,1:3,1,1] <- 50
  inf[1,1:3,1,1] <- s * 50
  
  number[1,1:3,2,1] <- 50
  inf[1,1:3,2,1] <- s * (1-v) * 50
  mbr.vec=c(1,2,5) ##added for the new data with 1, 2 and 5 bites 
  # Predict (p1 and q1), loops for rounds and bites p = prev in mice and q in mosquito
  for (a in 1){
    for(b in 1:3){
      
      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round == a & data.mosqu$Treatment==0)
      number[a+1,b,1,1] <- length(mosqu.temp$Oocyst)
      
      inf[a+1,b,1,2] <- (1 - (1 - r * (inf[a,b,1,1]/number[a,b,1,1])) ^ mbr.vec[b]) * number[a+1,b,1,2]                # mosquito to mouse
      inf[a+1,b,1,1] <- ((inf[a+1,b,1,2]/number[a+1,b,1,2]) * s) * number[a+1,b,1,1]                          # mouse to mosquito
      
      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment==1)
      number[a+1,b,2,1] <- length(mosqu.temp$Oocyst)
      
      inf[a+1,b,2,2] <- (1 - (1 - r * (inf[a,b,2,1]/number[a,b,2,1]) ) ^ mbr.vec[b]) * number[a+1,b,2,2]               # mosquito to mouse
      inf[a+1,b,2,1] <- ((inf[a+1,b,2,2]/number[a+1,b,2,2]) * s) * (1-v) * number[a+1,b,2,1]                  # mouse to mosquito
    }
  }
  
  
  
  # Arrange data into appropriate format for fitting
  data.inf<-array(NA, dim = c(2,3,2,2))    
  data.inf[1,1:3,1,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment==0]>0)
  data.inf[1,1:3,2,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment==1]>0)
  for(a in 1){
    for(b in 1:3){
      data.inf[a+1,b,1,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==mbr.vec[b] & data.mouse$Treatment==0]>0)
      data.inf[a+1,b,1,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment==0]>0)
      
      data.inf[a+1,b,2,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==mbr.vec[b] & data.mouse$Treatment==1]>0)
      data.inf[a+1,b,2,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment==1]>0)
    }
  }
  
  # inclusion/exclusion of q0 data
  #inf[1,2:5,,1]<-NA  
  inf[1,,,]<-NA
  
  loglik<- data.inf * log((inf/number))+(number-data.inf)*log((1-(inf/number)))
  -sum(loglik,na.rm=T)  
}

n.param<-3
vrs.model<-optim(rep(0.05,n.param),vrs.binom,method="L-BFGS-B",lower=rep(0.01,n.param),upper=rep(0.99,n.param))
vrs.model

#
## CIs
#

size.of.grid<-20
optim.model<-vrs.binom(vrs.model$par)
v.range<-seq(0,1,length=size.of.grid)
r.range<-seq(0,1,length=size.of.grid)
s.range<-seq(0,1,length=size.of.grid)

ci.grid.v<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid))
ci.grid.r<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid))
ci.grid.s<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid))

for(a in 1:size.of.grid){
  for(b in 1:size.of.grid){
    for(d in 1:size.of.grid){
      p.vec<-c(v.range[a],r.range[b],s.range[d])
      ci.n.param<-length(vrs.model$par) 
      ci.fit<-vrs.binom(p.vec)     
      ci.grid.v[a,b,d]<-ifelse(ci.fit<=optim.model+qchisq(0.95, ci.n.param)/2,v.range[a],NA)
      ci.grid.r[a,b,d]<-ifelse(ci.fit<=optim.model+qchisq(0.95, ci.n.param)/2,r.range[b],NA)
      ci.grid.s[a,b,d]<-ifelse(ci.fit<=optim.model+qchisq(0.95, ci.n.param)/2,s.range[d],NA)
    }
  }
  print(a)
}

v.ci<-range(ci.grid.v, na.rm=T)##effect size
r.ci<-range(ci.grid.r, na.rm=T)##per bite probability of infection from an infected mosquito to susceptible mouse
s.ci<-range(ci.grid.s, na.rm=T)##per bite probability of infection from an infected mouse to susceptible mosquito 
rbind(v.ci,r.ci,s.ci)
v<-mean(ci.grid.v, na.rm=T);r<-mean(ci.grid.r, na.rm=T);s<-mean(ci.grid.s, na.rm=T)
rbind(v,r,s)
