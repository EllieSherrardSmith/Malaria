rm(list = ls())
require(lme4)


data.mouse.a=read.table("D:\\IMPERIAL Nov 2014\\Malaria Modelling\\DATA\\ParasiteAllMASTER5.txt",header=TRUE)
summary(data.mouse.a)
dim(data.mouse.a)
##remove negative bites

rm.infbites<-data.mouse.a$TotalInf

rm.parasum.a<-data.mouse.a[,14:20]
rm.parasum<-numeric(length(rm.infbites))

for(i in 1:length(rm.infbites)){
  rm.parasum[i]<-sum(rm.parasum.a[i,], na.rm = TRUE)}

rm.prev<-ifelse(rm.parasum>0,1,0)

neg.inf<-which(rm.infbites==0)#&rm.prev==1)
length(neg.inf)

##master5 removes 259 of which 13 were +ve from uninfected bites

data.mouse<-data.mouse.a[-neg.inf,]

#count5
Parasite10<-data.mouse$Para10
Parasite9<-data.mouse$Para9
Parasite8<-data.mouse$Para8
Parasite7<-data.mouse$Para7
Parasite6<-data.mouse$Para6
parasum<-numeric(length(Parasite6))
for(i in 1:length(Parasite6)){
  parasum[i]<-sum(c(Parasite10[i],Parasite9[i],Parasite8[i],Parasite7[i],Parasite6[i]), na.rm = TRUE)
}
prev<-ifelse(parasum>0,1,0)
mean(prev)
Treatment<-as.factor(data.mouse$Treatment)
Rond<-as.factor(data.mouse$Round)
InfBites<-as.factor(data.mouse$TotalInf)
Sporo1<-data.mouse$Sporozoite1                #a score of 5 means no bite
Sporo2<-data.mouse$Sporozoite2                #a score of 0 means score of zero
Sporo3<-data.mouse$Sporozoite3
Sporo4<-data.mouse$Sporozoite4
Sporo5<-data.mouse$Sporozoite5
Sporo6<-data.mouse$Sporozoite6
Sporo7<-data.mouse$Sporozoite7
Sporo8<-data.mouse$Sporozoite8
Sporo9<-data.mouse$Sporozoite9
Sporo10<-data.mouse$Sporozoite10


Sporo<-cbind(Sporo1,Sporo2,Sporo3,Sporo4,Sporo5,Sporo6,Sporo7,Sporo8,Sporo9,Sporo10)


##full model going  negative
my.function<-function(my.k){
  
  k<-c(my.k[1],my.k[2],my.k[3],my.k[4],my.k[5],0,0)
  
  prob1<-1-k[Sporo[,1]+1]
  prob2<-1-k[Sporo[,2]+1]
  prob3<-1-k[Sporo[,3]+1] 
  prob4<-1-k[Sporo[,4]+1]
  prob5<-1-k[Sporo[,5]+1]
  prob6<-1-k[Sporo[,6]+1]
  prob7<-1-k[Sporo[,7]+1]
  prob8<-1-k[Sporo[,8]+1] 
  prob9<-1-k[Sporo[,9]+1]
  prob10<-1-k[Sporo[,10]+1]


  
  prob.inf.a<-1-prob1*prob2*prob3*prob4*prob5*prob6*prob7*prob8*prob9*prob10
  
  prob.inf<-ifelse(prob.inf.a<=0,0.000000001,ifelse(prob.inf.a>1,1-0.000000001,prob.inf.a))
  loglik<-prev*log(prob.inf-0.0000000001)+(1-prev)*log(1-prob.inf+0.0000000001)
  -sum(loglik)
}

n.param<-5

modelfull<-optim(rep(0.005,n.param),my.function,method="L-BFGS-B",
                 lower=rep(-0.1,n.param), upper=rep(0.99,n.param))
modelfull



##null

my.function<-function(my.k){
  
  prob.inf.a<-my.k[1]
  
  prob.inf<-ifelse(prob.inf.a<=0,0.000000001,ifelse(prob.inf.a>1,1-0.000000001,prob.inf.a))
  loglik<-prev*log(prob.inf-0.0000000001)+(1-prev)*log(1-prob.inf+0.0000000001)
  -sum(loglik)
}

n.param<-1

modelnull<-optim(rep(0.005,n.param),my.function,method="L-BFGS-B",
                 lower=rep(-0.1,n.param), upper=rep(0.99,n.param))
modelnull



##01234 model going  negative
my.function<-function(my.k){
  
  k<-c(my.k[1],my.k[1],my.k[1],my.k[1],my.k[1],0,0)
  
  prob1<-1-k[Sporo[,1]+1]
  prob2<-1-k[Sporo[,2]+1]
  prob3<-1-k[Sporo[,3]+1] 
  prob4<-1-k[Sporo[,4]+1]
  prob5<-1-k[Sporo[,5]+1]
  prob6<-1-k[Sporo[,6]+1]
  prob7<-1-k[Sporo[,7]+1]
  prob8<-1-k[Sporo[,8]+1] 
  prob9<-1-k[Sporo[,9]+1]
  prob10<-1-k[Sporo[,10]+1]


  
  prob.inf.a<-1-prob1*prob2*prob3*prob4*prob5*prob6*prob7*prob8*prob9*prob10
  
  prob.inf<-ifelse(prob.inf.a<=0,0.000000001,ifelse(prob.inf.a>1,1-0.000000001,prob.inf.a))
  loglik<-prev*log(prob.inf-0.0000000001)+(1-prev)*log(1-prob.inf+0.0000000001)
  -sum(loglik)
}

n.param<-1

model01234<-optim(rep(0.005,n.param),my.function,method="L-BFGS-B",
                  lower=rep(-0.1,n.param), upper=rep(0.99,n.param))
model01234


##1234 model going  negative
my.function<-function(my.k){
  
  k<-c(my.k[1],my.k[2],my.k[2],my.k[2],my.k[2],0,0)
  
  prob1<-1-k[Sporo[,1]+1]
  prob2<-1-k[Sporo[,2]+1]
  prob3<-1-k[Sporo[,3]+1] 
  prob4<-1-k[Sporo[,4]+1]
  prob5<-1-k[Sporo[,5]+1]
  prob6<-1-k[Sporo[,6]+1]
  prob7<-1-k[Sporo[,7]+1]
  prob8<-1-k[Sporo[,8]+1] 
  prob9<-1-k[Sporo[,9]+1]
  prob10<-1-k[Sporo[,10]+1]


  
  prob.inf.a<-1-prob1*prob2*prob3*prob4*prob5*prob6*prob7*prob8*prob9*prob10
  
  prob.inf<-ifelse(prob.inf.a<=0,0.000000001,ifelse(prob.inf.a>1,1-0.000000001,prob.inf.a))
  loglik<-prev*log(prob.inf-0.0000000001)+(1-prev)*log(1-prob.inf+0.0000000001)
  -sum(loglik)
}

n.param<-2

model1234<-optim(rep(0.005,n.param),my.function,method="L-BFGS-B",
                 lower=rep(-0.1,n.param), upper=rep(0.99,n.param))
model1234


##234 model going  negative
my.function<-function(my.k){
  
  k<-c(my.k[1],my.k[2],my.k[3],my.k[3],my.k[3],0,0)
  
  prob1<-1-k[Sporo[,1]+1]
  prob2<-1-k[Sporo[,2]+1]
  prob3<-1-k[Sporo[,3]+1] 
  prob4<-1-k[Sporo[,4]+1]
  prob5<-1-k[Sporo[,5]+1]
  prob6<-1-k[Sporo[,6]+1]
  prob7<-1-k[Sporo[,7]+1]
  prob8<-1-k[Sporo[,8]+1] 
  prob9<-1-k[Sporo[,9]+1]
  prob10<-1-k[Sporo[,10]+1]
  
  
  
  prob.inf.a<-1-prob1*prob2*prob3*prob4*prob5*prob6*prob7*prob8*prob9*prob10
  
  prob.inf<-ifelse(prob.inf.a<=0,0.000000001,ifelse(prob.inf.a>1,1-0.000000001,prob.inf.a))
  loglik<-prev*log(prob.inf-0.0000000001)+(1-prev)*log(1-prob.inf+0.0000000001)
  -sum(loglik)
}

n.param<-3

model234<-optim(rep(0.005,n.param),my.function,method="L-BFGS-B",
                lower=rep(-0.1,n.param), upper=rep(0.99,n.param))


model234


##34 model going  negative
my.function<-function(my.k){
  
  k<-c(my.k[1],my.k[2],my.k[3],my.k[4],my.k[4],0,0)
  
  prob1<-1-k[Sporo[,1]+1]
  prob2<-1-k[Sporo[,2]+1]
  prob3<-1-k[Sporo[,3]+1] 
  prob4<-1-k[Sporo[,4]+1]
  prob5<-1-k[Sporo[,5]+1]
  prob6<-1-k[Sporo[,6]+1]
  prob7<-1-k[Sporo[,7]+1]
  prob8<-1-k[Sporo[,8]+1] 
  prob9<-1-k[Sporo[,9]+1]
  prob10<-1-k[Sporo[,10]+1]


  
  prob.inf.a<-1-prob1*prob2*prob3*prob4*prob5*prob6*prob7*prob8*prob9*prob10
  
  prob.inf<-ifelse(prob.inf.a<=0,0.000000001,ifelse(prob.inf.a>1,1-0.000000001,prob.inf.a))
  loglik<-prev*log(prob.inf-0.0000000001)+(1-prev)*log(1-prob.inf+0.0000000001)
  -sum(loglik)
}

n.param<-4

model34<-optim(rep(0.005,n.param),my.function,method="L-BFGS-B",
               lower=rep(-0.1,n.param), upper=rep(0.99,n.param))
model34

##01 model going  negative 

my.function<-function(my.k){
  
  k<-c(my.k[1],my.k[1],my.k[2],my.k[3],my.k[4],0,0)
  
  prob1<-1-k[Sporo[,1]+1]
  prob2<-1-k[Sporo[,2]+1]
  prob3<-1-k[Sporo[,3]+1] 
  prob4<-1-k[Sporo[,4]+1]
  prob5<-1-k[Sporo[,5]+1]
  prob6<-1-k[Sporo[,6]+1]
  prob7<-1-k[Sporo[,7]+1]
  prob8<-1-k[Sporo[,8]+1] 
  prob9<-1-k[Sporo[,9]+1]
  prob10<-1-k[Sporo[,10]+1]
  
  
  
  prob.inf.a<-1-prob1*prob2*prob3*prob4*prob5*prob6*prob7*prob8*prob9*prob10
  
  prob.inf<-ifelse(prob.inf.a<=0,0.000000001,ifelse(prob.inf.a>1,1-0.000000001,prob.inf.a))
  loglik<-prev*log(prob.inf-0.0000000001)+(1-prev)*log(1-prob.inf+0.0000000001)
  -sum(loglik)
}

n.param<-4

model01<-optim(rep(0.005,n.param),my.function,method="L-BFGS-B",
                 lower=rep(-0.1,n.param), upper=rep(0.99,n.param))
model01


##012 model going  negative
my.function<-function(my.k){
  
  k<-c(my.k[1],my.k[1],my.k[1],my.k[2],my.k[3],0,0)
  
  prob1<-1-k[Sporo[,1]+1]
  prob2<-1-k[Sporo[,2]+1]
  prob3<-1-k[Sporo[,3]+1] 
  prob4<-1-k[Sporo[,4]+1]
  prob5<-1-k[Sporo[,5]+1]
  prob6<-1-k[Sporo[,6]+1]
  prob7<-1-k[Sporo[,7]+1]
  prob8<-1-k[Sporo[,8]+1] 
  prob9<-1-k[Sporo[,9]+1]
  prob10<-1-k[Sporo[,10]+1]


  
  prob.inf.a<-1-prob1*prob2*prob3*prob4*prob5*prob6*prob7*prob8*prob9*prob10
  
  prob.inf<-ifelse(prob.inf.a<=0,0.000000001,ifelse(prob.inf.a>1,1-0.000000001,prob.inf.a))
  loglik<-prev*log(prob.inf-0.0000000001)+(1-prev)*log(1-prob.inf+0.0000000001)
  -sum(loglik)
}

n.param<-3

model012<-optim(rep(0.005,n.param),my.function,method="L-BFGS-B",
                lower=rep(-0.1,n.param), upper=rep(0.99,n.param))
model012

##0123 model going  negative
my.function<-function(my.k){
  
  k<-c(my.k[1],my.k[1],my.k[1],my.k[1],my.k[2],0,0)
  
  prob1<-1-k[Sporo[,1]+1]
  prob2<-1-k[Sporo[,2]+1]
  prob3<-1-k[Sporo[,3]+1] 
  prob4<-1-k[Sporo[,4]+1]
  prob5<-1-k[Sporo[,5]+1]
  prob6<-1-k[Sporo[,6]+1]
  prob7<-1-k[Sporo[,7]+1]
  prob8<-1-k[Sporo[,8]+1] 
  prob9<-1-k[Sporo[,9]+1]
  prob10<-1-k[Sporo[,10]+1]


  
  prob.inf.a<-1-prob1*prob2*prob3*prob4*prob5*prob6*prob7*prob8*prob9*prob10
  
  prob.inf<-ifelse(prob.inf.a<=0,0.000000001,ifelse(prob.inf.a>1,1-0.000000001,prob.inf.a))
  loglik<-prev*log(prob.inf-0.0000000001)+(1-prev)*log(1-prob.inf+0.0000000001)
  -sum(loglik)
}

n.param<-2

model0123<-optim(rep(0.005,n.param),my.function,method="L-BFGS-B",
                 lower=rep(-0.1,n.param), upper=rep(0.99,n.param))
model0123

##01_234 model going  negative
my.function<-function(my.k){
  
  k<-c(my.k[1],my.k[1],my.k[2],my.k[2],my.k[2],0,0)
  
  prob1<-1-k[Sporo[,1]+1]
  prob2<-1-k[Sporo[,2]+1]
  prob3<-1-k[Sporo[,3]+1] 
  prob4<-1-k[Sporo[,4]+1]
  prob5<-1-k[Sporo[,5]+1]
  prob6<-1-k[Sporo[,6]+1]
  prob7<-1-k[Sporo[,7]+1]
  prob8<-1-k[Sporo[,8]+1] 
  prob9<-1-k[Sporo[,9]+1]
  prob10<-1-k[Sporo[,10]+1]


  
  prob.inf.a<-1-prob1*prob2*prob3*prob4*prob5*prob6*prob7*prob8*prob9*prob10
  
  prob.inf<-ifelse(prob.inf.a<=0,0.000000001,ifelse(prob.inf.a>1,1-0.000000001,prob.inf.a))
  loglik<-prev*log(prob.inf-0.0000000001)+(1-prev)*log(1-prob.inf+0.0000000001)
  -sum(loglik)
}

n.param<-2

model01_234<-optim(rep(0.005,n.param),my.function,method="L-BFGS-B",
                   lower=rep(-0.1,n.param), upper=rep(0.99,n.param))
model01_234


##012_34 model going  negative
my.function<-function(my.k){
  
  k<-c(my.k[1],my.k[1],my.k[1],my.k[2],my.k[2],0,0)
  
  prob1<-1-k[Sporo[,1]+1]
  prob2<-1-k[Sporo[,2]+1]
  prob3<-1-k[Sporo[,3]+1] 
  prob4<-1-k[Sporo[,4]+1]
  prob5<-1-k[Sporo[,5]+1]
  prob6<-1-k[Sporo[,6]+1]
  prob7<-1-k[Sporo[,7]+1]
  prob8<-1-k[Sporo[,8]+1] 
  prob9<-1-k[Sporo[,9]+1]
  prob10<-1-k[Sporo[,10]+1]


  
  prob.inf.a<-1-prob1*prob2*prob3*prob4*prob5*prob6*prob7*prob8*prob9*prob10
  
  prob.inf<-ifelse(prob.inf.a<=0,0.000000001,ifelse(prob.inf.a>1,1-0.000000001,prob.inf.a))
  loglik<-prev*log(prob.inf-0.0000000001)+(1-prev)*log(1-prob.inf+0.0000000001)
  -sum(loglik)
}

n.param<-2

model012_34<-optim(rep(0.005,n.param),my.function,method="L-BFGS-B",
                   lower=rep(-0.1,n.param), upper=rep(0.99,n.param))
model012_34


##12_34 model going  negative
my.function<-function(my.k){
  
  k<-c(my.k[1],my.k[1],my.k[2],my.k[3],my.k[3],0,0)
  
  prob1<-1-k[Sporo[,1]+1]
  prob2<-1-k[Sporo[,2]+1]
  prob3<-1-k[Sporo[,3]+1] 
  prob4<-1-k[Sporo[,4]+1]
  prob5<-1-k[Sporo[,5]+1]
  prob6<-1-k[Sporo[,6]+1]
  prob7<-1-k[Sporo[,7]+1]
  prob8<-1-k[Sporo[,8]+1] 
  prob9<-1-k[Sporo[,9]+1]
  prob10<-1-k[Sporo[,10]+1]


  
  prob.inf.a<-1-prob1*prob2*prob3*prob4*prob5*prob6*prob7*prob8*prob9*prob10
  
  prob.inf<-ifelse(prob.inf.a<=0,0.000000001,ifelse(prob.inf.a>1,1-0.000000001,prob.inf.a))
  loglik<-prev*log(prob.inf-0.0000000001)+(1-prev)*log(1-prob.inf+0.0000000001)
  -sum(loglik)
}

n.param<-3

model12_34<-optim(rep(0.005,n.param),my.function,method="L-BFGS-B",
                  lower=rep(-0.1,n.param), upper=rep(0.99,n.param))
model12_34


##01_34 model going  negative
my.function<-function(my.k){
  
  k<-c(my.k[1],my.k[1],my.k[2],my.k[3],my.k[3],0,0)
  
  prob1<-1-k[Sporo[,1]+1]
  prob2<-1-k[Sporo[,2]+1]
  prob3<-1-k[Sporo[,3]+1] 
  prob4<-1-k[Sporo[,4]+1]
  prob5<-1-k[Sporo[,5]+1]
  prob6<-1-k[Sporo[,6]+1]
  prob7<-1-k[Sporo[,7]+1]
  prob8<-1-k[Sporo[,8]+1] 
  prob9<-1-k[Sporo[,9]+1]
  prob10<-1-k[Sporo[,10]+1]


  
  prob.inf.a<-1-prob1*prob2*prob3*prob4*prob5*prob6*prob7*prob8*prob9*prob10
  
  prob.inf<-ifelse(prob.inf.a<=0,0.000000001,ifelse(prob.inf.a>1,1-0.000000001,prob.inf.a))
  loglik<-prev*log(prob.inf-0.0000000001)+(1-prev)*log(1-prob.inf+0.0000000001)
  -sum(loglik)
}

n.param<-3

model01_34<-optim(rep(0.005,n.param),my.function,method="L-BFGS-B",
                  lower=rep(-0.1,n.param), upper=rep(0.99,n.param))
model01_34



##01_23 model going  negative
my.function<-function(my.k){
  
  k<-c(my.k[1],my.k[1],my.k[2],my.k[2],my.k[3],0,0)
  
  prob1<-1-k[Sporo[,1]+1]
  prob2<-1-k[Sporo[,2]+1]
  prob3<-1-k[Sporo[,3]+1] 
  prob4<-1-k[Sporo[,4]+1]
  prob5<-1-k[Sporo[,5]+1]
  prob6<-1-k[Sporo[,6]+1]
  prob7<-1-k[Sporo[,7]+1]
  prob8<-1-k[Sporo[,8]+1] 
  prob9<-1-k[Sporo[,9]+1]
  prob10<-1-k[Sporo[,10]+1]


  
  prob.inf.a<-1-prob1*prob2*prob3*prob4*prob5*prob6*prob7*prob8*prob9*prob10
  
  prob.inf<-ifelse(prob.inf.a<=0,0.000000001,ifelse(prob.inf.a>1,1-0.000000001,prob.inf.a))
  loglik<-prev*log(prob.inf-0.0000000001)+(1-prev)*log(1-prob.inf+0.0000000001)
  -sum(loglik)
}

n.param<-3

model01_23<-optim(rep(0.005,n.param),my.function,method="L-BFGS-B",
                  lower=rep(-0.1,n.param), upper=rep(0.99,n.param))
model01_23



##123 model going  negative
my.function<-function(my.k){
  
  k<-c(my.k[1],my.k[2],my.k[2],my.k[2],my.k[3],0,0)
  
  prob1<-1-k[Sporo[,1]+1]
  prob2<-1-k[Sporo[,2]+1]
  prob3<-1-k[Sporo[,3]+1] 
  prob4<-1-k[Sporo[,4]+1]
  prob5<-1-k[Sporo[,5]+1]
  prob6<-1-k[Sporo[,6]+1]
  prob7<-1-k[Sporo[,7]+1]
  prob8<-1-k[Sporo[,8]+1] 
  prob9<-1-k[Sporo[,9]+1]
  prob10<-1-k[Sporo[,10]+1]


  
  prob.inf.a<-1-prob1*prob2*prob3*prob4*prob5*prob6*prob7*prob8*prob9*prob10
  
  prob.inf<-ifelse(prob.inf.a<=0,0.000000001,ifelse(prob.inf.a>1,1-0.000000001,prob.inf.a))
  loglik<-prev*log(prob.inf-0.0000000001)+(1-prev)*log(1-prob.inf+0.0000000001)
  -sum(loglik)
}

n.param<-3

model123<-optim(rep(0.005,n.param),my.function,method="L-BFGS-B",
                lower=rep(-0.1,n.param), upper=rep(0.99,n.param))
model123



##liklihood ratio test
my.mles<-numeric(15)
##null v 4
my.mles[1]<-modelnull$value
my.mles[2]<-model01234$value
my.mles[3]<-model1234$value
my.mles[4]<-model234$value
my.mles[5]<-model34$value
my.mles[6]<-modelfull$value
my.mles[7]<-model01$value
my.mles[8]<-model012$value
my.mles[9]<-model0123$value
my.mles[10]<-model01_234$value
my.mles[11]<-model012_34$value
my.mles[12]<-model12_34$value
my.mles[13]<-model01_34$value
my.mles[14]<-model01_23$value
my.mles[15]<-model123$value

my.df<-c(1  ,1  ,2  ,3	,4	,5	,4	,3	,2	,2	,2	,3	,3	,3,	3)
my.AIC<-my.df*2+2*my.mles
which.min(my.AIC)

cbind(seq(1,15,1),my.AIC)



##impact of negative bites

##full model going  negative
my.function<-function(my.k){
  
  k<-c(0,my.k[1],my.k[2],my.k[3],my.k[4],0,0)
  
  prob1<-1-k[Sporo[,1]+1]
  prob2<-1-k[Sporo[,2]+1]
  prob3<-1-k[Sporo[,3]+1] 
  prob4<-1-k[Sporo[,4]+1]
  prob5<-1-k[Sporo[,5]+1]
  prob6<-1-k[Sporo[,6]+1]
  prob7<-1-k[Sporo[,7]+1]
  prob8<-1-k[Sporo[,8]+1] 
  prob9<-1-k[Sporo[,9]+1]
  prob10<-1-k[Sporo[,10]+1]
  
  
  
  prob.inf.a<-1-prob1*prob2*prob3*prob4*prob5*prob6*prob7*prob8*prob9*prob10
  
  prob.inf<-ifelse(prob.inf.a<=0,0.00000000001,ifelse(prob.inf.a>1,1-0.00000000001,prob.inf.a))
  loglik<-prev*log(prob.inf)+(1-prev)*log(1-prob.inf)
  -sum(loglik)
}

n.param<-4

modelfullneg<-optim(rep(0.5,n.param),my.function,method="L-BFGS-B",
                 lower=rep(0.2,n.param), upper=rep(0.9,n.param))
modelfullneg


mle.Full<-modelfull$value
mle.no0<-modelfullneg$value

5*2+2*mle.Full
4*2+2*mle.no0

my.p<-1-pchisq(2*(mle.no0-mle.Full), 1)
my.p



##is one zero enough
is.zeros<-ifelse(data.mouse$count0>0,1,0)
#is.zeros<-data.mouse$count0
##full model going  negative
my.function<-function(my.k){
  
  k<-c(0,my.k[1],my.k[2],my.k[3],my.k[4],0,0)
  
  prob1<-1-k[Sporo[,1]+1]
  prob2<-1-k[Sporo[,2]+1]
  prob3<-1-k[Sporo[,3]+1] 
  prob4<-1-k[Sporo[,4]+1]
  prob5<-1-k[Sporo[,5]+1]
  prob6<-1-k[Sporo[,6]+1]
  prob7<-1-k[Sporo[,7]+1]
  prob8<-1-k[Sporo[,8]+1] 
  prob9<-1-k[Sporo[,9]+1]
  prob10<-1-k[Sporo[,10]+1]
  
  zero<-1-(is.zeros*my.k[5])
  
  prob.inf.a<-(1-prob1*prob2*prob3*prob4*prob5*prob6*prob7*prob8*prob9*prob10)*zero
  
  prob.inf<-ifelse(prob.inf.a<=0,0.00000000001,ifelse(prob.inf.a>1,1-0.00000000001,prob.inf.a))
  loglik<-prev*log(prob.inf)+(1-prev)*log(1-prob.inf)
  -sum(loglik)
}

n.param<-5

modelfullisZero<-optim(rep(0.05,n.param),my.function,method="L-BFGS-B",
                    lower=rep(0.001,n.param), upper=rep(0.99,n.param))
modelfullisZero




##best fit model
##ci
## a 5 by 5 parameter space

modelFull

size.of.grid<-30
k1.range<-seq(0,0.05,length=size.of.grid)
k2.range<-seq(0.15,0.55,length=size.of.grid)
k3.range<-seq(0.4,0.8,length=size.of.grid)
k4.range<-seq(0.4,0.85,length=size.of.grid)
k5.range<-seq(0.7,1,length=size.of.grid)
ci.grid.k1<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid,size.of.grid,size.of.grid))
ci.grid.k2<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid,size.of.grid,size.of.grid))
ci.grid.k3<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid,size.of.grid,size.of.grid))
ci.grid.k4<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid,size.of.grid,size.of.grid))
ci.grid.k5<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid,size.of.grid,size.of.grid))

for(a in 1:size.of.grid){
  for(b in 1:size.of.grid){
    for(d in 1:size.of.grid){
      for(e in 1:size.of.grid){
        for(f in 1:size.of.grid){
          ci.k<-c(k1.range[a],k2.range[b],k3.range[d],k4.range[e],k5.range[f])
          ci.n.param<-length(ci.k)
          ci.fit<-my.function(ci.k)
          ci.grid.k1[a,b,d,e,f]<-ifelse(ci.fit<=modelFull$value+qchisq(0.95, ci.n.param)/2,
                                    k1.range[a],NA)
          ci.grid.k2[a,b,d,e,f]<-ifelse(ci.fit<=modelFull$value+qchisq(0.95, ci.n.param)/2,
                                    k2.range[b],NA)
          ci.grid.k3[a,b,d,e,f]<-ifelse(ci.fit<=modelFull$value+qchisq(0.95, ci.n.param)/2,
                                    k3.range[d],NA)
          ci.grid.k4[a,b,d,e,f]<-ifelse(ci.fit<=modelFull$value+qchisq(0.95, ci.n.param)/2,
                                    k4.range[b],NA)
          ci.grid.k5[a,b,d,e,f]<-ifelse(ci.fit<=modelFull$value+qchisq(0.95, ci.n.param)/2,
                                    k5.range[d],NA)
        }
      }
    }
  }
}
  

k1.ci<-range(ci.grid.k1, na.rm = TRUE)
k2.ci<-range(ci.grid.k2, na.rm = TRUE)
k3.ci<-range(ci.grid.k3, na.rm = TRUE)
k4.ci<-range(ci.grid.k4, na.rm = TRUE)
k5.ci<-range(ci.grid.k5, na.rm = TRUE)

my.lowers<-rbind(k1.ci,k2.ci,k3.ci,k4.ci,k5.ci)[,1]
my.uppers<-rbind(k1.ci,k2.ci,k3.ci,k4.ci,k5.ci)[,2]

cbind(modelFull$par,my.lowers,my.uppers)
