plot(paras~meanooc2)



log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * meanooc2)) / (1 + exp(a + b * meanooc2)) ) 
  prev1<-paras  
  loglik1a<- prev1* log((pred1a)+0.0000001)+(1-prev1)*log(1-((pred1a)-0.0000001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2

nc<-seq(0,40,1)
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmod
pred<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) ) 
lines(pred~nc)

gom.binom<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  c<-p.vec[3]
  
  pred1<- (a * exp (b * exp(c *  meanooc2)))
  data1<- paras
  loglik1<- data1* log((pred1)+0.00001)+(1-data1)*log(1-((pred1)-0.00001))
  -sum(loglik1,na.rm=T)
}
n.param<-3
gommod<-optim(c(0.75,-2,-0.5),gom.binom,method="L-BFGS-B",lower=c(0.5,-10,-5),upper=c(0.99,-1,-0.0001))
gommod

nc<-seq(0,100,1)

pred2<-   gommod$par[1] * exp (gommod$par[2] * exp(gommod$par[3] * nc))


plot(paras~meanooc2,log="x",xlim=c(1,100),ylim=c(0,1),
     ylab="Mean prevalence blood-stage parasitemia",xlab="Mean oocyst count",
     cex.lab=1.4,cex=1.2,pch=19,col="chocolate4")
lines(nc,pred2,col="yellow",lwd=2)
arrows(1,0.495,14.84,0.495,length=0,col="chocolate4",lty=3)
#arrows(1,0.601,30,0.601,length=0,col="chocolate4",lty=3)
arrows(14.84,0,14.84,0.72250577,length=0,col="chocolate4",lty=3)


#oocysts to blood stage infection
a = -4.166956
b = 5.516592
aa = 0.9434476   
bb =-10.0000000
cc = -0.4614941
ncset <- seq(0,100,1)
z<-exp(a + b * (aa * exp(bb * exp(cc * ncset))))/(1 + exp(a + b * (aa * exp(bb * exp(cc * ncset)))))

#plot(z~ncset,log="x",ylab="Blood-stage prevalence", xlab = "Oocysts",ylim=c(0,1))
lines(z~ncset,col="chartreuse4",lwd=2)

#points(z~ncset,col="chartreuse4",pch=19)
arrows(1,0.72250577,mean(OZFER$oocysts),0.72250577,length=0,col="chocolate4",lty=3)
