#######################################                              
##                                   ##
## Oocyst and Sporozoite prevalence  ##
##   (binomial error distrib)        ##
##                                   ##
#######################################


#
## Data: from EffectSize_M2Mmodel_ChainBinomialGLMM.R
#

data.mouse3
data.mosqu3
data.mouse4
data.mosqu4

ds3<-data.mouse3[data.mouse3$treat!="SD" & data.mouse3$treat!="ATV",];ds3
dq3<-data.mosqu3[data.mosqu3$treat!="SD" & data.mosqu3$treat!="ATV",];dq3
ds4<-data.mouse4[data.mouse4$treat!="SD" & data.mouse4$treat!="ATV",];ds4
dq4<-data.mosqu4[data.mosqu4$treat!="SD" & data.mosqu4$treat!="ATV",];dq4

##RENAME AND GROUP
prevd<-expand.grid("treat"=unique(dq4$treat))
prevd$rep1_uninf<-dq3$uninf_mosq
prevd$rep1_inf<-dq3$inf_mosq
prevd$rep2_uninf<-dq4$uninf_mosq
prevd$rep2_inf<-dq4$inf_mosq

newc<-rep(prevd$treat,2)
uninf<-c(prevd$rep1_uninf,prevd$rep2_uninf)
inf<-c(prevd$rep1_inf,prevd$rep2_inf)
prev<-inf/(inf+uninf)

pdn<-prevd

pinf<-c(pdn$rep1_inf[2:5]/(pdn$rep1_inf[2:5]+pdn$rep1_uninf[2:5]),
        pdn$rep2_inf[2:5]/(pdn$rep2_inf[2:5]+pdn$rep2_uninf[2:5]))
        

pconinf<-c(rep((pdn$rep1_inf[1]/(pdn$rep1_inf[1]+pdn$rep1_uninf[1])),4),
           rep((pdn$rep2_inf[1]/(pdn$rep2_inf[1]+pdn$rep2_uninf[1])),4))
           

conc.vec<-rep(pdn$treat[2:5],2)
plot(conc.vec,1-(pinf/pconinf),ylim=c(0,1),bty="n",xlim=c(0,8),las=1,xlab="Treatment",ylab="Prevalence efficacy",cex=1.25,col="chartreuse4",pch=16)

((1-(pinf/pconinf))[1:4]+(1-(pinf/pconinf))[5:8])/2







