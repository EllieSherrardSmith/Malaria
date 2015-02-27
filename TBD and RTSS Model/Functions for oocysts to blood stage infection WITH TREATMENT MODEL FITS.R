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

#####
####### Libraries required
#####

library(MASS)
library(ggplot2)
library(deSolve)
library(R2OpenBUGS)

# x = oocysts
# y = sporozoites in salivary glands
# z = sporozoites in skin
# d = liver stage infections
# g = blood stage infections  

# f(x) = oocysts to sporozoites in salivary glands
# f(y) = sporozoites in salivary glands to sporozoites in skin
# f(z) = sporozoites in skin to liver stage infections
# f(d) = liver stage infections to blood stage infections

#######################################
## OOCYSTS TO BLOOD STAGE INFECTION  ##
#######################################
# Bloodstage infection = f(x) + f(y) + f(z) + f(d)

#########################################################################
##                                                                     ##
##      #####  #####       ####       ##    ####    ##        ###      ##
##    ###   ####   ###    ##  ##      ##   ##  ##   ##         ##      ##
##    ##     ##     ##    ##  ##    ####   ######   ##         ##      ##
##    ##     ##     ##    ##  ##   #  ##   ##       ##         ##      ##
##    ##     ##     ##     ####     ####    ####     ###      ####     ##
##                                                                     ##
#########################################################################

############################################################
##                                                        ## 
###                                                       ## 
####  Stage 1: Oocysts to Sporozoites in salivary gland   ##
###    Using function from Sinden et al. 2007             ##
##     Using oocyst data form Blagborough                 ##
##     Using sporozoite data from Medica and Sinnis 2005  ##
##                                                        ##
############################################################

#######################################
##   f(x) = a*x / (1 + b*x)
##   f(x) = 450*x / (1 + 0.018*x)     ##    ##think about the value of a (1250 - Stone 2013 / 62 - Sinden 2007)
##   f(xUPP) = 1200*x/(1+0.016*x)
##   f(xLOW) = 80*x/(1+0.02*x)
#######################################

##  Using data from Blagborough laboratory bites 1 - 10 
##  Round 1 data (N = 50 mosquitoes)

oocyst<-read.table("E:\\IMPERIAL Nov 2014\\Malaria Modelling\\1_oocyst to sg\\OocystsATV50Start.txt",header=T)
head(oocyst)
oocyst$PrevOoc<-ifelse(oocyst[,1]==0,0,1)##Prevalence = 74%
mean(oocyst$OocystsATV50Start)           ##Mean = 41.7, Nsamp = 50 

##original
x<-sort(oocyst$OocystsATV50Start)
yEst<-450*x/(1+0.018*x)
yhigher<-1200*x/(1+0.016*x)
ylower<-30*x/(1+0.02*x)
max_sporozoites<-max(y)

par(mfrow=c(2,2))
plot(yEst~x, pch=20, ylim=c(0,70000),xlim=c(0,250),
	xlab="Number of Oocysts",ylab="Number of salivary gland sporozoites")
############FIT
sat.binom<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1<- a*x/(1+b*x)


  data1<- yEst
   
  loglik1<- data1* log((pred1))+(1-data1)*log(1-((pred1)))
    
	-sum(loglik1,na.rm=T)
}
n.param<-2
satmod<-optim(c(450,0.018),sat.binom,method="L-BFGS-B",lower=c(0,0),upper=c(1000,1000))
satmod

ncA<-seq(0,max(x),1)
predA<-satmod$par[1]*ncA / (1 + satmod$par[2]*ncA)
lines(predA~ncA,col="red")

##lower CI
sat.binomL<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1<- a*x/(1+b*x)
  data1<- ylower
  loglik1<- data1* log((pred1))+(1-data1)*log(1-((pred1)))
    
	-sum(loglik1,na.rm=T)
}
n.param<-2
satmodL<-optim(c(30,0.02),sat.binomL,method="L-BFGS-B",lower=c(0,0),upper=c(1000,1000))
satmodL

nc<-seq(0,max(x),1)
predL<-satmodL$par[1]*nc / (1 + satmodL$par[2]*nc)
lines(predL~nc,col="red",lty=2)

##upper CI
sat.binomU<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1<- a*x/(1+b*x)
  data1<- yhigher
  loglik1<- data1* log((pred1))+(1-data1)*log(1-((pred1)))
    
	-sum(loglik1,na.rm=T)
}
n.param<-2
satmodU<-optim(c(1200,0.016),sat.binomU,method="L-BFGS-B",lower=c(0,0),upper=c(1500,1000))
satmodU

ncA<-seq(0,max(x),1)
predUA<-satmodU$par[1]*nc / (1 + satmodU$par[2]*nc)
lines(predUA~ncA,col="red",lty=2)

############################################################################ 
##                                                                        ##
###                                                                       ##
#### Stage 2: Sporozoites in salivary glands to sporozoites in the skin   ##
###  Using Medica and Sinnuds 2005 
###                                                                       ##
##                                                                        ##  
############################################################################

############################
## f(y) ~ c*y / (d + h*y)
## f(y) ~ 0.005750*y / (1 + 0.000018*y)   ##
## f(yUPP) ~ 0.034*y / (1 + 0.000018*y)
## f(yLOW) ~ 0.0029*y / (1 + 0.00018*y)
############################

##  Using data from Medica and Sinnis 2005  

sporo<-read.table("E:\\IMPERIAL Nov 2014\\Malaria Modelling\\2_sg to skin\\sg to skin.txt",header=T)
summary(sporo)

##original
y<-round(sporo$MedicaSinnisSalGlands);mean(y)##mean = 21589 sporozoites in glands
ywithKestimate<-mean(y)^2/(var(y)-mean(y));ywithKestimate
ywithKestimate2<-((mean(y)^2)-var(y)/length(y))/(var(y)-mean(y));ywithKestimate2

sporSGPrev<-ifelse(y==0,0,1);sum(sporSGPrev)
z<-round(sporo$MedicaSinnisMouseSkin);mean(z)##mean = 123 sporozoites injected
sporSkinPrev<-ifelse(z==0,0,1);sum(sporSkinPrev)##78% of infected mosquitoes pass on infection
zwithKestimate<-mean(z)^2/(var(z)-mean(z));zwithKestimate
zwithKestimate2<-((mean(z)^2)-var(z)/length(z))/(var(z)-mean(z));zwithKestimate2
#hist(z,breaks=20);mean(z)
sporo$z<-round(sporo$MedicaSinnisMouseSkin)
sporo$y<-round(sporo$MedicaSinnisSalGlands)

##linear fit ## think about the range for different data
plot(z~y,pch=20,xlim=c(0,70000),ylim=c(0,1200),
	xlab="Number of salivary gland sporozoites",ylab="Number of skin sporozoites")
#points(z[1:50]~y2,pch=20,xlim=c(0,40000),ylim=c(0,1200),col="blue",
#	xlab="Number of salivary gland sporozoites",ylab="Number of skin sporozoites")

		##Using Jin et al. 2007

		################################################################
		## from Jin et al 2007 Inf and Immun 75:5532-5539	
		################################################################

		## Estimate the negbin distribution of sporozoites in mosquitoes
		dmean = 11282; nsamp = 40; variance = 130899240 #sd^2 from Jin et al 
		k<-(dmean^2-(variance/nsamp))/(variance-dmean); k

		Jin_spor<-rnegbin (nsamp, dmean, k)     	# creates a negbin distribution with the data
		#Jin_spor<-sort(dist1) 				# sorts distribution lowest parasite count first

		## Estimate the negbin distribution of sporozoites in mouse
		dmeansk = 281.1; nsamp = 40; variancesk = 79923.6 #sd^2 from Jin et al 
		kskin<-(dmeansk^2-(variancesk/nsamp))/(variancesk-dmeansk); kskin

		Jin_skin<-rnegbin (nsamp, dmeansk, kskin)     	# creates a negbin distribution with the data
		#Jin_skin<-sort(dist2) 				# sorts distribution lowest parasite count first
		Jin_skin_prev<-ifelse(Jin_skin==0,0,1);sum(Jin_skin_prev)

		##Using Frischknecht et al 2004

		################################################################
		## from Frischknecht et al 2004 
		################################################################

		## Estimate the negbin distribution of sporozoites in mosquitoes
		dmean<-32000; nsamp = 60; k = 1 #estimated  
		
		Fr_spor<-rnegbin (nsamp, dmean, k)     	# creates a negbin distribution with the data
		#Fr_spor<-sort(dist3) 				# sorts distribution lowest parasite count first

		## Estimate the negbin distribution of sporozoites in mouse
		dmeansk = 114; nsamp = 52; variance = 160^2 #sd^2 from Frischknecht
		kskin3<-(dmeansk^2-(variance/nsamp))/(variance-dmeansk); kskin3

		Fr_skin<-rnegbin (nsamp, dmeansk, kskin3)     	  	# creates a negbin distribution with the data
		Fr_skin<-c(rep(0,8),Fr_skin)
		#Fr_skin<-c(rep(0,8),sort(dist4));length(Fr_skin) 	# make N=60 to match sal. gland and sorts distribution lowest parasite count first
		Fr_skin_prev<-ifelse(Fr_skin==0,0,1);sum(Fr_skin_prev)# 80%

 #points(Jin_skin~Jin_spor,col="blue",pch=20)
 #points(Fr_skin~Fr_spor,col="red",pch=20)
##tried a sigmoid fit (Gompertz) too because some infected mosquitoes do not cause infection in the mouse
##no feasible solution found

#
## Saturating function
#

############FIT
sat.binom<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1<- a*y/(1+b*y)		  ##MEDICA AND SINNIS 2005 ###try sigmoidal fit too
  pred2<- a*Jin_spor/(1+b*Jin_spor)##JIN ET AL 2007
  pred3<- a*Fr_spor/(1+b*Fr_spor)  ##FRISCHKNECHT 2004
 
  data1<- z
  data2<- Jin_skin
  data3<-Fr_skin 

  loglik1<- data1* log((pred1))+(1-data1)*log(1-((pred1)))
  loglik2<- data2* log((pred2))+(1-data2)*log(1-((pred2)))
  loglik3<- data3* log((pred3))+(1-data3)*log(1-((pred3)))
    
	-sum(loglik1,loglik2,loglik3,na.rm=T)
}
n.param<-2
satmod<-optim(c(0.005750,0.000018),sat.binom,method="L-BFGS-B",lower=c(0.001,0.000000001),upper=c(0.1,1))
satmod

nc<-seq(0,max(y),1)
pred<-satmod$par[1]*nc / (1 + satmod$par[2]*nc)
lines(pred~nc,col="red")

sat.binomL<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1<- a*y/(1+b*y)
  data1<- z
  loglik1<- data1* log((pred1))+(1-data1)*log(1-((pred1)))
    
	-sum(loglik1,na.rm=T)
}
n.param<-2
satmodL<-optim(c(0.0029,0.00018),sat.binomL,method="L-BFGS-B",lower=c(0,0),upper=c(1,1))
satmodL

nc<-seq(0,max(y),1)
predl<-satmodL$par[1]*nc / (1 + satmodL$par[2]*nc)
lines(predl~nc,col="red",lty=2)

sat.binomU<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1<- a*y/(1+b*y)
  data1<- z
  loglik1<- data1* log((pred1))+(1-data1)*log(1-((pred1)))
    
	-sum(loglik1,na.rm=T)
}
n.param<-2
satmodU<-optim(c(0.034,0.000018),sat.binomU,method="L-BFGS-B",lower=c(0,0),upper=c(1,1))
satmodU

nc<-seq(0,max(y),1)
predU<-satmodU$par[1]*nc / (1 + satmodU$par[2]*nc)
lines(predU~nc,col="red",lty=2)


##This means that for the different studies, there is a
##transfer rate of 0.0057 (Medica and Sinnis 2005);(mean(skin sporozoites)/mean(salivary gland))
##			 0.0249 (Jin et al 2007);
##			 0.00356(Frischknecht et al 2004)

##And for Medica and Sinnis the distribution of transfer can be calculated
sporo$PrSkin<-sporo$z/sporo$y ##ranging from 0 to 0.102; Neg bin distribution: 
sporo2<- sporo[with(sporo, order(z)), ]
		##1 sporozoite was transferred at a rate of 0.000268 and 0.0000971
		##2 sporozoites at rate 0.000690; 0.000130; 0.0000956...

		sporo$spN<-numeric(length(sporo$z))
		vec<-sporo$z
			for (i in 1:length(sporo$z)){
			sporo$spN[i]<-mean(sporo$PrSkin[sporo$z==vec[i]])}
plot(sporo$z,sporo$spN,log="xy",
	ylab="Proportion of sporozoites transferred",xlab="Sporozoites in skin post bite")
line<-lm(sporo$spN~sporo$z+0)
summary(lm(sporo$spN~sporo$z+0));abline(line, col="blue",untf=TRUE)


sat.binomPR<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1<- a*sporo$z/(1+b*sporo$z)		  ##MEDICA AND SINNIS 2005
  data1<- sporo$spN

  loglik1<- data1* log((pred1)-0.001)+(1-data1)*log(1-((pred1)-0.001))
	-sum(loglik1,na.rm=T)
}
n.param<-2
satmodPR<-optim(c(0.00350,0.000018),sat.binomPR,method="L-BFGS-B",lower=c(0.0001,0.00001),upper=c(0.001,0.01))
satmodPR

nc<-seq(1,max(z),1)
predU<-satmodPR$par[1]*nc / (1 + satmodPR$par[2]*nc)
#lines(predU~nc,col="blue",lty=2)

##################################################################
##Calculate a function to fit oocysts to skin stage infections
##And then the probability of blood stage infection from oocysts
##
##f(x) = 450*x / (1 + 0.018*x)
##f(y) = 0.005750*y / (1 + 0.000018*y)
##f(z) = (0.005750*(450*x / (1 + 0.018*x)))/(1+0.000018*(450*x / (1 + 0.018*x)))
zEST<-(0.005750*(450*x / (1 + 0.018*x)))/(1+0.000018*(450*x / (1 + 0.018*x)))
nc<-seq(1,200,1)
PredzEST<-(0.005750*(450*nc / (1 + 0.018*nc)))/(1+0.000018*(450*nc / (1 + 0.018*nc)))
##Now working backward, from predU I can calculate the estimated number of 
##sporozoites in the salivary glands that resulted in a given number of skin
##sporozoites
salGEstimate<-nc/predU;range(salGEstimate)
##
##
##
##From the first relationship between oocysts and sg sporozoites
plot(yEst~x, pch=20, ylim=c(0,70000),xlim=c(0,250),
	xlab="Number of Oocysts",ylab="Number of salivary gland sporozoites")
lines(predA~ncA,col="red")
segments(0, 10017.93, ncA[39], 10017.93,col = "blue", lty = 1, lwd = 2)
segments(ncA[39], 0, ncA[39],10017.93,col = "blue", lty = 1, lwd = 2)
lines(predUA~ncA,col="red",lty=2)
segments(0, 10017.93, ncA[11], 10017.93,col = "dark green", lty = 2, lwd = 1)
segments(ncA[11], 0, ncA[11],10017.93,col = "dark green", lty = 2, lwd = 1)

			plot(yEst~x, pch=20, log="xy",#ylim=c(0,70000),xlim=c(0,250),
				xlab="Number of Oocysts",ylab="Number of salivary gland sporozoites")
			lines(predA~ncA,col="red")
			segments(1, 10017.93, ncA[39], 10017.93,col = "blue", lty = 2, lwd = 1)
			segments(ncA[39], 1, ncA[39],10017.93,col = "blue", lty = 2, lwd = 1)
			lines(predUA~ncA,col="red",lty=2)
			segments(1, 10017.93, ncA[11], 10017.93,col = "dark green", lty = 2, lwd = 1)
			segments(ncA[11], 1, ncA[11],10017.93,col = "dark green", lty = 2, lwd = 1)
##
##
##And from the 2nd relationship between oocysts and skin sporozoites
##
###
#### FIGURE 1A
###
##
##
plot(PredzEST~nc, pch=20,
	xlab="Number of Oocysts",ylab="Number of sporozoites in skin")
lines(PredzEST~nc,col="red")
##
##
			plot(PredzEST~nc, pch=20, log="xy",
			xlab="Number of Oocysts",ylab="Number of sporozoites in skin")
			lines(PredzEST~nc,col="red")
##	
##
##
###So...One sporozoite will always result from minimum 10 oocysts and average 32 oocysts
###We need to reduce the number of oocysts to below these thresholds to reduce prevalence
##
##Now, what is the probability of these infections resulting in bloodstage infection?
##
##In the Blagborough control data, 73% of the control mice developed infection 
##(pooling the data for different biting pressures; 1 bite, 2, 5 or 10 bites)
##
##Using White et al. 2013 PLoS One: 
##Probability that k sporozoites from an infectious bite will release bloodstage infection
##is given by:
##
dist_k<-z#subset(z,z>0)
##
r<-zwithKestimate2 ##see above but basically the neg bin shape parameter
r<-0.011 ## from White et al. 2013 PLoS One
probK<-1-(1-r)^dist_k
##
###
#### FIGURE 1B
###
##
plot(sort(probK)~nc[1:59],pch=20,
	ylab="Probability that sporozoites cause blood stage infection",xlab="Sporozoites")
##
##
#################################################
## LOGISTIC FIT (logistic fit is better)
#################################################
####r =0.011
log.binomSK<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1<- (exp(a + b * nc[1:length(probK)])) / (1 + exp(a + b * nc[1:length(probK)])) 
  data1<- sort(probK)

  loglik1<- data1* log((pred1)+0.00001)+(1-data1)*log(1-((pred1)-0.00001))
  -sum(loglik1,na.rm=T)
}
n.param<-2

logmodSKlog<-optim(c(0,0.02),log.binomSK,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmodSKlog
ncsk<-seq(1,500,1)
predsklog<- (exp(logmodSKlog$par[1] + logmodSKlog$par[2] * ncsk)) / (1 + exp(logmodSKlog$par[1] + logmodSKlog$par[2] * ncsk))
lines(predsklog~ncsk,col="red",lty=1,lwd=3)
##
segments(0, 0.733, 44, 0.733,col = "blue", lty = 1, lwd = 2)
segments(44, 0, 44,0.733,col = "blue", lty = 1, lwd = 2)
##
##
##
PrOocBS<-numeric(150)
	for (j in 1:150){
		PrOocBS[j]<-predsklog[PredzEST[j]]}
##
###
#### FIGURE 1C
###
##	
	plot(PrOocBS[1:50]~nc[1:50],xlab="Number of Oocysts",ylab="Probability of bloodstage infection")
	plot(PrOocBS[1:50]~nc[1:50],xlab="Number of Oocysts",log="xy",ylab="Probability of bloodstage infection")
#################################################
## LOGISTIC FIT (logistic fit is better)
#################################################
####
log.binomSK2<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1<- (exp(a + b * nc[1:length(PrOocBS)])) / (1 + exp(a + b * nc[1:length(PrOocBS)])) 
  data1<- sort(PrOocBS)

  loglik1<- data1* log((pred1)+0.00001)+(1-data1)*log(1-((pred1)-0.00001))
  -sum(loglik1,na.rm=T)
}
n.param<-2

logmodSK2<-optim(c(0,0.02),log.binomSK2,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmodSK2
ncsk<-seq(1,50,1)
pred2<- (exp(logmodSK2$par[1] + logmodSK2$par[2] * ncsk)) / (1 + exp(logmodSK2$par[1] + logmodSK2$par[2] * ncsk))
lines(pred2~ncsk,col="red",lty=1,lwd=3)	
##
##
segments(0, 0.733, 33, 0.733,col = "blue", lty = 1, lwd = 2)
segments(33, 0, 33,0.733,col = "blue", lty = 1, lwd = 2)
##
pred2
##
###################################################################################################

#################################################
## GOMPERTZ FIT (but see logistic fit is better)
#################################################
sat.binomSK<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  c<-p.vec[3]
  
  #pred1<- (exp(a + b * nc[1:length(probK)])) / (1 + exp(a + b * nc[1:length(probK)])) 
  pred1<-(a * exp (b * exp(c * nc[1:length(PrOocBS)])))
  data1<- sort(PrOocBS)

  loglik1<- data1* log((pred1)+0.00001)+(1-data1)*log(1-((pred1)-0.00001))
  -sum(loglik1,na.rm=T)
}
n.param<-2

satmodSK<-optim(c(1,-28,-0.1),sat.binomPR,method="L-BFGS-B",lower=c(0.001,-50,-5),upper=c(5,-1,-0.01))
satmodSK

ncsk<-seq(1,100,1)
#predsk2<-(0.99* exp (-33 * exp(-0.16 * ncsk)));predsk2

predsk2<-(satmodSK$par[1] * exp (satmodSK$par[2] * exp(satmodSK$par[3] * ncsk)));predsk2
lines(predsk2~ncsk,col="blue")


################################################
## Estimate the distribution of sporozoites in the skin
## using the estimates from the probability function above
################################################
segments(0, 0.733, 43, 0.733,col = "blue", lty = 1, lwd = 2)
segments(43, 0, 43,0.733,col = "blue", lty = 1, lwd = 2)

MouseControls<-rnegbin(50,43,zwithKestimate2)


################################################
## Estimate the distribution of sporozoites in the skin
## using the estimates from the probability function above
################################################
		##atv25% = green
		segments(0, 0.50, 38, 0.50,col = "dark green", lty = 3, lwd = 1) ##ATV25_2BITES (1BITE = no infections)
		segments(38, 0, 38,0.50,col = "dark green", lty = 3, lwd = 1)

		segments(0, 0.72, 42.5, 0.72,col = "dark green", lty = 3, lwd = 1) ##ATV25_5BITES
		segments(42.5, 0, 42.5,0.72,col = "dark green", lty = 3, lwd = 1)

		segments(0, 0.58, 39.5, 0.58,col = "dark green", lty = 3, lwd = 1) ##ATV25_10BITES
		segments(39.5, 0, 39.5,0.58,col = "dark green", lty = 3, lwd = 1)

			MouseATV25_1BITE<-rnegbin(50,0,zwithKestimate2)
			MouseATV25_2BITE<-rnegbin(50,38,zwithKestimate2)
			MouseATV25_5BITE<-rnegbin(50,43,zwithKestimate2)
			MouseATV25_10BITE<-rnegbin(50,40,zwithKestimate2)


		##atv50% = red
		segments(0, 0.20, 31, 0.20,col = "red", lty = 3, lwd = 1) ##ATV50_2BITES (1BITE = no infections)
		segments(31, 0, 31,0.20,col = "red", lty = 3, lwd = 1)

		segments(0, 0.28, 33.5, 0.28,col = "red", lty = 3, lwd = 1) ##ATV50_5BITES
		segments(33.5, 0, 33.5,0.28,col = "red", lty = 3, lwd = 1)

		segments(0, 0.33, 34.5, 0.33,col = "red", lty = 3, lwd = 1) ##ATV50_10BITES
		segments(34.5, 0, 34.5,0.33,col = "red", lty = 3, lwd = 1)

			MouseATV50_1BITE<-rnegbin(50,0,zwithKestimate2)
			MouseATV50_2BITE<-rnegbin(50,20,zwithKestimate2)
			MouseATV50_5BITE<-rnegbin(50,34,zwithKestimate2)
			MouseATV50_10BITE<-rnegbin(50,35,zwithKestimate2)

					par(mfrow=c(2,4))
						hist(MouseATV25_1BITE,breaks=15);hist(MouseATV25_2BITE,breaks=15);hist(MouseATV25_5BITE,breaks=15);hist(MouseATV25_10BITE,breaks=15)
						hist(MouseATV50_1BITE,breaks=15);hist(MouseATV50_2BITE,breaks=15);hist(MouseATV50_5BITE,breaks=15);hist(MouseATV50_10BITE,breaks=15)

			plot(sort(MouseControls),sort(MouseControls),xlim=c(0,500),ylim=c(0,500),
					ylab="Sporozoites in mice per treatment",xlab="Sporozoites in mice CONTROLS")
			points(sort(MouseControls),sort(MouseATV50_1BITE),pch=20,col="red")
			points(sort(MouseControls),sort(MouseATV50_2BITE),pch=20,col="red")
			points(sort(MouseControls),sort(MouseATV50_5BITE),pch=20,col="red")
			points(sort(MouseControls),sort(MouseATV50_10BITE),pch=20,col="red")
			points(sort(MouseControls),sort(MouseATV25_1BITE),pch=21,col="dark green")
			points(sort(MouseControls),sort(MouseATV25_2BITE),pch=20,col="dark green")
			points(sort(MouseControls),sort(MouseATV25_5BITE),pch=20,col="dark green")
			points(sort(MouseControls),sort(MouseATV25_10BITE),pch=20,col="dark green")
				abline(1,1)

				
################################################################
##                                                            ##
###                                                           ##
#### Stage 3: Sporozoites in skin to liver stage parasites    ##
###                                                           ##
##                                                            ##
################################################################

##################################
## f(z) = k*z / (1 + m*z) 
## f(z) = 0.25*z / (1 + 0.0081*z)  ##
## f(zUPP) = 0.5*z/(1 + 0.004*z)
## f(zLOW) = 0.05*z/(1 + 0.018*z)
##################################

##  Using literature that suggests only 25% of sporozoites reach the liver
##  and that hepcidin acts to prevent maturation of liver stages after a saturation
##  point.


	##From Andrew's Control's study M2M; on day 10 in each the
	##number of mice with any gameto or parasitaemia were counted
	##this gave a total of 44/60 (73.3%) for the blanks; from
	##Medica and Sinnis 2005, 78% of the mosquitoes induced
	##infections in the mice; from Frischknecht et al. this is 80%
	
	##These values are used to estimate the probability of blood stage infection

##original
z<-sort(round(sporo$MedicaSinnisMouseSkin))
d<-0.25*z/(1 + 0.0081*z)
dUPP<-0.50*z/(1 + 0.004*z)
dLOW<-0.05*z/(1 + 0.018*z)

#plot(d~z,pch=20,ylim=c(0,120),
	xlab="Number of skin sporozoites",ylab="Liver stage parasites")

g<-3000*d / (0.05+0.015*d)
gUPP<-9000*d / (0.05+0.014*d)
gLOW<-100*d / (0.05+0.05*d)

#plot(g~d,pch=20,ylim=c(0,600000),
	xlab="Liver stage parasites",ylab="Blood stage infections")
#lines(g~d)

##The mean number of sporozoites injected in 


#################################################################
##                                                             ##
###                                                            ## 
#### COMBINED MODEL: Occysts to blood stage infections         ##
###                                                            ##
##                                                             ##
#################################################################


##MODEL f(g) = f(x) + f(y) +f(z) + f(d)

##Number of blood stage infections = (1250x / (1 + 0.018x)) + 0.00575*(1250x / (1 + 0.018x)) + 0 + 0.25*(0.00575*(1250x / (1 + 0.018x))) / (1 + 0.018*(0.00575*(1250x / (1 + 0.018x)))) + 1000*(0.25*(0.00575*(1250x / (1 + 0.018x))) / (1 + 0.018*(0.00575*(1250x / (1 + 0.018x))))) / (0.5 + 0.5*(0.25*(0.00575*(1250x / (1 + 0.018x))) / (1 + 0.018*(0.00575*(1250x / (1 + 0.018x))))))

####ORIGINAL MODEL ESTIMATES
a<-600   ##from f(x)
b<-0.018 ##from f(x)
c<-0.00575   ##from f(y)
d<-0.6       ##from f(y)
h<-0.000018 ##from f(y)
k<-0.25  ##from f(z)
m<-0.0081 ##from f(z)
p<-3000 ##from f(d)
q<-0.05  ##from f(d)
r<-0.015  ##from f(d)

g2 <- a*x / (1 + b*x) +
	c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))) +
	k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))))) +
	p*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))) / (q + r*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))))


####ORIGINAL MODEL ESTIMATES UPPER BOUNDS
a<-1250   ##from f(x)
b<-0.016  ##from f(x)
c<-0.034     ##from f(y)
d<-0.6         ##from f(y)
h<-0.000018 ##from f(y)
k<-0.5  ##from f(z)
m<-0.004 ##from f(z)
p<-9000 ##from f(d)
q<-0.05  ##from f(d)
r<-0.014 ##from f(d)

gUPP <- a*x / (1 + b*x) +
	c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))) +
	k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))))) +
	p*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))) / (q + r*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))))


####ORIGINAL MODEL ESTIMATES LOWER BOUNDS
a<-80   ##from f(x)
b<-0.02  ##from f(x)
c<-0.0029    ##from f(y)
d<-0.6         ##from f(y)
h<-0.00018     ##from f(y)
k<-0.05  ##from f(z)
m<-0.018 ##from f(z)
p<-100 ##from f(d)
q<-0.05  ##from f(d)
r<-0.05  ##from f(d)

gLOW <- a*x / (1 + b*x) +
	c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))) +
	k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))))) +
	p*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))) / (q + r*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))))

####VACCINE TRIALS...can vary these parameters to se what gives the steepest decline at low exposure
a<-30   ##from f(x)
b<-0.018  ##from f(x)
c<-0.00057    ##from f(y)
d<-0.3         ##from f(y)
h<-0.0018     ##from f(y)
k<-0.05  ##from f(z)
m<-0.018 ##from f(z)
p<-100 ##from f(d)
q<-0.05  ##from f(d)
r<-0.05  ##from f(d)

gLOWCVACC <- a*x / (1 + b*x) +
	c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))) +
	k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))))) +
	p*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))) / (q + r*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))))

##see script "exploring probabilities.r"
dataORIG<-data.frame(x,g2,gUPP,gLOW)
dataORIG$g2_50<-dataORIG$g2/2
dataORIG$g2_80<-1*(dataORIG$g2/5)
dataORIG$g2_99<-1*(dataORIG$g2/100)
#windows();par(mfrow=c(1,2))
plot(dataORIG$g2~dataORIG$x,xlab="Number of Oocysts",ylab="Blood stage infections",
	log="xy",
	ylim=c(0.1,700000),xlim=c(0,10),
	pch=20)
lines(dataORIG$g2~dataORIG$x,col="blue",lwd=3)
lines(dataORIG$gUPP~dataORIG$x,lty=2,lwd=3)
lines(dataORIG$gLOW~dataORIG$x,lty=2,lwd=3)
lines(dataORIG$g2_50~dataORIG$x,lty=2,lwd=2,col="grey")
lines(dataORIG$g2_80~dataORIG$x,lty=2,lwd=2,col="grey")
lines(dataORIG$g2_99~dataORIG$x,lty=2,lwd=2,col="grey")
lines(dataORIG$gLOWCVACC~dataORIG$x,lwd=2,col="red")
######################################################################
##
###
#### Create an artificial data set to investigate for which f(g) fits
###
##
######################################################################

#Apply a normal distribution to the g2 data: 
g2sim<-matrix(nrow=50,ncol=100)

for(i in 1:100){
g2sim[,i] <- sort(rnorm(50, mean=mean(g2), sd=sd(g2)))}
g2sim <- ifelse(g2sim<0,0,g2sim)

#hist(x)##indicates negative binomial
##N=50, mean(x)=41.68, var(x)=3072.875; 
k=((mean(x))^2-((var(x))/50))/((var(x))-(mean(x)))
k

x2sim<- matrix(nrow=50,ncol=100)

for(i in 1:100){
x2sim[,i] <- sort(rnegbin(50,41.68,k))}

#plot(g2sim[,1]~x2sim[,1],pch=20,ylim=c(0,500000))
for(i in 1:100){
points(g2sim[,i]~x2sim[,i],pch=20,col="red")}

g2AllSim<-c(g2sim);x2AllSim<-c(x2sim)
datasim<-data.frame(g2AllSim,x2AllSim)
points(datasim$g2AllSim~datasim$x2AllSim,col="red")

##What about a negbin distribution for g2?
#g2simNB<- matrix(nrow=50,ncol=100)
#k=((mean(g2))^2-((var(g2))/50))/((var(g2))-(mean(g2)));k
#g2simNG<- matrix(nrow=50,ncol=100)
#for(i in 1:100){
#g2simNG[,i] <- sort(rnegbin(50,mean(g2),k))}

#g2AllSimNB<-c(g2simNG);x2AllSim<-c(x2sim)
#datasimNG<-data.frame(g2AllSimNB,x2AllSim)
#points(datasimNG$g2AllSimNB~datasimNG$x2AllSim,col="green")
##Worse

lo <- loess(datasim$g2AllSim~datasim$x2AllSim)
xl <- seq(min(x),max(x), (max(x) - min(x))/10000)
lines(xl, predict(lo,xl), col="darkred", lwd=2)
lines(g2~x,col="blue",lwd=3)

randomRows = function(df,n){
   return(df[sample(nrow(df),n),])
}
trial1<-randomRows(datasim,5000)

##If a vaccine were working on 50% of people
trial1$g2AllSimVACC<-trial1$g2AllSim/2

points(trial1$g2AllSimVACC~trial1$x2AllSim,col="green",pch=20)
lo2 <- loess(trial1$g2AllSimVACC~trial1$x2AllSim)
x2 <- seq(min(trial1$x2AllSim),max(trial1$x2AllSim), (max(trial1$x2AllSim) - min(trial1$x2AllSim))/10000)
lines(x2, predict(lo2,x2), col="darkgreen", lwd=2)

##If vaccine works on 80% of the population reducing blood stage infections by 50%
trial2<-randomRows(datasim,5000);trial2$g2AllSimVACC<-trial2$g2AllSim/2
trial2$combined<-c(trial2$g2AllSimVACC[1:4000],trial2$g2AllSim[4001:5000])
points(trial2$combined~trial2$x2AllSim,col="yellow",pch=20)
lo3 <- loess(trial2$combined~trial2$x2AllSim)
x3 <- seq(min(trial2$x2AllSim),max(trial2$x2AllSim), (max(trial2$x2AllSim) - min(trial2$x2AllSim))/10000)
lines(x3, predict(lo3,x3), col="orange", lwd=2)



#########################
##
###
#### Maximum Likelihood
###
##
##################################
data<-data.frame(x,y,z,d,g2)

loglikeSI<-function(p.vec){
	a<-p.vec[1]
	b<-p.vec[2]
  		pred1<- (exp(a + b * data$g2)) / (1 + exp(a + b * data$g2)) 
  		data1<- data$x
  loglik1<- (data1* log(pred1+0.00001)+(1-data1)*log(1-(pred1-0.00001))) *2.4444
  -sum(loglik1,na.rm=T) # the ratio of datapoints being used here and the independent number of unique concentration*replicate combinations
}
n.param<-2
logmod<-optim(c(-0.01,0.1),loglikeSI,method="L-BFGS-B",lower=c(-1000,0),upper=c(1000,1000))
logmod

###############################################################################
##
## ##           ## ##  ##  ##         ##     ## ####    ####    ####
##  ##         ##      ### ##          ##   ##      #  ##  ##  ##  ## 
##   ##   #   ##   ##  ##  ####         ## ##   #####  ##      ##
##    ## ### ##    ##  ##  ## ##         ###    ## ##  ##  ##  ##  ##
##     ### ###     ###  ## ## ##          #     #####   ####    ####
##
#################################################################################

####BOTH VACCINE EFFECTS MODEL ESTIMATES
a<-300   ##from f(x)  
b<-0.018 ##from f(x)
c<-0.002875  ##from f(y)
d<-0.6       ##from f(y)
h<-0.000018  ##from f(y)
k<-0.125  ##from f(z)
m<-0.0081 ##from f(z)
p<-300 ##from f(d)
q<-0.05  ##from f(d)
r<-0.015  ##from f(d)


g2VV <- a*x / (1 + b*x) +
	c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))) +
	k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))))) +
	p*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))) / (q + r*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))))


####BOTH VACCINE EFFECTS ESTIMATES UPPER BOUNDS
a<-625   ##from f(x)
b<-0.016  ##from f(x)
c<-0.017     ##from f(y)
d<-0.6         ##from f(y)
h<-0.000018    ##from f(y)
k<-0.25  ##from f(z)
m<-0.004 ##from f(z)
p<-900 ##from f(d)
q<-0.05  ##from f(d)
r<-0.014  ##from f(d)

gUPPVV <- a*x / (1 + b*x) +
	c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))) +
	k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))))) +
	p*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))) / (q + r*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))))

####BOTH VACCINE EFFECTS ESTIMATES LOWER BOUNDS
a<-40   ##from f(x)
b<-0.02  ##from f(x)
c<-0.00145    ##from f(y)
d<-0.6         ##from f(y)
h<-0.00018    ##from f(y)
k<-0.025  ##from f(z)
m<-0.018 ##from f(z)
p<-10    ##from f(d)
q<-0.05   ##from f(d)
r<-0.05  ##from f(d)

gLOWVV <- a*x / (1 + b*x) +
	c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))) +
	k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))))) +
	p*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))) / (q + r*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))))

lines(g2VV~x,col="blue")
lines(gUPPVV~x,lty=2,col="blue")
lines(gLOWVV~x,lty=2,col="blue")

###############################
##
###
#### RTSS Only
###
##
################################
####RTSS VACCINE EFFECTS MODEL ESTIMATES
a<-600   ##from f(x)  
b<-0.018 ##from f(x)
c<-0.0057  ##from f(y)
d<-0.6       ##from f(y)
h<-0.000018  ##from f(y)
k<-0.125  ##from f(z)
m<-0.0081 ##from f(z)
p<-300 ##from f(d)
q<-0.05  ##from f(d)
r<-0.015  ##from f(d)


g2VRTSS <- a*x / (1 + b*x) +
	c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))) +
	k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))))) +
	p*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))) / (q + r*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))))


####RTSS VACCINE EFFECTS ESTIMATES UPPER BOUNDS
a<-1250   ##from f(x)
b<-0.016  ##from f(x)
c<-0.034     ##from f(y)
d<-0.6         ##from f(y)
h<-0.000018    ##from f(y)
k<-0.25  ##from f(z)
m<-0.004 ##from f(z)
p<-900 ##from f(d)
q<-0.05  ##from f(d)
r<-0.014  ##from f(d)

gUPPVRTSS <- a*x / (1 + b*x) +
	c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))) +
	k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))))) +
	p*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))) / (q + r*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))))

####RTSS VACCINE EFFECTS ESTIMATES LOWER BOUNDS
a<-80   ##from f(x)
b<-0.02  ##from f(x)
c<-0.0029    ##from f(y)
d<-0.6         ##from f(y)
h<-0.00018    ##from f(y)
k<-0.025  ##from f(z)
m<-0.018 ##from f(z)
p<-10    ##from f(d)
q<-0.05   ##from f(d)
r<-0.05  ##from f(d)

gLOWVRTSS <- a*x / (1 + b*x) +
	c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))) +
	k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))))) +
	p*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))) / (q + r*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))))

lines(g2VRTSS~x,col="red")
lines(gUPPVRTSS~x,lty=2,col="red")
lines(gLOWVRTSS~x,lty=2,col="red")

###############################
##
###
#### ATV Only
###
##
################################
####ATV VACCINE EFFECTS MODEL ESTIMATES
a<-300   ##from f(x)  
b<-0.018 ##from f(x)
c<-0.002875  ##from f(y)
d<-0.6       ##from f(y)
h<-0.000018  ##from f(y)
k<-0.25  ##from f(z)
m<-0.0081 ##from f(z)
p<-3000 ##from f(d)
q<-0.05  ##from f(d)
r<-0.015  ##from f(d)


g2VATV <- a*x / (1 + b*x) +
	c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))) +
	k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))))) +
	p*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))) / (q + r*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))))


####ATV VACCINE EFFECTS ESTIMATES UPPER BOUNDS
a<-625   ##from f(x)
b<-0.016  ##from f(x)
c<-0.017     ##from f(y)
d<-0.6         ##from f(y)
h<-0.000018    ##from f(y)
k<-0.5  ##from f(z)
m<-0.004 ##from f(z)
p<-9000 ##from f(d)
q<-0.05  ##from f(d)
r<-0.014  ##from f(d)

gUPPVATV <- a*x / (1 + b*x) +
	c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))) +
	k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))))) +
	p*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))) / (q + r*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))))

####ATV VACCINE EFFECTS ESTIMATES LOWER BOUNDS
a<-40   ##from f(x)
b<-0.02  ##from f(x)
c<-0.00145    ##from f(y)
d<-0.6         ##from f(y)
h<-0.00018    ##from f(y)
k<-0.05  ##from f(z)
m<-0.018 ##from f(z)
p<-100   ##from f(d)
q<-0.05   ##from f(d)
r<-0.05  ##from f(d)

gLOWVATV <- a*x / (1 + b*x) +
	c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))) +
	k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))))) +
	p*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))) / (q + r*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))))

lines(g2VATV~x,col="green")
lines(gUPPVATV~x,lty=2,col="green")
lines(gLOWVATV~x,lty=2,col="green")



plot(g2~x,log="xy",xlab="Number of Oocysts",ylab="Blood stage infections",pch=20,ylim=c(1,1000000))
lines(g2~x+0)
lines(gUPP~x,lty=2)
lines(gLOW~x,lty=2)
lines(g2VV~x,log="xy",col="blue");lines(gLOWVV~x,log="xy",lty=2,col="blue");lines(gUPPVV~x,log="xy",lty=2,col="blue")
lines(g2VRTSS~x,log="xy",col="red");lines(gLOWVRTSS~x,log="xy",lty=2,col="red");lines(gUPPVRTSS~x,log="xy",lty=2,col="red")
lines(g2VATV~x,log="xy",col="green");lines(gLOWVATV~x,log="xy",lty=2,col="green");lines(gUPPVATV~x,log="xy",lty=2,col="green")




##do the 'ATV effects only' and the 'RTSS effects only'

#
##
###Stochasticity in a logistic model
##
#

##Run stochplots.R script for functions


#####################################################################################
##                                                                                 ##  
##   ####### ####    ###  ##     ##    ###             ### ##    ##   ##           ## 
##   ##      ##  #  ## ## ###   ###    ###             ### ##    ##   ##    ###    ##
##   ##      ## ##  ## ## ## # # ##     ##             ##  ##        ####  ## ##   ## 
##   ####    ####   ## ## ##  #  ##      ##    ###    ##   ####  ##   ##  ##   ##  ##  
##   ##      ## #   ## ## ##     ##       ##  ## ##  ##    ## ## ##   ##  #######  ##
##   ##      ## ##  ## ## ##     ##        ####   ####     ## ## ##   ##  ##       ##
##   ##      ##  ##  ###  ##     ##         ##     ##      ## ## ###   ##  #####   ##
##                                                                                 ##
#####################################################################################

#####################################################################
##                                                                 ##
###                                                                ##
#### Probability of k oocysts causing bloodstage infection         ##
###                                                                ##
##                                                                 ##
#####################################################################

##From White et al. 2013 PLoS One e61395

##Negative binomial shape
x<-sort(oocyst$OocystsATV50Start)
n<-mean(g2)
sdn<-var(g2)
ra<-n^2/(sdn^2-n)

##probBS is "probability the a single oocyst will release bloodstage infection"
##ooc.k<-((ka + r -1)/ka)*(((r^r)*(n*(probBS))^ka/((n*(probBS)+r)^(r+ka)))

a<-600   ##from f(x)
b<-0.018 ##from f(x)
c<-0.00575   ##from f(y)
d<-0.6       ##from f(y)
h<-0.000018 ##from f(y)
k<-0.25  ##from f(z)
m<-0.0081 ##from f(z)
p<-3000 ##from f(d)
q<-0.05  ##from f(d)
r<-0.015  ##from f(d)

g2 <- a*x / (1 + b*x) +
	c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))) +
	k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x))))) +
	p*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))) / (q + r*(k*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))) / (1 + m*(c*(a*x / (1 + b*x)) / (d + h*(a*x / (1 + b*x)))))))

probBS<-g2/1000000
##If the dose you get is proportional to the amount of blood stage infection predicted then
##we can use the outputted number of blood stage infections as a proxy to estimate probability of 
##blood stage infection per oocyst

##So we get

ka<-seq(1:50)
ooc.k<-matrix(nrow=length(x),ncol=length(ka))

for(i in 1:length(ka)){
ooc.k[,i]<-((ka[i] + ra -1)/ka[i])*(((ra^ra)*(n*(probBS))^ka[i])/((n*(probBS)+ra)^(ra+ka[i])))
}
max(ooc.k) ## 

PRooc<-numeric(50);
for ( i in 1:50){
	PRooc[i]<-max(ooc.k[,i],na.rm=TRUE)}
plot(PRooc~ka,col="grey",pch=20,
	xlab="Number of oocysts",ylab="Probability of blood stage infection")
	lines(PRooc~ka,col="grey",lty=2)
#points(PRoocLOW~ka,col="grey",pch=20);lines(PRoocLOW~ka,col="grey",lty=2)

####################################################
##
###
#### Using Michael's model from 2013
###
##
######################################################

##Exponential 

NN <- length(g2)
N_lhs <- 2
max_spz <- 1300

Qooc <- oocyst$OocystsATV50Start
bu<-mean(Qooc)
sd_bu<-sd(Qooc)
QoocPrev<-ifelse(Qooc>0,1,0)

Qsg <-   sample(round(sporo$MedicaSinnisSalGlands),50)
n<-mean(Qsg)
sd_n<-sd(Qsg)
QsgPrev<-ifelse(Qsg>0,1,0)

Qskin <- sample(round(sporo$MedicaSinnisMouseSkin),50)
mu<-mean(Qskin)
sd_mu<-sd(Qskin)
QskinPrev<-ifelse(Qskin>0,1,0)

Qbs <- g2
delta<-mean(Qbs)
sd_delta<-sd(Qbs)
QbsPrev<-ifelse(Qbs>0,1,0)


mod_base <- function(params){
	bu	   <- params[1] ##mean for oocysts in mosquito midgut (negbin)
	sd_bu    <- params[2] ##sd for oocysts in mosquito midgut (negbin)
#	n        <- params[3] ##mean for sporozoites entering salivary gland from oocysts (negbin)
#	sd_n     <- params[4] ##sd for sporozoites entering salivary gland from oocysts (negbin) 
#	mu       <- params[5] ##mean of the negbin probability function to be estimated for sporozoites entering skin
#	sd_mu    <- params[6] ##sd of the distribution of sporozoites each challenge
	delta    <- params[3] ##mean number of merozoites released per sporozoite (gamma distribution)
	sd_delta <- params[4] ##sd of merozoites released per sporozoite (gamma distribution)
	VE       <- params[5] 
	############################
	## Secondary NegBin parameters

	p1 = (sd_bu^2-bu)/(sd_bu^2)
	r1 = (bu^2)/(sd_bu^2-bu)

#	p2 = (sd_n^2-n)/(sd_n^2)
#	r2 = (n^2)/(sd_n^2-n)

#	p3 = (sd_mu^2-mu)/(sd_mu^2)
#	r3 = (mu^2)/(sd_mu^2-mu)

	############################
	## Secondary Gamma parameters

	theta_g <- sd_delta*sd_delta/delta

	##############################
	## Dose response

	DR <- rep(VE-1, NN)                                 ##from Michaels 2013 paper
	DR[which(QbsPrev==0)] <- 1
	DR[which(DR<min_DR)] <- min_DR
	p_spz = bu*DR/(bu*DR + r1)

II <- 1:max_spz


	logL <- 0
	for(j in 1:NN){
		if( QbsPrev[j]==1 ){
			coeffs <- lgamma(II+r1) - lgamma(II+1) - lgamma(r1) + r1*log(1-p_spz[j]) + II*log(p_spz[j])
			coeffs <- exp(coeffs)

			logL <- logL + log(sum( coeffs*dgamma(Qbs[j], shape=delta*II/theta_g, scale=theta_g) ))
		}
		if( QbsPrev[j]==0 ){
			logL <- logL + r1*log(1-p_spz[j])
		}
	}

	-logL
}


############################
## Define limits for parameters and perform 
## MLE model fitting

temp_mat <- matrix(NA, nrow=N_lhs, ncol=6)

lower <- c(1, 1,        10,    10,    0)
upper <- c(1000,1000,   100000,100000,1)

ui <- rbind( diag(5), c(0,0,0,0,-1) )
ci <- c(lower, -1)

theta <- randomLHS(N_lhs,5)
theta <- t( lower +  t(theta)*(0.25*upper-lower) )

max_MLE <- 1e6
par_MLE <- rep(NA, 5)


for(j in 1:N_lhs){
	MLE_base <- constrOptim(theta=theta[j,], f=mod_base, grad=NULL, ui=ui, ci=ci,
							outer.iterations = 100, outer.eps = 1e-06)
		
	temp_mat[j,1:5] <- MLE_base$par
	temp_mat[j,6]   <- -MLE_base$value
	
	if( MLE_base$value < max_MLE ){ 
		max_MLE <- -MLE_base$value
		best_MLE_base <- MLE_base 			
	}	
}

bu_mle     <- best_MLE_base$par[1]
sd_bu_mle  <- best_MLE_base$par[2]
#n_mle	<- best_MLE_base$par[3]
#sd_n_mle   <- best_MLE_base$par[4]
#mu_mle     <- best_MLE_base$par[5]
#sd_mu_mle  <- best_MLE_base$par[6]
delta_mle    <- best_MLE_base$par[3]
sd_delta_mle <- best_MLE_base$par[4]
VE           <- best_MLE_base$par[5]
p1_mle <- (sd_bu_mle^2-bu_mle)/(sd_bu_mle^2)
r1_mle <- (bu_mle^2)/(sd_bu_mle^2-bu_mle) 

p2_mle <- (sd_n_mle^2-n_mle)/(sd_n_mle^2)
r2_mle <- (n_mle^2)/(sd_n_mle^2-n_mle) 

p3_mle <- (sd_mu_mle^2-mu_mle)/(sd_mu_mle^2)
r3_mle <- (mu_mle^2)/(sd_mu_mle^2-mu_mle) 

p4_mle <- (sd_delta_mle^2-delta_mle)/(sd_delta_mle^2)
r4_mle <- (delta_mle^2)/(sd_delta_mle^2-delta_mle) 











