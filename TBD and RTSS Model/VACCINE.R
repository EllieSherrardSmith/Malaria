date.convert <- function(xx){
  
  day   <- as.numeric( substr(as.vector(xx), start=1, stop=2) )
  month <- substr(as.vector(xx), start=4, stop=6)
  year  <- substr(as.vector(xx), start=8, stop=9)
  
  n_day <- 0
  
  ############################################
  ## years 
  
  if(year=="00"){
    n_day <- 0
  }
  if(year=="01"){
    n_day <- 365 + 1
  }
  if(year=="02"){
    n_day <- 2*365 + 1
  }
  if(year=="03"){
    n_day <- 3*365 + 1
  }
  if(year=="04"){
    n_day <- 4*365 + 1
  }
  if(year=="05"){
    n_day <- 5*365 + 2
  }
  if(year=="06"){
    n_day <- 6*365 + 2
  }
  if(year=="07"){
    n_day <- 7*365 + 2
  }	
  if(year=="08"){
    n_day <- 8*365 + 2
  }
  if(year=="09"){
    n_day <- 9*365 + 3
  }
  
  #############################################
  ## months
  
  if(month=="Jan"){
    n_day <- n_day + 0     ## 31 days
  }
  if(month=="Feb"){
    n_day <- n_day + 31    ## 28 days
  }
  if(month=="Mar"){
    n_day <- n_day + 59    ## 31 days
  }
  if(month=="Apr"){
    n_day <- n_day + 90    ## 30 days
  }
  if(month=="May"){
    n_day <- n_day + 120   ## 31 days
  }
  if(month=="Jun"){
    n_day <- n_day + 151   ## 30 days
  }
  if(month=="Jul"){
    n_day <- n_day + 181   ## 31 days
  }
  if(month=="Aug"){
    n_day <- n_day + 212   ## 31 days
  }
  if(month=="Sep"){
    n_day <- n_day + 243   ## 30 days
  }
  if(month=="Oct"){
    n_day <- n_day + 273   ## 31 days
  }
  if(month=="Nov"){
    n_day <- n_day + 304   ## 30 days
  }
  if(month=="Dec"){
    n_day <- n_day + 334   ## 31 days
  }
  
  if( year %in% c("00", "04", "08") ){
    n_day <- n_day+1
    if( month=="Jan" || month=="Feb" ){	
      n_day <- n_day-1
    }
  }
  
  
  n_day <- n_day + day
  
  n_day
}



VACC<-read.csv("D:\\IMPERIAL Nov 2014\\Malaria Modelling\\From Michael 18Dec2014\\Data\\EFFICACY.csv",header=TRUE)
head(VACC)
VACC$dat1<-date.convert(VACC$VACC1)
VACC$dat2<-date.convert(VACC$VACC2)
VACC$dat3<-date.convert(VACC$VACC3)
VACC$datLAST<-date.convert(VACC$Visit.date..)
VACC$triallength<-VACC$datLAST-VACC$dat1

VACC$foiCON<-0.08761196
VACC$foiATV<-0.04823961

VACC$VErCon<-(exp(VEas01b*VACC$foiCON*VACC$triallength)-1)/
             (exp(VACC$foiCON*VACC$triallength)-1)

VACC$VErATV<-(exp(VEas01b*VACC$foiATV*VACC$triallength)-1)/
  (exp(VACC$foiATV*VACC$triallength)-1)

summary(VACC$VErCon)
summary(VACC$VErATV)
plot(VACC$VErCon~VACC$triallength,ylim=c(0,0.15),ylab="Comparative risk-based Vaccine Efficacy")
lines(VACC$VErCon~VACC$triallength)
points(VACC$VErATV~VACC$triallength,pch=20)
lines(VACC$VErATV~VACC$triallength,lty=2)
control<-subset(VACC,Treatment=="Challenge control" | Treatment== "Rechallenge control")
summary(control)
36/36

as01b<-subset(VACC,Treatment=="Malaria vaccine(DMA150A48) mix AS01B adjuvant(DAS01B009A2)")
summary(as01b)
VEas01b<-1-(23/(21+23))


as02a<-subset(VACC,Treatment=="Malaria vaccine(DMA150A48) mix AS02A adjuvant(AS02A013A9)")
summary(as02a)
VEas02a<-1-(35/(35+18))

##From summarydata, control probability of infection (FORCE OF INFECTION) = 0.8259394
##From summarydata, ATV25 probability of infection (FORCE OF INFECTION) = 0.04823961

##T = time of trial (from White et al. 2010 Malaria Journal 9:82) is