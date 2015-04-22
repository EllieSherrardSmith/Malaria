library(nlme)
library(rstan)
library(MASS)
library(boot)
library(coda)
library(R2OpenBUGS)
library(ggplot2)
library("Rlab")
##########################################################################
## 
##  ##         ##       ########      ##     
##  ###      ##  ##        ##       ##  ##   
##  ## #    ##    ##       ##      ##    ##   
##  ##  #   ########       ##      ########  
##  ##  #   ##    ##       ##     ##     ##  
##  ## #   ##      ##      ##    ##       ##
##  ###   ##        ##     ##   ##         ##
##  
##########################################################################


oocysts<-read.csv("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\oocystsbites2to5.csv",header=TRUE)
head(oocysts)


##OOCYSTS
oocystsC<-c(#oocysts$oocystsbites1control[oocysts$round=="day41"][1:24],
            #oocysts$oocystsbites1control[oocysts$round=="day72"][1:24],
            #oocysts$oocystsbites1control[oocysts$round=="day103"][1:24],
            #oocysts$oocystsbites1control[oocysts$round=="day134"][1:24],
            oocysts$oocystsbites2control[oocysts$round=="day41"][1:24],
           oocysts$oocystsbites2control[oocysts$round=="day72"][1:24],
           oocysts$oocystsbites2control[oocysts$round=="day103"][1:24],
           oocysts$oocystsbites2control[oocysts$round=="day134"][1:24],
           oocysts$oocystsbites3control[oocysts$round=="day41"][1:24],
           oocysts$oocystsbites3control[oocysts$round=="day72"][1:24],
           oocysts$oocystsbites3control[oocysts$round=="day103"][1:24],
           oocysts$oocystsbites3control[oocysts$round=="day134"][1:24],
           oocysts$oocystsbites4control[oocysts$round=="day41"][1:24],
           oocysts$oocystsbites4control[oocysts$round=="day72"][1:24],
           oocysts$oocystsbites4control[oocysts$round=="day103"][1:24],
           oocysts$oocystsbites4control[oocysts$round=="day134"][1:24],
           oocysts$oocystsbites5control[oocysts$round=="day41"][1:24],
           oocysts$oocystsbites5control[oocysts$round=="day72"][1:24],
           oocysts$oocystsbites5control[oocysts$round=="day103"][1:24],
           oocysts$oocystsbites5control[oocysts$round=="day134"][1:24])
length(oocystsC)
prevooc<-ifelse(oocystsC==0,0,1)
prevooc1<-c(sum(sum(prevooc[1:24])/24,sum(prevooc[25:48])/24,sum(prevooc[49:72])/24,sum(prevooc[73:96])/24)/4,
            sum(sum(prevooc[97:120])/24,sum(prevooc[121:144])/24,sum(prevooc[145:168])/24,sum(prevooc[169:192])/24)/4,
            sum(sum(prevooc[193:216])/24,sum(prevooc[217:240])/24,sum(prevooc[241:264])/24,sum(prevooc[265:288])/24)/4,
            sum(sum(prevooc[289:312])/24,sum(prevooc[313:336])/24,sum(prevooc[337:360])/24,sum(prevooc[360:384])/24)/4)

freqoocC<-numeric(length(unique(oocystsC)))
oocystsC2<-sort(unique(oocystsC))
for (i in 1:length(oocystsC2)){ 
  freqoocC[i]<-sum(ifelse(oocystsC==unique(oocystsC2)[i],1,0))}
probNooc<-numeric(length(freqoocC))
for (j in 1:length(freqoocC)){
probNooc[j]<-freqoocC[j]/sum(freqoocC)}
freqdistoocC<-data.frame(oocystsC2,probNooc);colnames(freqdistoocC)[1]<-"Nooc"

oocystsT<-c(#oocysts$oocystsbites1atv[oocysts$round=="day41"][1:24],
            #oocysts$oocystsbites1atv[oocysts$round=="day72"][1:24],
            #oocysts$oocystsbites1atv[oocysts$round=="day103"][1:24],
            #oocysts$oocystsbites1atv[oocysts$round=="day134"][1:24],
            oocysts$oocystsbites2atv[oocysts$round=="day41"][1:24],
            oocysts$oocystsbites2atv[oocysts$round=="day72"][1:24],
            oocysts$oocystsbites2atv[oocysts$round=="day103"][1:24],
            oocysts$oocystsbites2atv[oocysts$round=="day134"][1:24],
            oocysts$oocystsbites3atv[oocysts$round=="day41"][1:24],
            oocysts$oocystsbites3atv[oocysts$round=="day72"][1:24],
            oocysts$oocystsbites3atv[oocysts$round=="day103"][1:24],
            oocysts$oocystsbites3atv[oocysts$round=="day134"][1:24],
            oocysts$oocystsbites4atv[oocysts$round=="day41"][1:24],
            oocysts$oocystsbites4atv[oocysts$round=="day72"][1:24],
            oocysts$oocystsbites4atv[oocysts$round=="day103"][1:24],
            oocysts$oocystsbites4atv[oocysts$round=="day134"][1:24],
            oocysts$oocystsbites5atv[oocysts$round=="day41"][1:24],
            oocysts$oocystsbites5atv[oocysts$round=="day72"][1:24],
            oocysts$oocystsbites5atv[oocysts$round=="day103"][1:24],
            oocysts$oocystsbites5atv[oocysts$round=="day134"][1:24])
length(oocystsT)
prevooc<-ifelse(oocystsT==0,0,1)
prevoocT<-c(sum(sum(prevooc[1:24])/24,sum(prevooc[25:48])/24,sum(prevooc[49:72])/24,sum(prevooc[73:96])/24)/4,
            sum(sum(prevooc[97:120])/24,sum(prevooc[121:144])/24,sum(prevooc[145:168])/24,sum(prevooc[169:192])/24)/4,
            sum(sum(prevooc[193:216])/24,sum(prevooc[217:240])/24,sum(prevooc[241:264])/24,sum(prevooc[265:288])/24)/4,
            sum(sum(prevooc[289:312])/24,sum(prevooc[313:336])/24,sum(prevooc[337:360])/24,sum(prevooc[360:384])/24)/4)
EffOoc<-(prevooc1-prevoocT)/prevooc1

freqoocT<-numeric(length(unique(oocystsT)))
oocystsT2<-sort(unique(oocystsT))
for (i in 1:length(oocystsT2)){ 
  freqoocT[i]<-sum(ifelse(oocystsT==unique(oocystsT2)[i],1,0))}
probNoocT<-numeric(length(freqoocT))
for (j in 1:length(freqoocT)){
  probNoocT[j]<-freqoocT[j]/sum(freqoocT)}
freqdistoocT<-data.frame(oocystsT2,probNoocT);colnames(freqdistoocT)[1]<-"NoocT"

meanoocysts<-c(#mean(oocysts$oocystsbites1control[oocysts$round=="day41"],na.rm=TRUE),
               #mean(oocysts$oocystsbites1control[oocysts$round=="day72"],na.rm=TRUE),
               #mean(oocysts$oocystsbites1control[oocysts$round=="day103"],na.rm=TRUE),
               #mean(oocysts$oocystsbites1control[oocysts$round=="day134"],na.rm=TRUE),
               mean(oocysts$oocystsbites2control[oocysts$round=="day41"],na.rm=TRUE),
               mean(oocysts$oocystsbites2control[oocysts$round=="day72"],na.rm=TRUE),
               mean(oocysts$oocystsbites2control[oocysts$round=="day103"],na.rm=TRUE),
               mean(oocysts$oocystsbites2control[oocysts$round=="day134"],na.rm=TRUE),
               mean(oocysts$oocystsbites3control[oocysts$round=="day41"],na.rm=TRUE),
               mean(oocysts$oocystsbites3control[oocysts$round=="day72"],na.rm=TRUE),
               mean(oocysts$oocystsbites3control[oocysts$round=="day103"],na.rm=TRUE),
               mean(oocysts$oocystsbites3control[oocysts$round=="day134"],na.rm=TRUE),
               mean(oocysts$oocystsbites4control[oocysts$round=="day41"],na.rm=TRUE),
               mean(oocysts$oocystsbites4control[oocysts$round=="day72"],na.rm=TRUE),
               mean(oocysts$oocystsbites4control[oocysts$round=="day103"],na.rm=TRUE),
               mean(oocysts$oocystsbites4control[oocysts$round=="day134"],na.rm=TRUE),
               mean(oocysts$oocystsbites5control[oocysts$round=="day41"],na.rm=TRUE),
               mean(oocysts$oocystsbites5control[oocysts$round=="day72"],na.rm=TRUE),
               mean(oocysts$oocystsbites5control[oocysts$round=="day103"],na.rm=TRUE),
               mean(oocysts$oocystsbites5control[oocysts$round=="day134"],na.rm=TRUE),
               #mean(oocysts$oocystsbites1atv[oocysts$round=="day41"],na.rm=TRUE),
               #mean(oocysts$oocystsbites1atv[oocysts$round=="day72"],na.rm=TRUE),
               #mean(oocysts$oocystsbites1atv[oocysts$round=="day103"],na.rm=TRUE),
               #mean(oocysts$oocystsbites1atv[oocysts$round=="day134"],na.rm=TRUE),
               mean(oocysts$oocystsbites2atv[oocysts$round=="day41"],na.rm=TRUE),
               mean(oocysts$oocystsbites2atv[oocysts$round=="day72"],na.rm=TRUE),
               mean(oocysts$oocystsbites2atv[oocysts$round=="day103"],na.rm=TRUE),
               mean(oocysts$oocystsbites2atv[oocysts$round=="day134"],na.rm=TRUE),
               mean(oocysts$oocystsbites3atv[oocysts$round=="day41"],na.rm=TRUE),
               mean(oocysts$oocystsbites3atv[oocysts$round=="day72"],na.rm=TRUE),
               mean(oocysts$oocystsbites3atv[oocysts$round=="day103"],na.rm=TRUE),
               mean(oocysts$oocystsbites3atv[oocysts$round=="day134"],na.rm=TRUE),
               mean(oocysts$oocystsbites4atv[oocysts$round=="day41"],na.rm=TRUE),
               mean(oocysts$oocystsbites4atv[oocysts$round=="day72"],na.rm=TRUE),
               mean(oocysts$oocystsbites4atv[oocysts$round=="day103"],na.rm=TRUE),
               mean(oocysts$oocystsbites4atv[oocysts$round=="day134"],na.rm=TRUE),
               mean(oocysts$oocystsbites5atv[oocysts$round=="day41"],na.rm=TRUE),
               mean(oocysts$oocystsbites5atv[oocysts$round=="day72"],na.rm=TRUE),
               mean(oocysts$oocystsbites5atv[oocysts$round=="day103"],na.rm=TRUE),
               mean(oocysts$oocystsbites5atv[oocysts$round=="day134"],na.rm=TRUE))


spors<-read.csv("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\sporozoites.csv",header=TRUE)
spors$prevBS<-ifelse(spors$Parasitemia > 0 | spors$Gametocytemia > 0, 1, 0)
head(spors)
##MEAN PARASITEMIA IN MICE
parasit<-cbind(
spors$Parasitemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 1],
spors$Parasitemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 2],
spors$Parasitemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 3],
spors$Parasitemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 4],
spors$Parasitemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 1],
spors$Parasitemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 2],
spors$Parasitemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 3],
spors$Parasitemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 4],
spors$Parasitemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 1],
spors$Parasitemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 2],
spors$Parasitemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 3],
spors$Parasitemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 4],
spors$Parasitemia[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 1],
spors$Parasitemia[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 2],
spors$Parasitemia[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 3],
spors$Parasitemia[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 4])
parasitem<-numeric(16)
for (i in 1:16){
  parasitem[i]<-sum(parasit[,i])/5}

parasitT<-cbind(
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 4],
  spors$Parasitemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 4],
  spors$Parasitemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 4],
  spors$Parasitemia[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 4])
parasitemT<-numeric(16)
for (i in 1:16){
  parasitemT[i]<-sum(parasitT[,i])/5}

##PREVALENCE IN MICE
PREV_C<-cbind(
#spors$prevBS[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 1],
#spors$prevBS[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 2],
#spors$prevBS[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 3],
#spors$prevBS[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 4],
spors$prevBS[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 1],
spors$prevBS[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 2],
spors$prevBS[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 3],
spors$prevBS[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 4],
spors$prevBS[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 1],
spors$prevBS[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 2],
spors$prevBS[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 3],
spors$prevBS[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 4],
spors$prevBS[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 1],
spors$prevBS[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 2],
spors$prevBS[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 3],
spors$prevBS[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 4],
spors$prevBS[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 1],
spors$prevBS[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 2],
spors$prevBS[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 3],
spors$prevBS[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 4])

PREV_T<-cbind(
#  spors$prevBS[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 1],
#  spors$prevBS[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 2],
#  spors$prevBS[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 3],
#  spors$prevBS[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 4],
  spors$prevBS[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 1],
  spors$prevBS[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 2],
  spors$prevBS[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 3],
  spors$prevBS[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 4],
  spors$prevBS[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 1],
  spors$prevBS[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 2],
  spors$prevBS[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 3],
  spors$prevBS[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 4],
  spors$prevBS[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 1],
  spors$prevBS[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 2],
  spors$prevBS[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 3],
  spors$prevBS[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 4],
  spors$prevBS[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 1],
  spors$prevBS[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 2],
  spors$prevBS[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 3],
  spors$prevBS[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 4])

###sPOROZOITES
sporsbites1<-subset(spors,Bites==1 & Treatment==0);sporsbites1
newa1<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==1]);
spb1r1<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))
newb1<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==2]);
spb1r2<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))
newc1<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==3]);
spb1r3<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))
newd1<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==4]);
spb1r4<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))


sporsbites2<-subset(spors,Bites==2 & Treatment==0);sporsbites2
    a1<-sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==1],sporsbites2$Sporozoite2[sporsbites2$Round==1]);
    spb2r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
      b1<-sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==2],sporsbites2$Sporozoite2[sporsbites2$Round==2]);
      spb2r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
        c1<-sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==3],sporsbites2$Sporozoite2[sporsbites2$Round==3]);
        spb2r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
          d1<-sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==4],sporsbites2$Sporozoite2[sporsbites2$Round==4]);
          spb2r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))


sporsbites3<-subset(spors,Bites==3 & Treatment==0);sporsbites3
    ee1<-sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==1],sporsbites3$Sporozoite2[sporsbites3$Round==1],sporsbites3$Sporozoite3[sporsbites3$Round==1]);
    spb3r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
      ff1<-sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==2],sporsbites3$Sporozoite2[sporsbites3$Round==2],sporsbites3$Sporozoite3[sporsbites3$Round==2]);
      spb3r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
        g1<-sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==3],sporsbites3$Sporozoite2[sporsbites3$Round==3],sporsbites3$Sporozoite3[sporsbites3$Round==3]);
        spb3r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
          h1<-sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==4],sporsbites3$Sporozoite2[sporsbites3$Round==4],sporsbites3$Sporozoite3[sporsbites3$Round==4]);
          spb3r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

sporsbites4<-subset(spors,Bites==4 & Treatment==0);sporsbites4
    jj1<-sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==1],sporsbites4$Sporozoite2[sporsbites4$Round==1],sporsbites4$Sporozoite3[sporsbites4$Round==1],sporsbites4$Sporozoite4[sporsbites4$Round==1]);
    spb4r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
      kk1<-sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==2],sporsbites4$Sporozoite2[sporsbites4$Round==2],sporsbites4$Sporozoite3[sporsbites4$Round==2],sporsbites4$Sporozoite4[sporsbites4$Round==2]);
      spb4r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
        l1<-sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==3],sporsbites4$Sporozoite2[sporsbites4$Round==3],sporsbites4$Sporozoite3[sporsbites4$Round==3],sporsbites4$Sporozoite4[sporsbites4$Round==3]);
        spb4r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
          m1<-sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==4],sporsbites4$Sporozoite2[sporsbites4$Round==4],sporsbites4$Sporozoite3[sporsbites4$Round==4],sporsbites4$Sporozoite4[sporsbites4$Round==4]);
          spb4r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

sporsbites5<-subset(spors,Bites==5 & Treatment==0);sporsbites5
    nn1<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==1],sporsbites5$Sporozoite2[sporsbites5$Round==1],sporsbites5$Sporozoite3[sporsbites5$Round==1],sporsbites5$Sporozoite4[sporsbites5$Round==1],sporsbites5$Sporozoite5[sporsbites5$Round==1]);
    spb5r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
      oo1<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==2],sporsbites5$Sporozoite2[sporsbites5$Round==2],sporsbites5$Sporozoite3[sporsbites5$Round==2],sporsbites5$Sporozoite4[sporsbites5$Round==2],sporsbites5$Sporozoite5[sporsbites5$Round==2]);
      spb5r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
        pp1<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==3],sporsbites5$Sporozoite2[sporsbites5$Round==3],sporsbites5$Sporozoite3[sporsbites5$Round==3],sporsbites5$Sporozoite4[sporsbites5$Round==3],sporsbites5$Sporozoite5[sporsbites5$Round==3]);
        spb5r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
          qq1<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==4],sporsbites5$Sporozoite2[sporsbites5$Round==4],sporsbites5$Sporozoite3[sporsbites5$Round==4],sporsbites5$Sporozoite4[sporsbites5$Round==4],sporsbites5$Sporozoite5[sporsbites5$Round==4]);
          spb5r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

spors_C<-rbind(#spb1r1,spb1r2,spb1r3,spb1r4,
               spb2r1,spb2r2,spb2r3,spb2r4,
               spb3r1,spb3r2,spb3r3,spb3r4,
               spb4r1,spb4r2,spb4r3,spb4r4,
               spb5r1,spb5r2,spb5r3,spb5r4)

sporsbites1<-subset(spors,Bites==1 & Treatment==1);sporsbites1
new_a<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==1]);
spb1r1<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))
new_b<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==2]);
spb1r2<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))
new_c<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==3]);
spb1r3<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))
new_d<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==4]);
spb1r4<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))


sporsbites2<-subset(spors,Bites==2 & Treatment==1);sporsbites2
   a<- sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==1],sporsbites2$Sporozoite2[sporsbites2$Round==1]);
    spb2r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
    b<-  sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==2],sporsbites2$Sporozoite2[sporsbites2$Round==2]);
      spb2r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
    c<-    sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==3],sporsbites2$Sporozoite2[sporsbites2$Round==3]);
        spb2r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
      d<-    sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==4],sporsbites2$Sporozoite2[sporsbites2$Round==4]);
          spb2r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))


sporsbites3<-subset(spors,Bites==3 & Treatment==1);sporsbites3
   ee<- sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==1],sporsbites3$Sporozoite2[sporsbites3$Round==1],sporsbites3$Sporozoite3[sporsbites3$Round==1]);
    spb3r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
    ff<-  sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==2],sporsbites3$Sporozoite2[sporsbites3$Round==2],sporsbites3$Sporozoite3[sporsbites3$Round==2]);
      spb3r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
      g<-  sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==3],sporsbites3$Sporozoite2[sporsbites3$Round==3],sporsbites3$Sporozoite3[sporsbites3$Round==3]);
        spb3r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
         h<- sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==4],sporsbites3$Sporozoite2[sporsbites3$Round==4],sporsbites3$Sporozoite3[sporsbites3$Round==4]);
          spb3r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

sporsbites4<-subset(spors,Bites==4 & Treatment==1);sporsbites4
    jj<- sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==1],sporsbites4$Sporozoite2[sporsbites4$Round==1],sporsbites4$Sporozoite3[sporsbites4$Round==1],sporsbites4$Sporozoite4[sporsbites4$Round==1]);
    spb4r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
    kk<-  sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==2],sporsbites4$Sporozoite2[sporsbites4$Round==2],sporsbites4$Sporozoite3[sporsbites4$Round==2],sporsbites4$Sporozoite4[sporsbites4$Round==2]);
      spb4r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
      l<-  sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==3],sporsbites4$Sporozoite2[sporsbites4$Round==3],sporsbites4$Sporozoite3[sporsbites4$Round==3],sporsbites4$Sporozoite4[sporsbites4$Round==3]);
        spb4r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
         m<- sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==4],sporsbites4$Sporozoite2[sporsbites4$Round==4],sporsbites4$Sporozoite3[sporsbites4$Round==4],sporsbites4$Sporozoite4[sporsbites4$Round==4]);
          spb4r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

sporsbites5<-subset(spors,Bites==5 & Treatment==1);sporsbites5
    nn<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==1],sporsbites5$Sporozoite2[sporsbites5$Round==1],sporsbites5$Sporozoite3[sporsbites5$Round==1],sporsbites5$Sporozoite4[sporsbites5$Round==1],sporsbites5$Sporozoite5[sporsbites5$Round==1]);
    spb5r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
      oo<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==2],sporsbites5$Sporozoite2[sporsbites5$Round==2],sporsbites5$Sporozoite3[sporsbites5$Round==2],sporsbites5$Sporozoite4[sporsbites5$Round==2],sporsbites5$Sporozoite5[sporsbites5$Round==2]);
      spb5r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
        pp<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==3],sporsbites5$Sporozoite2[sporsbites5$Round==3],sporsbites5$Sporozoite3[sporsbites5$Round==3],sporsbites5$Sporozoite4[sporsbites5$Round==3],sporsbites5$Sporozoite5[sporsbites5$Round==3]);
        spb5r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
          qq<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==4],sporsbites5$Sporozoite2[sporsbites5$Round==4],sporsbites5$Sporozoite3[sporsbites5$Round==4],sporsbites5$Sporozoite4[sporsbites5$Round==4],sporsbites5$Sporozoite5[sporsbites5$Round==4]);
          spb5r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
  
spors_T<-rbind(#spb1r1,spb1r2,spb1r3,spb1r4,
               spb2r1,spb2r2,spb2r3,spb2r4,
               spb3r1,spb3r2,spb3r3,spb3r4,
               spb4r1,spb4r2,spb4r3,spb4r4,
               spb5r1,spb5r2,spb5r3,spb5r4)
spors_Cprev<-spors_Tprev<-numeric(16)
for(i in 1:16){
spors_Cprev[i]<-1-(spors_C[i,1]/sum(spors_C[i,]))
spors_Tprev[i]<-1-(spors_T[i,1]/sum(spors_T[i,]))
}

effectSpors<-(spors_Cprev-spors_Tprev)/spors_Cprev
EFFSPmbr<-c(sum(effectSpors[1:4])/4,
            sum(effectSpors[5:8])/4,
            sum(effectSpors[9:12])/4,
            sum(effectSpors[13:16])/4)


MEANsp<-c(#mean(newa1),mean(newb1),mean(newc1),mean(newd1),
          mean(a1),mean(b1),mean(c1),mean(d1),
            mean(ee1),mean(ff1),mean(g1),mean(h1),
            mean(jj1),mean(kk1),mean(l1),mean(m1),
            mean(nn1),mean(oo1),mean(pp1),mean(qq1),
          #mean(new_a),mean(new_b),mean(new_c),mean(new_d),  
          mean(a),mean(b),mean(c),mean(d),
            mean(ee),mean(ff),mean(g),mean(h),
            mean(jj),mean(kk),mean(l),mean(m),
            mean(nn),mean(oo),mean(pp),mean(qq))

prevcon<-prevtreat<-numeric(16)
  for (i in 1:16){
    prevcon[i]<-c(sum(PREV_C[,i])/5)
    prevtreat[i]<-c(sum(PREV_T[,i])/5)
    }

prevs<-c(prevcon,prevtreat)
prevmbrC<-c(sum(prevcon[1:4])/4,
           sum(prevcon[5:8])/4,
           sum(prevcon[9:12])/4,
           sum(prevcon[13:16])/4)
prevmbrT<-c(sum(prevtreat[1:4])/4,
            sum(prevtreat[5:8])/4,
            sum(prevtreat[9:12])/4,
            sum(prevtreat[13:16])/4)
meanprevpergroup<-(prevcon+prevtreat)/2
######################################
##
###
####  Data simple plots
###
###
##
######################################
par(mfrow=c(2,2))
parasitemia<-c(parasitem,parasitemT)
plot(c(parasitem,parasitemT)~prevs,xlim=c(0,1),ylim=c(0,14))
data2<-data.frame(parasitemia,prevs)

vec2<-seq(0,1,by=0.2)
boxplot(data2$parasitemia[data2$prevs==0],data2$parasitemia[data2$prevs==0.2],
        data2$parasitemia[data2$prevs==0.4],data2$parasitemia[data2$prevs==0.6],
        data2$parasitemia[data2$prevs==0.8],data2$parasitemia[data2$prevs==1],
        col="blue",xaxt="n",xlab="Mean Prevalence",ylab="Parasitemia (%)")
axis(1,at=seq(1,6,1),labels=vec2)



############################################################################
##
## 1. Fit for the mean distribution of sporozoite scores to oocyst counts
##
##
##
############################################################################
par(mfrow=c(2,1))
plot(MEANsp~meanoocysts,xlab="Mean oocysts",ylab="Mean sporozoite scores")
sat.binom<-function(p.vec){
  
  #a<-p.vec[1]
  a<-p.vec[1]
  b<-p.vec[2]
  c<-p.vec[3]
  d<-p.vec[4]
  
  pred<- (a * meanoocysts^c)/(d + b * meanoocysts^c)
  
  data1<-MEANsp
  
  loglik<- data1* log((pred)+0.001)+(1-data1)*log(1-((pred)-0.001))
  
  
  -sum(loglik,na.rm=T)
}
n.param<-4
satmod<-optim(c(0.9,0.2,0.6,4),sat.binom,method="L-BFGS-B",lower=c(0,0.2,0.5,4),upper=c(0.95,1,0.65,5))
satmod
satmod$par[1]<-0.9342045
satmod$par[2]<-0.2166749
satmod$par[3]<-0.5936314
satmod$par[4]<-4.1838088
nc<-seq(0,max(meanoocysts),1)
pred<-(satmod$par[1] * nc^satmod$par[3])/(satmod$par[4] + satmod$par[2] * nc^satmod$par[3]) 
lines(nc,pred,lwd=2,lty=3,col="red")
#
## CIs for estimates from saturating function model
#

optim.model<-sat.binom(satmod$par)

size.of.grid<-20
a.range<-seq(0.7,0.99,length=size.of.grid)
b.range<-seq(0.15,0.6,length=size.of.grid)
c.range<-seq(0.3,0.8,length=size.of.grid)
d.range<-seq(2.5,5,length=size.of.grid)
pds<-expand.grid("a"=a.range,"b"=b.range,"c"=c.range,"d"=d.range)
pds$modcom<-NA

for(i in 1:length(pds$a)){
  p.vec<-c(pds$a[i],pds$b[i],pds$c[i],pds$d[i])
  ci.n.param<-length(p.vec) 
  ci.fit<-sat.binom(p.vec)     
  pds$modcom[i]<-ifelse(1-pchisq(2*(max(ci.fit,optim.model)-min(ci.fit,optim.model)),1) < 0.05,"discard","keep")
  ##print(i)
}
pds.new<-subset(pds,pds$modcom=="keep")

length(pds$a)
length(pds.new$a)

##

q1a<-quantile(pds.new$a,0.025);q1a##
q2a<-quantile(pds.new$a,0.975);q2a##
q1b<-quantile(pds.new$b,0.025);q1b## 
q2b<-quantile(pds.new$b,0.975);q2b##
q1c<-quantile(pds.new$c,0.025);q1c##
q2c<-quantile(pds.new$c,0.975);q2c##
q1d<-quantile(pds.new$d,0.025);q1d##
q2d<-quantile(pds.new$d,0.975);q2d##

predlower<-   (q1a * nc^q1c)/(q1d + q2b * nc^q1c) 
predupper<-   (q2a * nc^q2c)/(q2d + q1b * nc^q2c)  

par(mfrow=c(1,2));par(mar=c(5,5,5,5))
plot(meanoocysts,MEANsp,
     ylim=c(0,3),ylab="Mean sporozoite score",
     xlim=c(0,max(meanoocysts)),xlab="Oocyst mean intensity",cex.lab=2)
lines(predupper~nc)
lines(predlower~nc)
polygon(c(nc, rev(nc)),c(predupper,rev(predlower)),border=NA, col="aquamarine1")
lines(pred~nc)
points(meanoocysts,MEANsp,col="aquamarine4",pch=16)

############################################################################
##
## 2. Fit for the prevalence of infection in mice to oocyst counts from mosquito
##
##
##
############################################################################

datprev<-data.frame(prevs,meanoocysts,MEANsp)
datprev$treat<-c(rep("Con",16),rep("Treat",16))
datprev$prevs<-ifelse(datprev$meanoocysts==0,0,datprev$prevs)

par(mfrow=c(2,1));par(mar=c(5,5,2,2))
plot(c(0,prevs)~c(0,meanoocysts),ylim=c(0,1),xlim=c(0,max(meanoocysts)),xlab="Mean oocysts",ylab="Prevalence Mice")
log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * c(0,meanoocysts))) / (1 + exp(a + b * c(0,meanoocysts))) ) 
  prev1<-c(0,prevs)
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmod
nc<-seq(0,max(meanoocysts),1)
pred2<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) ) 
lines(nc,pred2,lwd=2,lty=2,col="red")


#fit individual Gompertz models to each TREATMENT group
out.nls<-nlsList(datprev$prevs ~ SSgompertz(log(datprev$meanoocysts),a0,b0,b1|datprev$treat),data=datprev)
## SSgompertz(x,a0,b0,b1): y(f(x)) = a0 exp(-b0 b1^x)
#Gompertz parameters for each group
coef(out.nls)
#plot(intervals(out.nls),layout=c(3,1))

apply(coef(out.nls), 2, mean)->parm.means
apply(coef(out.nls), 2, range)->parm.range
parm.means
parm.range

gom.binom<-function(p.vec){
  a0<-p.vec[1]
  b0<-p.vec[2]
  b1<-p.vec[3]
  
  pred1<- (a0 * exp (-b0 * b1 ^datprev$meanoocysts)) 
  data1<- datprev$prevs
  loglik1<- data1* log((pred1)+0.00001)+(1-data1)*log(1-((pred1)-0.00001))
  -sum(loglik1,na.rm=T)
}
n.param<-3
gommod<-optim(c(0.7266714, 1.3155981, 0.8747033),gom.binom,method="L-BFGS-B",lower=c(0.7,0.3,0.6),upper=c(0.9,2.3,0.89))
gommod

nc<-seq(0,max(meanoocysts),1)
pred2a<-(gommod$par[1] * exp (-gommod$par[2] * gommod$par[3] ^  nc))
lines(nc,pred2a,lwd=2,lty=2,col="blue")


gom.binom<-function(p.vec){
  Z<-p.vec[1]
  B<-p.vec[2]
  G0<-p.vec[3]
  
  pred1<- (Z/B) * exp (-exp(G0 - B * meanoocysts))
  data1<- prevs
  loglik1<- data1* log((pred1)+0.00001)+(1-data1)*log(1-((pred1)-0.00001))
  -sum(loglik1,na.rm=T)
}
n.param<-3
gommod<-optim(c(0.25, 0.24, 0.9),gom.binom,method="L-BFGS-B",lower=c(0.10,0.15,0.6),upper=c(0.15,0.8,1))
gommod

nc<-seq(0,max(meanoocysts),1)
pred2b<-(gommod$par[1]/gommod$par[2]) * exp (-exp(gommod$par[3] - gommod$par[2] * nc))
lines(nc,pred2b,lwd=2,lty=3,col="blue")




##########################################################################
##
## 3. Fit the distributions of the data and estimate the real sporozoite counts and prevalence 
##    with and without treatment to get the difference in the probaility of transmission
##  
##
###########################################################################
data1<-list(N_C=16,
            N_T=16,
            #N_T1=16 ##ATV
            #N_T2=16 ##4B7
            #N_T3=16 ##PEvaccine
            #N_T4=16 ##PEvaccine + ATV
            #N_T5=16 ##PEvaccine + 4B7
            N_ooc=24,
            N_mice=5,
            ooc_count_C = structure(.Data = c(oocystsC),
                          .Dim=c(24,16)),
            ooc_count_T = structure(.Data = c(oocystsT),
                                    .Dim=c(24,16)),
            #oc_count_T1 = structure(.Data = c(oocystsT),
            #                        .Dim=c(24,16)),
            #ooc_count_T2 = structure(.Data = c(oocystsT),
            #                        .Dim=c(24,16)),
            #ooc_count_T3 = structure(.Data = c(oocystsT),
            #                        .Dim=c(24,16)),
            #ooc_count_T4 = structure(.Data = c(oocystsT),
            #                        .Dim=c(24,16)),
            #ooc_count_T5 = structure(.Data = c(oocystsT),
            #                        .Dim=c(24,16)),
            prev_C = structure(.Data =PREV_C,.Dim=c(5,16)),
            prev_T = structure(.Data =PREV_T,.Dim=c(5,16)),
            #prev_T1 = structure(.Data =PREV_T,.Dim=c(5,16)),
            #prev_T2 = structure(.Data =PREV_T,.Dim=c(5,16)),
            #prev_T3 = structure(.Data =PREV_T,.Dim=c(5,16)),
            #prev_T4 = structure(.Data =PREV_T,.Dim=c(5,16)),
            #prev_T5 = structure(.Data =PREV_T,.Dim=c(5,16)),
            N_bin=5,
            bin_edge=c(0,1,10,100,1000,10000),
            s_count_C = structure(.Data=spors_C,.Dim=c(16,5)),
            s_count_T = structure(.Data=spors_T,.Dim=c(16,5))#,
            #s_count_T1 = structure(.Data=spors_T,.Dim=c(16,5)),
            #s_count_T2 = structure(.Data=spors_T,.Dim=c(16,5)),
            #s_count_T3 = structure(.Data=spors_T,.Dim=c(16,5)),
            #s_count_T4 = structure(.Data=spors_T,.Dim=c(16,5)),
            #s_count_T5 = structure(.Data=spors_T,.Dim=c(16,5))
            )

stan_rdump(ls(data1), "Ellie.data.R", envir=list2env(data1))
fit1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\mice.censored_sp2.stan", data=data1,
             iter=1000, chains=2)
print(fit1)

params = extract(fit1)

names(params)

sampler_params<-get_sampler_params(fit1, inc_warmup=FALSE)
sapply(sampler_params, function(x) c(x[, 'accept_stat__']))

plot(as.mcmc(sapply(sampler_params, function(x) c(x[, 'accept_stat__']))[,2]))


##Now take the average theta estimates for the controls and compare to the average theta for treatments
##to give an overall efficacy estimate see Paddy's recent paper draft April 2015 (Sc - St / Sc)
meanthetaCb2<-mean(params$theta_C[501:1000,1:4])
meanthetaTb2<-mean(params$theta_T[501:1000,1:4])
meanthetaCb3<-mean(params$theta_C[501:1000,5:8])
meanthetaTb3<-mean(params$theta_T[501:1000,5:8])
meanthetaCb4<-mean(params$theta_C[501:1000,9:12])
meanthetaTb4<-mean(params$theta_T[501:1000,9:12])
meanthetaCb5<-mean(params$theta_C[501:1000,13:16])
meanthetaTb5<-mean(params$theta_T[501:1000,13:16])

mbr<-c(2,3,4,5)
meanEffect<-c((meanthetaCb2-meanthetaTb2)/meanthetaCb2,(meanthetaCb3-meanthetaTb3)/meanthetaCb3,(meanthetaCb4-meanthetaTb4)/meanthetaCb4,(meanthetaCb5-meanthetaTb5)/meanthetaCb5)

vec<-seq(0.025,0.975,by=0.025)
thetCb2<-thetCb3<-thetCb4<-thetCb5<-numeric(length(vec))
thetTb2<-thetTb3<-thetTb4<-thetTb5<-numeric(length(vec))
for (i in 1:length(vec)){
  thetCb2[i]<-c(quantile(params$theta_C[501:1000,1:4],vec[i]))
  thetCb3[i]<-c(quantile(params$theta_C[501:1000,5:8],vec[i]))
  thetCb4[i]<-c(quantile(params$theta_C[501:1000,9:12],vec[i]))
  thetCb5[i]<-c(quantile(params$theta_C[501:1000,13:16],vec[i]))
  
  thetTb2[i]<-c(quantile(params$theta_T[501:1000,1:4],vec[i]))
  thetTb3[i]<-c(quantile(params$theta_T[501:1000,5:8],vec[i]))
  thetTb4[i]<-c(quantile(params$theta_T[501:1000,9:12],vec[i]))
  thetTb5[i]<-c(quantile(params$theta_T[501:1000,13:16],vec[i]))
  }
maineffCupp95<-c(thetCb2[39],thetCb3[39],thetCb4[39],thetCb5[39])
maineffTupp95<-c(thetTb2[39],thetTb3[39],thetTb4[39],thetTb5[39])

maineffCLOW95<-c(thetCb2[1],thetCb3[1],thetCb4[1],thetCb5[1])
maineffTLOW95<-c(thetTb2[1],thetTb3[1],thetTb4[1],thetTb5[1])

EFFu<-(maineffCupp95-maineffTupp95)/maineffCupp95
EFFl<-(maineffCLOW95-maineffTLOW95)/maineffCLOW95


CB2<-sum(rbern(16,prob=meanthetaCb2))/length(rbern(16,prob=meanthetaCb2))
CB3<-sum(rbern(16,prob=meanthetaCb3))/length(rbern(16,prob=meanthetaCb3))
CB4<-sum(rbern(16,prob=meanthetaCb4))/length(rbern(16,prob=meanthetaCb4))
CB5<-sum(rbern(16,prob=meanthetaCb5))/length(rbern(16,prob=meanthetaCb5))
maineffectC<-c(CB2,CB3,CB4,CB5)

TB2<-sum(rbern(16,prob=meanthetaTb2))/length(rbern(16,prob=meanthetaTb2))
TB3<-sum(rbern(16,prob=meanthetaTb3))/length(rbern(16,prob=meanthetaTb3))
TB4<-sum(rbern(16,prob=meanthetaTb4))/length(rbern(16,prob=meanthetaTb4))
TB5<-sum(rbern(16,prob=meanthetaTb5))/length(rbern(16,prob=meanthetaTb5))
maineffectT<-c(TB2,TB3,TB4,TB5);sum(maineffectC,maineffectT)/8##this is probability of infection from mosquito to mouse

#############################
##
###
#### Plot effect size per mosquito biting rate (95%CI)
###
##
###############################

plot(maineffectC*100~mbr,ylab="Effect Size % (95% CI)",
     ylim=c(-40,100),
     xlab="Mosquito biting rate",
     main="Impact of treatment on Infection",
     xlim=c(1,5),pch="",cex.lab=1.2)

for (i in 1:4){
  segments(mbr[i], EFFu[i]*100, x1 = mbr[i], y1 = EFFl[i]*100,
           col  = terrain.colors(10), lty = 1, lwd = 5)
}
points(meanEffect*100~mbr,col="black",pch=20)
lines(meanEffect*100~mbr,col="black",pch=20)
text(3.5,100,"Overall Effect Size = 13.0%")
text(3.5,90,"A positive value indicates drug treatment")
text(3.5,82,"benefits the hosts in comparison to control")
abline(h=0,lty=2,col="lightgrey")
text(2.5,-30,"Probability of infection mosquito to mouse = 59.4%")

##and the overall effect size
meanthetaC<-mean(params$theta_C[501:1000,1:16])
meanthetaT<-mean(params$theta_T[501:1000,1:16])
(meanthetaC-meanthetaT)/meanthetaC
##EffOoc
##EFFSPmbr
lines(EffOoc*100~mbr,pch=20,col="grey25",lty=2)
lines(EFFSPmbr*100~mbr,pch=20,col="grey15",lty=3)

legend(1,20,legend=c("Model estimate (95% CI)","Oocysts","Sporozoites"),
       lty=c(1,2,3),col=c("forestgreen","grey25","grey15"),lwd=c(3,1,1))

######################################################
######################################################
thetasC<-thetasT<-thetasupT<-thetasupC<-thetasloT<-thetasloC<-numeric(16)
for (i in 1:16){
  thetasC[i]<-mean(params$theta_C[501:1000,i])
  thetasT[i]<-mean(params$theta_T[501:1000,i])
  thetasupC[i]<-quantile(params$theta_C[501:1000,i],0.975)
  thetasupT[i]<-quantile(params$theta_T[501:1000,i],0.975)
  thetasloC[i]<-quantile(params$theta_C[501:1000,i],0.025)
  thetasloT[i]<-quantile(params$theta_T[501:1000,i],0.025)
}

effectsp<-(thetasC-thetasT)/thetasC
effectspL<-(thetasupC-thetasupT)/thetasupC
effectspU<-(thetasloC-thetasloT)/thetasloC
meanprevpergroup<-(prevcon+prevtreat)/2
###########################
##
###
#### Effect size vs parasitemia of the treatment group / Overall
###
##
#############################
parasitemia<-(parasitem+parasitemT)/2
plot(effectsp~parasitemia,xlim=c(0,12),ylim=c(-0.5,1),
     xlab="Parasitemia (%)",ylab="Effect Size TBI")
log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * parasitemia)) / (1 + exp(a + b * parasitemia)) ) 
  prev1<-effectsp
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmod
nc<-seq(0,12,0.01)
pred2<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) )
lines(nc,pred2,lwd=2,lty=2,col="red")

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * parasitemia)) / (1 + exp(a + b * parasitemia)) ) 
  prev1<-effectspU
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmod
nc<-seq(0,12,0.01)
pred2u<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) )
lines(nc,pred2u,lwd=2,lty=2,col="red")

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * parasitemia)) / (1 + exp(a + b * parasitemia)) ) 
  prev1<-effectspL
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmod
nc<-seq(0,12,0.01)
pred2L<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) )
lines(nc,pred2L,lwd=2,lty=2,col="red")
polygon(c(nc, rev(nc)),c(pred2u,rev(pred2L)),border=NA, col="aquamarine1")
lines(nc,pred2,lwd=2,lty=2,col="red")
points(effectsp~parasitemia,col="blue",pch=20)

##################################
##
###
##### Plot Effect size vs host prevalence
###
#######################
plot(c(1,effectsp)~c(0,meanprevpergroup),
     xlim=c(0,1),xlab="Host prevalence",
     ylim=c(-0.5,1),ylab="Effect size TBI")

##summary(glm(effectsp~parasitemT))
log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * c(0,meanprevpergroup))) / (1 + exp(a + b * c(0,meanprevpergroup))) ) 
  prev1<-c(1,effectsp)
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmod
nc<-seq(0,1,0.01)
pred2<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) ) 
lines(nc,pred2,lwd=2,lty=2,col="red")

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * c(0,meanprevpergroup))) / (1 + exp(a + b * c(0,meanprevpergroup))) ) 
  prev1<-c(1,effectspL)
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmod
nc<-seq(0,1,0.01)
pred2u<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) ) 
lines(nc,pred2u,lwd=2,lty=2,col="red")

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * c(0,meanprevpergroup))) / (1 + exp(a + b * c(0,meanprevpergroup))) ) 
  prev1<-c(1,effectspU)
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmod
nc<-seq(0,1,0.01)
pred2l<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) ) 
lines(nc,pred2l,lwd=2,lty=2,col="red")
polygon(c(nc, rev(nc)),c(pred2u,rev(pred2l)),border=NA, col="aquamarine1")
lines(nc,pred2,lwd=2,lty=2,col="red")
points(c(1,effectsp)~c(0,meanprevpergroup),col="blue",pch=20)






meansporsdist<-c(25.04,0.66,16.80,15.24,20.98,85.70,16.21,19.77,33.36,26.91,7.02,19.28,18.37,29.67,31.31,36.44)
meansporsdist<-c(15.48783,3.774894,18.89685,11.44132,29.17819,2.514613,54.84545,7.54126)



##calculate the lowest mean for each popualtion given the break down of sporozoites
a<-spors_C[,2]*1;b<-spors_C[,3]*11;cc<-spors_C[,4]*101;d<-spors_C[,5]*1001;total<-a+b+cc+d

b2r1<-b2r2<-b2r3<-b2r4<-numeric(10);d2<-data.frame(b2r1,b2r2,b2r3,b2r4)
b3r1<-b3r2<-b3r3<-b3r4<-numeric(15);d3<-data.frame(b3r1,b3r2,b3r3,b3r4)
b4r1<-b4r2<-b4r3<-b4r4<-numeric(20);d4<-data.frame(b4r1,b4r2,b4r3,b4r4)
b5r1<-b5r2<-b5r3<-b5r4<-numeric(25);d5<-data.frame(b5r1,b5r2,b5r3,b5r4)

for (i in 1:4){
  d2[,i]<-c(rep(0,spors_C[i,1]),rep(1,spors_C[i,2]),rep(11,spors_C[i,3]),rep(101,spors_C[i,4]),rep(1001,spors_C[i,5]))
  d3[,i]<-c(rep(0,spors_C[i+4,1]),rep(1,spors_C[i+4,2]),rep(11,spors_C[i+4,3]),rep(101,spors_C[i+4,4]),rep(1001,spors_C[i+4,5]))
  d4[,i]<-c(rep(0,spors_C[i+8,1]),rep(1,spors_C[i+8,2]),rep(11,spors_C[i+8,3]),rep(101,spors_C[i+8,4]),rep(1001,spors_C[i+8,5]))
  d5[,i]<-c(rep(0,spors_C[i+12,1]),rep(1,spors_C[i+12,2]),rep(11,spors_C[i+12,3]),rep(101,spors_C[i+12,4]),rep(1001,spors_C[i+12,5]))
}


lowestmeanC<-c(total[1:4]/10,total[5:8]/15,total[9:12]/20,total[13:16]/25)

a<-spors_T[,2]*1;b<-spors_T[,3]*11;cc<-spors_T[,4]*101;d<-spors_T[,5]*1001;total<-a+b+cc+d

b2r1<-b2r2<-b2r3<-b2r4<-numeric(10);d2T<-data.frame(b2r1,b2r2,b2r3,b2r4)
b3r1<-b3r2<-b3r3<-b3r4<-numeric(15);d3T<-data.frame(b3r1,b3r2,b3r3,b3r4)
b4r1<-b4r2<-b4r3<-b4r4<-numeric(20);d4T<-data.frame(b4r1,b4r2,b4r3,b4r4)
b5r1<-b5r2<-b5r3<-b5r4<-numeric(25);d5T<-data.frame(b5r1,b5r2,b5r3,b5r4)

for (i in 1:4){
  d2T[,i]<-c(rep(0,spors_T[i,1]),rep(1,spors_T[i,2]),rep(11,spors_T[i,3]),rep(101,spors_T[i,4]),rep(1001,spors_T[i,5]))
  d3T[,i]<-c(rep(0,spors_T[i+4,1]),rep(1,spors_T[i+4,2]),rep(11,spors_T[i+4,3]),rep(101,spors_T[i+4,4]),rep(1001,spors_T[i+4,5]))
  d4T[,i]<-c(rep(0,spors_T[i+8,1]),rep(1,spors_T[i+8,2]),rep(11,spors_T[i+8,3]),rep(101,spors_T[i+8,4]),rep(1001,spors_T[i+8,5]))
  d5T[,i]<-c(rep(0,spors_T[i+12,1]),rep(1,spors_T[i+12,2]),rep(11,spors_T[i+12,3]),rep(101,spors_T[i+12,4]),rep(1001,spors_T[i+12,5]))
}
lowestmeanT<-c(total[1:4]/10,total[5:8]/15,total[9:12]/20,total[13:16]/25)


DATA1<-c(lowestmeanC,lowestmeanT)      ##Minimum that the mean can be for each of the populations given distribution of sporozoites

a<-spors_C[,2]*9;b<-spors_C[,3]*99;cc<-spors_C[,4]*999;d<-spors_C[,5]*99999;total<-a+b+cc+d
himeanC<-c(total[1:4]/10,total[5:8]/15,total[9:12]/20,total[13:16]/25)

a<-spors_T[,2]*9;b<-spors_T[,3]*99;cc<-spors_T[,4]*999;d<-spors_T[,5]*99999;total<-a+b+cc+d
himeanT<-c(total[1:4]/10,total[5:8]/15,total[9:12]/20,total[13:16]/25)
DATA2<-c(himeanC,himeanT)      ##Minimum that the mean can be for each of the populations given distribution of sporozoites




meansporsdist<-c(11.3,100.2,211.5,11.4,155.4,147.9,142.5,10.7,21.5,126.1,207.1,112,
                 102.16,95.8,217.4,58.08,1.2,0.1,0.3,0,8.4,13.7,2.53,87.8,
                 13.45,32,326.7,408.25,13.48,167.04,137.28,143.12)
distsp<-meansporsdist/sum(meansporsdist)                
meanoocystsdist<-c(meanoocysts)
plot(DATA2~meanoocystsdist)
points(DATA1~meanoocystsdist,col="red",pch=16)
summary(lm(meansporsdist~meanoocystsdist+0))

#####################################################################
##
##  Linear model for minimum actual counts
##
######################################################################
datalm<-list(N=32,
             ooc_count = meanoocystsdist,
             s_count = meansporsdist)

stan_rdump(ls(datalm), "Ellie.datalm.R", envir=list2env(datalm))


flm <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\MODEL3stan_lm.stan", data=datalm,
           iter=1000, chains=4)
print(flm);params = extract(flm)
names(params)
nooc<-seq(0,100,1)
NspPredLM<-3.51*nooc + 0
NspPredLMmin<-2.55*nooc + 0
NspPredLMmax<-4.51*nooc + 0
plot(NspPredLMmax~nooc)
points(NspPredLMmin~nooc)
points(NspPredLM~nooc,pch=16)

points(meanoocystsdist,meansporsdist,pch=19,col="red")
dataC_lm<-data.frame(nooc,NspPredLM,NspPredLMmin,NspPredLMmax);head(dataC_lm)

controls<-merge(freqdistoocC, dataC_lm, by.x = "Nooc", by.y = "nooc",all.x=TRUE);head(controls)
atvs<-merge(freqdistoocT, dataC_lm, by.x = "NoocT", by.y = "nooc",all.x=TRUE);head(atvs)


##########################################################################
##
##  Saturating relationship for minimum counts
##
###########################################################################
datatemp<-data.frame(meanoocystsdist,meansporsdist)
datatemp2<-datatemp[with(datatemp, order(meanoocystsdist)), ]
datasatmod<-list(N=32,
             ooc_count = datatemp2$meanoocystsdist,
             s_count = datatemp2$meansporsdist)
fsatmod <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\MODEL3stan_satmod.stan", data=datasatmod,
            iter=1000, chains=4)
print(fsatmod);params2 = extract(fsatmod)
names(params2)

sat.binom<-function(p.vec){

  b<-p.vec[1]
  d<-p.vec[2]
  
  pred<- (b * meanoocystsdist)/(1 + d * meanoocystsdist) 
  
  data1<-meansporsdist
  
  loglik<- data1* log((pred)+0.001)+(1-data1)*log(1-((pred)-0.001))
  
  
  -sum(loglik,na.rm=T)
}
n.param<-3
satmod<-optim(c(8,0.02),sat.binom,method="L-BFGS-B",lower=c(8,0.001),upper=c(400,0.02))
satmod
pred3<-(satmod$par[1] * nc)/(1 + satmod$par[2] * nc) 
lines(nc,pred3,lwd=2,lty=2,col="red")



######################################################################################
##
##   tRYING THE MODEL IN oPENbUGS
##
##
########################################################################################

##Simpler (Oocysts to prevalence)

data<-list(N_ooc=384,
            N_mice=80,
            ooccountC = c(oocystsC),
            ##ooccountT = c(oocystsT),
            prev_C = c(PREV_C))##,
            ##prev_T = c(PREV_T))



inits <- function(){
  list(log_ooc_C = rnorm(data$N_ooc, 0, 20),
       beta_thetaA = rnorm(data$N_mice, 0, 1),
       beta_thetaB = rnorm(data$N_mice, 0, 1),
       alpha_theta = rnorm(data$N_mice, 0, 1))
}

M2Msim1 <- bugs(data, inits, model.file = "C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\openbugsSimple2.txt",
                parameters = c("log_mu_ooc_C", "log_sigma_ooc_C",
                               "log_mu_ooc_T", "log_sigma_ooc_T",
                               "beta_thetaA", "beta_thetaB","alpha_theta"),
                n.chains = 3, n.iter = 1000, debug=TRUE)








data<-list(N_C=16,
            N_T=16,
            N_ooc=24,
            N_mice=5,
            ooc_count_C = structure(.Data = c(oocystsC),
                                    .Dim=c(24,16)),
            ooc_count_T = structure(.Data = c(oocystsT),
                                    .Dim=c(24,16)),
            prev_C = structure(.Data =PREV_C,.Dim=c(5,16)),
            prev_T = structure(.Data =PREV_T,.Dim=c(5,16)),
            N_bin=5,
            bin_edge=c(0,1,10,100,1000,10000),
            s_count_C = structure(.Data=spors_C,.Dim=c(16,5)),
            s_count_T = structure(.Data=spors_T,.Dim=c(16,5))
)


inits <- function(){
       list(log_mu_ooc_C = rnorm(data$N_ooc, 0, 200),
             log_sigma_ooc_C = rnorm(data$N_ooc, 0, 200),
             log_mu_ooc_T = rnorm(data$N_ooc, 0, 100),
             log_sigma_ooc_T = rnorm(data$N_ooc, 0, 100),
             beta_muA = rnorm(data$N_C, 0, 200),
             beta_muB = rnorm(data$N_C, 0, 200),
             beta_sigmaA = rnorm(data$N_C, 0, 200),
             beta_sigmaB = rnorm(data$N_C, 0, 200),
             beta_thetaA = rnorm(data$N_mice, 0, 1),
             beta_thetaB = rnorm(data$N_mice, 0, 1),
             alpha_mu = rnorm(data$N_C, 0, 200),
             alpha_sigma = rnorm(data$N_C, 0, 200),
             alpha_theta = rnorm(data$N_mice, 0, 1))
}

M2Msim1 <- bugs(data, inits, model.file = "C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\openbugs.txt",
                                           parameters = c("log_mu_ooc_C", "log_sigma_ooc_C",
                                                           "log_mu_ooc_T", "log_sigma_ooc_T",
                                                           "beta_muA", "beta_muB", "beta_sigmaA", "beta_sigmaB",
                                                           "beta_thetaA", "beta_thetaB",
                                                           "alpha_mu", "alpha_sigma", "alpha_theta"),
                                           n.chains = 3, n.iter = 1000, debug=TRUE)

