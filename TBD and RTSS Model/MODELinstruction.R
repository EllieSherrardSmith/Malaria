
library(rstan)
library(MASS)
library(boot)
library(coda)

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
oocystsC<-c(oocysts$oocystsbites2control[oocysts$round=="day41"][1:24],
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

freqoocC<-numeric(length(unique(oocystsC)))
oocystsC2<-sort(unique(oocystsC))
for (i in 1:length(oocystsC2)){ 
  freqoocC[i]<-sum(ifelse(oocystsC==unique(oocystsC2)[i],1,0))}
probNooc<-numeric(length(freqoocC))
for (j in 1:length(freqoocC)){
probNooc[j]<-freqoocC[j]/sum(freqoocC)}
freqdistoocC<-data.frame(oocystsC2,probNooc);colnames(freqdistoocC)[1]<-"Nooc"

oocystsT<-c(oocysts$oocystsbites2atv[oocysts$round=="day41"][1:24],
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
freqoocT<-numeric(length(unique(oocystsT)))
oocystsT2<-sort(unique(oocystsT))
for (i in 1:length(oocystsT2)){ 
  freqoocT[i]<-sum(ifelse(oocystsT==unique(oocystsT2)[i],1,0))}
probNoocT<-numeric(length(freqoocT))
for (j in 1:length(freqoocT)){
  probNoocT[j]<-freqoocT[j]/sum(freqoocT)}
freqdistoocT<-data.frame(oocystsT2,probNoocT);colnames(freqdistoocT)[1]<-"NoocT"

meanoocysts<-c(mean(oocysts$oocystsbites2control[oocysts$round=="day41"],na.rm=TRUE),
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

##PREVALENCE IN MICE
PREV_C<-cbind(

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

spors_C<-rbind(spb2r1,spb2r2,spb2r3,spb2r4,
               spb3r1,spb3r2,spb3r3,spb3r4,
               spb4r1,spb4r2,spb4r3,spb4r4,
               spb5r1,spb5r2,spb5r3,spb5r4)



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
  
spors_T<-rbind(spb2r1,spb2r2,spb2r3,spb2r4,
               spb3r1,spb3r2,spb3r3,spb3r4,
               spb4r1,spb4r2,spb4r3,spb4r4,
               spb5r1,spb5r2,spb5r3,spb5r4)

MEANsp<-c(mean(a1),mean(b1),mean(c1),mean(d1),
            mean(ee1),mean(ff1),mean(g1),mean(h1),
            mean(jj1),mean(kk1),mean(l1),mean(m1),
            mean(nn1),mean(oo1),mean(pp1),mean(qq1),
            mean(a),mean(b),mean(c),mean(d),
            mean(ee),mean(ff),mean(g),mean(h),
            mean(jj),mean(kk),mean(l),mean(m),
            mean(nn),mean(oo),mean(pp),mean(qq))

############################################################################
##
## 1. Fit for the mean distribution of sporozoite scores to oocyst counts
##
##
##
############################################################################

plot(MEANsp~meanoocysts)
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
n.param<-3
satmod<-optim(c(0.9,0.6,1,4),sat.binom,method="L-BFGS-B",lower=c(0,0.001,0.01,0.1),upper=c(10,10,10,5))
satmod
nc<-seq(0,max(meanoocysts),1)
pred<-(satmod$par[1] * nc^satmod$par[3])/(satmod$par[4] + satmod$par[2] * nc^satmod$par[3]) 
lines(nc,pred,lwd=2,lty=2,col="red")

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

par(mfrow=c(1,1));par(mar=c(5,5,5,5))
plot(meanoocysts,MEANsp,
     ylim=c(0,3),ylab="Mean sporozoite score",
     xlim=c(0,max(meanoocysts)),xlab="Oocyst mean intensity",cex.lab=2)
lines(predupper~nc)
lines(predlower~nc)
polygon(c(nc, rev(nc)),c(predupper,rev(predlower)),border=NA, col="aquamarine1")
lines(pred~nc)
points(meanoocysts,MEANsp,col="aquamarine4",pch=16)


##########################################################################
##
## 2. Fit the distributions of the data and estimate the real sporozoite counts and prevalence 
##    with and without treatment to get the difference in the probaility of transmission
##  
##
###########################################################################
data1<-list(N_C=16,
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
            bin_edge=c(0,1,10,100,1000,1002),
            s_count_C = structure(.Data=spors_C,.Dim=c(16,5)),
            s_count_T = structure(.Data=spors_T,.Dim=c(16,5))
            )

stan_rdump(ls(data1), "Ellie.data.R", envir=list2env(data1))
fit1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\mice.censored_sp.stan", data=data1,
             iter=100, chains=2)
print(fit1)

params = extract(fit1)

names(params)
params$mu_s_C

sampler_params<-get_sampler_params(fit1, inc_warmup=FALSE)
sapply(sampler_params, function(x) c(x[, 'accept_stat__']))

plot(as.mcmc(sapply(sampler_params, function(x) c(x[, 'accept_stat__']))[,2]))

     #
     ##
######## estimate oocyst distribution with and without treatment
     ##
     #
N <- 24
pred_ooc_distC_mean <- exp(mean(params$log_mu_ooc_C)) / exp(mean(params$log_sigma_ooc_C))
pred_ooc_distC_var <- (exp(mean(params$log_mu_ooc_C)) / exp(mean(params$log_sigma_ooc_C))^2) * (exp(mean(params$log_sigma_ooc_C)) + 1)
pred_ooc_distC_k <- ((pred_ooc_distC_mean^2 - pred_ooc_distC_var) / N) / (pred_ooc_distC_var - pred_ooc_distC_mean)
  
PredictedOocystsC<-rnegbin(N,pred_ooc_distC_mean,pred_ooc_distC_k)

pred_ooc_distT_mean <- exp(mean(params$log_mu_ooc_T)) / exp(mean(params$log_sigma_ooc_T))
pred_ooc_distT_var <- (exp(mean(params$log_mu_ooc_T)) / exp(mean(params$log_sigma_ooc_T))^2) * (exp(mean(params$log_sigma_ooc_T)) + 1)
pred_ooc_distT_k <- ((pred_ooc_distT_mean^2 - pred_ooc_distT_var) / N) / (pred_ooc_distT_var - pred_ooc_distT_mean)

PredictedOocystsT<-rnegbin(N,pred_ooc_distT_mean,pred_ooc_distT_k)

     #
     ##
######## estimate sporozoite distribution with and without treatment
     ##
     #
N <- 25
PredictedSpsC<-rnegbin(N,mean(params$sim_ooc_count_C),mean(var(params$sim_ooc_count_C)))
PredictedSpsT<-rnegbin(N,mean(params$sim_ooc_count_T),mean(var(params$sim_ooc_count_T)))

     #
     ##
######## estimate PREVALENCE IN MICE with and without treatment
     ##
     #
mu_s_C <- exp(  params$beta_mu[1] * mean(params$log_mu_ooc_C)
                + params$beta_mu[2] * mean(params$log_sigma_ooc_C)
                + mean(params$alpha_mu))
sigma_s_C <- exp(  params$beta_sigma[1] * mean(params$log_mu_ooc_C)
                   + params$beta_sigma[2] * mean(params$log_sigma_ooc_C)
                   + mean(params$alpha_sigma))
theta_C <- inv.logit(  params$beta_theta[1] * log(mu_s_C)
                          + params$beta_theta[2] * log(sigma_s_C)
                          + mean(params$alpha_theta))

PredPrev_C <- c(dbinom(0, size=50, prob=theta_C))
                

mu_s_T <- exp(  params$beta_mu[1] * mean(params$log_mu_ooc_T)
                   + params$beta_mu[2] * mean(params$log_sigma_ooc_T)
                   + mean(params$alpha_mu))
sigma_s_T <- exp(  params$beta_sigma[1] * mean(params$log_mu_ooc_T)
                      + params$beta_sigma[2] * mean(params$log_sigma_ooc_T)
                      + mean(params$alpha_sigma))
theta_T <- inv.logit(  params$beta_theta[1] * log(mu_s_T)
                       + params$beta_theta[2] * log(sigma_s_T)
                       + mean(params$alpha_theta))

PredPrev_T <- c(dbinom(0, size=50, prob=theta_T))

##So the difference in the probability of infection is 

PredPrev_C - PredPrev_T  ## 0.2447927 

par(mfrow=c(2,2))
hist(PredictedOocystsC,xlab="Oocysts from Controls")
hist(PredictedOocystsT,xlab="Oocysts from TREATMENT")

hist(PredictedSpsC,xlab="Sporozoites from Controls")
hist(PredictedSpsT,xlab="Sporozoites from TREATMENT")

##With ATV-25 we can reduce the probability of infection by 24.5% 

#################
#################
##Model 2
##Removing the control and treatment comparison
data2<-list(N_Comb=32,
            N_ooc=24,
            N_mice=5,
            ooc_count = structure(.Data = c(oocystsC,oocystsT),
                                    .Dim=c(24,32)),
            prev = structure(.Data =c(PREV_C,PREV_T),.Dim=c(5,32)),
            N_bin=5,
            bin_edge=c(0,1,10,100,1000,1002),
            s_count = structure(.Data=rbind(spors_C,spors_T),.Dim=c(32,5)))
stan_rdump(ls(data2), "Ellie.data2.R", envir=list2env(data2))


##And considering solely the control data
data2control<-list(N_Comb=16,
            N_ooc=24,
            N_mice=5,
            ooc_count = structure(.Data = c(oocystsC),
                                  .Dim=c(24,16)),
            prev = structure(.Data =c(PREV_C),.Dim=c(5,16)),
            N_bin=5,
            bin_edge=c(0,1,10,100,1000,1002),
            s_count = structure(.Data=rbind(spors_C),.Dim=c(16,5)))

sum(s_count[,1]);sum(s_count[,2]);sum(s_count[,3]);sum(s_count[,4]);sum(s_count[,5])

data2atvs<-list(N_Comb=16,
                   N_ooc=24,
                   N_mice=5,
                   ooc_count = structure(.Data = c(oocystsT),
                                         .Dim=c(24,16)),
                   prev = structure(.Data =c(PREV_T),.Dim=c(5,16)),
                   N_bin=5,
                   bin_edge=c(0,1,10,100,1000,1002),
                   s_count = structure(.Data=rbind(spors_T),.Dim=c(16,5)))

fit2C <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\MODEL2stan.stan", data=data2controls,
             iter=100, chains=4)
print(fit2C);params = extract(fit2);names(params)

fit2T <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\MODEL2stan.stan", data=data2controls,
              iter=100, chains=4)
print(fit2T);paramsT = extract(fit2T);names(paramsT)

##cALCULATE THE RESPECTIVE PREDICTED POPULATIONS OF OOCYSTS AND SPOROZOITES AND PREVALENCE IN MICE

Nsp_mu<-(exp(params$beta_mu[1] * params$logmu_ooc + params$beta_mu[2] * params$logmu_ooc + params$alpha_mu))
Nsp_sigma<-(exp(params$beta_sigma[1] * params$logsigma_ooc + params$beta_sigma[2] * params$logsigma_ooc + params$alpha_mu))

sampmeanSPmu<-exp(params$beta_mu[1] * params$logmu_ooc + params$beta_mu[2] * params$logsigma_ooc + params$alpha_mu)
sampmeanSPsigma<-exp(params$beta_sigma[1] * params$logsigma_ooc + params$beta_sigma[2] * params$logsigma_ooc + params$alpha_sigma)
sampvarSP<-sampmeanSPmu+(sampmeanSPmu^2/sampmeanSPsigma)
N=200
predNs_k<-(sampmeanSPmu^2-(sampvarSP/N))/(sampvarSP-sampmeanSPmu)

NSP<-rnegbin(N,sampmeanSPmu,predNs_k)
sort(NSP)
length(NSP[NSP==0])
length(NSP[NSP>0 & NSP < 11])
length(NSP[NSP>10 & NSP < 101])
length(NSP[NSP==100 & NSP <1001])
length(NSP[NSP>1000])


###################################################################
##
## tRYING EACH GROUP independently
##
##
###################################

########################################
##gp A (bite2 round1 Treatment = control)

d1<-list(N_Comb=1,
         N_ooc=32,
         N_mice=5,
         ooc_count = structure(.Data = c(oocysts$oocystsbites2control[oocysts$round=="day41"][1:32]),
                                     .Dim=c(32,1)),
         prev = structure(.Data =c(PREV_C[,1]),.Dim=c(5,1)),
         N_bin=5,
         bin_edge=c(0,1,10,100,1000,1002),
         s_count = structure(.Data=rbind(spors_C[1,]),.Dim=c(1,5)))
f1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\MODEL2stan.stan", data=d1,
             iter=1000, chains=4)

print(f1);params = extract(f1)
names(params)
meangpA<-mean(params$mu_s)/mean(params$sigma_s)
vargpA<-(mean(params$mu_s)/(mean(params$sigma_s))^2) * (mean(params$sigma_s) + 1)
N = 10
kgpA = (meangpA^2-(vargpA/N))/(vargpA-meangpA)
meangpA;vargpA

Nsp_gpA<-rnegbin(N,meangpA,kgpA)
sort(Nsp_gpA)
length(Nsp_gpA[Nsp_gpA==0])
length(Nsp_gpA[Nsp_gpA>0 & Nsp_gpA < 11])
length(Nsp_gpA[Nsp_gpA>10 & Nsp_gpA < 101])
length(Nsp_gpA[Nsp_gpA==100 & Nsp_gpA <1001])
length(Nsp_gpA[Nsp_gpA>1000])

#######################
## And gp B (bite2 round2 Treatment = control)

d2<-list(N_Comb=1,
         N_ooc=45,
         N_mice=5,
         ooc_count = structure(.Data = c(oocysts$oocystsbites2control[oocysts$round=="day72"][1:45]),
                               .Dim=c(45,1)),
         prev = structure(.Data =c(PREV_C[,2]),.Dim=c(5,1)),
         N_bin=5,
         bin_edge=c(0,1,10,100,1000,1002),
         s_count = structure(.Data=rbind(spors_C[2,]),.Dim=c(1,5)))
f2 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\MODEL2stan.stan", data=d2,
           iter=1000, chains=4)


print(f2);params = extract(f2)
names(params)
meansp<-mean(params$mu_s)/mean(params$sigma_s)
varsp<-(mean(params$mu_s)/(mean(params$sigma_s))^2) * (mean(params$sigma_s) + 1)
meansp;varsp
#######################
## And gp XXX (bite XX round XX Treatment = XXXXX)

d8<-list(N_Comb=1,
         N_ooc=45,
         N_mice=5,
         ooc_count = structure(.Data = c(oocysts$oocystsbites3control[oocysts$round=="day134"]),
                               .Dim=c(45,1)),
         prev = structure(.Data =c(PREV_C[,8]),.Dim=c(5,1)),
         N_bin=5,
         bin_edge=c(0,1,10,100,1000,10000),
         s_count = structure(.Data=rbind(spors_C[8,]),.Dim=c(1,5)))
f8 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\MODEL2stan.stan", data=d8,
           iter=1000, chains=4)

print(f8);params = extract(f8)
names(params)
meansp<-mean(params$mu_s)/mean(params$sigma_s)
varsp<-(mean(params$mu_s)/(mean(params$sigma_s))^2) * (mean(params$sigma_s) + 1)
meansp;varsp

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

datamodb<-list(N_C=4,
               N_T=4,
               N_ooc=24,
               N_sp=25,
               N_mice=5,
               ooc_count_C = structure(.Data = c(oocystsC[289:384]),
                                       .Dim=c(24,4)),
               ooc_count_T = structure(.Data = c(oocystsT[289:384]),
                                       .Dim=c(24,4)),
               prev_C = structure(.Data =PREV_C[,13:16],.Dim=c(5,4)),
               prev_T = structure(.Data =PREV_T[,13:16],.Dim=c(5,4)),
               s_count_C = structure(.Data=cbind(d5[,1],d5[,2],d5[,3],d5[,4]),.Dim=c(25,4)),
               s_count_T = structure(.Data=cbind(d5T[,1],d5T[,2],d5T[,3],d5T[,4]),.Dim=c(25,4))
)

datamodb4<-list(N_C=4,
               N_T=4,
               N_ooc=24,
               N_sp=20,
               N_mice=5,
               ooc_count_C = structure(.Data = c(oocystsC[193:288]),
                                       .Dim=c(24,4)),
               ooc_count_T = structure(.Data = c(oocystsT[193:288]),
                                       .Dim=c(24,4)),
               prev_C = structure(.Data =PREV_C[,9:12],.Dim=c(5,4)),
               prev_T = structure(.Data =PREV_T[,9:12],.Dim=c(5,4)),
               s_count_C = structure(.Data=cbind(d4[,1],d4[,2],d4[,3],d4[,4]),.Dim=c(20,4)),
               s_count_T = structure(.Data=cbind(d4T[,1],d4T[,2],d4T[,3],d4T[,4]),.Dim=c(20,4))
)

datamodb3<-list(N_C=4,
                N_T=4,
                N_ooc=24,
                N_sp=15,
                N_mice=5,
                ooc_count_C = structure(.Data = c(oocystsC[97:192]),
                                        .Dim=c(24,4)),
                ooc_count_T = structure(.Data = c(oocystsT[97:192]),
                                        .Dim=c(24,4)),
                prev_C = structure(.Data =PREV_C[,5:8],.Dim=c(5,4)),
                prev_T = structure(.Data =PREV_T[,5:8],.Dim=c(5,4)),
                s_count_C = structure(.Data=cbind(d3[,1],d3[,2],d3[,3],d3[,4]),.Dim=c(15,4)),
                s_count_T = structure(.Data=cbind(d3T[,1],d3T[,2],d3T[,3],d3T[,4]),.Dim=c(15,4))
)

datamodb2<-list(N_C=4,
                N_T=4,
                N_ooc=24,
                N_sp=10,
                N_mice=5,
                ooc_count_C = structure(.Data = c(oocystsC[1:96]),
                                        .Dim=c(24,4)),
                ooc_count_T = structure(.Data = c(oocystsT[1:96]),
                                        .Dim=c(24,4)),
                prev_C = structure(.Data =PREV_C[,1:4],.Dim=c(5,4)),
                prev_T = structure(.Data =PREV_T[,1:4],.Dim=c(5,4)),
                s_count_C = structure(.Data=cbind(d2[,1],d2[,2],d2[,3],d2[,4]),.Dim=c(10,4)),
                s_count_T = structure(.Data=cbind(d2T[,1],d2T[,2],d2T[,3],d2T[,4]),.Dim=c(10,4))
)
fit1test <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\MODELb.stan", data=datamodb,
                  iter=100, chains=4)
print(fit1test)

fitB4test <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\MODELb.stan", data=datamodb4,
                 iter=100, chains=4)
print(fitB4test)

fitB3test <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\MODELb.stan", data=datamodb3,
                  iter=100, chains=4)
print(fitB3test)

fitB2test <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\MODELb.stan", data=datamodb2,
                  iter=100, chains=4)
print(fitB2test)



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
nooc<-seq(0,280,1)
NspPredLM<-3.51*nooc + 0
NspPredLMmin<-2.55*nooc + 0
NspPredLMmax<-4.51*nooc + 0
plot(NspPredLMmax~nooc)
points(NspPredLMmin~nooc)
points(NspPredLM~nooc,pch=16)
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




#########################################################
##########################################################
r = 0.5
m = 5.3
k = 10

nb1<-((r/(r+m))^r) *
  (((k - r - 1) * (k - r - 2) * (k - r - 3) * (k - r - 4) * (k - r - 5) * (k - r - 6) * (k - r - 7) * (k - r - 8) * (k - r - 9) * (k - r - 10))/(factorial(k))) *
  (m/(r+m))^k


nb2<-((r/(r+m))^r) *
  (((k - r - 1) * (k - r - 2) * (k - r - 3) * (k - r - 4) * (k - r - 5) * (k - r - 6) * (k - r - 7) * (k - r - 8) * (k - r - 9) )/(factorial(k))) *
  (m/(r+m))^k


nb3<-((r/(r+m))^r) *
  (((k - r - 1) * (k - r - 2) * (k - r - 3) * (k - r - 4) * (k - r - 5) * (k - r - 6) * (k - r - 7) * (k - r - 8) )/(factorial(k))) *
  (m/(r+m))^k


nb4<-((r/(r+m))^r) *
  (((k - r - 1) * (k - r - 2) * (k - r - 3) * (k - r - 4) * (k - r - 5) * (k - r - 6) * (k - r - 7) )/(factorial(k))) *
  (m/(r+m))^k

nb5<-((r/(r+m))^r) *
  (((k - r - 1) * (k - r - 2) * (k - r - 3) * (k - r - 4) * (k - r - 5) * (k - r - 6)  )/(factorial(k))) *
  (m/(r+m))^k


nb6<-((r/(r+m))^r) *
  (((k - r - 1) * (k - r - 2) * (k - r - 3) * (k - r - 4) * (k - r - 5) )/(factorial(k))) *
  (m/(r+m))^k


nb7<-((r/(r+m))^r) *
  (((k - r - 1) * (k - r - 2) * (k - r - 3) * (k - r - 4)  )/(factorial(k))) *
  (m/(r+m))^k

nb8<-((r/(r+m))^r) *
  (((k - r - 1) * (k - r - 2) * (k - r - 3) )/(factorial(k))) *
  (m/(r+m))^k


nb9<-((r/(r+m))^r) *
  (((k - r - 1) * (k - r - 2)  )/(factorial(k))) *
  (m/(r+m))^k


nb91<-((r/(r+m))^r) *
  (((k - r - 1)  )/(factorial(k))) *
  (m/(r+m))^k
nb1;nb2;nb3;nb4;nb5;nb6;nb7;nb8;nb9;nb91
nb1+nb2+nb3+nb4+nb5+nb6+nb7+nb8+nb9+nb91

