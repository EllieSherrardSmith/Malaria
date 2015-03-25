library(rstan)
library(MASS)
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
    sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==1],sporsbites2$Sporozoite2[sporsbites2$Round==1]);
    spb2r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
      sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==2],sporsbites2$Sporozoite2[sporsbites2$Round==2]);
      spb2r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
        sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==3],sporsbites2$Sporozoite2[sporsbites2$Round==3]);
        spb2r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
          sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==4],sporsbites2$Sporozoite2[sporsbites2$Round==4]);
          spb2r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))


sporsbites3<-subset(spors,Bites==3 & Treatment==0);sporsbites3
    sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==1],sporsbites3$Sporozoite2[sporsbites3$Round==1],sporsbites3$Sporozoite3[sporsbites3$Round==1]);
    spb3r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
      sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==2],sporsbites3$Sporozoite2[sporsbites3$Round==2],sporsbites3$Sporozoite3[sporsbites3$Round==2]);
      spb3r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
        sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==3],sporsbites3$Sporozoite2[sporsbites3$Round==3],sporsbites3$Sporozoite3[sporsbites3$Round==3]);
        spb3r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
          sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==4],sporsbites3$Sporozoite2[sporsbites3$Round==4],sporsbites3$Sporozoite3[sporsbites3$Round==4]);
          spb3r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

sporsbites4<-subset(spors,Bites==4 & Treatment==0);sporsbites4
    sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==1],sporsbites4$Sporozoite2[sporsbites4$Round==1],sporsbites4$Sporozoite3[sporsbites4$Round==1],sporsbites4$Sporozoite4[sporsbites4$Round==1]);
    spb4r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
      sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==2],sporsbites4$Sporozoite2[sporsbites4$Round==2],sporsbites4$Sporozoite3[sporsbites4$Round==2],sporsbites4$Sporozoite4[sporsbites4$Round==2]);
      spb4r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
        sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==3],sporsbites4$Sporozoite2[sporsbites4$Round==3],sporsbites4$Sporozoite3[sporsbites4$Round==3],sporsbites4$Sporozoite4[sporsbites4$Round==3]);
        spb4r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
          sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==4],sporsbites4$Sporozoite2[sporsbites4$Round==4],sporsbites4$Sporozoite3[sporsbites4$Round==4],sporsbites4$Sporozoite4[sporsbites4$Round==4]);
          spb4r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

sporsbites5<-subset(spors,Bites==5 & Treatment==0);sporsbites5
    sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==1],sporsbites5$Sporozoite2[sporsbites5$Round==1],sporsbites5$Sporozoite3[sporsbites5$Round==1],sporsbites5$Sporozoite4[sporsbites5$Round==1],sporsbites5$Sporozoite5[sporsbites5$Round==1]);
    spb5r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
      sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==2],sporsbites5$Sporozoite2[sporsbites5$Round==2],sporsbites5$Sporozoite3[sporsbites5$Round==2],sporsbites5$Sporozoite4[sporsbites5$Round==2],sporsbites5$Sporozoite5[sporsbites5$Round==2]);
      spb5r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
        sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==3],sporsbites5$Sporozoite2[sporsbites5$Round==3],sporsbites5$Sporozoite3[sporsbites5$Round==3],sporsbites5$Sporozoite4[sporsbites5$Round==3],sporsbites5$Sporozoite5[sporsbites5$Round==3]);
        spb5r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
          sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==4],sporsbites5$Sporozoite2[sporsbites5$Round==4],sporsbites5$Sporozoite3[sporsbites5$Round==4],sporsbites5$Sporozoite4[sporsbites5$Round==4],sporsbites5$Sporozoite5[sporsbites5$Round==4]);
          spb5r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

spors_C<-rbind(spb2r1,spb2r2,spb2r3,spb2r4,
               spb3r1,spb3r2,spb3r3,spb3r4,
               spb4r1,spb4r2,spb4r3,spb4r4,
               spb5r1,spb5r2,spb5r3,spb5r4)


sporsbites2<-subset(spors,Bites==2 & Treatment==1);sporsbites2
    sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==1],sporsbites2$Sporozoite2[sporsbites2$Round==1]);
    spb2r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
      sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==2],sporsbites2$Sporozoite2[sporsbites2$Round==2]);
      spb2r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
        sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==3],sporsbites2$Sporozoite2[sporsbites2$Round==3]);
        spb2r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
          sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==4],sporsbites2$Sporozoite2[sporsbites2$Round==4]);
          spb2r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))


sporsbites3<-subset(spors,Bites==3 & Treatment==1);sporsbites3
    sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==1],sporsbites3$Sporozoite2[sporsbites3$Round==1],sporsbites3$Sporozoite3[sporsbites3$Round==1]);
    spb3r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
      sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==2],sporsbites3$Sporozoite2[sporsbites3$Round==2],sporsbites3$Sporozoite3[sporsbites3$Round==2]);
      spb3r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
        sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==3],sporsbites3$Sporozoite2[sporsbites3$Round==3],sporsbites3$Sporozoite3[sporsbites3$Round==3]);
        spb3r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
          sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==4],sporsbites3$Sporozoite2[sporsbites3$Round==4],sporsbites3$Sporozoite3[sporsbites3$Round==4]);
          spb3r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

sporsbites4<-subset(spors,Bites==4 & Treatment==1);sporsbites4
    sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==1],sporsbites4$Sporozoite2[sporsbites4$Round==1],sporsbites4$Sporozoite3[sporsbites4$Round==1],sporsbites4$Sporozoite4[sporsbites4$Round==1]);
    spb4r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
      sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==2],sporsbites4$Sporozoite2[sporsbites4$Round==2],sporsbites4$Sporozoite3[sporsbites4$Round==2],sporsbites4$Sporozoite4[sporsbites4$Round==2]);
      spb4r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
        sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==3],sporsbites4$Sporozoite2[sporsbites4$Round==3],sporsbites4$Sporozoite3[sporsbites4$Round==3],sporsbites4$Sporozoite4[sporsbites4$Round==3]);
        spb4r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
          sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==4],sporsbites4$Sporozoite2[sporsbites4$Round==4],sporsbites4$Sporozoite3[sporsbites4$Round==4],sporsbites4$Sporozoite4[sporsbites4$Round==4]);
          spb4r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

sporsbites5<-subset(spors,Bites==5 & Treatment==1);sporsbites5
    sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==1],sporsbites5$Sporozoite2[sporsbites5$Round==1],sporsbites5$Sporozoite3[sporsbites5$Round==1],sporsbites5$Sporozoite4[sporsbites5$Round==1],sporsbites5$Sporozoite5[sporsbites5$Round==1]);
    spb5r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
      sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==2],sporsbites5$Sporozoite2[sporsbites5$Round==2],sporsbites5$Sporozoite3[sporsbites5$Round==2],sporsbites5$Sporozoite4[sporsbites5$Round==2],sporsbites5$Sporozoite5[sporsbites5$Round==2]);
      spb5r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
        sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==3],sporsbites5$Sporozoite2[sporsbites5$Round==3],sporsbites5$Sporozoite3[sporsbites5$Round==3],sporsbites5$Sporozoite4[sporsbites5$Round==3],sporsbites5$Sporozoite5[sporsbites5$Round==3]);
        spb5r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
          sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==4],sporsbites5$Sporozoite2[sporsbites5$Round==4],sporsbites5$Sporozoite3[sporsbites5$Round==4],sporsbites5$Sporozoite4[sporsbites5$Round==4],sporsbites5$Sporozoite5[sporsbites5$Round==4]);
          spb5r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
  
spors_T<-rbind(spb2r1,spb2r2,spb2r3,spb2r4,
               spb3r1,spb3r2,spb3r3,spb3r4,
               spb4r1,spb4r2,spb4r3,spb4r4,
               spb5r1,spb5r2,spb5r3,spb5r4)

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
            bin_edge=c(0,1,10,100,1000,10000),
            s_count_C = structure(.Data=spors_C,.Dim=c(16,5)),
            s_count_T = structure(.Data=spors_T,.Dim=c(16,5))
            )


fit1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\MODEL.stan", data=data1,
             iter=100, chains=4)
print(fit1)

plot(fit1)
params = extract(fit1)

names(params)
params$mu_s_C


sampler_params<-get_sampler_params(fit1, inc_warmup=FALSE)
sapply(sampler_params, function(x) c(x[, 'accept_stat__']))

#################
#################
##Model 2
data2<-list(N_Comb=32,
            N_ooc=24,
            N_mice=5,
            ooc_count = structure(.Data = c(oocystsC,oocystsT),
                                    .Dim=c(24,32)),
            prev = structure(.Data =c(PREV_C,PREV_T),.Dim=c(5,32)),
            N_bin=5,
            bin_edge=c(0,1,10,100,1000,10000),
            s_count = structure(.Data=rbind(spors_C,spors_T),.Dim=c(32,5)))

fit2 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\MODEL2stan.stan", data=data2,
             iter=100, chains=4)
print(fit2)
plot(fit2)

params = extract(fit2)
names(params)
traceplot(fit2)

##cALCULATE THE RESPECTIVE PREDICTED POPULATIONS OF OOCYSTS AND SPOROZOITES AND PREVALENCE IN MICE
sampmean<-mean(params$logmu)
sampvar<-mean(params$logmu)+((mean(params$logmu)^2)/mean(params$logsigma_ooc))
N=24
predNooc_k<--(sampmean^2-(sampvar/N))/(sampvar-sampmean)

NOOC<-rnegbin(N,sampmean,predNooc_k)




sampler_params<-get_sampler_params(fit2, inc_warmup=FALSE)
sapply(sampler_params, function(x) c(x[, 'accept_stat__']))

Ns<-seq(0,1000,1)
Nooc<-seq(0,150,1)
x<-mean(params$sigma_s)

Nsp_mu<-(exp(params$beta_mu[1] * params$logmu_ooc + params$beta_mu[2] * params$logmu_ooc + params$alpha_mu))
Nsp_sigma<-(exp(params$beta_sigma[1] * params$logsigma_ooc + params$beta_sigma[2] * params$logsigma_ooc + params$alpha_mu))

sampmeanSPmu<-exp(params$beta_mu[1] * params$logmu_ooc + params$beta_mu[2] * params$logsigma_ooc + params$alpha_mu)
sampmeanSPsigma<-exp(params$beta_sigma[1] * params$logsigma_ooc + params$beta_sigma[2] * params$logsigma_ooc + params$alpha_sigma)
sampvarSP<-sampmeanSPmu+(sampmeanSPmu^2/sampmeanSPsigma)
N=length(params$beta_mu)
predNs_k<-(sampmeanSPmu^2-(sampvarSP/N))/(sampvarSP-sampmeanSPmu)

Ns<-sort(rnegbin(length(probInf),mean(sampmeanSPmu),mean(predNs_k)))
x<-mean(params$theta)

probInf<-(exp(params$beta_mu[2] * sampmeanSPmu + params$beta_sigma[2] * sampmeanSPsigma + params$alpha_theta))/
  (1 + exp(params$beta_mu[2] * sampmeanSPmu + params$beta_sigma[2] * sampmeanSPsigma + params$alpha_theta))^x *
  (1-(exp(params$beta_mu[2] * sampmeanSPmu + params$beta_sigma[2] * sampmeanSPsigma + params$alpha_theta)/
     (1 + exp(params$beta_mu[2] * sampmeanSPmu + params$beta_sigma[2] * sampmeanSPsigma + params$alpha_theta))))^(1-x)

plot(probInf~Ns,log="x")
  