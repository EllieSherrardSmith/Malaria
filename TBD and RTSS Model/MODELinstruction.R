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

size.of.grid<-10
a.range<-seq(0.7,0.99,length=size.of.grid)
b.range<-seq(0.1,0.6,length=size.of.grid)
c.range<-seq(0.3,0.7,length=size.of.grid)
d.range<-seq(3.6,4.6,length=size.of.grid)
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

plot(meanoocysts,MEANsp,
     ylim=c(0,3),ylab="Mean sporozoite score",
     xlim=c(0,max(meanoocysts)),xlab="Oocyst mean intensity")
lines(predupper~nc)
lines(predlower~nc)
polygon(c(nc, rev(nc)),c(predupper,rev(predlower)),border=NA, col="aquamarine1")
lines(pred~nc)
points(meanoocysts,MEANsp,col="aquamarine4",pch=19)
##########################################################################
##
## 2. Fit the distributions of the data and estimate the real sporozoite counts
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
            bin_edge=c(0,1,10,100,1000,10000),
            s_count_C = structure(.Data=spors_C,.Dim=c(16,5)),
            s_count_T = structure(.Data=spors_T,.Dim=c(16,5))
            )


fit1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\MODEL.stan", data=data1,
             iter=100, chains=4)
print(fit1)

params = extract(fit1)

names(params)
params$mu_s_C

sampler_params<-get_sampler_params(fit1, inc_warmup=FALSE)
sapply(sampler_params, function(x) c(x[, 'accept_stat__']))

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
            bin_edge=c(0,1,10,100,1000,10000),
            s_count = structure(.Data=rbind(spors_C,spors_T),.Dim=c(32,5)))

##And considering solely the control data
data2control<-list(N_Comb=16,
            N_ooc=24,
            N_mice=5,
            ooc_count = structure(.Data = c(oocystsC),
                                  .Dim=c(24,16)),
            prev = structure(.Data =c(PREV_C),.Dim=c(5,16)),
            N_bin=5,
            bin_edge=c(0,1,10,100,1000,10000),
            s_count = structure(.Data=rbind(spors_C),.Dim=c(16,5)))

sum(s_count[,1]);sum(s_count[,2]);sum(s_count[,3]);sum(s_count[,4]);sum(s_count[,5])

fit2 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\MODEL2stan.stan", data=data2,
             iter=100, chains=4)
print(fit2)
plot(fit2)

params = extract(fit2)
names(params)
traceplot(fit2)

##cALCULATE THE RESPECTIVE PREDICTED POPULATIONS OF OOCYSTS AND SPOROZOITES AND PREVALENCE IN MICE
sampmean<-mean(params$mu_s)
sampvar<-mean(params$mu_s)+((mean(params$mu_s)^2)/mean(params$sigma_s))
N=500
k<-(sampmean^2-(sampvar/N))/(sampvar-sampmean)

Ns<-rnegbin(N,sampmean,k)
sort(Ns)
length(Ns[Ns==0])
length(Ns[Ns>0 & Ns < 11])
length(Ns[Ns>10 & Ns < 101])
length(Ns[Ns==100 & Ns <1001])

sampmean2<-mean(params$logmu_ooc)
sampvar2<-mean(params$logmu_ooc)+((mean(params$logmu_ooc)^2)/exp(mean(params$logsigma_ooc)))
N=500
k2<-(sampmean2^2-(sampvar2/N))/(sampvar2-sampmean2)

Nooc<-rnegbin(N,sampmean2,k2)
sort(Nooc)
length(Nooc[Nooc==0])
length(Nooc[Nooc>0 & Nooc < 11])
length(Nooc[Nooc>10 & Nooc < 101])
length(Nooc[Nooc==100 & Nooc <1001])

##from Probability oocyst causing infection (1 - (1-s)^x) where x == N oocysts and s is theta (probability of infection given 1 oocyst)
x<-seq(1,70,1)
Nsporos <- ((1 - (1/exp(beta_mu[1] * logmu_ooc + beta_mu[2] * logsigma_ooc + alpha_mu))^(x-1)) * (1/exp(beta_mu[1] * logmu_ooc + beta_mu[2] * logsigma_ooc + alpha_mu))
plot(Nsporos ~ x)            


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



#################
#################
##Model 3
data3<-list(N_Comb=32,
            N_ooc=24,
            ooc_count = structure(.Data = c(oocystsC,oocystsT),
                                  .Dim=c(24,32)),
            N_bin=5,
            bin_edge=c(0,1,10,100,1000,10000),
            s_count = structure(.Data=rbind(spors_C,spors_T),.Dim=c(32,5)))

fit3 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\MODEL3stan.stan", data=data3,
             iter=100, chains=4)
print(fit3)
plot(fit3)

params = extract(fit2)
names(params)
traceplot(fit2)

##cALCULATE THE RESPECTIVE PREDICTED POPULATIONS OF OOCYSTS AND SPOROZOITES AND PREVALENCE IN MICE
sampmean<-mean(params$mu_s)
sampvar<-mean(params$mu_s)+((mean(params$mu_s)^2)/mean(params$sigma_s))
N=500
k<-(sampmean^2-(sampvar/N))/(sampvar-sampmean)

Ns<-rnegbin(N,sampmean,k)
sort(Ns)

sampler_params<-get_sampler_params(fit2, inc_warmup=FALSE)
sapply(sampler_params, function(x) c(x[, 'accept_stat__']))


