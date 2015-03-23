ooc<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\oocysts.txt",header=TRUE)
head(ooc)

spors<-read.csv("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\sporozoites.csv",header=TRUE)
spors[21:40,]
     
spors$prevBS<-ifelse(spors$Parasitemia > 0 | spors$Gametocytemia > 0, 1, 0)

prev<-c(rep(spors$prevBS[spors$Round==1 & spors$Bites==1 & spors$Treatment==0],1),
  rep(spors$prevBS[spors$Round==1 & spors$Bites==2 & spors$Treatment==0],2),
  rep(spors$prevBS[spors$Round==1 & spors$Bites==3 & spors$Treatment==0],3),
  rep(spors$prevBS[spors$Round==1 & spors$Bites==4 & spors$Treatment==0],4),
  rep(spors$prevBS[spors$Round==1 & spors$Bites==5 & spors$Treatment==0],5),
  
  rep(spors$prevBS[spors$Round==2 & spors$Bites==1 & spors$Treatment==0],1),
  rep(spors$prevBS[spors$Round==2 & spors$Bites==2 & spors$Treatment==0],2),
  rep(spors$prevBS[spors$Round==2 & spors$Bites==3 & spors$Treatment==0],3),
  rep(spors$prevBS[spors$Round==2 & spors$Bites==4 & spors$Treatment==0],4),
  rep(spors$prevBS[spors$Round==2 & spors$Bites==5 & spors$Treatment==0],5),
  
  rep(spors$prevBS[spors$Round==3 & spors$Bites==1 & spors$Treatment==0],1),
  rep(spors$prevBS[spors$Round==3 & spors$Bites==2 & spors$Treatment==0],2),
  rep(spors$prevBS[spors$Round==3 & spors$Bites==3 & spors$Treatment==0],3),
  rep(spors$prevBS[spors$Round==3 & spors$Bites==4 & spors$Treatment==0],4),
  rep(spors$prevBS[spors$Round==3 & spors$Bites==5 & spors$Treatment==0],5),
  
  rep(spors$prevBS[spors$Round==4 & spors$Bites==1 & spors$Treatment==0],1),
  rep(spors$prevBS[spors$Round==4 & spors$Bites==2 & spors$Treatment==0],2),
  rep(spors$prevBS[spors$Round==4 & spors$Bites==3 & spors$Treatment==0],3),
  rep(spors$prevBS[spors$Round==4 & spors$Bites==4 & spors$Treatment==0],4),
  rep(spors$prevBS[spors$Round==4 & spors$Bites==5 & spors$Treatment==0],5),
  
  rep(spors$prevBS[spors$Round==1 & spors$Bites==1 & spors$Treatment==1],1),
  rep(spors$prevBS[spors$Round==1 & spors$Bites==2 & spors$Treatment==1],2),
  rep(spors$prevBS[spors$Round==1 & spors$Bites==3 & spors$Treatment==1],3),
  rep(spors$prevBS[spors$Round==1 & spors$Bites==4 & spors$Treatment==1],4),
  rep(spors$prevBS[spors$Round==1 & spors$Bites==5 & spors$Treatment==1],5),
  
  rep(spors$prevBS[spors$Round==2 & spors$Bites==1 & spors$Treatment==1],1),
  rep(spors$prevBS[spors$Round==2 & spors$Bites==2 & spors$Treatment==1],2),
  rep(spors$prevBS[spors$Round==2 & spors$Bites==3 & spors$Treatment==1],3),
  rep(spors$prevBS[spors$Round==2 & spors$Bites==4 & spors$Treatment==1],4),
  rep(spors$prevBS[spors$Round==2 & spors$Bites==5 & spors$Treatment==1],5),
  
  rep(spors$prevBS[spors$Round==3 & spors$Bites==1 & spors$Treatment==1],1),
  rep(spors$prevBS[spors$Round==3 & spors$Bites==2 & spors$Treatment==1],2),
  rep(spors$prevBS[spors$Round==3 & spors$Bites==3 & spors$Treatment==1],3),
  rep(spors$prevBS[spors$Round==3 & spors$Bites==4 & spors$Treatment==1],4),
  rep(spors$prevBS[spors$Round==3 & spors$Bites==5 & spors$Treatment==1],5),
  
  rep(spors$prevBS[spors$Round==4 & spors$Bites==1 & spors$Treatment==1],1),
  rep(spors$prevBS[spors$Round==4 & spors$Bites==2 & spors$Treatment==1],2),
  rep(spors$prevBS[spors$Round==4 & spors$Bites==3 & spors$Treatment==1],3),
  rep(spors$prevBS[spors$Round==4 & spors$Bites==4 & spors$Treatment==1],4),
  rep(spors$prevBS[spors$Round==4 & spors$Bites==5 & spors$Treatment==1],5))
  
VEC<-c(1,2,3,4,5)

bitesC1<-bitesC2<-bitesC3<-bitesC4<-bitesT1<-bitesT2<-bitesT3<-bitesT4<-numeric(5)
for (i in 1:5){
      
  bitesC1[i]<-sum(spors$prevBS[spors$Round==1 & spors$Bites==VEC[i] & spors$Treatment==0])/length(spors$prevBS[spors$Round==1 & spors$Bites==VEC[i] & spors$Treatment==0])
  bitesC2[i]<-sum(spors$prevBS[spors$Round==2 & spors$Bites==VEC[i] & spors$Treatment==0])/length(spors$prevBS[spors$Round==2 & spors$Bites==VEC[i] & spors$Treatment==0])
  bitesC3[i]<-sum(spors$prevBS[spors$Round==3 & spors$Bites==VEC[i] & spors$Treatment==0])/length(spors$prevBS[spors$Round==3 & spors$Bites==VEC[i] & spors$Treatment==0])
  bitesC4[i]<-sum(spors$prevBS[spors$Round==4 & spors$Bites==VEC[i] & spors$Treatment==0])/length(spors$prevBS[spors$Round==4 & spors$Bites==VEC[i] & spors$Treatment==0])

  bitesT1[i]<-sum(spors$prevBS[spors$Round==1 & spors$Bites==VEC[i] & spors$Treatment==1])/length(spors$prevBS[spors$Round==1 & spors$Bites==VEC[i] & spors$Treatment==1])
  bitesT2[i]<-sum(spors$prevBS[spors$Round==2 & spors$Bites==VEC[i] & spors$Treatment==1])/length(spors$prevBS[spors$Round==2 & spors$Bites==VEC[i] & spors$Treatment==1])
  bitesT3[i]<-sum(spors$prevBS[spors$Round==3 & spors$Bites==VEC[i] & spors$Treatment==1])/length(spors$prevBS[spors$Round==3 & spors$Bites==VEC[i] & spors$Treatment==1])
  bitesT4[i]<-sum(spors$prevBS[spors$Round==4 & spors$Bites==VEC[i] & spors$Treatment==1])/length(spors$prevBS[spors$Round==4 & spors$Bites==VEC[i] & spors$Treatment==1])
  

}

roundbiteprev<-c(rep(bitesC1[1],5),rep(bitesC1[2],10),rep(bitesC1[3],15),rep(bitesC1[4],20),rep(bitesC1[5],25),
        rep(bitesC2[1],5),rep(bitesC2[2],10),rep(bitesC2[3],15),rep(bitesC2[4],20),rep(bitesC2[5],25),
        rep(bitesC3[1],5),rep(bitesC3[2],10),rep(bitesC3[3],15),rep(bitesC3[4],20),rep(bitesC3[5],25),
        rep(bitesC4[1],5),rep(bitesC4[2],10),rep(bitesC4[3],15),rep(bitesC4[4],20),rep(bitesC4[5],25),
        rep(bitesT1[1],5),rep(bitesT1[2],10),rep(bitesT1[3],15),rep(bitesT1[4],20),rep(bitesT1[5],25),
        rep(bitesT2[1],5),rep(bitesT2[2],10),rep(bitesT2[3],15),rep(bitesT2[4],20),rep(bitesT2[5],25),
        rep(bitesT3[1],5),rep(bitesT3[2],10),rep(bitesT3[3],15),rep(bitesT3[4],20),rep(bitesT3[5],25),
        rep(bitesT4[1],5),rep(bitesT4[2],10),rep(bitesT4[3],15),rep(bitesT4[4],20),rep(bitesT4[5],25))
length(roundbiteprev)

spors1.1a<-c(spors$Sporozoite1[spors$Round==1 & spors$Bites==1 & spors$Treatment==0])
spors1.2a<-c(spors$Sporozoite1[spors$Round==1 & spors$Bites==2 & spors$Treatment==0],spors$Sporozoite2[spors$Round==1 & spors$Bites==2 & spors$Treatment==0])
spors1.3a<-c(spors$Sporozoite1[spors$Round==1 & spors$Bites==3 & spors$Treatment==0],spors$Sporozoite2[spors$Round==1 & spors$Bites==3 & spors$Treatment==0],
            spors$Sporozoite3[spors$Round==1 & spors$Bites==3 & spors$Treatment==0])
spors1.4a<-c(spors$Sporozoite1[spors$Round==1 & spors$Bites==4 & spors$Treatment==0],spors$Sporozoite2[spors$Round==1 & spors$Bites==4 & spors$Treatment==0],
            spors$Sporozoite3[spors$Round==1 & spors$Bites==4 & spors$Treatment==0],spors$Sporozoite4[spors$Round==1 & spors$Bites==4 & spors$Treatment==0])
spors1.5a<-c(spors$Sporozoite1[spors$Round==1 & spors$Bites==5 & spors$Treatment==0],spors$Sporozoite2[spors$Round==1 & spors$Bites==5 & spors$Treatment==0],
            spors$Sporozoite3[spors$Round==1 & spors$Bites==5 & spors$Treatment==0],spors$Sporozoite4[spors$Round==1 & spors$Bites==5 & spors$Treatment==0],
            spors$Sporozoite4[spors$Round==1 & spors$Bites==5 & spors$Treatment==0])
spors2.1a<-c(spors$Sporozoite1[spors$Round==2 & spors$Bites==1 & spors$Treatment==0])
spors2.2a<-c(spors$Sporozoite1[spors$Round==2 & spors$Bites==2 & spors$Treatment==0],spors$Sporozoite2[spors$Round==2 & spors$Bites==2 & spors$Treatment==0])
spors2.3a<-c(spors$Sporozoite1[spors$Round==2 & spors$Bites==3 & spors$Treatment==0],spors$Sporozoite2[spors$Round==2 & spors$Bites==3 & spors$Treatment==0],
            spors$Sporozoite3[spors$Round==2 & spors$Bites==3 & spors$Treatment==0])
spors2.4a<-c(spors$Sporozoite1[spors$Round==2 & spors$Bites==4 & spors$Treatment==0],spors$Sporozoite2[spors$Round==2 & spors$Bites==4 & spors$Treatment==0],
            spors$Sporozoite3[spors$Round==2 & spors$Bites==4 & spors$Treatment==0],spors$Sporozoite4[spors$Round==2 & spors$Bites==4 & spors$Treatment==0])
spors2.5a<-c(spors$Sporozoite1[spors$Round==2 & spors$Bites==5 & spors$Treatment==0],spors$Sporozoite2[spors$Round==2 & spors$Bites==5 & spors$Treatment==0],
            spors$Sporozoite3[spors$Round==2 & spors$Bites==5 & spors$Treatment==0],spors$Sporozoite4[spors$Round==2 & spors$Bites==5 & spors$Treatment==0],
            spors$Sporozoite4[spors$Round==2 & spors$Bites==5 & spors$Treatment==0])
spors3.1a<-c(spors$Sporozoite1[spors$Round==3 & spors$Bites==1 & spors$Treatment==0])
spors3.2a<-c(spors$Sporozoite1[spors$Round==3 & spors$Bites==2 & spors$Treatment==0],spors$Sporozoite2[spors$Round==3 & spors$Bites==2 & spors$Treatment==0])
spors3.3a<-c(spors$Sporozoite1[spors$Round==3 & spors$Bites==3 & spors$Treatment==0],spors$Sporozoite2[spors$Round==3 & spors$Bites==3 & spors$Treatment==0],
            spors$Sporozoite3[spors$Round==3 & spors$Bites==3 & spors$Treatment==0])
spors3.4a<-c(spors$Sporozoite1[spors$Round==3 & spors$Bites==4 & spors$Treatment==0],spors$Sporozoite2[spors$Round==3 & spors$Bites==4 & spors$Treatment==0],
            spors$Sporozoite3[spors$Round==3 & spors$Bites==4 & spors$Treatment==0],spors$Sporozoite4[spors$Round==3 & spors$Bites==4 & spors$Treatment==0])
spors3.5a<-c(spors$Sporozoite1[spors$Round==3 & spors$Bites==5 & spors$Treatment==0],spors$Sporozoite2[spors$Round==3 & spors$Bites==5 & spors$Treatment==0],
            spors$Sporozoite3[spors$Round==3 & spors$Bites==5 & spors$Treatment==0],spors$Sporozoite4[spors$Round==3 & spors$Bites==5 & spors$Treatment==0],
            spors$Sporozoite4[spors$Round==3 & spors$Bites==5 & spors$Treatment==0])
spors4.1a<-c(spors$Sporozoite1[spors$Round==4 & spors$Bites==1 & spors$Treatment==0])
spors4.2a<-c(spors$Sporozoite1[spors$Round==4 & spors$Bites==2 & spors$Treatment==0],spors$Sporozoite2[spors$Round==4 & spors$Bites==2 & spors$Treatment==0])
spors4.3a<-c(spors$Sporozoite1[spors$Round==4 & spors$Bites==3 & spors$Treatment==0],spors$Sporozoite2[spors$Round==4 & spors$Bites==3 & spors$Treatment==0],
            spors$Sporozoite3[spors$Round==4 & spors$Bites==3 & spors$Treatment==0])
spors4.4a<-c(spors$Sporozoite1[spors$Round==4 & spors$Bites==4 & spors$Treatment==0],spors$Sporozoite2[spors$Round==4 & spors$Bites==4 & spors$Treatment==0],
            spors$Sporozoite3[spors$Round==4 & spors$Bites==4 & spors$Treatment==0],spors$Sporozoite4[spors$Round==4 & spors$Bites==4 & spors$Treatment==0])
spors4.5a<-c(spors$Sporozoite1[spors$Round==4 & spors$Bites==5 & spors$Treatment==0],spors$Sporozoite2[spors$Round==4 & spors$Bites==5 & spors$Treatment==0],
            spors$Sporozoite3[spors$Round==4 & spors$Bites==5 & spors$Treatment==0],spors$Sporozoite4[spors$Round==4 & spors$Bites==5 & spors$Treatment==0],
            spors$Sporozoite4[spors$Round==4 & spors$Bites==5 & spors$Treatment==0])

spors1.1<-c(spors$Sporozoite1[spors$Round==1 & spors$Bites==1 & spors$Treatment==1])
spors1.2<-c(spors$Sporozoite1[spors$Round==1 & spors$Bites==2 & spors$Treatment==1],spors$Sporozoite2[spors$Round==1 & spors$Bites==2 & spors$Treatment==1])
spors1.3<-c(spors$Sporozoite1[spors$Round==1 & spors$Bites==3 & spors$Treatment==1],spors$Sporozoite2[spors$Round==1 & spors$Bites==3 & spors$Treatment==1],
            spors$Sporozoite3[spors$Round==1 & spors$Bites==3 & spors$Treatment==1])
spors1.4<-c(spors$Sporozoite1[spors$Round==1 & spors$Bites==4 & spors$Treatment==1],spors$Sporozoite2[spors$Round==1 & spors$Bites==4 & spors$Treatment==1],
            spors$Sporozoite3[spors$Round==1 & spors$Bites==4 & spors$Treatment==1],spors$Sporozoite4[spors$Round==1 & spors$Bites==4 & spors$Treatment==1])
spors1.5<-c(spors$Sporozoite1[spors$Round==1 & spors$Bites==5 & spors$Treatment==1],spors$Sporozoite2[spors$Round==1 & spors$Bites==5 & spors$Treatment==1],
            spors$Sporozoite3[spors$Round==1 & spors$Bites==5 & spors$Treatment==1],spors$Sporozoite4[spors$Round==1 & spors$Bites==5 & spors$Treatment==1],
            spors$Sporozoite4[spors$Round==1 & spors$Bites==5 & spors$Treatment==1])
spors2.1<-c(spors$Sporozoite1[spors$Round==2 & spors$Bites==1 & spors$Treatment==1])
spors2.2<-c(spors$Sporozoite1[spors$Round==2 & spors$Bites==2 & spors$Treatment==1],spors$Sporozoite2[spors$Round==2 & spors$Bites==2 & spors$Treatment==1])
spors2.3<-c(spors$Sporozoite1[spors$Round==2 & spors$Bites==3 & spors$Treatment==1],spors$Sporozoite2[spors$Round==2 & spors$Bites==3 & spors$Treatment==1],
            spors$Sporozoite3[spors$Round==2 & spors$Bites==3 & spors$Treatment==1])
spors2.4<-c(spors$Sporozoite1[spors$Round==2 & spors$Bites==4 & spors$Treatment==1],spors$Sporozoite2[spors$Round==2 & spors$Bites==4 & spors$Treatment==1],
            spors$Sporozoite3[spors$Round==2 & spors$Bites==4 & spors$Treatment==1],spors$Sporozoite4[spors$Round==2 & spors$Bites==4 & spors$Treatment==1])
spors2.5<-c(spors$Sporozoite1[spors$Round==2 & spors$Bites==5 & spors$Treatment==1],spors$Sporozoite2[spors$Round==2 & spors$Bites==5 & spors$Treatment==1],
            spors$Sporozoite3[spors$Round==2 & spors$Bites==5 & spors$Treatment==1],spors$Sporozoite4[spors$Round==2 & spors$Bites==5 & spors$Treatment==1],
            spors$Sporozoite4[spors$Round==2 & spors$Bites==5 & spors$Treatment==1])
spors3.1<-c(spors$Sporozoite1[spors$Round==3 & spors$Bites==1 & spors$Treatment==1])
spors3.2<-c(spors$Sporozoite1[spors$Round==3 & spors$Bites==2 & spors$Treatment==1],spors$Sporozoite2[spors$Round==3 & spors$Bites==2 & spors$Treatment==1])
spors3.3<-c(spors$Sporozoite1[spors$Round==3 & spors$Bites==3 & spors$Treatment==1],spors$Sporozoite2[spors$Round==3 & spors$Bites==3 & spors$Treatment==1],
            spors$Sporozoite3[spors$Round==3 & spors$Bites==3 & spors$Treatment==1])
spors3.4<-c(spors$Sporozoite1[spors$Round==3 & spors$Bites==4 & spors$Treatment==1],spors$Sporozoite2[spors$Round==3 & spors$Bites==4 & spors$Treatment==1],
            spors$Sporozoite3[spors$Round==3 & spors$Bites==4 & spors$Treatment==1],spors$Sporozoite4[spors$Round==3 & spors$Bites==4 & spors$Treatment==1])
spors3.5<-c(spors$Sporozoite1[spors$Round==3 & spors$Bites==5 & spors$Treatment==1],spors$Sporozoite2[spors$Round==3 & spors$Bites==5 & spors$Treatment==1],
            spors$Sporozoite3[spors$Round==3 & spors$Bites==5 & spors$Treatment==1],spors$Sporozoite4[spors$Round==3 & spors$Bites==5 & spors$Treatment==1],
            spors$Sporozoite4[spors$Round==3 & spors$Bites==5 & spors$Treatment==1])
spors4.1<-c(spors$Sporozoite1[spors$Round==4 & spors$Bites==1 & spors$Treatment==1])
spors4.2<-c(spors$Sporozoite1[spors$Round==4 & spors$Bites==2 & spors$Treatment==1],spors$Sporozoite2[spors$Round==4 & spors$Bites==2 & spors$Treatment==1])
spors4.3<-c(spors$Sporozoite1[spors$Round==4 & spors$Bites==3 & spors$Treatment==1],spors$Sporozoite2[spors$Round==4 & spors$Bites==3 & spors$Treatment==1],
            spors$Sporozoite3[spors$Round==4 & spors$Bites==3 & spors$Treatment==1])
spors4.4<-c(spors$Sporozoite1[spors$Round==4 & spors$Bites==4 & spors$Treatment==1],spors$Sporozoite2[spors$Round==4 & spors$Bites==4 & spors$Treatment==1],
            spors$Sporozoite3[spors$Round==4 & spors$Bites==4 & spors$Treatment==1],spors$Sporozoite4[spors$Round==4 & spors$Bites==4 & spors$Treatment==1])
spors4.5<-c(spors$Sporozoite1[spors$Round==4 & spors$Bites==5 & spors$Treatment==1],spors$Sporozoite2[spors$Round==4 & spors$Bites==5 & spors$Treatment==1],
            spors$Sporozoite3[spors$Round==4 & spors$Bites==5 & spors$Treatment==1],spors$Sporozoite4[spors$Round==4 & spors$Bites==5 & spors$Treatment==1],
            spors$Sporozoite4[spors$Round==4 & spors$Bites==5 & spors$Treatment==1])

length(spors1.1);length(spors1.2);length(spors1.3);length(spors1.4);length(spors1.5)
length(spors2.1);length(spors2.2);length(spors2.3);length(spors2.4);length(spors2.5)
length(spors3.1);length(spors3.2);length(spors3.3);length(spors3.4);length(spors3.5)
length(spors4.1);length(spors4.2);length(spors4.3);length(spors4.4);length(spors4.5)


sporozoites<-c(spors1.1a,spors1.2a,spors1.3a,spors1.4a,spors1.5a,spors2.1a,spors2.2a,spors2.3a,spors2.4a,spors2.5a,
               spors3.1a,spors3.2a,spors3.3a,spors3.4a,spors3.5a,spors4.1a,spors4.2a,spors4.3a,spors4.4a,spors4.5a,
               spors1.1,spors1.2,spors1.3,spors1.4,spors1.5,spors2.1,spors2.2,spors2.3,spors2.4,spors2.5,
               spors3.1,spors3.2,spors3.3,spors3.4,spors3.5,spors4.1,spors4.2,spors4.3,spors4.4,spors4.5)

oocysts<-c(rep(ooc$meanooc[ooc$bites==1 & ooc$round=="DAY41" & ooc$treatment=="CONTROL"],length(spors1.1a)),
           rep(ooc$meanooc[ooc$bites==2 & ooc$round=="DAY41" & ooc$treatment=="CONTROL"],length(spors1.2a)),
           rep(ooc$meanooc[ooc$bites==3 & ooc$round=="DAY41" & ooc$treatment=="CONTROL"],length(spors1.3a)),
           rep(ooc$meanooc[ooc$bites==4 & ooc$round=="DAY41" & ooc$treatment=="CONTROL"],length(spors1.4a)),
           rep(ooc$meanooc[ooc$bites==5 & ooc$round=="DAY41" & ooc$treatment=="CONTROL"],length(spors1.5a)),
           
           rep(ooc$meanooc[ooc$bites==1 & ooc$round=="DAY72" & ooc$treatment=="CONTROL"],length(spors2.1a)),
           rep(ooc$meanooc[ooc$bites==2 & ooc$round=="DAY72" & ooc$treatment=="CONTROL"],length(spors2.2a)),
           rep(ooc$meanooc[ooc$bites==3 & ooc$round=="DAY72" & ooc$treatment=="CONTROL"],length(spors2.3a)),
           rep(ooc$meanooc[ooc$bites==4 & ooc$round=="DAY72" & ooc$treatment=="CONTROL"],length(spors2.4a)),
           rep(ooc$meanooc[ooc$bites==5 & ooc$round=="DAY72" & ooc$treatment=="CONTROL"],length(spors2.5a)),
           
           rep(ooc$meanooc[ooc$bites==1 & ooc$round=="DAY103" & ooc$treatment=="CONTROL"],length(spors3.1a)),
           rep(ooc$meanooc[ooc$bites==2 & ooc$round=="DAY103" & ooc$treatment=="CONTROL"],length(spors3.2a)),
           rep(ooc$meanooc[ooc$bites==3 & ooc$round=="DAY103" & ooc$treatment=="CONTROL"],length(spors3.3a)),
           rep(ooc$meanooc[ooc$bites==4 & ooc$round=="DAY103" & ooc$treatment=="CONTROL"],length(spors3.4a)),
           rep(ooc$meanooc[ooc$bites==5 & ooc$round=="DAY103" & ooc$treatment=="CONTROL"],length(spors3.5a)),
           
           rep(ooc$meanooc[ooc$bites==1 & ooc$round=="DAY134" & ooc$treatment=="CONTROL"],length(spors2.1a)),
           rep(ooc$meanooc[ooc$bites==2 & ooc$round=="DAY134" & ooc$treatment=="CONTROL"],length(spors2.2a)),
           rep(ooc$meanooc[ooc$bites==3 & ooc$round=="DAY134" & ooc$treatment=="CONTROL"],length(spors2.3a)),
           rep(ooc$meanooc[ooc$bites==4 & ooc$round=="DAY134" & ooc$treatment=="CONTROL"],length(spors2.4a)),
           rep(ooc$meanooc[ooc$bites==5 & ooc$round=="DAY134" & ooc$treatment=="CONTROL"],length(spors2.5a)),
           
           rep(ooc$meanooc[ooc$bites==1 & ooc$round=="DAY41" & ooc$treatment=="ATV"],length(spors1.1)),
           rep(ooc$meanooc[ooc$bites==2 & ooc$round=="DAY41" & ooc$treatment=="ATV"],length(spors1.2)),
           rep(ooc$meanooc[ooc$bites==3 & ooc$round=="DAY41" & ooc$treatment=="ATV"],length(spors1.3)),
           rep(ooc$meanooc[ooc$bites==4 & ooc$round=="DAY41" & ooc$treatment=="ATV"],length(spors1.4)),
           rep(ooc$meanooc[ooc$bites==5 & ooc$round=="DAY41" & ooc$treatment=="ATV"],length(spors1.5)),
           
           rep(ooc$meanooc[ooc$bites==1 & ooc$round=="DAY72" & ooc$treatment=="ATV"],length(spors2.1)),
           rep(ooc$meanooc[ooc$bites==2 & ooc$round=="DAY72" & ooc$treatment=="ATV"],length(spors2.2)),
           rep(ooc$meanooc[ooc$bites==3 & ooc$round=="DAY72" & ooc$treatment=="ATV"],length(spors2.3)),
           rep(ooc$meanooc[ooc$bites==4 & ooc$round=="DAY72" & ooc$treatment=="ATV"],length(spors2.4)),
           rep(ooc$meanooc[ooc$bites==5 & ooc$round=="DAY72" & ooc$treatment=="ATV"],length(spors2.5)),
           
           rep(ooc$meanooc[ooc$bites==1 & ooc$round=="DAY103" & ooc$treatment=="ATV"],length(spors3.1)),
           rep(ooc$meanooc[ooc$bites==2 & ooc$round=="DAY103" & ooc$treatment=="ATV"],length(spors3.2)),
           rep(ooc$meanooc[ooc$bites==3 & ooc$round=="DAY103" & ooc$treatment=="ATV"],length(spors3.3)),
           rep(ooc$meanooc[ooc$bites==4 & ooc$round=="DAY103" & ooc$treatment=="ATV"],length(spors3.4)),
           rep(ooc$meanooc[ooc$bites==5 & ooc$round=="DAY103" & ooc$treatment=="ATV"],length(spors3.5)),
           
           rep(ooc$meanooc[ooc$bites==1 & ooc$round=="DAY134" & ooc$treatment=="ATV"],length(spors2.1)),
           rep(ooc$meanooc[ooc$bites==2 & ooc$round=="DAY134" & ooc$treatment=="ATV"],length(spors2.2)),
           rep(ooc$meanooc[ooc$bites==3 & ooc$round=="DAY134" & ooc$treatment=="ATV"],length(spors2.3)),
           rep(ooc$meanooc[ooc$bites==4 & ooc$round=="DAY134" & ooc$treatment=="ATV"],length(spors2.4)),
           rep(ooc$meanooc[ooc$bites==5 & ooc$round=="DAY134" & ooc$treatment=="ATV"],length(spors2.5)))
     
roundbite<-c(rep("11c",5),rep("12c",10),rep("13c",15),rep("14c",20),rep("15c",25),rep("21c",5),rep("22c",10),rep("23c",15),rep("24c",20),rep("25c",25),
             rep("31c",5),rep("32c",10),rep("33c",15),rep("34c",20),rep("35c",25),rep("41c",5),rep("42c",10),rep("43c",15),rep("44c",20),rep("45c",25),
             rep("11t",5),rep("12t",10),rep("13t",15),rep("14t",20),rep("15t",25),rep("21t",5),rep("22t",10),rep("23t",15),rep("24t",20),rep("25t",25),
             rep("31t",5),rep("32t",10),rep("33t",15),rep("34t",20),rep("35t",25),rep("41t",5),rep("42t",10),rep("43t",15),rep("44t",20),rep("45t",25))

meanspor<-numeric(length(unique(roundbite)))
for (i in 1:length(unique(roundbite))){
  meanspor[i]<-mean(data$sporozoites[data$roundbite==unique(roundbite)[i]])
}

meanspors<-c(rep(meanspor[1],5),rep(meanspor[2],10),rep(meanspor[3],15),rep(meanspor[4],20),rep(meanspor[5],25),
             rep(meanspor[6],5),rep(meanspor[7],10),rep(meanspor[8],15),rep(meanspor[9],20),rep(meanspor[10],25),
             rep(meanspor[11],5),rep(meanspor[12],10),rep(meanspor[13],15),rep(meanspor[14],20),rep(meanspor[15],25),
             rep(meanspor[16],5),rep(meanspor[17],10),rep(meanspor[18],15),rep(meanspor[19],20),rep(meanspor[20],25),
             rep(meanspor[21],5),rep(meanspor[22],10),rep(meanspor[23],15),rep(meanspor[24],20),rep(meanspor[25],25),
             rep(meanspor[26],5),rep(meanspor[27],10),rep(meanspor[28],15),rep(meanspor[29],20),rep(meanspor[30],25),
             rep(meanspor[31],5),rep(meanspor[32],10),rep(meanspor[33],15),rep(meanspor[34],20),rep(meanspor[35],25),
             rep(meanspor[36],5),rep(meanspor[37],10),rep(meanspor[38],15),rep(meanspor[39],20),rep(meanspor[40],25))

data<-data.frame(sporozoites,oocysts,roundbite,prev,roundbiteprev,meanspors)
dim(data);dim(ooc)
head(data)

###############################################################
##
###
####   Mean data Blagborough 2013 Nature Communications
###
##
##############################################################
plot(data$meanspors~data$oocysts,ylim=c(0,4),xlim=c(0,70))

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * data$oocysts)) / (1 + exp(a + b * data$oocysts)) ) * 1.8
  prev1<-data$meanspors 
  loglik1a<- prev1* log((pred1a/ 1.8)+0.0000001)+(1-prev1)*log(1-((pred1a/ 1.8)-0.0000001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2

nc<-seq(0,80,1)
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmod
pred<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) )    *  1.8
lines(pred~nc)

gom.binom<-function(p.vec){
  a<-p.vec[1]
  b<-p.vec[2]
  c<-p.vec[3]
  
  pred1<- (a * exp (b * exp(c *  data$oocysts))) * 1.8
  data1<- data$meanspor
  loglik1<- data1* log((pred1/1.8)+0.00001)+(1-data1)*log(1-((pred1/1.8)-0.00001))
  -sum(loglik1,na.rm=T)
}
n.param<-3
gommod<-optim(c(0.75,-2,-0.5),gom.binom,method="L-BFGS-B",lower=c(0.5,-10,-10),upper=c(0.99,-1,-0.0001))
gommod

nc<-seq(0,80,1)

pred2<-   gommod$par[1] * exp (gommod$par[2] * exp(gommod$par[3] * nc)) * 1.8
lines(nc,pred2,col="yellow",lwd=2)


sat.binom<-function(p.vec){
  
  #a<-p.vec[1]
  b<-p.vec[1]
  d<-p.vec[2]
  ch<-p.vec[3]
  dg<-p.vec[4]
  
  pred<- (b * data$oocysts^ch)/(dg + d * data$oocysts^ch)
  
  data1<-data$meanspors
  
  loglik<- data1* log((pred)+0.001)+(1-data1)*log(1-((pred)-0.001))

  
  -sum(loglik,na.rm=T)
}
n.param<-3
satmod<-optim(c(0.9,0.6,1,4),sat.binom,method="L-BFGS-B",lower=c(0,0.001,0.01,0.1),upper=c(1,1,10,10))
satmod

pred3<-(satmod$par[1] * nc^satmod$par[3])/(satmod$par[4] + satmod$par[2] * nc^satmod$par[3]) 
lines(nc,pred3,lwd=2,lty=2,col="red")



lm(data$meanspors~data$oocysts)

my0<-71
my1<-10
my2<-10
my3<-5
my4<-4

e.g.
## myS my.ooc  round bites
## 0      15       1     1
## 0      15       1     1
## 4      15       1     1
## 2      7        1     2
## 3      7        1     2  
## 4      7        1     5


my.function<-function(my.sporo,my.ooc)
{
my.k=0.1
my.pred<-rnbinom(600,size=my.k,mu=1.15)##can try diff distributions
pred0<-length(my.pred[my.pred==0])
pred1<-length(my.pred[my.pred<11])-pred0
pred2<-length(my.pred[my.pred<101])-pred0-pred1
pred3<-length(my.pred[my.pred<1001])-pred0-pred2
pred4<-length(my.pred[my.pred>1000])
#my.mean<-(b * data$oocysts^ch)/(dg + d * data$oocysts^ch) ##can try diff functions

#my.k<=k1+k2*my.ooc                                        ##to vary k with mean
}
my.funct<-optim(c(10,1),my.function,method="L-BFGS-B",lower=c(1,0),upper=c(100,4))
my.funct

###solving the differential eqn gives the curve
