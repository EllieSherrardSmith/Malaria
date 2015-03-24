library(rstan)

oocysts<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\Bite5Oocysts.txt",header=TRUE)
head(oocysts)

spors<-read.csv("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\sporozoites.csv",header=TRUE)
spors$prevBS<-ifelse(spors$Parasitemia > 0 | spors$Gametocytemia > 0, 1, 0)
head(spors)
prevbites5temp<-subset(spors,Bites==5 & Treatment == 0)
prevbites5ATV<-subset(spors,Bites==5 & Treatment == 1)

sporsbites5<-stack(prevbites5temp[16:20,6:10]);sporsbites5$values
pred0<-length(sporsbites5$values[sporsbites5$values==0])
pred1<-length(sporsbites5$values[sporsbites5$values==1])
pred2<-length(sporsbites5$values[sporsbites5$values==2])
pred3<-length(sporsbites5$values[sporsbites5$values==3])
pred4<-length(sporsbites5$values[sporsbites5$values==4])
round4C<-c(pred0,pred1,pred2,pred3,pred4)##

sporos<-c(round1C[1],round2C[1],round3C[1],round4C[1],
          round1C[2],round2C[2],round3C[2],round4C[2],
          round1C[3],round2C[3],round3C[3],round4C[3],
          round1C[4],round2C[4],round3C[4],round4C[4]#,
          #round1C[5],round2C[5],round3C[5],round4C[5]
          )

sporsbites5<-stack(prevbites5ATV[16:20,6:10]);sporsbites5$values
pred0<-length(sporsbites5$values[sporsbites5$values==0])
pred1<-length(sporsbites5$values[sporsbites5$values==1])
pred2<-length(sporsbites5$values[sporsbites5$values==2])
pred3<-length(sporsbites5$values[sporsbites5$values==3])
pred4<-length(sporsbites5$values[sporsbites5$values==4])
round4T<-c(pred0,pred1,pred2,pred3,pred4)##

sporosT<-c(round1T[1],round2T[1],round3T[1],round4T[1],
           round1T[2],round2T[2],round3T[2],round4T[2],
           round1T[3],round2T[3],round3T[3],round4T[3],
           round1T[4],round2T[4],round3T[4],round4T[4]#,
           #round1T[5],round2T[5],round3T[5],round4T[5]
           )

data1<-list(N_C=4,
            N_T=4,
            N_ooc=45,
            N_mice=5,
            ooc_count_C = structure(.Data = c(oocysts$oocystsbites5control),
                          .Dim=c(45,4)),
            ooc_count_T = structure(.Data = c(oocysts$oocystsbites5atv),
                                    .Dim=c(45,4)),
            prev_C = structure(.Data =c(prevbites5temp$prevBS),.Dim=c(5,4)),
            prev_T = structure(.Data =c(prevbites5ATV$prevBS),.Dim=c(5,4)),
            N_bin=4,
            bin_edge=c(0,1,10,100,1000),
            s_count_C = structure(.Data=c(sporos),
                                  .Dim=c(4,4)),
            s_count_T = structure(.Data=c(sporosT),
                                  .Dim=c(4,4))
            )


fit1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\MODEL.stan", data=data1,
             iter=100, chains=4)
print(fit1)

