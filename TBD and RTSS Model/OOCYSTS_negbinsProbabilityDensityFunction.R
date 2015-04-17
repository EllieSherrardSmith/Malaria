##
##Estimating the negative binomial for different populations of oocysts

y1<-c(oocysts$oocystsbites2control[oocysts$round=="day41"])##N = 32
y2<-c(oocysts$oocystsbites2control[oocysts$round=="day72"])##45
y3<-c(oocysts$oocystsbites2control[oocysts$round=="day103"])##45
y4<-c(oocysts$oocystsbites2control[oocysts$round=="day134"])##45

freqooc<-numeric(length(unique(y1)))
for (i in 1:length(unique(y1))){ 
  freqooc[i]<-sum(ifelse(y1==unique(y1)[i],1,0),na.rm=TRUE)}##12/32
freqooc
freqooc<-numeric(length(unique(y2)))
for (i in 1:length(unique(y2))){ 
  freqooc[i]<-sum(ifelse(y2==unique(y2)[i],1,0),na.rm=TRUE)}##12/32
freqooc
freqooc<-numeric(length(unique(y3)))
for (i in 1:length(unique(y3))){ 
  freqooc[i]<-sum(ifelse(y3==unique(y3)[i],1,0),na.rm=TRUE)}##12/32
freqooc
freqooc<-numeric(length(unique(y4)))
for (i in 1:length(unique(y4))){ 
  freqooc[i]<-sum(ifelse(y4==unique(y4)[i],1,0),na.rm=TRUE)}##12/32
freqooc


y1<-c(oocysts$oocystsbites3control[oocysts$round=="day41"])##32
y2<-c(oocysts$oocystsbites3control[oocysts$round=="day72"])##36
y3<-c(oocysts$oocystsbites3control[oocysts$round=="day103"])##45
y4<-c(oocysts$oocystsbites3control[oocysts$round=="day134"])##45

y1<-c(oocysts$oocystsbites4control[oocysts$round=="day41"])##24
y2<-c(oocysts$oocystsbites4control[oocysts$round=="day72"])##39
y3<-c(oocysts$oocystsbites4control[oocysts$round=="day103"])##45
y4<-c(oocysts$oocystsbites4control[oocysts$round=="day134"])##45

y1<-c(oocysts$oocystsbites5control[oocysts$round=="day41"])##45
y2<-c(oocysts$oocystsbites5control[oocysts$round=="day72"])##45
y3<-c(oocysts$oocystsbites5control[oocysts$round=="day103"])##45
y4<-c(oocysts$oocystsbites5control[oocysts$round=="day134"])##45

mu1<-mean(y1,na.rm=TRUE)
mu2<-mean(y2,na.rm=TRUE)
mu3<-mean(y3,na.rm=TRUE)
mu4<-mean(y4,na.rm=TRUE)


alpha<-8 ##allow alpha to vary for different groups
amu1<-mu1*alpha
amu2<-mu2*alpha
amu3<-mu3*alpha
amu4<-mu4*alpha

ynbr1<- exp(y1*log(amu1/(1+amu1))-
          (1/alpha)*log(1+amu1)+
            log(gamma(y1+1/alpha))-
            log(gamma(y1+1))-
            log(gamma(1/alpha))) ###PAGE 33 and for alpha pg 207 IN: J Hilbe 2011 Negative binomial regression: understanding and modelling overdispersed count data 2nd Ed

ynbr2<- exp(y2*log(amu2/(1+amu2))-
              (1/alpha)*log(1+amu2)+
              log(gamma(y2+1/alpha))-
              log(gamma(y2+1))-
              log(gamma(1/alpha)))

ynbr3<- exp(y3*log(amu3/(1+amu3))-
              (1/alpha)*log(1+amu3)+
              log(gamma(y3+1/alpha))-
              log(gamma(y3+1))-
              log(gamma(1/alpha)))

ynbr4<- exp(y4*log(amu4/(1+amu4))-
              (1/alpha)*log(1+amu4)+
              log(gamma(y4+1/alpha))-
              log(gamma(y4+1))-
              log(gamma(1/alpha)))

b2r1<-data.frame(y1,ynbr1);b2r1dat <- unique(b2r1[order(y1),]);b2r1dat <- na.omit(b2r1dat)
b2r2<-data.frame(y2,ynbr2);b2r2dat <- unique(b2r2[order(y2),]);b2r2dat <- na.omit(b2r2dat)
b2r3<-data.frame(y3,ynbr3);b2r3dat <- unique(b2r3[order(y3),]);b2r3dat <- na.omit(b2r3dat)
b2r4<-data.frame(y4,ynbr4);b2r4dat <- unique(b2r4[order(y4),]);b2r4dat <- na.omit(b2r4dat)
TwoBitesTEMP1<-merge(b2r1dat,b2r2dat,by.x="y1",by.y="y2",all = TRUE)
TwoBitesTEMP2<-merge(TwoBitesTEMP1,b2r3dat,by.x="y1",by.y="y3",all = TRUE)
TwoBites<-merge(TwoBitesTEMP2,b2r4dat,by.x="y1",by.y="y4",all = TRUE) ###use alpha = 8

plot(TwoBites$y1,TwoBites$ynbr1,col="black",pch=5,ylim=c(0,0.2),xlim=c(0,200))
points(TwoBites$y1,TwoBites$ynbr2,col="black",pch=5)
points(TwoBites$y1,TwoBites$ynbr3,col="black",pch=5)
points(TwoBites$y1,TwoBites$ynbr4,col="black",pch=5)

b3r1<-data.frame(y1,ynbr1);b3r1dat <- unique(b3r1[order(y1),]);b3r1dat <- na.omit(b3r1dat)
b3r2<-data.frame(y2,ynbr2);b3r2dat <- unique(b3r2[order(y2),]);b3r2dat <- na.omit(b3r2dat)
b3r3<-data.frame(y3,ynbr3);b3r3dat <- unique(b3r3[order(y3),]);b3r3dat <- na.omit(b3r3dat)
b3r4<-data.frame(y4,ynbr4);b3r4dat <- unique(b3r4[order(y4),]);b3r4dat <- na.omit(b3r4dat)
ThreeBitesTEMP1<-merge(b3r1dat,b3r2dat,by.x="y1",by.y="y2",all = TRUE)
ThreeBitesTEMP2<-merge(ThreeBitesTEMP1,b3r3dat,by.x="y1",by.y="y3",all = TRUE)
ThreeBites<-merge(ThreeBitesTEMP2,b3r4dat,by.x="y1",by.y="y4",all = TRUE)

b4r1<-data.frame(y1,ynbr1);b4r1dat <- unique(b4r1[order(y1),]);b4r1dat <- na.omit(b4r1dat)
b4r2<-data.frame(y2,ynbr2);b4r2dat <- unique(b4r2[order(y2),]);b4r2dat <- na.omit(b4r2dat)
b4r3<-data.frame(y3,ynbr3);b4r3dat <- unique(b4r3[order(y3),]);b4r3dat <- na.omit(b4r3dat)
b4r4<-data.frame(y4,ynbr4);b4r4dat <- unique(b4r4[order(y4),]);b4r4dat <- na.omit(b4r4dat)
FourBitesTEMP1<-merge(b4r1dat,b4r2dat,by.x="y1",by.y="y2",all = TRUE)
FourBitesTEMP2<-merge(FourBitesTEMP1,b4r3dat,by.x="y1",by.y="y3",all = TRUE)
FourBites<-merge(FourBitesTEMP2,b4r4dat,by.x="y1",by.y="y4",all = TRUE)

b5r1<-data.frame(y1,ynbr1);b5r1dat <- unique(b5r1[order(y1),]);b5r1dat <- na.omit(b5r1dat)
b5r2<-data.frame(y2,ynbr2);b5r2dat <- unique(b5r2[order(y2),]);b5r2dat <- na.omit(b5r2dat)
b5r3<-data.frame(y3,ynbr3);b5r3dat <- unique(b5r3[order(y3),]);b5r3dat <- na.omit(b5r3dat)
b5r4<-data.frame(y4,ynbr4);b5r4dat <- unique(b5r4[order(y4),]);b5r4dat <- na.omit(b5r4dat)
FiveBitesTEMP1<-merge(b5r1dat,b5r2dat,by.x="y1",by.y="y2",all = TRUE)
FiveBitesTEMP2<-merge(FiveBitesTEMP1,b5r3dat,by.x="y1",by.y="y3",all = TRUE)
FiveBites<-merge(FiveBitesTEMP2,b5r4dat,by.x="y1",by.y="y4",all = TRUE)

TwoBites[1,2];ThreeBites[1,2];FourBites[1,2];FiveBites[1,2]
TwoBites[2,2];ThreeBites[2,2];FourBites[2,2];FiveBites[2,2]
TwoBites[3,2];ThreeBites[3,2];FourBites[3,2];FiveBites[3,2]
TwoBites[4,2];ThreeBites[4,2];FourBites[4,2];FiveBites[4,2]

##Estimating the negative binomial for different populations of oocysts (atv treatment)

y1<-c(oocysts$oocystsbites2atv[oocysts$round=="day41"])##N = 33
y2<-c(oocysts$oocystsbites2atv[oocysts$round=="day72"])##45
y3<-c(oocysts$oocystsbites2atv[oocysts$round=="day103"])##45
y4<-c(oocysts$oocystsbites2atv[oocysts$round=="day134"])##45

freqooc<-numeric(length(unique(y1)))
for (i in 1:length(unique(y1))){ 
  freqooc[i]<-sum(ifelse(y1==unique(y1)[i],1,0),na.rm=TRUE)}##12/32
freqooc
freqooc<-numeric(length(unique(y2)))
for (i in 1:length(unique(y2))){ 
  freqooc[i]<-sum(ifelse(y2==unique(y2)[i],1,0),na.rm=TRUE)}##12/32
freqooc
freqooc<-numeric(length(unique(y3)))
for (i in 1:length(unique(y3))){ 
  freqooc[i]<-sum(ifelse(y3==unique(y3)[i],1,0),na.rm=TRUE)}##12/32
freqooc
freqooc<-numeric(length(unique(y4)))
for (i in 1:length(unique(y4))){ 
  freqooc[i]<-sum(ifelse(y4==unique(y4)[i],1,0),na.rm=TRUE)}##12/32
freqooc


y1<-c(oocysts$oocystsbites3atv[oocysts$round=="day41"])##31
y2<-c(oocysts$oocystsbites3atv[oocysts$round=="day72"])##34
y3<-c(oocysts$oocystsbites3atv[oocysts$round=="day103"])##45
y4<-c(oocysts$oocystsbites3atv[oocysts$round=="day134"])##45

y1<-c(oocysts$oocystsbites4atv[oocysts$round=="day41"])##25
y2<-c(oocysts$oocystsbites4atv[oocysts$round=="day72"])##40
y3<-c(oocysts$oocystsbites4atv[oocysts$round=="day103"])##45
y4<-c(oocysts$oocystsbites4atv[oocysts$round=="day134"])##45

y1<-c(oocysts$oocystsbites5atv[oocysts$round=="day41"])##45
y2<-c(oocysts$oocystsbites5atv[oocysts$round=="day72"])##45
y3<-c(oocysts$oocystsbites5atv[oocysts$round=="day103"])##45
y4<-c(oocysts$oocystsbites5atv[oocysts$round=="day134"])##45

mu1<-mean(y1,na.rm=TRUE)
mu2<-mean(y2,na.rm=TRUE)
mu3<-mean(y3,na.rm=TRUE)
mu4<-mean(y4,na.rm=TRUE)


alpha<-4 ##allow alpha to vary for different groups
amu1<-mu1*alpha
amu2<-mu2*alpha
amu3<-mu3*alpha
amu4<-mu4*alpha

ynbr1<- exp(y1*log(amu1/(1+amu1))-
              (1/alpha)*log(1+amu1)+
              log(gamma(y1+1/alpha))-
              log(gamma(y1+1))-
              log(gamma(1/alpha)))

ynbr2<- exp(y2*log(amu2/(1+amu2))-
              (1/alpha)*log(1+amu2)+
              log(gamma(y2+1/alpha))-
              log(gamma(y2+1))-
              log(gamma(1/alpha)))

ynbr3<- exp(y3*log(amu3/(1+amu3))-
              (1/alpha)*log(1+amu3)+
              log(gamma(y3+1/alpha))-
              log(gamma(y3+1))-
              log(gamma(1/alpha)))

ynbr4<- exp(y4*log(amu4/(1+amu4))-
              (1/alpha)*log(1+amu4)+
              log(gamma(y4+1/alpha))-
              log(gamma(y4+1))-
              log(gamma(1/alpha)))

b2r1<-data.frame(y1,ynbr1);b2r1dat <- unique(b2r1[order(y1),]);b2r1dat <- na.omit(b2r1dat)
b2r2<-data.frame(y2,ynbr2);b2r2dat <- unique(b2r2[order(y2),]);b2r2dat <- na.omit(b2r2dat)
b2r3<-data.frame(y3,ynbr3);b2r3dat <- unique(b2r3[order(y3),]);b2r3dat <- na.omit(b2r3dat)
b2r4<-data.frame(y4,ynbr4);b2r4dat <- unique(b2r4[order(y4),]);b2r4dat <- na.omit(b2r4dat)
TwoBitesTEMP1<-merge(b2r1dat,b2r2dat,by.x="y1",by.y="y2",all = TRUE)
TwoBitesTEMP2<-merge(TwoBitesTEMP1,b2r3dat,by.x="y1",by.y="y3",all = TRUE)
TwoBitesT<-merge(TwoBitesTEMP2,b2r4dat,by.x="y1",by.y="y4",all = TRUE) ###use alpha = 8


b3r1<-data.frame(y1,ynbr1);b3r1dat <- unique(b3r1[order(y1),]);b3r1dat <- na.omit(b3r1dat)
b3r2<-data.frame(y2,ynbr2);b3r2dat <- unique(b3r2[order(y2),]);b3r2dat <- na.omit(b3r2dat)
b3r3<-data.frame(y3,ynbr3);b3r3dat <- unique(b3r3[order(y3),]);b3r3dat <- na.omit(b3r3dat)
b3r4<-data.frame(y4,ynbr4);b3r4dat <- unique(b3r4[order(y4),]);b3r4dat <- na.omit(b3r4dat)
ThreeBitesTEMP1<-merge(b3r1dat,b3r2dat,by.x="y1",by.y="y2",all = TRUE)
ThreeBitesTEMP2<-merge(ThreeBitesTEMP1,b3r3dat,by.x="y1",by.y="y3",all = TRUE)
ThreeBitesT<-merge(ThreeBitesTEMP2,b3r4dat,by.x="y1",by.y="y4",all = TRUE)

b4r1<-data.frame(y1,ynbr1);b4r1dat <- unique(b4r1[order(y1),]);b4r1dat <- na.omit(b4r1dat)
b4r2<-data.frame(y2,ynbr2);b4r2dat <- unique(b4r2[order(y2),]);b4r2dat <- na.omit(b4r2dat)
b4r3<-data.frame(y3,ynbr3);b4r3dat <- unique(b4r3[order(y3),]);b4r3dat <- na.omit(b4r3dat)
b4r4<-data.frame(y4,ynbr4);b4r4dat <- unique(b4r4[order(y4),]);b4r4dat <- na.omit(b4r4dat)
FourBitesTEMP1<-merge(b4r1dat,b4r2dat,by.x="y1",by.y="y2",all = TRUE)
FourBitesTEMP2<-merge(FourBitesTEMP1,b4r3dat,by.x="y1",by.y="y3",all = TRUE)
FourBitesT<-merge(FourBitesTEMP2,b4r4dat,by.x="y1",by.y="y4",all = TRUE)

b5r1<-data.frame(y1,ynbr1);b5r1dat <- unique(b5r1[order(y1),]);b5r1dat <- na.omit(b5r1dat)
b5r2<-data.frame(y2,ynbr2);b5r2dat <- unique(b5r2[order(y2),]);b5r2dat <- na.omit(b5r2dat)
b5r3<-data.frame(y3,ynbr3);b5r3dat <- unique(b5r3[order(y3),]);b5r3dat <- na.omit(b5r3dat)
b5r4<-data.frame(y4,ynbr4);b5r4dat <- unique(b5r4[order(y4),]);b5r4dat <- na.omit(b5r4dat)
FiveBitesTEMP1<-merge(b5r1dat,b5r2dat,by.x="y1",by.y="y2",all = TRUE)
FiveBitesTEMP2<-merge(FiveBitesTEMP1,b5r3dat,by.x="y1",by.y="y3",all = TRUE)
FiveBitesT<-merge(FiveBitesTEMP2,b5r4dat,by.x="y1",by.y="y4",all = TRUE)

par(mfrow=c(1,2))
plot(TwoBites$y1,TwoBites$ynbr1,col="black",pch=15,ylim=c(0,1),xlim=c(0,200),main="Oocyst distributions")
points(TwoBites$y1,TwoBites$ynbr2,col="black",pch=15)
points(TwoBites$y1,TwoBites$ynbr3,col="black",pch=15)
points(TwoBites$y1,TwoBites$ynbr4,col="black",pch=15)

points(ThreeBites$y1,ThreeBites$ynbr2,col="blue",pch=15)
points(ThreeBites$y1,ThreeBites$ynbr2,col="blue",pch=15)
points(ThreeBites$y1,ThreeBites$ynbr3,col="blue",pch=15)
points(ThreeBites$y1,ThreeBites$ynbr4,col="blue",pch=15)

points(FourBites$y1,FourBites$ynbr2,col="red",pch=15)
points(FourBites$y1,FourBites$ynbr2,col="red",pch=15)
points(FourBites$y1,FourBites$ynbr3,col="red",pch=15)
points(FourBites$y1,FourBites$ynbr4,col="red",pch=15)

points(FiveBites$y1,FiveBites$ynbr2,col="yellow",pch=15)
points(FiveBites$y1,FiveBites$ynbr2,col="yellow",pch=15)
points(FiveBites$y1,FiveBites$ynbr3,col="yellow",pch=15)
points(FiveBites$y1,FiveBites$ynbr4,col="yellow",pch=15)

points(TwoBitesT$y1,TwoBitesT$ynbr1,col="black",pch=1)
points(TwoBitesT$y1,TwoBitesT$ynbr2,col="black",pch=1)
points(TwoBitesT$y1,TwoBitesT$ynbr3,col="black",pch=1)
points(TwoBitesT$y1,TwoBitesT$ynbr4,col="black",pch=1)

points(ThreeBitesT$y1,ThreeBitesT$ynbr2,col="blue",pch=1)
points(ThreeBitesT$y1,ThreeBitesT$ynbr2,col="blue",pch=1)
points(ThreeBitesT$y1,ThreeBitesT$ynbr3,col="blue",pch=1)
points(ThreeBitesT$y1,ThreeBitesT$ynbr4,col="blue",pch=1)

points(FourBitesT$y1,FourBitesT$ynbr2,col="red",pch=1)
points(FourBitesT$y1,FourBitesT$ynbr2,col="red",pch=1)
points(FourBitesT$y1,FourBitesT$ynbr3,col="red",pch=1)
points(FourBitesT$y1,FourBitesT$ynbr4,col="red",pch=1)

points(FiveBitesT$y1,FiveBitesT$ynbr2,col="yellow",pch=1)
points(FiveBitesT$y1,FiveBitesT$ynbr2,col="yellow",pch=1)
points(FiveBitesT$y1,FiveBitesT$ynbr3,col="yellow",pch=1)
points(FiveBitesT$y1,FiveBitesT$ynbr4,col="yellow",pch=1)


##
##Estimating the negative binomial for different populations of sporozoites
##(from loading data in MODELinstruction)
y1temp<-a1;y1<-ifelse(a1==0,0,ifelse(a1==1,1,ifelse(a1==2,11,ifelse(a1==3,101,1001))))
y2temp<-b1;y2<-ifelse(b1==0,0,ifelse(b1==1,1,ifelse(b1==2,11,ifelse(b1==3,101,1001))))
y3temp<-c1;y3<-ifelse(c1==0,0,ifelse(c1==1,1,ifelse(c1==2,11,ifelse(c1==3,101,1001))))
y4temp<-d1;y4<-ifelse(d1==0,0,ifelse(d1==1,1,ifelse(d1==2,11,ifelse(d1==3,101,1001))))

freqooc<-numeric(length(unique(y1)))
for (i in 1:length(unique(y1))){ 
  freqooc[i]<-sum(ifelse(y1==unique(y1)[i],1,0),na.rm=TRUE)}##12/32
freqooc
freqooc<-numeric(length(unique(y2)))
for (i in 1:length(unique(y2))){ 
  freqooc[i]<-sum(ifelse(y2==unique(y2)[i],1,0),na.rm=TRUE)}##12/32
freqooc
freqooc<-numeric(length(unique(y3)))
for (i in 1:length(unique(y3))){ 
  freqooc[i]<-sum(ifelse(y3==unique(y3)[i],1,0),na.rm=TRUE)}##12/32
freqooc
freqooc<-numeric(length(unique(y4)))
for (i in 1:length(unique(y4))){ 
  freqooc[i]<-sum(ifelse(y4==unique(y4)[i],1,0),na.rm=TRUE)}##12/32
freqooc
y1temp<-ee1;y1<-ifelse(ee1==0,0,ifelse(ee1==1,1,ifelse(ee1==2,11,ifelse(ee1==3,101,1001))))
y2temp<-ff1;y2<-ifelse(ff1==0,0,ifelse(ff1==1,1,ifelse(ff1==2,11,ifelse(ff1==3,101,1001))))
y3temp<-g1;y3<-ifelse(g1==0,0,ifelse(g1==1,1,ifelse(g1==2,11,ifelse(g1==3,101,1001))))
y4temp<-h1;y4<-ifelse(h1==0,0,ifelse(h1==1,1,ifelse(h1==2,11,ifelse(h1==3,101,1001))))


y1temp<-jj1;y1<-ifelse(jj1==0,0,ifelse(jj1==1,1,ifelse(jj1==2,11,ifelse(jj1==3,101,1001))))
y2temp<-kk1;y2<-ifelse(kk1==0,0,ifelse(kk1==1,1,ifelse(kk1==2,11,ifelse(kk1==3,101,1001))))
y3temp<-l1;y3<-ifelse(l1==0,0,ifelse(l1==1,1,ifelse(l1==2,11,ifelse(l1==3,101,1001))))
y4temp<-m1;y4<-ifelse(m1==0,0,ifelse(m1==1,1,ifelse(m1==2,11,ifelse(m1==3,101,1001))))


y1temp<-nn1;y1<-ifelse(nn1==0,0,ifelse(nn1==1,1,ifelse(nn1==2,11,ifelse(nn1==3,101,1001))))
y2temp<-oo1;y2<-ifelse(oo1==0,0,ifelse(oo1==1,1,ifelse(oo1==2,11,ifelse(oo1==3,101,1001))))
y3temp<-pp1;y3<-ifelse(pp1==0,0,ifelse(pp1==1,1,ifelse(pp1==2,11,ifelse(pp1==3,101,1001))))
y4temp<-qq1;y4<-ifelse(qq1==0,0,ifelse(qq1==1,1,ifelse(qq1==2,11,ifelse(qq1==3,101,1001))))

mu1<-mean(y1,na.rm=TRUE)
mu2<-mean(y2,na.rm=TRUE)
mu3<-mean(y3,na.rm=TRUE)
mu4<-mean(y4,na.rm=TRUE)


alpha<-8 ##allow alpha to vary for different groups
amu1<-mu1*alpha
amu2<-mu2*alpha
amu3<-mu3*alpha
amu4<-mu4*alpha

ynbr1<- exp(y1*log(amu1/(1+amu1))-
              (1/alpha)*log(1+amu1)+
              log(gamma(y1+1/alpha))-
              log(gamma(y1+1))-
              log(gamma(1/alpha)))

ynbr2<- exp(y2*log(amu2/(1+amu2))-
              (1/alpha)*log(1+amu2)+
              log(gamma(y2+1/alpha))-
              log(gamma(y2+1))-
              log(gamma(1/alpha)))

ynbr3<- exp(y3*log(amu3/(1+amu3))-
              (1/alpha)*log(1+amu3)+
              log(gamma(y3+1/alpha))-
              log(gamma(y3+1))-
              log(gamma(1/alpha)))

ynbr4<- exp(y4*log(amu4/(1+amu4))-
              (1/alpha)*log(1+amu4)+
              log(gamma(y4+1/alpha))-
              log(gamma(y4+1))-
              log(gamma(1/alpha)))

b2r1<-data.frame(y1,ynbr1);b2r1dat <- unique(b2r1[order(y1),]);b2r1dat <- na.omit(b2r1dat)
b2r2<-data.frame(y2,ynbr2);b2r2dat <- unique(b2r2[order(y2),]);b2r2dat <- na.omit(b2r2dat)
b2r3<-data.frame(y3,ynbr3);b2r3dat <- unique(b2r3[order(y3),]);b2r3dat <- na.omit(b2r3dat)
b2r4<-data.frame(y4,ynbr4);b2r4dat <- unique(b2r4[order(y4),]);b2r4dat <- na.omit(b2r4dat)
TwoBitesTEMP1<-merge(b2r1dat,b2r2dat,by.x="y1",by.y="y2",all = TRUE)
TwoBitesTEMP2<-merge(TwoBitesTEMP1,b2r3dat,by.x="y1",by.y="y3",all = TRUE)
TwoBitesSP<-merge(TwoBitesTEMP2,b2r4dat,by.x="y1",by.y="y4",all = TRUE)


b3r1<-data.frame(y1,ynbr1);b3r1dat <- unique(b3r1[order(y1),]);b3r1dat <- na.omit(b3r1dat)
b3r2<-data.frame(y2,ynbr2);b3r2dat <- unique(b3r2[order(y2),]);b3r2dat <- na.omit(b3r2dat)
b3r3<-data.frame(y3,ynbr3);b3r3dat <- unique(b3r3[order(y3),]);b3r3dat <- na.omit(b3r3dat)
b3r4<-data.frame(y4,ynbr4);b3r4dat <- unique(b3r4[order(y4),]);b3r4dat <- na.omit(b3r4dat)
ThreeBitesTEMP1<-merge(b3r1dat,b3r2dat,by.x="y1",by.y="y2",all = TRUE)
ThreeBitesTEMP2<-merge(ThreeBitesTEMP1,b3r3dat,by.x="y1",by.y="y3",all = TRUE)
ThreeBitesSP<-merge(ThreeBitesTEMP2,b3r4dat,by.x="y1",by.y="y4",all = TRUE)

b4r1<-data.frame(y1,ynbr1);b4r1dat <- unique(b4r1[order(y1),]);b4r1dat <- na.omit(b4r1dat)
b4r2<-data.frame(y2,ynbr2);b4r2dat <- unique(b4r2[order(y2),]);b4r2dat <- na.omit(b4r2dat)
b4r3<-data.frame(y3,ynbr3);b4r3dat <- unique(b4r3[order(y3),]);b4r3dat <- na.omit(b4r3dat)
b4r4<-data.frame(y4,ynbr4);b4r4dat <- unique(b4r4[order(y4),]);b4r4dat <- na.omit(b4r4dat)
FourBitesTEMP1<-merge(b4r1dat,b4r2dat,by.x="y1",by.y="y2",all = TRUE)
FourBitesTEMP2<-merge(FourBitesTEMP1,b4r3dat,by.x="y1",by.y="y3",all = TRUE)
FourBitesSP<-merge(FourBitesTEMP2,b4r4dat,by.x="y1",by.y="y4",all = TRUE)


b5r1<-data.frame(y1,ynbr1);b5r1dat <- unique(b5r1[order(y1),]);b5r1dat <- na.omit(b5r1dat)
b5r2<-data.frame(y2,ynbr2);b5r2dat <- unique(b5r2[order(y2),]);b5r2dat <- na.omit(b5r2dat)
b5r3<-data.frame(y3,ynbr3);b5r3dat <- unique(b5r3[order(y3),]);b5r3dat <- na.omit(b5r3dat)
b5r4<-data.frame(y4,ynbr4);b5r4dat <- unique(b5r4[order(y4),]);b5r4dat <- na.omit(b5r4dat)
FiveBitesTEMP1<-merge(b5r1dat,b5r2dat,by.x="y1",by.y="y2",all = TRUE)
FiveBitesTEMP2<-merge(FiveBitesTEMP1,b5r3dat,by.x="y1",by.y="y3",all = TRUE)
FiveBitesSP<-merge(FiveBitesTEMP2,b5r4dat,by.x="y1",by.y="y4",all = TRUE)

y1temp<-a;y1<-ifelse(a==0,0,ifelse(a==1,1,ifelse(a==2,11,ifelse(a==3,101,1001))))
y2temp<-b;y2<-ifelse(b==0,0,ifelse(b==1,1,ifelse(b==2,11,ifelse(b==3,101,1001))))
y3temp<-c;y3<-ifelse(c==0,0,ifelse(c==1,1,ifelse(c==2,11,ifelse(c==3,101,1001))))
y4temp<-d;y4<-ifelse(d==0,0,ifelse(d==1,1,ifelse(d==2,11,ifelse(d==3,101,1001))))

freqooc<-numeric(length(unique(y1)))
for (i in 1:length(unique(y1))){ 
  freqooc[i]<-sum(ifelse(y1==unique(y1)[i],1,0),na.rm=TRUE)}##12/32
freqooc
freqooc<-numeric(length(unique(y2)))
for (i in 1:length(unique(y2))){ 
  freqooc[i]<-sum(ifelse(y2==unique(y2)[i],1,0),na.rm=TRUE)}##12/32
freqooc
freqooc<-numeric(length(unique(y3)))
for (i in 1:length(unique(y3))){ 
  freqooc[i]<-sum(ifelse(y3==unique(y3)[i],1,0),na.rm=TRUE)}##12/32
freqooc
freqooc<-numeric(length(unique(y4)))
for (i in 1:length(unique(y4))){ 
  freqooc[i]<-sum(ifelse(y4==unique(y4)[i],1,0),na.rm=TRUE)}##12/32
freqooc
y1temp<-ee;y1<-ifelse(ee==0,0,ifelse(ee==1,1,ifelse(ee==2,11,ifelse(ee==3,101,1001))))
y2temp<-ff;y2<-ifelse(ff==0,0,ifelse(ff==1,1,ifelse(ff==2,11,ifelse(ff==3,101,1001))))
y3temp<-g;y3<-ifelse(g==0,0,ifelse(g==1,1,ifelse(g==2,11,ifelse(g==3,101,1001))))
y4temp<-h;y4<-ifelse(h==0,0,ifelse(h==1,1,ifelse(h==2,11,ifelse(h==3,101,1001))))


y1temp<-jj;y1<-ifelse(jj==0,0,ifelse(jj==1,1,ifelse(jj==2,11,ifelse(jj==3,101,1001))))
y2temp<-kk;y2<-ifelse(kk==0,0,ifelse(kk==1,1,ifelse(kk==2,11,ifelse(kk==3,101,1001))))
y3temp<-l;y3<-ifelse(l==0,0,ifelse(l==1,1,ifelse(l==2,11,ifelse(l==3,101,1001))))
y4temp<-m;y4<-ifelse(m==0,0,ifelse(m==1,1,ifelse(m==2,11,ifelse(m==3,101,1001))))


y1temp<-nn;y1<-ifelse(nn==0,0,ifelse(nn==1,1,ifelse(nn==2,11,ifelse(nn==3,101,1001))))
y2temp<-oo;y2<-ifelse(oo==0,0,ifelse(oo==1,1,ifelse(oo==2,11,ifelse(oo==3,101,1001))))
y3temp<-pp;y3<-ifelse(pp==0,0,ifelse(pp==1,1,ifelse(pp==2,11,ifelse(pp==3,101,1001))))
y4temp<-qq;y4<-ifelse(qq==0,0,ifelse(qq==1,1,ifelse(qq==2,11,ifelse(qq==3,101,1001))))

plot(TwoBitesSP$y1,TwoBitesSP$ynbr1,col="black",pch=15,ylim=c(0,1),xlim=c(0,200),main="Sporozoite distributions")
points(TwoBitesSP$y1,TwoBitesSP$ynbr2,col="black",pch=15)
points(TwoBitesSP$y1,TwoBitesSP$ynbr3,col="black",pch=15)
points(TwoBitesSP$y1,TwoBitesSP$ynbr4,col="black",pch=15)

points(ThreeBitesSP$y1,ThreeBitesSP$ynbr2,col="blue",pch=15)
points(ThreeBitesSP$y1,ThreeBitesSP$ynbr2,col="blue",pch=15)
points(ThreeBitesSP$y1,ThreeBitesSP$ynbr3,col="blue",pch=15)
points(ThreeBitesSP$y1,ThreeBitesSP$ynbr4,col="blue",pch=15)

points(FourBitesSP$y1,FourBitesSP$ynbr2,col="red",pch=15)
points(FourBitesSP$y1,FourBitesSP$ynbr2,col="red",pch=15)
points(FourBitesSP$y1,FourBitesSP$ynbr3,col="red",pch=15)
points(FourBitesSP$y1,FourBitesSP$ynbr4,col="red",pch=15)

points(FiveBitesSP$y1,FiveBitesSP$ynbr2,col="yellow",pch=15)
points(FiveBitesSP$y1,FiveBitesSP$ynbr2,col="yellow",pch=15)
points(FiveBitesSP$y1,FiveBitesSP$ynbr3,col="yellow",pch=15)
points(FiveBitesSP$y1,FiveBitesSP$ynbr4,col="yellow",pch=15)

points(TwoBitesSPT$y1,TwoBitesSPT$ynbr1,col="black",pch=1)
points(TwoBitesSPT$y1,TwoBitesSPT$ynbr2,col="black",pch=1)
points(TwoBitesSPT$y1,TwoBitesSPT$ynbr3,col="black",pch=1)
points(TwoBitesSPT$y1,TwoBitesSPT$ynbr4,col="black",pch=1)

points(ThreeBitesSPT$y1,ThreeBitesSPT$ynbr2,col="blue",pch=1)
points(ThreeBitesSPT$y1,ThreeBitesSPT$ynbr2,col="blue",pch=1)
points(ThreeBitesSPT$y1,ThreeBitesSPT$ynbr3,col="blue",pch=1)
points(ThreeBitesSPT$y1,ThreeBitesSPT$ynbr4,col="blue",pch=1)

points(FourBitesSPT$y1,FourBitesSPT$ynbr2,col="red",pch=1)
points(FourBitesSPT$y1,FourBitesSPT$ynbr2,col="red",pch=1)
points(FourBitesSPT$y1,FourBitesSPT$ynbr3,col="red",pch=1)
points(FourBitesSPT$y1,FourBitesSPT$ynbr4,col="red",pch=1)

points(FiveBitesSPT$y1,FiveBitesSPT$ynbr2,col="yellow",pch=1)
points(FiveBitesSPT$y1,FiveBitesSPT$ynbr2,col="yellow",pch=1)
points(FiveBitesSPT$y1,FiveBitesSPT$ynbr3,col="yellow",pch=1)
points(FiveBitesSPT$y1,FiveBitesSPT$ynbr4,col="yellow",pch=1)
##################################
require(MASS)
poisson_syn <- function(nobs = 25, off = 0, xv = c(1, -0.5, 1)) {
  p <- length(xv) - 1
  X <- cbind(1,matrix(rnorm(nobs * p), ncol = p))
  xb <- X %*% xv
  exb <- exp(xb + off)
  py <- rpois(nobs, exb)
  out <- data.frame(cbind(py, X[,-1]))
  names(out) <- c("py",paste("x", 1:p, sep=""))
  return(out)
}
sim.data<-poisson_syn(nobs = 25, xv = c(2,0.75,-1.25))
mypo<-glm(py ~ ., family=poisson, data = sim.data)
summary(mypo)
mysim <- function()
{
  
  nobs <- 200
  x1 <- runif(nobs)
  x2 <- runif(nobs)
  py <- rpois(nobs, exp(2 + 0.75*x1 - -1.25 * x2))
  poi <- glm(py ~ x1 + x2, family=poisson)
    pr <- sum(residuals(poi, type="pearson")^2)
    prdisp <- pr/poi$df.residual
    beta <- poi$coef
    list(beta,prdisp)
}
B <- replicate(100, mysim())
apply(matrix(unlist(B[1,]),3,20),1,mean)
mean(unlist(B[2,]))
