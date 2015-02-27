##########################################################################################
##
## M2M, estimate effect sizes for drug treatments from mouse-to-mouse experiments
##
##########################################################################################

## Set up mosquito data

mosqOocystSlot3 = read.table("D:\\IMPERIAL Nov 2014\\Andrew Blagborough\\12Feb2015_4DrugTreatments\\M2M ChainBinomial Effect Size\\Mosq Oocysts Slot 3.txt",header=TRUE)

      mosqOocystSlot3$prev = ifelse(mosqOocystSlot3$oocysts < 1, 0, 1)
      treat<-unique(mosqOocystSlot3$treat)

            data.mosqu3=expand.grid("treat"=treat)
            data.mosqu3$uninf_mosq = NA
            data.mosqu3$inf_mosq = NA
            data.mosqu3$tot_mosq = NA

                        data.mosqu3[1,3]<-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="Blank"])
                        data.mosqu3[2,3]<-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="SD"])
                        data.mosqu3[3,3]<-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="ATV"])
                        data.mosqu3[4,3]<-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="UCT"])
                        data.mosqu3[5,3]<-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZPIP"])
                        data.mosqu3[6,3]<-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZFER"])
                        data.mosqu3[7,3]<-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZDSM"])

                        data.mosqu3[1,4]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="Blank"])
                        data.mosqu3[2,4]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="SD"])
                        data.mosqu3[3,4]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="ATV"])
                        data.mosqu3[4,4]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="UCT"])
                        data.mosqu3[5,4]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZPIP"])
                        data.mosqu3[6,4]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZFER"])
                        data.mosqu3[7,4]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZDSM"])

                        data.mosqu3[1,2]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="Blank"])-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="Blank"])
                        data.mosqu3[2,2]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="SD"])-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="SD"])
                        data.mosqu3[3,2]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="ATV"])-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="ATV"])
                        data.mosqu3[4,2]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="UCT"])-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="UCT"])
                        data.mosqu3[5,2]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZPIP"])-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZPIP"])
                        data.mosqu3[6,2]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZFER"])-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZFER"])
                        data.mosqu3[7,2]<-length(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZDSM"])-sum(mosqOocystSlot3$prev[mosqOocystSlot3$treat=="OZDSM"])


##Set up mosquito data for slot 4
mosqOocystSlot4 = read.table("D:\\IMPERIAL Nov 2014\\Andrew Blagborough\\12Feb2015_4DrugTreatments\\M2M ChainBinomial Effect Size\\Mosq Oocysts Slot 4.txt",header=TRUE)

      mosqOocystSlot4$prev = ifelse(mosqOocystSlot4$oocysts < 1, 0, 1)
      treat<-unique(mosqOocystSlot4$treat)

            data.mosqu4=expand.grid("treat"=treat)
            data.mosqu4$uninf_mosq = NA
            data.mosqu4$inf_mosq = NA
            data.mosqu4$tot_mosq = NA

                        data.mosqu4[1,3]<-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="Blank"])
                        data.mosqu4[2,3]<-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="SD"])
                        data.mosqu4[3,3]<-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="ATV"])
                        data.mosqu4[4,3]<-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="UCT"])
                        data.mosqu4[5,3]<-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZPIP"])
                        data.mosqu4[6,3]<-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZFER"])
                        data.mosqu4[7,3]<-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZDSM"])

                        data.mosqu4[1,4]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="Blank"])
                        data.mosqu4[2,4]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="SD"])
                        data.mosqu4[3,4]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="ATV"])
                        data.mosqu4[4,4]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="UCT"])
                        data.mosqu4[5,4]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZPIP"])
                        data.mosqu4[6,4]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZFER"])
                        data.mosqu4[7,4]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZDSM"])

                        data.mosqu4[1,2]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="Blank"])-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="Blank"])
                        data.mosqu4[2,2]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="SD"])-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="SD"])
                        data.mosqu4[3,2]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="ATV"])-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="ATV"])
                        data.mosqu4[4,2]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="UCT"])-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="UCT"])
                        data.mosqu4[5,2]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZPIP"])-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZPIP"])
                        data.mosqu4[6,2]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZFER"])-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZFER"])
                        data.mosqu4[7,2]<-length(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZDSM"])-sum(mosqOocystSlot4$prev[mosqOocystSlot4$treat=="OZDSM"])


data.mouse3=read.table("D:\\IMPERIAL Nov 2014\\Andrew Blagborough\\12Feb2015_4DrugTreatments\\M2M ChainBinomial Effect Size\\M2M Mouse slot 3.txt",header=TRUE)
data.mouse4=read.table("D:\\IMPERIAL Nov 2014\\Andrew Blagborough\\12Feb2015_4DrugTreatments\\M2M ChainBinomial Effect Size\\M2M Mouse slot 4.txt",header=TRUE)

data.mouse3
data.mosqu3
data.mouse4
data.mosqu4

# remove data from other treatments
ds3<-data.mouse3[data.mouse3$treat!="SD" & data.mouse3$treat!="ATV",];ds3
dq3<-data.mosqu3[data.mosqu3$treat!="SD" & data.mosqu3$treat!="ATV",];dq3
ds4<-data.mouse4[data.mouse4$treat!="SD" & data.mouse4$treat!="ATV",];ds4
dq4<-data.mosqu4[data.mosqu4$treat!="SD" & data.mosqu4$treat!="ATV",];dq4


vrs.binom<-function(p.vec){
  
  v1<-p.vec[1]
  v2<-p.vec[2]
  v3<-p.vec[3]
  v4<-p.vec[4]
  r<-p.vec[5]
  s<-p.vec[6]
  
  #################################
  ##################################
  ################################### Define the slot 3 data
  ##################################
  #################################
  
  ######### Table dimensions correspond to:############################### 
  ## TABLES: (inf1 - number of infected vs number = number at start     ##
  ##   Round (1 = 1st count; 2 = second count)                          ##
  ##   Bites (columns 1 = 2 bites, 2 = 5 bites, 3 = 10 bites),          ##
  ##   Treatment (blank = 1, UCT = 2, OZPIP = 3, OZFER = 4, OZDSM = 5), ##
  ##   Species (Mosquito = 1, Mouse = 2); number infected               ##
  ########################################################################
  
  inf1<-array(NA, dim = c(2,3,5,2))
  colnames(inf1)<-c("Nbites2","Nbites5","Nbites10")
  
  number1<-array(NA, dim = c(2,3,5,2))
  colnames(number1)<-c("Nbites2","Nbites5","Nbites10")
  
  
  # Enter p0
  inf1[1,,,2]<-5
  
  for (i in 1:5){
  number1[,1,i,2]<-5
  number1[,2,i,2]<-5
  number1[,3,i,2]<-5
  }
  
  # Pre-calc q0
  for(k in 1:5){
    number1[,,k,1] <- dq3$tot_mosq[k]  
  }
  
  # mosquito infection
  inf1[1,,1,1] <- s * number1[1,,1,1]
  inf1[1,,2,1] <- s * number1[1,,2,1]*(1-v1)
  inf1[1,,3,1] <- s * number1[1,,3,1]*(1-v2)
  inf1[1,,4,1] <- s * number1[1,,4,1]*(1-v3)
  inf1[1,,5,1] <- s * number1[1,,5,1]*(1-v4)
  
  # mouse infection
  a<-1
  mbr.vec<-c(2,5,10)
  for(b in 1:3){
    inf1[a+1,b,,2] <- (1 - (1 - r * (inf1[a,b,,1]/number1[a,b,,1])) ^ mbr.vec[b]) * number1[a+1,b,,2]
  }
  
  # Arrange data into appropriate format for fitting
  data.inf1<-array(NA, dim = c(2,3,5,2))  
  colnames(data.inf1)<-c("Nbites2","Nbites5","Nbites10")
  
  for(k in 1:5){
    data.inf1[1,,k,1] <- dq3$inf_mosq[k]
  }
  
  a<-1
  for(b in 1:3){
    data.inf1[a+1,b,1,2]<-ds3$inf_mou[ds3$treat=="Blank" & ds3$mbr==mbr.vec[b]]
    data.inf1[a+1,b,2,2]<-ds3$inf_mou[ds3$treat=="UCT" & ds3$mbr==mbr.vec[b]]
    data.inf1[a+1,b,3,2]<-ds3$inf_mou[ds3$treat=="OZPIP" & ds3$mbr==mbr.vec[b]]
    data.inf1[a+1,b,4,2]<-ds3$inf_mou[ds3$treat=="OZFER" & ds3$mbr==mbr.vec[b]]
    data.inf1[a+1,b,5,2]<-ds3$inf_mou[ds3$treat=="OZDSM" & ds3$mbr==mbr.vec[b]]
  }
  
  # remove entered mouse data 
  inf1[1,,,2]<-NA
  
  # and pseudoreplicated mosquito data
  inf1[1,2:3,,1]<-NA
  
  ###########################
  ############################
  ############################# Repeat for second replicate  
  ############################
  ###########################
  
  # Round, Bites, Treatment, Species; number infected
  inf2<-array(NA, dim = c(2,3,5,2))
  colnames(inf2)<-c("Nbites2","Nbites5","Nbites10")
  
  # Total number
  number2<-array(NA, dim = c(2,3,5,2))
  
  # Enter p0
  inf2[1,,,2]<-5
  
  for (j in 1:5){
  number2[,1,j,2]<-5 # because always 5 mice (alter if 4 or less)
  number2[,2,j,2]<-5
  number2[,3,j,2]<-5
  }
  
  # Pre-calc q0
  for(k in 1:5){
    number2[,,k,1] <- dq4$tot_mosq[k]  
  }
  
  # mosquito infection
  inf2[1,,1,1] <- s * number2[1,,1,1]
  inf2[1,,2,1] <- s * number2[1,,2,1]*(1-v1)
  inf2[1,,3,1] <- s * number2[1,,3,1]*(1-v2)
  inf2[1,,4,1] <- s * number2[1,,2,1]*(1-v3)
  inf2[1,,5,1] <- s * number2[1,,3,1]*(1-v4)  
  
  # mouse infection
  a<-1
  mbr.vec<-c(2,5,10)
  for(b in 1:3){
    inf2[a+1,b,,2] <- (1 - (1 - r * (inf2[a,b,,1]/number2[a,b,,1])) ^ mbr.vec[b]) * number2[a+1,b,,2]
  }
  
  # Arrange data into appropriate format for fitting
  data.inf2<-array(NA, dim = c(2,3,5,2))  
  
  for(k in 1:5){
    data.inf2[1,,k,1] <- dq4$inf_mosq[k]
  }
  
  a<-1
  for(b in 1:3){
    data.inf2[a+1,b,1,2]<-ds4$inf_mou[ds4$treat=="Blank" & ds4$mbr==mbr.vec[b]]
    data.inf2[a+1,b,2,2]<-ds4$inf_mou[ds4$treat=="UCT" & ds4$mbr==mbr.vec[b]]
    data.inf2[a+1,b,3,2]<-ds4$inf_mou[ds4$treat=="OZPIP" & ds4$mbr==mbr.vec[b]]
    data.inf2[a+1,b,4,2]<-ds4$inf_mou[ds4$treat=="OZFER" & ds4$mbr==mbr.vec[b]]
    data.inf2[a+1,b,5,2]<-ds4$inf_mou[ds4$treat=="OZDSM" & ds4$mbr==mbr.vec[b]]
  }
  
  # remove entered mouse data 
  inf2[1,,,2]<-NA
  
  # and pseudoreplicated mosquito data
  inf2[1,2:3,,1]<-NA
  
  # combine predictions and data from two replicates for fitting
  inf<-cbind(inf1,inf2)
  data.inf<-cbind(data.inf1,data.inf2)
  number<-cbind(number1,number2)
  
  loglik<- data.inf * log((inf/number))+(number-data.inf)*log((1-(inf/number)))
  -sum(loglik,na.rm=T)  
}

n.param<-6
vrs.model<-optim(rep(0,n.param),vrs.binom,method="L-BFGS-B",lower=rep(0.01,n.param),upper=rep(0.99,n.param))
vrs.model


#
##
#
#
## CIs  ?????????????
#
optim.model<-vrs.binom(vrs.model$par)

size.of.grid<-10
v1.range<-seq(0.01,0.99,length=size.of.grid)
v2.range<-seq(0.01,0.99,length=size.of.grid)
v3.range<-seq(0.01,0.99,length=size.of.grid)
v4.range<-seq(0.01,0.99,length=size.of.grid)
v5.range<-seq(0.01,0.99,length=size.of.grid)
v6.range<-seq(0.01,0.99,length=size.of.grid)

ci.grid.v1<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid,size.of.grid,size.of.grid,size.of.grid))
ci.grid.v2<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid,size.of.grid,size.of.grid,size.of.grid))
ci.grid.v3<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid,size.of.grid,size.of.grid,size.of.grid))
ci.grid.v4<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid,size.of.grid,size.of.grid,size.of.grid))
ci.grid.v5<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid,size.of.grid,size.of.grid,size.of.grid))
ci.grid.v6<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid,size.of.grid,size.of.grid,size.of.grid))

for(a in 1:size.of.grid){
  for(b in 1:size.of.grid){
    for(d in 1:size.of.grid){
      for(e in 1:size.of.grid){
        for(g in 1:size.of.grid){
          for(h in 1:size.of.grid){
        p.vec<-c(v1.range[a],v2.range[b],v3.range[d],v4.range[e],v5.range[g],v6.range[h])
        ci.n.param<-length(p.vec) 
        ci.fit<-vrs.binom(p.vec)     
        ci.grid.v1[a,b,d,e,g,h]<-ifelse(ci.fit<=optim.model+qchisq(0.95, ci.n.param)/2,v1.range[a],NA)
        ci.grid.v2[a,b,d,e,g,h]<-ifelse(ci.fit<=optim.model+qchisq(0.95, ci.n.param)/2,v2.range[b],NA)
        ci.grid.v3[a,b,d,e,g,h]<-ifelse(ci.fit<=optim.model+qchisq(0.95, ci.n.param)/2,v3.range[d],NA)
        ci.grid.v4[a,b,d,e,g,h]<-ifelse(ci.fit<=optim.model+qchisq(0.95, ci.n.param)/2,v4.range[e],NA)
        ci.grid.v5[a,b,d,e,g,h]<-ifelse(ci.fit<=optim.model+qchisq(0.95, ci.n.param)/2,v5.range[g],NA)
        ci.grid.v6[a,b,d,e,g,h]<-ifelse(ci.fit<=optim.model+qchisq(0.95, ci.n.param)/2,v6.range[h],NA)
      }}}
    }
  }
  print(a)
}

v1.ci<-range(ci.grid.v1, na.rm=T)
v2.ci<-range(ci.grid.v2, na.rm=T)
v3.ci<-range(ci.grid.v3, na.rm=T)
v4.ci<-range(ci.grid.v4, na.rm=T)
v5.ci<-range(ci.grid.v5, na.rm=T)
v6.ci<-range(ci.grid.v6, na.rm=T)
rbind(v1.ci,v2.ci,v3.ci,v4.ci,v5.ci,v6.ci)
