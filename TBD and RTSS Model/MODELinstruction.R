library(rstan)

data1<-list(N_C=4,
            N_T=4,
            N_ooc=6,
            N_mice=5,
            ooc_count_C = structure(.Data = c(0,24,13,6,0,17,0,0,0,34,23,13,23,34,61,12,1,1,0,0,0,23,24,25),
                          .Dim=c(6,4)),
            ooc_count_T = structure(.Data = c(0,24,13,16,0,0,0,0,0,3,23,13,2,0,61,12,1,0,0,0,0,0,24,25),
                                    .Dim=c(6,4)),
            prev_C = structure(.Data =c(0,0,0,0,1,1,0,0,1,1,1,1,0,0,1,0,1,0,0,1),.Dim=c(5,4)),
            prev_T = structure(.Data =c(0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,1),.Dim=c(5,4)),
            N_bin=4,
            bin_edge=c(0,1,10,100,1000),
            s_count_C = structure(.Data=c(0,0,2,4,3,2,1,0,1,2,0,1,1,2,0,3),
                                  .Dim=c(4,4)),
            s_count_T = structure(.Data=c(0,0,0,4,0,2,1,0,1,0,0,1,1,0,0,3),
                                  .Dim=c(4,4))
            )


fit1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\MODEL.stan", data=data1,
             iter=100, chains=4)
print(fit1)

