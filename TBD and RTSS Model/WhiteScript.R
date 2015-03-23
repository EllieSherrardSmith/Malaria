#####################################################################
#####################################################################
##                                                                 ##
## CODE FOR FITTING MODELS TO CROSS-SECTIONAL ANTIBODY TITRE DATA  ##
##                                                                 ##
## Please feel free to share modify the code as you see fit        ##   
## (but please maintain appropriate accreditation)                 ##
##                                                                 ##   
## Michael White                                                   ##
## Imperial College Lonodon                                        ##
## m.white08@imperial.ac.uk                                        ##
##                                                                 ##
#####################################################################
#####################################################################

library(MASS)

###############################################
###############################################
##          ##                               ##
##   ####   ##  ####    ####  ######  ####   ##
##  ##  ##  ##  ## ##  ##  ##   ##   ##  ##  ##
##  ##  ##  ##  ##  ## ######   ##   ######  ##
##  ##  ##  ##  ## ##  ##  ##   ##   ##  ##  ##
##   ####   ##  ####   ##  ##   ##   ##  ##  ##
##          ##                               ##
###############################################
###############################################

###############################################
## 0.1 Read in data

##From NatCommsBlagboroughData
AB_data<-data
head(AB_data)
###############################################
## 0.2 Prepare data for plotting

ooc_bins     <- seq(from=0, to=70, by=5)
ooc_bins_mid <- seq(from=2.5, to=67.5, by=5) 

N_bins <- length(ooc_bins) - 1 


GMT_bins      <- rep(NA, N_bins)

AB_range_bins <- matrix(NA, nrow=N_bins, ncol=3)
colnames(AB_range_bins) <- c("med", "low", "high")

for(i in 1:N_bins)
{
  index <- intersect( which(AB_data[,2]>ooc_bins[i]), which(AB_data[,2]<=ooc_bins[i+1]) ) 
  temp  <- AB_data[index,1]
  
  GMT_bins[i] = exp(mean(log(temp+1)))
  
  AB_range_bins[i,] <- quantile( temp, prob=c(0.5, 0.025, 0.975) )
}


###############################################
## 0.3 Plot data

par(mfrow=c(1,1))

plot(x=ooc_bins_mid, y=GMT_bins-1, 
     pch=15, cex=2,
     #log="y", 
     xlim=c(0,80), ylim=c(0,4),
     xlab="oocysts", ylab="Geometric mean sporozoites")


for(i in 1:(N_bins-1))
{
  arrows(x0=ooc_bins_mid[i], y0=AB_range_bins[i,2], 
         x1=ooc_bins_mid[i], y1=AB_range_bins[i,3], 
         length=0.03, angle=90, code=3, col="black", lwd=1)	
}



###################################################
###################################################
##        ##                                     ##
##   ##   ##  #     #  ####  ####   ##### ##     ##
##  ###   ##  ##   ## ##  ## ## ##  ##    ##     ##
##   ##   ##  ####### ##  ## ##  ## ####  ##     ##
##   ##   ##  ## # ## ##  ## ## ##  ##    ##     ##
##  ####  ##  ##   ##  ####  ####   ##### #####  ##
##        ##                                     ##
###################################################
###################################################


################################################### 
## 1.1 MODEL   
##Linear model
abline(lm(AB_data[,1]~AB_data[,2]+0),col="blue",lty=2)

##Gompertz FUNCTION
a=0.9900000; b=-2.0454663; c= -0.4158159
points((a * exp (b * exp(c *  data$oocysts)))~data$oocysts,col="red",pch=19)


par_MC <- c( 0.03449)  ## (alpha)

model_M1 <- function(a, par)
{
  alpha <- par[1]
  #rr    <- par[2]
  #sigma <- par[3]
  
  AB_sporozoites   <- alpha
  AB_sporozoites
}


loglike_M1 <- function( par )
{
  alpha <- par[1]
  rr    <- par[2]
  sigma <- par[3]
  
  AB_model <- model_M1( AB_data[,1], par )
  
  mu <- log(AB_model) 
  
  loglike <- - log(AB_data[,2]) - log(2.506628*sigma) - 0.5*( (log(AB_data[,2])-mu)/sigma )^2
  
  sum( loglike )
}


###################################################
## 1.3 PRIOR

prior_M1 <- function( par )
{
  alpha <- par[1]
  rr    <- par[2]
  sigma <- par[3]
  
  ######################################
  ## Uniform prior on alpha ~ U(0,100)
  
  if( alpha>0 && alpha<100 )
  {
    prior_alpha <- 1/100
  }else{
    prior_alpha <- -1e6
  }
  
  ######################################
  ## Uniform prior on rr ~ U(0,10)
  
  if( rr>0 && rr<10 )
  {
    prior_rr <- 1/10
  }else{
    prior_rr <- -1e6
  }
  
  ######################################
  ## Uniform prior on sigma ~ U(0,10)
  
  if( sigma>0 && sigma<10 )
  {
    prior_sigma <- 1/10
  }else{
    prior_sigma <- -1e6
  }
  
  prior <- prior_alpha + prior_rr + prior_sigma
  
  prior
}


#################################################
#################################################
##          ##                                 ##
##   ####   ##  #     #  ####  #     #  ####   ##
##  ##  ##  ##  ##   ## ##  ## ##   ## ##  ##  ##
##     ##   ##  ####### ##     ####### ##      ##
##    ##    ##  ## # ## ##  ## ## # ## ##  ##  ##
##   #####  ##  ##   ##  ####  ##   ##  ####   ##
##          ##                                 ##
#################################################
#################################################


N_mcmc  = 10000      ## Number of MCMC iterations


#################################################
## 2.1 Robbins-munro step scaler

N_adapt     <- N_mcmc/10
step_scale  <- 1
MCMC_accept <- 0


rm_scale <- function(step_scale, mc, log_prob)
{
  dd <- exp(log_prob)
  if( dd < -30 ){ dd <- 0 }
  dd <- min( dd, 1 )
  
  rm_temp <- ( dd - 0.23 )/( (mc+1)/(0.01*N_adapt+1) )
  
  out <- step_scale*exp(rm_temp)
  
  out <- max( out, 0.05 )
  out <- min( out, 5)
  out
}


#################################################
## 2.2 Prepare object for MCMC fitting

MCMC_par           <- matrix(NA, nrow=N_mcmc, ncol=4)
colnames(MCMC_par) <- c("alpha", "rr", "sigma", "loglike")


#########################################################
## 2.3 Implement MCMC iterations

par_MC <- c(1, 0.1, 0.5)  ## (alpha, rr, sigma)

Sigma_MC <- rbind( c(0.0049203245, 3.320142e-04, 1.090574e-04),
                   c(0.0003320142, 2.999677e-05, 7.919239e-06),
                   c(0.0001090574, 7.919239e-06, 9.645573e-04) )

loglike_MC <- loglike_M1( par_MC ) + prior_M1( par_MC )



for(mc in 1:N_mcmc)
{
  par_MCp1 <- mvrnorm(n=1, mu=par_MC, Sigma=step_scale*Sigma_MC)
  
  
  if( par_MCp1[1] > 0 &&
        par_MCp1[2] > 0 &&
        par_MCp1[3] > 0  ){
    
    loglike_MCp1 <- loglike_M1( par_MCp1 ) + prior_M1( par_MCp1 )
    
    
    log_prob <- min( loglike_MCp1-loglike_MC, 0 )           
    
    if( log(runif(1)) < log_prob ) 
    {
      par_MC <- par_MCp1
      
      loglike_MC  <- loglike_MCp1
      MCMC_accept <- MCMC_accept + 1                       
    }
    
    if( mc < N_adapt ){
      step_scale <- rm_scale( step_scale, mc, log_prob)
    }
  }
  
  MCMC_par[mc,1:3] <- par_MC
  MCMC_par[mc,4]   <- loglike_MC
}


#########################################################
## 2.4 Examine MCMC chains


par(mfrow=c(2,2))



#####################################
## PANEL 1: alpha MCMC chain

plot(x=1:N_mcmc, y=MCMC_par[,1], 
     pch=19, col="grey", cex=0.25,
     xlab="MCMC iteration", ylab="alpha", 
     main="alpha")




#####################################
## PANEL 2: rr MCMC chain

plot(x=1:N_mcmc, y=MCMC_par[,2], 
     pch=19, col="grey", cex=0.25,
     xlab="MCMC iteration", ylab="rr", 
     main="rr")




#####################################
## PANEL 3: sigma MCMC chain

plot(x=1:N_mcmc, y=MCMC_par[,3], 
     pch=19, col="grey", cex=0.25,
     xlab="MCMC iteration", ylab="sigma", 
     main="sigma" )




#####################################
## PANEL 4: likelihood

plot(x=1:N_mcmc, y=MCMC_par[,4], 
     pch=19, col="grey", cex=0.25,
     xlab="MCMC iteration", ylab="likelihood", 
     main="likelihood" )







#########################################################
## 2.5 Examine posterior distribution

MCMC_burn <- MCMC_par[floor(0.2*nrow(MCMC_par)):(nrow(MCMC_par)-1),]


par(mfrow=c(1,3))


#####################################
## PANEL 1: alpha MCMC posterior


DEN <- density( MCMC_burn[,1] )

QUANT <- quantile( MCMC_burn[,1], prob=c(0.025, 0.5, 0.975) )

plot(x=DEN$x, y=DEN$y, type='l',
     xlim=c(0, max(DEN$x)),
     xlab="alpha", ylab="", 
     main="posterior: alpha" )


low_index  = which(DEN$x<QUANT[1])
mid_index  = intersect( which(DEN$x>=QUANT[1]), which(DEN$x<=QUANT[3]) )
high_index = which(DEN$x>QUANT[3])

polygon( x=c( DEN$x[low_index], rev(DEN$x[low_index]) ),
         y=c( rep(0,length(low_index)), rev(DEN$y[low_index]) ), 
         col="pink")

polygon( x=c( DEN$x[mid_index], rev(DEN$x[mid_index]) ),
         y=c( rep(0,length(mid_index)), rev(DEN$y[mid_index]) ), 
         col="grey")

polygon( x=c( DEN$x[high_index], rev(DEN$x[high_index]) ),
         y=c( rep(0,length(high_index)), rev(DEN$y[high_index]) ), 
         col="pink")

points(x=rep(QUANT[2],2), y=c(0,max(DEN$y)), type='l', lty="dashed", lwd=2)





#####################################
## PANEL 2: rr MCMC posterior


DEN <- density( MCMC_burn[,2] )

QUANT <- quantile( MCMC_burn[,2], prob=c(0.025, 0.5, 0.975) )

plot(x=DEN$x, y=DEN$y, type='l',
     xlim=c(0, max(DEN$x)),
     xlab="rr", ylab="", 
     main="posterior: rr" )


low_index  = which(DEN$x<QUANT[1])
mid_index  = intersect( which(DEN$x>=QUANT[1]), which(DEN$x<=QUANT[3]) )
high_index = which(DEN$x>QUANT[3])

polygon( x=c( DEN$x[low_index], rev(DEN$x[low_index]) ),
         y=c( rep(0,length(low_index)), rev(DEN$y[low_index]) ), 
         col="pink")

polygon( x=c( DEN$x[mid_index], rev(DEN$x[mid_index]) ),
         y=c( rep(0,length(mid_index)), rev(DEN$y[mid_index]) ), 
         col="grey")

polygon( x=c( DEN$x[high_index], rev(DEN$x[high_index]) ),
         y=c( rep(0,length(high_index)), rev(DEN$y[high_index]) ), 
         col="pink")

points(x=rep(QUANT[2],2), y=c(0,max(DEN$y)), type='l', lty="dashed", lwd=2)




#####################################
## PANEL 3: sigma MCMC posterior


DEN <- density( MCMC_burn[,3] )

QUANT <- quantile( MCMC_burn[,3], prob=c(0.025, 0.5, 0.975) )

plot(x=DEN$x, y=DEN$y, type='l',
     xlim=c(0, max(DEN$x)),
     xlab="sigma", ylab="", 
     main="posterior: sigma" )


low_index  = which(DEN$x<QUANT[1])
mid_index  = intersect( which(DEN$x>=QUANT[1]), which(DEN$x<=QUANT[3]) )
high_index = which(DEN$x>QUANT[3])

polygon( x=c( DEN$x[low_index], rev(DEN$x[low_index]) ),
         y=c( rep(0,length(low_index)), rev(DEN$y[low_index]) ), 
         col="pink")

polygon( x=c( DEN$x[mid_index], rev(DEN$x[mid_index]) ),
         y=c( rep(0,length(mid_index)), rev(DEN$y[mid_index]) ), 
         col="grey")

polygon( x=c( DEN$x[high_index], rev(DEN$x[high_index]) ),
         y=c( rep(0,length(high_index)), rev(DEN$y[high_index]) ), 
         col="pink")

points(x=rep(QUANT[2],2), y=c(0,max(DEN$y)), type='l', lty="dashed", lwd=2)




#############################################
#############################################
##          ##                             ##
##   ####   ##  ###### #####  ###  ######  ##
##  ##  ##  ##    ##   ##    ##      ##    ##
##     ##   ##    ##   ####   ###    ##    ##
##  ##  ##  ##    ##   ##       ##   ##    ##
##   ####   ##    ##   #####  ###    ##    ##
##          ##                             ##
#############################################
#############################################

#############################################
## 3.1 Extract posterior medians and 
##     calculate model prediction


par_median <- apply(X=MCMC_burn[,1:3], MARGIN=2, FUN=median)



age_seq <- seq(from=0, to=60, by=1)

M1_predict <- model_M1(age_seq, par_median )





###############################################
## 3.1 Plot data and model prediction

par(mfrow=c(1,1))


plot(x=age_bins_mid, y=GMT_bins, 
     pch=15, cex=2,
     log="y", xlim=c(0,60), ylim=c(0.1,200),
     xlab="age (years)", ylab="Geometric mean antibody titre", 
     main="Antibody acquisition Model 1 fit"  )


for(i in 1:N_bins)
{
  arrows(x0=age_bins_mid[i], y0=AB_range_bins[i,2], 
         x1=age_bins_mid[i], y1=AB_range_bins[i,3], 
         length=0.03, angle=90, code=3, col="black", lwd=1)	
}


points(x=age_seq, y=M1_predict, 
       type='l', lwd=3, col="blue")


