###Log fit - be sure to use quotes around the variable names in the call

  y <- datprev$prevs
  x <- datprev$meanoocysts
  
  log.ss <- nls(y ~ SSlogis(x, phi1, phi2, phi3))
  
  #C
  C <- summary(log.ss)$coef[1]
  #a
  A <- exp((summary(log.ss)$coef[2]) * (1/summary(log.ss)$coef[3]))
  #k
  K <- (1 / summary(log.ss)$coef[3])
  
  plot(y ~ x + 0, main = "Logistic Function", xlab="meanoocysts", ylab="prevalence")
  lines(0:max(x), predict(log.ss, data.frame(x=0:max(x))), col="red")
  
  r1 <- sum((x - mean(x))^2)
  r2 <- sum(residuals(log.ss)^2)
  
  r_sq <- (r1 - r2) / r1
  
  out <- data.frame(cbind(c(C=C, a=A, k=K, R.value=sqrt(r_sq))))
 

