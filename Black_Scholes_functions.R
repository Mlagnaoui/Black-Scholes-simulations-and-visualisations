# Standard Brownian motion (one dimension)

BM <- function(n,plot){
  
  require(ggplot2)
  
  gauss <- c(0,rnorm(n,mean=0,sd=sqrt(1/n)))
  brown <- cumsum(gauss)
  data <- data.frame(time=seq(0,1,by=1/n),B=brown)
  
  if (plot==T) {
    p <-ggplot(data,aes(x=time,y=B))+geom_line()
    print(p)
  }
  return(data)
}

# Diffusion

diffusion <- function(init_value,r,sigm,n,plot){
  
  require(ggplot2)
  
  w <- BM(n,plot=F)[,2]
  h <- seq(0,1,by=1/n) 
  
  S1 <- sigm*w
  S2 <- (r-(sigm^2)/2)*h
  
  data <- data.frame(time = h , S = init_value*exp(S1+S2) ) 
  
  if (plot==T) {
    p <-ggplot(data,aes(x=time,y=S))+geom_line()
    print(p)
  }
  
  return(data)
}

#plot multiple diffusions
plot_MC_diffusion <- function(init_value,r,sigm,n,N_sim){
  
  require(ggplot2)
  
  data <- data.frame(time = seq(0,1,by=1/n) , S = rep(0,n+1))
  p <- ggplot(data,aes(x=time,y=S))
  
  for (i in 1:N_sim){
    data$S <- diffusion(init_value,r,sigm,n,plot = F)[,2]
    names(data[,2]) <- "S"
    
    p <- p + geom_line(data = data)
  }

  print(p)
}

# European call option

eur_options <- function(S_init,K,t,r,sigm){
  a <- (log(S_init/K)+(r-(sigm^2)/2)*t)/(sigm*sqrt(t))
  b <- a + sigm*sqrt(t)
  
  call <- S_init*pnorm(b)-exp(-r*t)*K*pnorm(a)
  put <- exp(-r*t)*K*pnorm(-a)-S_init*pnorm(-b)
  
  return(c(call,put))
}

eur_options(100,103,1,0.01,0.05)
