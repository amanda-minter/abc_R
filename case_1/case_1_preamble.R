library(deSolve)

SIR<- function( t, x, parameters ) { 
  ##Inputs:
  #t time 
  #x state variables
  # parameters parameter vector
  S <- x[1]
  I <- x[2]
  R <- x[3]
  N <- S+I+R
  #The with statement means we can use the parameter names as in parameters
  with( as.list(parameters), { 
    dS <- -beta*S*I/N
    dI <- beta*S*I/N-gamma*I
    dR <- gamma*I
    res <- c(dS,dI,dR)
    list(res)
  }
  )
}

run_model<-function(S0,beta,gamma){
  #Function to run model with dede or lsoda
  out<-lsoda(func = SIR,
                        y = c(S=S0,I=1,R=0),
                        parms = c(beta=beta,gamma=gamma),
                        times = seq(1, 17,by=1))
  return(out[,c('I','R')])
}

calc_distance <- function(D, D_star){
  # Define sum of squared errors
  # Vectorised function
  dist <- sqrt(sum((D - D_star)^2))
  return(dist)
}


calc_distance_final_size <- function(mu, mu_star){
  # Define sum of squared errors
  # Vectorised function
  dist <- sqrt((mu - mu_star)^2)
  return(dist)
}


summary_stat<-function(D){
  mu<- tail(D,n=1)
  return(mu)
}



