

# Distance kernel: Power low
K<- function(d, alpha){
	return((d)^(-2*alpha))
} 

run_model<-function(alpha, beta){
	inf<-rep(0, nrow(data))
	inf[inf0]<-1
	K1<-K(d, alpha)  
	diag(K1) <- 0  
	lambda <-as.numeric(beta* rowSums(K1[,inf0]) )
	lambda[inf0]<-0 
	u<-runif(length(lambda))
	new.inf<-which(u<1-exp(-lambda))
	if(length(new.inf)>0) inf[new.inf]<-2
	return(inf)
}

calc_distance <- function(obs, sim){
	abs(length(obs[obs==2])-length(sim[sim==2]))
}

# Perturbation kernel 
rK <- function(mean, sigma, min, max){   
	return(rtmvnorm(1, mean=mean, sigma=sigma, lower=min, upper=max)) 
}

		   #  Identity function: H(x)= 1 if x=T
H <- function(x) as.numeric(x>0)

#  Test if prior is non zero
prior.non.zero<-function(par){
	prod(sapply(1:length(par), function(a) H(par[a]-lm.low[a])* H(lm.upp[a]-par[a])))
}
		    
Norm.Eucl.dist<-function(p1,p2){
	sqrt(sum(((p1-p2)/(lm.upp-lm.low))^2)) }

#  Covariance based on M neighbours
getSigmaNeighbours<-function(M, theta, Theta){
	dist<- sapply(1:N, function(a) Norm.Eucl.dist(as.numeric(theta), as.numeric(Theta[a,])))
	temp<-data.frame(no=seq(1,N), dist)
	temp<-temp[order(temp$dist),]
	sigma<-cov(Theta[temp$no[1:(M+1)],])
	return(sigma)
}
