

# Distance kernel: Power low
K<- function(d, alpha){
	return((d)^(-2*alpha))
} 

run_model<-function(alpha, beta){
	inf<-data.frame(id=inf0, day=rep(0,length(inf0)))
	K1<-K(d, alpha)  
	diag(K1) <- 0  
	t<-1
	while(t<= 365){
		lambda <-as.numeric((beta/365)* rowSums(K1[,inf$id]))
		lambda[inf$id]<-0 
		u<-runif(length(lambda))
		new.inf<-which(u<1-exp(-lambda))
		if(length(new.inf)>0) inf<-rbind(inf, data.frame(id=new.inf, day=t))
		if(nrow(inf)==nrow(data)) t<-365
		t<-t+1
	}
	return(inf[inf$day>0,])
}

# The distance measure
calc_distance <- function(inf){
	if(nrow(inf)>0) {
		near.dist.sim<-sapply(1:nrow(inf), function(a) min(as.numeric(d[inf$id[a], inf0])))
	}  else {
		near.dist.sim<-0
	}
	n.sim<-sapply(1:length(uniq.dist), function(a) length(which(near.dist.sim==uniq.dist[a])))
	return(sqrt(sum((n.obs-n.sim)^2)))
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
