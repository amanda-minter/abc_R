

run_model<-function(N0, age_sh, age_rt, beta, f_E){
	
	#  Transmission betweem age classes
	A<-(2/3)*matrix(1L,nrow=n_age, ncol=n_age)
	for(i in 1:n_age){
		A[[i,i]]<-1
		if(i<n_age){
			for(j in (i+1):n_age){
				if(abs(i-j)==1) { A[[i,j]]<-11/12; A[[j,i]]<-11/12 }
				if(abs(i-j)==2) { A[[i,j]]<-5/6; A[[j,i]]<-5/6 }
				if(abs(i-j)==3) { A[[i,j]]<-3/4; A[[j,i]]<-3/4}
			}
		}
	}
	# Number of individuals in each class
	dist_pop<-c(dgamma(0.5:19.5, shape = age_sh, rate=age_rt), sum(dgamma(20.5:100.5, shape =age_sh, rate=age_rt)))
	dist_pop<-round(N0*dist_pop/sum(dist_pop)) 
	
	# Define the initial conditions 
	t0 <- 0; 
	I0 <- rep(0,n_age)
	E0<-pmax(round(f_E*dist_pop),1)
	R0 <- rep(0, n_age)
	S0<-pmax(dist_pop-I0-E0-R0,1)
	
	t<-t0; S<-S0; E<-E0; I<-I0; R<-R0
	
	cases<-rep(0,t_end)
	cases_age<-rep(0,n_age)
	
	while (t<t_end) {
		t<-t+1
		S_old<-S; E_old<-E; I_old<-I; R_old<-R
		# S->E  
		lambda <- beta*colSums( A * I)
		n.exp<-sapply(1:n_age, function(a) rbinom(1,S[a],1-exp(-lambda[a])))
		S<-pmax(S-n.exp,0); E<-E+n.exp
		cases[t] <- sum(n.exp)
		cases_age <- cases_age+n.exp
		# E->I  
		n.inf<-sapply(1:n_age, function(a) rbinom(1, E_old[a],1-exp(-1/nu)))
		E <- pmax(E-n.inf,0); I <- I+n.inf
		# I->R  
		n.rem<-sapply(1:n_age, function(a) rbinom(1, I_old[a], 1-exp(-1/mu)))
		I <- pmax(I-n.rem,0); R <-R+n.rem
		if (sum(I+E)<= 0) t<-t_end
		if (sum(S)<= 0)  t<-t_end
	}
	# Weekly cases
	wc <- sapply(seq(1,length(cases)-7,7), function(i) sum(cases[i:(i+6)]))
	
	# Cases in three age groups
	ac<-sapply(1:3, function(a) sum(cases_age[cl.to.test[a]]))
	
	return(list(wc, ac))
}

calc_distance <- function(wc, ac){
	sre1<-sqrt(sum((wc[1:(n_obs)]- msl_data$cases[1:n_obs])^2) +  sum((wc[(n_obs+1):length(wc)])^2))
	if(sum(ac)>0) {
		sre2<-sqrt(sum((msl_age_perc-100*ac/sum(ac))^2))
	} else { 	
		sre2<-sqrt(sum((msl_age_perc)^2)) 
	}
  return(c(sre1, sre2))
}

# Perturbation kernel 
rK <- function(mean, sigma){   
	return(rtmvnorm(1,mean=mean, sigma=sigma, lower=lm.low, upper=lm.upp)) 
}

		   #  Identity function: H(x)= 1 if x=T
H <- function(x) as.numeric(x>0)

#  Test if prior is non zero
prior.non.zero<-function(par){
	prod(sapply(1:5, function(a) H(par[a]-lm.low[a])* H(lm.upp[a]-par[a])))
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
