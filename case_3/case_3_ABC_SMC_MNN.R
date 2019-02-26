library(tmvtnorm)
 
source('case_3_preamble.R')
data <- read.csv(file.path("..", "data", "citrus_tristeza_data.csv"))
# Status of data classified as: 0-  susceptable, 1 - infected in 1981; 2 - infected in 1982
#  Indexes of infected trees
inf0<-data$id[data$status==1] 

# Number of neighbours for covariance matrix calculations
M <- 50

# Number of particles
N <- 1000

# Number of simulations for each parameter set
n <- 1

# Thresholds
epsilon<- c(40, 30, 20, 10, 5, 3, 1)

# Number of populations
T <- length(epsilon)

#  Lower and upper boundaries for priors
lm.low<-c(0.01, 0)
lm.upp<-c(5, 10)

# Empty matrices to store results (population plus 5 model parameters)
res.old<-matrix(ncol=2,nrow=N)
res.new<-matrix(ncol=2,nrow=N)

# Empty vectors to store weights
w.old<-rep(NA, N)
w.new<-rep(NA, N)

# Calculate the distances between trees
xy <- cbind(data$x, data$y)
d<- as.matrix(dist(xy, method = 'euclidean', diag = TRUE, upper = TRUE))


for(t in 1:T){  

	#Initiate counter
	i<-1	
	while(i <= N){ # While the number of accepted particles is less than N_particles
   		if(t==1){
    			# Sample from prior distributions 
 			alpha<-runif(1,min=lm.low[1], max=lm.upp[1])
			beta<-runif(1, min=lm.low[2], max=lm.upp[2])
		} else {
			#  Select particle from previous generation
			p<-sample(seq(1,N),1,prob=w.old)		
			sigma<-Sigma[[p]]
			par<- rK(as.numeric(res.old[p,]),sigma, lm.low, lm.upp)
			alpha<-par[1]
			beta<-par[2]
		}
      	#  Test if prior non zero
      	if(prior.non.zero(c(alpha,beta))) {
    			# Set number of accepted simulations to zero
    			m<-0
    			distance <-rep(NA,n)
    			for(j in 1:n){
    				D_star<-run_model(alpha, beta)     
    				# Calculate distances 
    				dist<-calc_distance(data$status, D_star)
    				distance[j] <-dist    
    				if(dist <= epsilon[t]){ # If distance is less than tolerance
    					m<-m+1
    				}
    			}	
    			if (m>0){
    				# Store results
    				res.new[i,]<-c(alpha, beta)  
      			# Calculate weights
      			w1<-prod(sapply(1:2, function(b) dunif(res.new[i,b], min=lm.low[b], max=lm.upp[b])))
				if(t==1){
					w2<-1
				} else {
					w2<-sum(sapply(1:N, function(a) w.old[a]* dtmvnorm(res.new[i,], mean=res.old[a,], sigma=sigma, lower=lm.low, upper=lm.upp)))
				}
      			w.new[i] <- (m/n)*w1/w2
      			# Update counter
      			i <- i+1
      			print(paste0('Population: ', t, ", particle: ", i))
      			}
    		} 
    	}
    	Sigma <- list(NA, N)
    for(p in 1:N){
    	Sigma[[p]]<- getSigmaNeighbours(M, res.new[p,], res.new) 
    }
    	res.old<-res.new
	w.old<-w.new/sum(w.new)

 	write.csv(res.new, file = paste("results_case_3_ABC_SMC_MNN_pop_",t,".csv",sep=""), row.names=FALSE)
}
