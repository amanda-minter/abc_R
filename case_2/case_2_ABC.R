
source('case_2_preamble.R')
msl_data <- read.csv(file.path("..", "data", "Measles_data_time.csv"))

# Length of observed outbreak (weeks)
n_obs<-nrow(msl_data)

# Age classes: 0.5-1, 1-2,..., 19-20, 20-100
age<-data.frame(lower=c(0.5, seq(1,20)), upper=c(seq(1,20),100)) 
n_age<-nrow(age)  

#  Observed percentage in three age groups
msl_age_perc<-c(42, 30, 28) 
cl.to.test<-c(1:5, 6:14, 15:21)

#  Latent period
nu<- 7 

# Infectious period
mu<- 7 

# Max duration of an outbreak
t_end<-1000 

# Number of particles
N <- 1000

# Epsilon value for temporal data
epsilon_T <- 10000

# Epsilon value for age data
epsilon_A <- 3

#  Lower and upper boundaries for priors
#  Lower and upper boundaries for priors
lm.low<-c(140*10^3, 0.8, 0.1, 0, 0)
lm.upp<-c(300*10^3, 1.4, 0.4, 5*10^(-6), 0.001)


# Empty matrix to store results (5 model parameters)
res<-matrix(ncol=5,nrow=N)


#Initiate counter
i<-1

while(i <= N){ # While the number of accepted particles is less than N_particles
    
    # Sample from prior distributions 
 	N0_star<- runif(1,min=lm.low[1], max=lm.upp[1])
	age_sh_star<-runif(1, min=lm.low[2], max=lm.upp[2])
	age_rt_star<-runif(1, min=lm.low[3], max=lm.upp[3])
	beta_star<- runif(1, min=lm.low[4], max=lm.upp[4]) 
	f_E_star<- runif(1,min=lm.low[5], max=lm.upp[5]) 
      
    # Simulate data set from the model
    D_star<-run_model(N0_star, age_sh_star, age_rt_star, beta_star, f_E_star)
      
    # Calculate distance  
    distance <- calc_distance(D_star[[1]], D_star[[2]])
      
    if((distance[1] <= epsilon_T) & (distance[2] <= epsilon_A)){ # If both distances are less than their tolerances
      # Store results
      res[i,]<-c(N0_star,age_sh_star,age_rt_star,beta_star,f_E_star)   
      # Update counter
      i <- i+1
    }
}

write.csv(res, file = "results_case_2_ABC_.csv")



