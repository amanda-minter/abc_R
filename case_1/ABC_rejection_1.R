# Load in model functions
source('case_1_preamble.R')

# Read in data file
D <- read.csv(file.path("..", "data", "data.csv"))

# What was the last day of the epidemic
t_final<-20

#### ABC set up #### 
# Number of particles
N <- 1000

# Epsilon value
epsilon <- 20

# Empty matrix to store results (3 model parameters and the distance )
res<-matrix(ncol=4,nrow=N)
colnames(res)<-c('S_0','beta','gamma','distance')
# With correct number of rows

#### ABC algoirthm ####

#Initiate counter
i<-1

while(i <= N){ # While the number of accepted particles is less than N_particles
  
  # Sample from prior distributions 
  S0_star<-rpois(1,100)
  beta_star<-runif(1,0,3)
  gamma_star<-runif(1,0.,0.4)
  
  # Simulate data set from the model
  D_star<-run_model(S0_star,beta_star,gamma_star)
  
  # Calculate distance  
  distance <- calc_distance(D[,1], D_star[,1])+calc_distance(D[,2], D_star[,2])
  
  if(distance <= epsilon){ # If the distance is less than the tolerance
    # Store results
    res[i,]<-c(S0_star,beta_star,gamma_star,distance)
    
    # Update counter
    i <- i+1
    print(i)
  }
}
# Save data to csv file
write.csv(res,'ABC_1_res.csv',row.names = F)
