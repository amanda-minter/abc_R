# Load in model functions
source("case_1/case_1_preamble.R")

# Read in data file
D <- read.csv("data/data.csv")

# What was the last day of the epidemic
t_final <- 17

#### ABC set up #### 
N <- 1000 # Number of accepted particles
epsilon <- 20 # Epsilon value
n_par <- 3 # How many parameters will be estimated
res <- matrix(ncol = n_par + 1, nrow = N) # Empty matrix to store results
colnames(res)<-c("S_0", "beta", "gamma", "distance")

#### ABC algoirthm ####

#Initiate counter
i <- 1
while(i <= N){ # While the number of accepted particles is less than N_particles
  
  # Sample from prior distributions 
  S0_star <- rpois(1, 100)
  beta_star <- runif(1, 0, 3)
  gamma_star <- runif(1, 0, 1)
  
  # Simulate data set from the model
  D_star <- run_model(S0_star, beta_star, gamma_star)
  
  # Calculate distance  
  distance <- calc_distance(D$I, D_star$I) + calc_distance(D$R, D_star$R)
  
  if(distance <= epsilon){ # If the distance is less than the tolerance
    # Store results
    res[i,] <- c(S0_star, beta_star, gamma_star, distance)
    # Update counter
    i <- i + 1
  }
}

# Save data to csv file
write.csv(res, "ABC_1_res.csv", row.names = F)
