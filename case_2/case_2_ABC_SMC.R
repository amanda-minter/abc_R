# install.packages("tmvtnorm",dependencies=TRUE, repos='http://cran.rstudio.com/')

library(tmvtnorm)
 
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

# Epsilon values for temporal data 
epsilon_T <- c(50000, 27500, 25000, 22500, 20000, 17500, 15000, 12500, 11000, 10000)  

# Epsilon values for age data 
epsilon_A <- c(15, 10, 9, 8, 7, 6, 5, 4, 3.5, 3)

# Number of generations
G <- length(epsilon_T)

# Number of simulations for each parameter set
n <- 1

#  Lower and upper boundaries for priors
lm.low<-c(140*10^3, 0.8, 0.1, 0, 0)
lm.upp<-c(300*10^3, 1.4, 0.4, 5*10^(-6), 0.001)


# Empty matrices to store results (5 model parameters)
res.old<-matrix(ncol=5,nrow=N)
res.new<-matrix(ncol=5,nrow=N)

# Empty vectors to store weights
w.old<-matrix(ncol=1,nrow=N)
w.new<-matrix(ncol=1,nrow=N)

for(g in 1:G){  

    #Initiate counter
    i<-1    
    while(i <= N){ # While the number of accepted particles is less than N_particles
        if(g==1){
                # Sample from prior distributions 
            N0_star<- runif(1,min=lm.low[1], max=lm.upp[1])
            age_sh_star<-runif(1, min=lm.low[2], max=lm.upp[2])
            age_rt_star<-runif(1, min=lm.low[3], max=lm.upp[3])
            beta_star<- runif(1, min=lm.low[4], max=lm.upp[4]) 
            f_E_star<- runif(1,min=lm.low[5], max=lm.upp[5]) 
        } else {
            #  Select particle from previous generation
            p<-sample(seq(1,N),1,prob=w.old)
            par<- rK(res.old[p,],sigma)
            N0_star<-par[1]
            age_sh_star<-par[2]
            age_rt_star<-par[3]
            beta_star<-par[4]
            f_E_star<-par[5]
        }
        #  Test if prior non zero
        if(prior.non.zero(c(N0_star,age_sh_star,age_rt_star,beta_star,f_E_star))) {
                # Set number of accepted simulations to zero
                m<-0
                distance <-matrix(ncol=2,nrow=n)
                for(j in 1:n){
                    D_star<-run_model(N0_star, age_sh_star, age_rt_star, beta_star, f_E_star)     
                    # Calculate distances 
                    calc.dist<-calc_distance(D_star[[1]], D_star[[2]])
                    distance[j,] <-calc.dist    
                    if((calc.dist[1] <= epsilon_T[g]) & (calc.dist[2] <= epsilon_A[g])){ # If both distances are less than their tolerances
                        m<-m+1
                    }
                }   
                if (m>0){
                    # Store results
                    res.new[i,]<-c(N0_star, age_sh_star, age_rt_star, beta_star, f_E_star)  
                # Calculate weights
                w1<-prod(sapply(1:5, function(b) dunif(res.new[i,b], min=lm.low[b], max=lm.upp[b])))
                if(g==1){
                    w2<-1
                } else {
                    w2<-sum(sapply(1:N, function(a) w.old[a]* dtmvnorm(res.new[i,], mean=res.old[a,], sigma=sigma, lower=lm.low, upper=lm.upp)))
                }
                w.new[i] <- (m/n)*w1/w2
                # Update counter
                i <- i+1
                print(paste0('Generation: ', g, ", particle: ", i))
                }
            } 
        }
        sigma <- cov(res.new) 
        res.old<-res.new
    w.old<-w.new/sum(w.new)

    write.csv(res.new, file = paste("results_case_2_ABC_SMC_gen_",g,".csv",sep=""), row.names=FALSE)
}

