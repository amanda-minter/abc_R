
############################################################################
###  Plot parameter histograms and PCA results for ABC rejections algorithm
############################################################################


ABC_res<-read.table('parameters_case_2_ABC.txt', header=T)

png('posteriors_case_2_ABC.png',width=4000,height=3000,units='px',res=300)
par(mfrow=c(2,3), cex=1.75, mar=c(4,2,1,1)+0.1) 
hist(ABC_res[,1], 30, col="#AEAEAE", xlab=expression("N"[0]), main="", ylab="")
hist(ABC_res[,2], 30, col="#AEAEAE", xlab=expression("a"[rt]), main="", ylab="")
hist(ABC_res[,3], 30, col="#AEAEAE", xlab=expression("a"[sh]), main="", ylab="")
hist(ABC_res[,4], 30, col="#AEAEAE", xlab=expression(beta), main="", ylab="")
hist(ABC_res[,5], 30, col="#AEAEAE", xlab=expression("f"[E]), main="", ylab="")
dev.off()



############################################################################
###  Plot observed and simulated data
############################################################################

set.seed(400) 
source('case_2_preamble.R')
msl_data <- read.csv(file.path("..", "data", "Measles_data_time.csv"))
msl_age_perc<-c(42, 30, 28) 
cl.to.test<-c(1:5, 6:14, 15:21)
n_obs<-nrow(msl_data)
age<-data.frame(lower=c(0.5, seq(1,20)), upper=c(seq(1,20),100)) 
n_age<-nrow(age)  
nu<- 7 
mu<- 7 
t_end<-1000 

col<-"#4D4D4D"

png('model_run_case_2_ABC.png', width=4000, height=2000, units='px', res=300)

par(mfrow=c(1,2), cex=2, mar=c(4,2,1,1)+0.1) 

plot(msl_data[,3],pch=20,ylab='Number',xlab='Time (weeks)',col=2, ylim=c(0,11000),main='')

age_gr<-matrix(NA, ncol=3, nrow=10)
for(j in 1:10){   
	i<-sample(1000,1)   
	sim_data<-run_model(ABC_res$N0[i], ABC_res$age_sh[i], ABC_res$age_rt[i], ABC_res$beta[i], ABC_res$f_E[i])
	age_gr[j,]<-100*sim_data[[2]]/sum(sim_data[[2]])
	lines(sim_data[[1]],col=col,lwd=2)   
} 
points(msl_data[,3],pch=20,col=2) 

data<-data.frame(obs=msl_age_perc, sim=sapply(1:3, function(a) mean(age_gr[,a])))
barplot(t(as.matrix(data)), main='', xlab="Age group (years)", ylab = "Percent", beside=TRUE, col=c(2, col), names.arg=c("0.5-5","5-15","16+"))
legend("topright", c("Observed","Simulated"), bty="n",  cex=0.7, fill=c(2, col))

dev.off()