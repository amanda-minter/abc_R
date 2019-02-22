
############################################################################
###  Plot parameter histograms and PCA results for ABC rejections algorithm
############################################################################

library("factoextra")
library(ggpubr) 
library(ggplot2) 

ABC_res<-read.table('parameters_case_2_ABC.txt', header=T)
colnames(ABC_res)[2:3]<-c("a_sh","a_rt")

plot.hist<- function(x, xlab){
	qplot(x, xlab=xlab, geom="histogram", bins=25, fill=I("#AEAEAE"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position="none") 
}

h1<- plot.hist(ABC_res[,1], expression(N0))
h2<- plot.hist(ABC_res[,2], expression(a_sh))
h3<- plot.hist(ABC_res[,3], expression(a_rt)) 
h4<- plot.hist(ABC_res[,4], expression(beta)) 
h5<- plot.hist(ABC_res[,5], expression(f_E)) 

res.pca <- prcomp(ABC_res, scale = TRUE)
fig.pca<- fviz_pca_var(res.pca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE, title="")

pca.var<-get_pca_var(res.pca)
head(pca.var$contrib)

png('posteriors_case_2_ABC.png',width=2000,height=1000,units='px',res=300) 

ggarrange(h1,h2,h3,h4,h5,fig.pca, labels = c("A", "B", "C", "D", "E", "F"),ncol = 3, nrow = 2)

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

png('model_run_case_2_ABC.png',width=2000,height=1000,units='px',res=300)

par(mfrow=c(1,2)) 

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

############################################################################
###  Plot parameters for ABC-SMC algorithm
############################################################################

ABC_SMC_res<-read.table('parameters_case_2_ABC_SMC.txt', header=T)

col<-matrix(c(0, 0, 0, 254, 254, 203, 254, 236, 159, 253, 216, 117, 253, 177, 75, 252, 140, 59, 251, 77, 41, 226, 25, 27, 188, 0, 37, 0, 0, 255)/255, nrow=10, ncol=3,byrow = TRUE)

png('parameters_case_2_ABC_SMC.png',width=2000,height=1000,units='px',res=300) 

par(mfrow=c(1,2))

x<-ABC_SMC_res[ABC_SMC_res$population==1,]
plot(x$N0, x$beta, col=rgb(col[[1,1]], col[[1,2]],col[[1,3]]), xlab=expression(N_0),ylab=expression(beta),main='')
for (t in 2:10){
	x<-ABC_SMC_res[ABC_SMC_res$population==t,]
	points(x$N0, x$beta, col=rgb(col[[t,1]], col[[t,2]],col[[t,3]]))
}

x<-ABC_SMC_res[ABC_SMC_res$population==1,]
plot(x$age_sh, x$age_rt, col=rgb(col[[1,1]], col[[1,2]],col[[1,3]]), xlab=expression(a_sh), ylab=expression(a_rt),main='')
for (t in 2:10){
	x<-ABC_SMC_res[ABC_SMC_res$population==t,]
	points(x$age_sh, x$age_rt, col=rgb(col[[t,1]], col[[t,2]],col[[t,3]]))
}

dev.off()
