############################################################################
###  Plot posterior and distance kernels
############################################################################

ABC_res<-read.table('parameters_case_3_ABC.txt', header=T)

set.seed(100) 
source('case_3_preamble.R')
mxd<-10
ind<-sample(1:nrow(ABC_res),100) 

png('Case_study_3_posterior_distr.png',width=4000,height=4000,units='px',res=300)
par(mfrow=c(2,3), cex=1.75, mar=c(4,4,2,2)+0.1) 
hist(ABC_res[,1], 30, col="#AEAEAE", xlab=expression(alpha), main="", ylab="")
hist(ABC_res[,2], 30, col="#AEAEAE", xlab=expression(beta), main="", ylab="")
plot(ABC_res[,1], ABC_res[,2], xlab=expression(alpha), ylab=expression(beta))
plot(seq(1, mxd, by=0.01), K(seq(1, mxd, by=0.01), ABC_res[ind[1],1]), type='l', xlab='d', ylab="K(d)", col='gray', main="", xlim=c(0,mxd), ylim=c(0,1))
for(i in 2:100){
	points(seq(1,mxd, by=0.01), K(seq(1,mxd, by=0.01), ABC_res[ind[i],1]), type='l', col='gray')
}

dev.off()

