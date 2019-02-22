
############################################################################
###  Plot map of susceptible and infected trees
############################################################################

data <- read.csv(file.path("..", "data", "citrus_tristeza_data.csv"))

plot(data$x, data$y, pch=1, cex=1, xaxt='n', yaxt='n', ann=FALSE)
points(data$x[data$status==1], data$y[data$status==1], pch=19, col='grey', cex=1)
points(data$x[data$status==2], data$y[data$status==2], pch=19, col='black', cex=1)

############################################################################
###  Plot posterior and distance kernels
############################################################################

mxd<-10
res1<-read.table("results_case_3_n1.txt", header=T)	
res2<-read.table("results_case_3_n10.txt", header=T)	
res3<-read.table("results_case_3_n100.txt", header=T)


par(mfrow=c(2,3)) 
hist(res1$alpha, 20, xlab=expression(alpha), main="n=1", xlim=c(0.5,1.7), col="#AEAEAE" )
hist(res2$alpha, 20, xlab=expression(alpha), main="n=10", xlim=c(0.5,1.7), col="#AEAEAE" )
hist(res3$alpha, 20, xlab=expression(alpha), main="n=100", xlim=c(0.5,1.7), col="#AEAEAE" )
plot(seq(1, mxd, by=0.01), K(seq(1, mxd, by=0.01), res1$alpha[1]), type='l', xlab='d', ylab="K(d)", col='gray', main="")
for(i in 2:N){
	points(seq(1,mxd, by=0.01), K(seq(1,mxd, by=0.01), res1$alpha[i]), type='l', col='gray')
}
plot(seq(1, mxd, by=0.01), K(seq(1, mxd, by=0.01), res2$alpha[1]), type='l', xlab='d', ylab="K(d)", col='gray', main="")
for(i in 2:N){
	points(seq(1,mxd, by=0.01), K(seq(1,mxd, by=0.01), res2$alpha[i]), type='l', col='gray')
}
plot(seq(1, mxd, by=0.01), K(seq(1, mxd, by=0.01), res3$alpha[1]), type='l', xlab='d', ylab="K(d)", col='gray', main="")
for(i in 2:N){
	points(seq(1,mxd, by=0.01), K(seq(1,mxd, by=0.01), res3$alpha[i]), type='l', col='gray')
}
