ABC_1_res<-read.csv('ABC_1_res.csv')
ABC_2_res<-read.csv('ABC_2_res.csv')
ABC_3_res<-read.csv('ABC_3_res.csv')

cols<-grey.colors(3)

S_0_xlim<-c(min(ABC_1_res[,"S_0"],ABC_2_res[,"S_0"],ABC_3_res[,"S_0"]),max(ABC_1_res[,"S_0"],ABC_2_res[,"S_0"],ABC_3_res[,"S_0"]))
beta_xlim<-c(min(ABC_1_res[,"beta"],ABC_2_res[,"beta"],ABC_3_res[,"beta"]),max(ABC_1_res[,"beta"],ABC_2_res[,"beta"],ABC_3_res[,"beta"]))
gamma_xlim<-c(min(ABC_1_res[,"gamma"],ABC_2_res[,"gamma"],ABC_3_res[,"gamma"]),max(ABC_1_res[,"gamma"],ABC_2_res[,"gamma"],ABC_3_res[,"gamma"]))
R0_xlim<-c(1,13)

S_0_ylim<-c(0,0.4)
beta_ylim<-c(0,6)
gamma_ylim<-c(0,14)
R0_ylim<-c(0,0.8)

breaks_S0<-seq(S_0_xlim[1],S_0_xlim[2],l=15)
breaks_beta<-seq(beta_xlim[1],beta_xlim[2],l=15)
breaks_gamma<-seq(gamma_xlim[1],gamma_xlim[2],l=15)
breaks_R0<-seq(R0_xlim[1],R0_xlim[2],l=15)

png('posteriors_case_1.png',width=2000,height=2000,units='px',res=300)
par(mfrow=c(3,4),mar=c(4,2,2,2)+0.1)
hist(ABC_1_res[,"S_0"],xlab=expression(S(0)),main='Initial no. susceptibles',col=cols[1],xlim=S_0_xlim,freq=F,ylim=S_0_ylim,breaks=breaks_S0)
abline(v=99,col=4,lty=2,lwd=2)
hist(ABC_1_res[,"beta"],xlab=expression(beta),main='Transmission rate',col=cols[1],xlim=beta_xlim,freq=F,ylim=beta_ylim,breaks=breaks_beta)
abline(v=1.5,col=4,lty=2,lwd=2)
hist(ABC_1_res[,"gamma"],xlab=expression(gamma),main='Recovery rate',col=cols[1],xlim=gamma_xlim,freq=F,ylim=gamma_ylim,breaks=breaks_gamma)
abline(v=0.5,col=4,lty=2,lwd=2)
hist(ABC_1_res[,"beta"]/ABC_1_res[,"gamma"],xlab=expression(R[0]),main='R0',col=cols[1],xlim=R0_xlim,freq=F,ylim=R0_ylim,breaks=breaks_R0)
abline(v=3,col=4,lty=2,lwd=2)
par(mar=c(4,2,1,2)+0.1)
hist(ABC_2_res[,"S_0"],xlab=expression(S(0)),main='',col=cols[2],xlim=S_0_xlim,freq=F,ylim=S_0_ylim)
abline(v=99,col=4,lty=2,lwd=2)
hist(ABC_2_res[,"beta"],xlab=expression(beta),main='',col=cols[2],xlim=beta_xlim,freq=F,ylim=beta_ylim)
abline(v=1.5,col=4,lty=2,lwd=2)
hist(ABC_2_res[,"gamma"],xlab=expression(gamma),main='',col=cols[2],xlim=gamma_xlim,freq=F,ylim=gamma_ylim)
abline(v=0.5,col=4,lty=2,lwd=2)
hist(ABC_2_res[,"beta"]/ABC_2_res[,"gamma"],xlab=expression(R[0]),main='',col=cols[2],xlim=R0_xlim,freq=F,ylim=R0_ylim)
abline(v=3,col=4,lty=2,lwd=2)

hist(ABC_3_res[,"S_0"],xlab=expression(S(0)),main='',col=cols[3],xlim=S_0_xlim,freq=F,ylim=S_0_ylim,breaks=breaks_S0)
abline(v=99,col=4,lty=2,lwd=2)
hist(ABC_3_res[,"beta"],xlab=expression(beta),main='',col=cols[3],xlim=beta_xlim,freq=F,ylim=beta_ylim,breaks=breaks_beta)
abline(v=1.5,col=4,lty=2,lwd=2)
hist(ABC_3_res[,"gamma"],xlab=expression(gamma),main='',col=cols[3],xlim=gamma_xlim,freq=F,ylim=gamma_ylim,breaks=breaks_gamma)
abline(v=0.5,col=4,lty=2,lwd=2)
hist(ABC_3_res[,"beta"]/ABC_3_res[,"gamma"],xlab=expression(R[0]),main='',col=cols[3],xlim=R0_xlim,freq=F,ylim=R0_ylim,breaks=breaks_R0)
abline(v=3,col=4,lty=2,lwd=2)
dev.off()

png('correlation_case_1.png',width=2000,height=1000,units='px',res=300)
par(mfrow=c(1,3))
plot(ABC_1_res[,"beta"],ABC_1_res[,"gamma"])
plot(ABC_2_res[,"beta"],ABC_2_res[,"gamma"])
plot(ABC_3_res[,"beta"],ABC_3_res[,"gamma"])
dev.off()

# Load the data
data <- read.csv(file.path("..", "data", "data.csv"))
source("case_1_preamble.R")

set.seed(20)
###plot of model outbreak
png('model_run_case_1.png',width=2200,height=800,units='px',res=300)
par(mfrow=c(1,3))
plot(data[,1],pch=19,ylab='Number',xlab='Time (day)',col=2,ylim=c(0,max(data)),main='ABC-rejection 1')
legend('topleft',c('Infected','Recovered'),pch=c(19,17),col=c(2,3),bty='n')
for(j in 1:10){
  i<-sample(1000,1)
  sim_data_1<-run_model(S0=ABC_1_res[i,1],beta=ABC_1_res[i,2],gamma=ABC_1_res[i,3])
  lines(sim_data_1[,1],col=cols[1],lwd=2)
  lines(sim_data_1[,2],col=cols[1],lwd=2)
}
points(data[,1],pch=19,col=2)
points(data[,2],pch=17,col=3)


plot(data[,1],pch=19,ylab='Number',xlab='Time (day)',col=2,ylim=c(0,max(data)),main='ABC-rejection 2')
points(data[,2],pch=17,col=3)
#legend('topleft',c('Infected','Recovered'),pch=c(19,17),col=c(2,3),bty='n')
for(j in 1:10){
  i<-sample(1000,1)
  sim_data_2<-run_model(S0=ABC_2_res[i,1],beta=ABC_2_res[i,2],gamma=ABC_2_res[i,3])
  lines(sim_data_2[,1],col=cols[2],lwd=2)
  lines(sim_data_2[,2],col=cols[2],lwd=2)
}
points(data[,1],pch=19,col=2)
points(data[,2],pch=17,col=3)


plot(data[,1],pch=19,ylab='Number',xlab='Time (day)',col=2,ylim=c(0,max(data)),main='ABC-rejection 3')
points(data[,2],pch=17,col=3)
for(j in 1:10){
  i<-sample(1000,1)
  sim_data_3<-run_model(S0=ABC_3_res[i,1],beta=ABC_3_res[i,2],gamma=ABC_3_res[i,3])
  lines(sim_data_3[,1],col=cols[3],lwd=2)
  lines(sim_data_3[,2],col=cols[3],lwd=2)
}
points(data[,1],pch=19,col=2)
points(data[,2],pch=17,col=3)
dev.off()
