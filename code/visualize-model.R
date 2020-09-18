#source("code/MSE_Model_JS.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"
source("code/0_libraries.R") #load packages that are relevant
source("code/2_model_parameters.R") # base parameters
source("code/3_mse_model.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"


# quick routine to see one model iteration result
rm(.Random.seed)
phi.CV.seed<-round(100000*runif(1),0)
process.noise.seed<-round(100000*runif(1),0)
A = 10  #allee effect threshold 
delta<-.05 # discount rate
process.noise = 0.5 #temporally uncorrelated variance (sd) in little r 
p = 10 #price per unit biomass
c=200 # cost to achieve F


Bmsy<- 70
MSY<-25
K<-(3*Bmsy^2 - 2*A*Bmsy)/( 2*Bmsy-A)
r<-MSY/(Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K))
Fmsy<-MSY/Bmsy
max.F<-Fmsy
B.lim<-20 # lower biomass limit for harvest control rule

B.start<-50
model.output.lowCV<-est.NPV(years,K,A,r,phi.CV.low=0.1,phi.CV.high=0.1,delta,process.noise,p,B.start,B.lim,B.crit,max.F,phi.CV.seed,process.noise.seed,c)
model.output.highCV<-est.NPV(years,K,A,r,phi.CV.low=0.5,phi.CV.high=0.5,delta,process.noise,p,B.start,B.lim,B.crit,max.F,phi.CV.seed,process.noise.seed,c)

model.output.highCV

F.low.CV<-model.output.lowCV$Y/model.output.lowCV$B[-21]
Profit.lowCV<-p*model.output.lowCV$Y-c*F.low.CV

F.high.CV<-model.output.highCV$Y/model.output.highCV$B[-21]
Profit.highCV<-p*model.output.highCV$Y-c*F.high.CV


par(mfrow=c(2,2),las=1,mai=c(1,1.0,0.5,0.5))
plot(1:20,model.output.lowCV$Y,type="l",lwd=2,col="red",ylim=c(0,100),xlab="years",ylab="catch or biomass",yaxs="i")
lines(1:20,model.output.lowCV$B[-21],lwd=2,col="blue")
lines(1:20,model.output.lowCV$Bhat[-21],lwd=2,col="black")

plot(1:20,Profit.lowCV,type="l",lwd=2)


plot(1:20,model.output.highCV$Y,type="l",lwd=2,col="red",ylim=c(0,100),xlab="years",ylab="catch or biomass",yaxs="i")
lines(1:20,model.output.highCV$B[-21],lwd=2,col="blue")
lines(1:20,model.output.highCV$Bhat[-21],lwd=2,col="black")

plot(1:20,Profit.highCV,type="l",lwd=2)

print(paste("Model outputs =",model.output.lowCV$NPV,",",model.output.highCV$NPV))

print(paste("return on investment =",round(model.output.lowCV$NPV-model.output.highCV$NPV)))



btest <- model.output.lowCV$B
# length(which(be[years]<A))/ length(B.vec)


# -------------------------------------------------------------------------
par(mfrow=c(1,1))
plot(1:21, model.output.highCV$B,type='l')
abline(h = A,col='red')
threshold = K/2
abline(h = threshold,col='red',lty=2)

dangerzone(B.vec = model.output.highCV$B,A = A,thresh = threshold)
A

