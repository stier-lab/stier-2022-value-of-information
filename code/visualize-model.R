#source("code/MSE_Model_JS.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"
source("code/0_libraries.R") #load packages that are relevant
source("code/2_model_parameters.R") # base parameters
source("code/3_mse_model.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"


# quick routine to see one model iteration result
rm(.Random.seed)
phi.CV.seed<-round(100000*runif(1),0)
process.noise.seed<-round(100000*runif(1),0)
A = 10  #allee effect threshold 
delta = .05 # discount rate
process.noise = 0 #temporally uncorrelated variance (sd) in little r 
p = 10 #price per unit biomass
c = 200 # cost to achieve F


Bmsy<- 70
MSY<-25
K<-(3*Bmsy^2 - 2*A*Bmsy)/( 2*Bmsy-A)
r<-MSY/(Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K))
Fmsy<-MSY/Bmsy
max.F<-0.5*Fmsy
B.lim<-20 # lower biomass limit for harvest control rule

B.start<-61
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
par(mfrow=c(1,2))
plot(1:21, model.output.highCV$B,type='n',ylim=c(0,200),ylab='B',xlab = "Year")
title("Bstart = 100")
for(i in 1:20){
  B.start = 100
  phi.CV.seed<-round(100000*runif(1),0)
  process.noise.seed<-round(100000*runif(1),0)
  model.output.highCV <- est.NPV(years,K,A,r,phi.CV.low=0.1,phi.CV.high=0.1,delta,process.noise,p,B.start,B.lim,B.crit,max.F,phi.CV.seed,process.noise.seed,c)
  lines(1:21,model.output.highCV$B,
        col = rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50"))
}
abline(h = A,col='red')
threshold = K/2
abline(h = threshold,col='red',lty=2)

B.start = 100 #start below A
plot(1:21, model.output.highCV$B,type='n',ylim=c(0,200),ylab='B',xlab = "Year")
title("Bstart = 100, sCV=0.5")
for(i in 1:20){
  phi.CV.seed<-round(100000*runif(1),0)
  process.noise.seed<-round(100000*runif(1),0)
  model.output.highCV <- est.NPV(years,K,A,r,phi.CV.low=0.5,phi.CV.high=0.5,delta,process.noise,p,B.start,B.lim,B.crit,max.F,phi.CV.seed,process.noise.seed,c)
  lines(1:21,model.output.highCV$B,
        col = rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50"))
}
abline(h = A,col='red')
threshold = K/2
abline(h = threshold,col='red',lty=2)


# -------------------------------------------------------------------------
#as slop
  
  for(i in 1:22)
  max.F<-1*Fmsy
  A=30
  plot(1:21, model.output.highCV$B,type='n',ylim=c(0,200),ylab='B',xlab = "Year")
  abline(h = A,col='red')
  threshold = K/2
  abline(h = threshold,col='red',lty=2)
  
  emat<-matrix(0,ncol=5,nrow=20)
  
  for(i in 1:20){
    B.start = 75
    phi.CV.seed<-round(100000*runif(1),0)
    process.noise.seed<-round(100000*runif(1),0)
    model.output.highCV <- est.NPV(years,K,A,r,phi.CV.low=0.5,phi.CV.high=0.5,delta,process.noise,p,B.start,B.lim,B.crit,max.F,phi.CV.seed,process.noise.seed,c)
    lines(1:21,model.output.highCV$B,
          col = rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50"))
    dangerzone(model.output.highCV$B,A=10,thresh=K/2)
    model.output.highCV$TP
    
    # temp_mat <- matrix(NA,nrow=length(model.output.highCV$B)+1,ncol=2)
    # 
    # for(j in 1:length(model.output.highCV$B)){
    #   temp_mat[j+1,1]<-ifelse(model.output.highCV$B[j+1]>threshold & model.output.highCV$B[j]<threshold,1,0) # number of dangers 
    #   temp_mat[j+1,2]<-ifelse(model.output.highCV$B[j+1]<threshold & model.output.highCV$B[j]>threshold,1,0) # number of rescues 
    # }
    
    emat[i,1]<-dangerzone(model.output.highCV$B,A=10,thresh=K/2)
    emat[i,2]<-model.output.highCV$TP
    emat[i,3]<-length(which(model.output.highCV$B <K/2 & model.output.highCV$B >A & model.output.highCV$TP==0)) #this only gives us when collapse but not necessarily recovery
    # emat[i,4]<-colSums(temp_mat,na.rm=T)[2]#num dangerzone recoveries 
    # emat[i,5]<-colSums(temp_mat,na.rm=T)[1]#num dangerzone entries

  }

  #can you come up with a way to say how much more successful you are at rescuing when you are at high levels of monitoring compared to low, for a given fishing effort
  #that'd be something like the ratio of times a time series goes to collapse when it goes to the danger zone compared to getting rescued
  #which of the biomass <K/2 and collapses not sure how to measure that yet gotta think some more 
  
  #maximim fraction dz   
print(max(emat[,1]))
  
  df <- as.data.frame(emat)
colnames(df) <-c("fraction-danger","tipped","num_pop_rescues","num_dangers","num_rescues")
df$frac_rescue <-df$num_rescues/df$num_dangers

mean(df$frac_rescue,na.rm=T)

vec<-rpois(30,5)
evec<-rep(NA,length(vec))
for(i in 1:length(vec)){
  evec[i]<-ifelse(vec[i+1]>4 & vec[i] >4,1,0)
}
    

vec2<-lag(vec)
t_df<-data.frame(vec,vec2)
t_df%>%
  mutate("tip" = 
           case_when(vec>4 & vec2<4 ~ 1)
  )
