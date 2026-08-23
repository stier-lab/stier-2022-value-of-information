#########
##Value of Information
#########

years = 20 #time of simulation
delta<-.05 # discount rate
process.noise = 0 #temporally uncorrelated variance (sd) in little r 
p = 10 #price per unit biomass
c=200 # cost to achieve F
# parameterize the model based on A,BMSY and MSY
A = 10  #allee effect threshold 

Bmsy<- 70
MSY<-25
K<-(3*Bmsy^2 - 2*A*Bmsy)/( 2*Bmsy-A)
r<-MSY/(Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K))
Fmsy<-MSY/Bmsy
max.F<-Fmsy
B.lim<-max(A,20) # lower biomass limit for harvest control rule

#############################################################
est.NPV<-function(years,K,A,r,phi.CV,delta,process.noise,p,B.start,B.lim,max.F,phi.CV.seed,process.noise.seed,c){
  # Figure out reference points
  Bmsy<- A/3 + K/3 + (A^2 - A*K + K^2)^(1/2)/3
  MSY<-r*Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K)
  Fmsy<-MSY/Bmsy
  
  B.vec<-rep(NA,years+1)
  Bhat.vec<-rep(NA,years)
  Y.vec<-rep(NA,years)
  phi<-Bmsy*phi.CV
  B.vec[1]<-B.start
  # Get observation errors
  set.seed(phi.CV.seed)
  B.errors<-exp(rnorm(years,mean=(0-phi.CV^2/2),sd=phi.CV))
  
  # get process errors
  set.seed(process.noise.seed)
  process.errors<-rnorm(years,mean=0,sd=process.noise)
  ## Solve for harvest control rule parameters
  
  Fo<- -(max.F*B.lim)/(Bmsy - B.lim)
  b<-max.F/(Bmsy - B.lim)
  
  for (i in 1:years){
    Byear<-B.vec[i]
    Bhat.vec[i]<-Byear*B.errors[i]
    
    # Determine F based on Harvest Control Rule
    if (Bhat.vec[i]>=Bmsy) F<-max.F
    if (Bhat.vec[i]<=Bmsy) F<-Fo+b*Bhat.vec[i]
    if (Bhat.vec[i]<=B.lim) F<-0
    Y.vec[i]<-min(F*Bhat.vec[i],Byear)
    production<-r*(1-Byear/K)*(Byear/K-A/K)+rnorm(1,mean=0,sd=process.noise)
    B.vec[i+1]<-max(0.1,Byear+Byear*production-Y.vec[i])
  }
  Value<-Y.vec*p-c*Y.vec/B.vec[1:years]
  discount.vec<-1/((1+delta)^seq(0,(years-1)))
  NPV<-sum(Value*discount.vec)
  return(list(NPV=NPV,Y=Y.vec,B=B.vec,Bhat=Bhat.vec))
}

################################################

repeat.model<-function(n.iters=100,B.start,B.lim,lowCV=0.05,highCV=0.1,years,K,A,r,delta,process.noise,p,max.F,c,phi.seeds,process.seeds){
  return.invest<-rep(NA,n.iters)
phi.CV.seed.save<-rep(NA,n.iters)
for (i in 1:n.iters){
  
  phi.CV.seed<-phi.seeds[i]
  process.noise.seed<-process.seeds[i]

  phi.CV.seed.save[i]<-phi.CV.seed
  
model.output.lowCV<-est.NPV(years,K,A,r,phi.CV=lowCV,delta,process.noise,p,B.start,B.lim,max.F,phi.CV.seed,process.noise.seed,c)

model.output.highCV<-est.NPV(years,K,A,r,phi.CV=highCV,delta,process.noise,p,B.start,B.lim,max.F,phi.CV.seed,process.noise.seed,c)

return.invest[i]<-(model.output.lowCV$NPV-model.output.highCV$NPV)
}
#print(c(length(unique(return.invest)),length(unique(phi.CV.seed.save))),zero.print=".")
return.invest<-unique(return.invest)
# Remove outliers
outlier.index<-which(abs(scale(return.invest))>=3)
result<-ifelse(length(outlier.index>1),mean(return.invest[-outlier.index],na.rm=T),mean(return.invest,na.rm=T))
return(result)

}

start.B.list<-seq(10,100,by=1)

Return.invest.out<-rep(NA,length(start.B.list))
n.iters=500
rm(.Random.seed)
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)
for (i in 1:length(start.B.list)){
  
  Return.invest.out[i]<-repeat.model(n.iters,B.start=start.B.list[i],B.lim,lowCV=0.05,highCV=0.1,years,K,A,r,delta=.05,process.noise=0.0,p,max.F,c,phi.seeds,process.seeds)
  
}
par(las=1,mfrow=c(1,1))
plot(start.B.list,Return.invest.out,type="l",col="black",lwd=2,xlab="Initial Biomass",ylab="Return on Investment",xlim=c(10,100))
abline(v=A,lwd=2,lty="dotted")
