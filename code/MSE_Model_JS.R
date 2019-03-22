#############################################################
#Define the net present value for a fixed monitoring investment 
#############################################################

#below are three models (est.NPV,repeat.model, and repeat.model2)

est.NPV<-function(years,K,A,r,phi.CV.low,phi.CV.high,delta,process.noise,p,B.start,B.lim,B.crit,max.F,phi.CV.seed,process.noise.seed,c){
  
  # Figure out reference points given param inputs for K, A, MSY, and Bmsy
  Bmsy<- A/3 + K/3 + (A^2 - A*K + K^2)^(1/2)/3
  MSY<-r*Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K)
  Fmsy<-MSY/Bmsy
  B.lim<-0.25*Bmsy
  
  B.vec<-rep(NA,years) #Biomass through time # why +1?
  B.vec[1]<-B.start
  Bhat.vec<-rep(NA,years) #Estimated biomass through time
  Bhat.vec[1]<-B.start
  Y.vec<-rep(NA,years) #yield through time
  F.vec<-rep(NA,years)
  #phi<-Bmsy*phi.CV #CV determines precision of Bhat estimate
  
  #B.vec[1]<-B.start #starting biomass
  phi.CV<-rep(NA,years)
  
  # Get observation errors for low and high CV
  set.seed(phi.CV.seed) #observation errors depend on seed
  B.errors.low<-exp(rnorm(years,mean=(0-phi.CV.low^2/2),sd=phi.CV.low)) #lognormal observation error
  B.errors.high<-exp(rnorm(years,mean=(0-phi.CV.high^2/2),sd=phi.CV.high)) #lognormal observation error
  
  # get process errors
  set.seed(process.noise.seed) 
  process.errors<-rnorm(years,mean=0,sd=process.noise)
  
  ## Solve for harvest control rule parameters
  Fo<- -(max.F*B.lim)/(Bmsy - B.lim)
  b<-max.F/(Bmsy - B.lim)
  
  for (i in 1:years){
    
    # MANAGEMENT MODEL: Determine F based on Harvest Control Rule
    if (Bhat.vec[i]>Bmsy) F<- max.F
    if (Bhat.vec[i]<=Bmsy) F<-Fo+b*Bhat.vec[i]
    if (Bhat.vec[i]<=B.lim) F<-0
    
    F.vec[i]<-F/max.F
    
    Y.vec[i]<-min(F*Bhat.vec[i],B.vec[i]) # prevents harvesting more biomass than is available
    
    # OPERATING MODEL: determines the true biomass fluctions through time
    production<-r*(1-B.vec[i]/K)*(B.vec[i]/K-A/K)+rnorm(1,mean=0,sd=process.noise) #production happens pre F
    B.vec[i+1]<-max(0.1,(B.vec[i]+B.vec[i]*production-Y.vec[i]))
    
    # SAMPLING MODEL
    #Bhat.vec[i]<-Byear*B.errors[i] old: only works with single cv  #should be i+1?
    
    #Sampling model where monitoring continues to increase even if crash happens  As added +1 to i so that 
    #the error int eh second year is a funciton of the new biomass not the old 
    Bhat.vec[i+1]<-ifelse(Bhat.vec[i]<B.crit,
                          B.vec[i+1]*B.errors.low[i],
                          B.vec[i+1]*B.errors.high[i])
    
    #Sampling model where monitoring is high in window between B.lim and B.crit
    
    #     if(Bhat.vec[i]>B.crit) Bhat.vec[i+1] <- B.vec[i]*B.errors.high[i]
    #     if(Bhat.vec[i]<B.crit) Bhat.vec[i+1] <- B.vec[i]*B.errors.low[i]
    #     if(Bhat.vec[i]<B.lim) Bhat.vec[i+1] <- B.vec[i]*B.errors.high[i]
    
    
    #RECORD CV for each time step for Adaptive CV of Monitoring 
    phi.CV[i+1] <- ifelse(Bhat.vec[i]<B.crit,
                          phi.CV.low,
                          phi.CV.high)
    
    
    #     if(Bhat.vec[i]>B.crit) phi.CV[i+1] <-phi.CV.high
    #     if(Bhat.vec[i]<B.crit) phi.CV[i+1] <-phi.CV.low
    #     if(Bhat.vec[i]<B.lim)  phi.CV[i+1] <-phi.CV.high
    
    #Byear <- ifelse(i>1,B.vec[i],B.start) #starting biomass
    #start simulation with set initial deviation based on initial value 
    #Bhat.vec[1] <- ifelse(B.start>B.crit,sample(B.errors.high,1)*B.start,sample(B.errors.low,1)*B.start) 
    
  }
  
  # RESPONSE VARIABLES
  Value<-(Y.vec*p)-(c*Y.vec/B.vec[1:years])
  discount.vec<-1/((1+delta)^seq(0,(years-1)))
  NPV<-sum(Value*discount.vec)
  BB<- sum(length(which(B.vec<=(.25*median(B.vec)))),length(which(B.vec>=(2.25*median(B.vec)))))/length(B.vec) #Freq Bonanza or Bust years
  #TP <- length(which(B.vec[years]<A))/ length(B.vec) #did the final biomass dip below A 
  TP <- ifelse(B.vec[years]>A,0,1)
  TPBMSY<-ifelse(B.vec[years]>(0.25*Bmsy),0,1)
  phi.CV <-phi.CV
  
  moncost<-sum(ci*exp(-cs*phi.CV),na.rm=T)
  #moncost <-sum(cm/phi.CV,na.rm=T) #monitoring cost is the cm constatn (just a random number) divided by the cv. 
  #moncost<-sum(-10*cm*phi.CV+50,na.rm=T) #to show that adaptive monitoring is same price when linear
  return(list(NPV=NPV,Y=Y.vec,B=B.vec,Bhat=Bhat.vec,BB=BB,TP=TP,TPBMSY=TPBMSY,phi.CV=phi.CV,cost.monitor=moncost,pF=F.vec))
}


######
# Test est.NPV function 
######

source("code/ModelParameters_v1.R") # load base parameters

mf0.5<-max.F*0.5
mf1.3<-max.F*1.3
mf1.9<-max.F*1.9
phi.CV.low=phi.CV.high=0.0
A=10
B.start=85

t<-est.NPV(years,K,A,r,phi.CV.low,phi.CV.high,delta,process.noise,p,B.start,B.lim,B.crit,max.F=mf1.9,phi.CV.seed,process.noise.seed,c)
print(t)


plot(t$phi.CV[-1])

#Test
sum(ci*exp(-cs*t$phi.CV),na.rm=T)

#test cost function for monitoring
cvec=seq(0.05,0.5,by=0.01)

cm2=ci*exp(-cs*cvec)
plot(cvec,cm2,type="l")



#############################################################
#Repeat model (repeat.model2) that iterates est.npv and extracts NPV, Fraction of "Crashes" (FC), and prop tipped (TP) 
#############################################################

repeat.model2<-function(n.iters,B.start,B.lim,years,K,A,r,phi.CV,delta,process.noise,p,max.F,phi.seeds,process.seeds){
  value<-rep(NA,n.iters)
  BB<-rep(NA,n.iters) #Boom and Bust
  TP<-rep(NA,n.iters) #tipping points
  TPBMSY<-rep(NA,n.iters)#tipping over Bmsy
  dB<-rep(NA,n.iters) #Deviation of estimation from true biomass
  B<-rep(NA,n.iters) #Biomass
  Y<-rep(NA,n.iters) #yield
  phi.CV<-rep(NA,n.iters)
  cost.monitor<-rep(NA,n.iters)
  NPV_minusCM <-rep(NA,n.iters)
  pFmax<-rep(NA,n.iters)
  
  phi.CV.seed.save<-rep(NA,n.iters)
  
  
  for (i in 1:n.iters){
    
    phi.CV.seed<-phi.seeds[i]
    process.noise.seed<-process.seeds[i]
    
    phi.CV.seed.save[i]<-phi.CV.seed
    
    model.output<-est.NPV(years,K,A,r,phi.CV.low,phi.CV.high,delta,process.noise,p,B.start,B.lim,B.crit,max.F,phi.CV.seed,process.noise.seed,c)
    
    
    value[i] <- model.output$NPV #note this is different than value above
    BB[i] <-model.output$BB 
    TP[i] <-model.output$TP
    TPBMSY[i]<-model.output$TPBMSY
    dB[i] <-median(abs(model.output$B/model.output$Bhat))
    B[i] <-mean(model.output$B)
    Y[i] <-median(model.output$Y)
    phi.CV[i] <-mean(model.output$phi.CV,na.rm=T)
    cost.monitor[i] <- model.output$cost.monitor
    NPV_minusCM[i] <-model.output$NPV-model.output$cost.monitor
    pFmax[i]<-max(model.output$pF)
  }
  
  return(list(value=value,BB=BB,TP=TP,TPBMSY=TPBMSY,dB=dB,B=B,Y=Y,phi.CV=phi.CV,cost.monitor=cost.monitor,NPV_minusCM=NPV_minusCM,pFmax=pFmax))
}




############################################################
# Test repeat.model2 
############################################################

start.B.list<-seq(20,100,by=1)

#Set up iterations for repeat model
n.iters=100
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

A=10
Bmsy<- A/3 + K/3 + (A^2 - A*K + K^2)^(1/2)/3 #Biomass at MSY
B.lim<-max(A,0.25*Bmsy) # lower biomass limit for harvest control rule 
MSY<-r*Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K) #MSY
Fmsy<-MSY/Bmsy #Fishing mortality that produces MSY


mf0.5<-Fmsy*0.5
mf1.3<-Fmsy*1.3
mf1.5<-Fmsy*1.5
mf2.0<-Fmsy*2.0
phi.CV.low=phi.CV.high=0.0

#not that ptip is very dependent upon the starting density 

value = repeat.model2(n.iters,B.start=70,B.lim,years,K,A,r,phi.CV,delta=.05,process.noise,p,max.F=mf0.5,phi.seeds,process.seeds)
return.value<-median(c(value[[1]]))
return.BB<-median(c(value[[2]]))
return.TP<-sum(value[[3]])/n.iters #fraction of the replicate runs where the population dips below A 
return.TPMGMT<-sum(value[[4]])/n.iters
return.dB<-value[[5]]
return.B <-value[[6]]
return.cm<-value[[9]]
value



##########################################################################################################################
#Graveyard
##########################################################################################################################




# #############################################################
# #OLD Repeat Model: Repeat that calculates theh deviation of low and high investment monitoring investment record NPV
# #############################################################
# 
# repeat.model<-function(n.iters=100,B.start,B.lim,lowCV=0.05,highCV=0.1,years,K,A,r,delta,process.noise,p,max.F,c,phi.seeds,process.seeds){
#   return.invest<-rep(NA,n.iters)
#   phi.CV.seed.save<-rep(NA,n.iters)
#   
#   for (i in 1:n.iters){
#     
#     phi.CV.seed<-phi.seeds[i]
#     process.noise.seed<-process.seeds[i]
#     
#     phi.CV.seed.save[i]<-phi.CV.seed
#     
#     model.output.lowCV<-est.NPV(years,K,A,r,phi.CV=lowCV,delta,process.noise,p,B.start,B.lim,max.F,phi.CV.seed,process.noise.seed,c)
#     model.output.highCV<-est.NPV(years,K,A,r,phi.CV=highCV,delta,process.noise,p,B.start,B.lim,max.F,phi.CV.seed,process.noise.seed,c)
#     
#     return.invest[i]<-(model.output.lowCV$NPV-model.output.highCV$NPV)
#   }
#   
#   #print(c(length(unique(return.invest)),length(unique(phi.CV.seed.save))),zero.print=".")
#   return.invest<-unique(return.invest) #pull out unique values from simulaiton
#   
#   # Remove outliers
#   outlier.index<-which(abs(scale(return.invest))>=3) #identify outliers
#   result<-ifelse(length(outlier.index>1),mean(return.invest[-outlier.index],na.rm=T),mean(return.invest,na.rm=T))
#   return(result)
#   
# }
