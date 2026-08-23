#############################################################
#Define the net present value for a fixed monitoring investment 
#############################################################

#below are three models (est.NPV,repeat.model, and repeat.model2)

est.NPV<-function(years,K,A,r,phi.CV,delta,process.noise,p,B.start,B.lim,max.F,phi.CV.seed,process.noise.seed,c){
  
  # Figure out reference points given param inputs for K, A, MSY, and Bmsy
  Bmsy<- A/3 + K/3 + (A^2 - A*K + K^2)^(1/2)/3
  MSY<-r*Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K)
  Fmsy<-MSY/Bmsy
  
  B.vec<-rep(NA,years+1) #Biomass through time
  Bhat.vec<-rep(NA,years) #Estimated biomass through time
  Y.vec<-rep(NA,years) #yield through time
  phi<-Bmsy*phi.CV #CV determines precision of Bhat estimate
  B.vec[1]<-B.start #starting biomass
  
  # Get observation errors
  set.seed(phi.CV.seed) #observation errors depend on seed
  B.errors<-exp(rnorm(years,mean=(0-phi.CV^2/2),sd=phi.CV)) #lognormal observation error
  
#   # attempt to include adaptive monitoring here 
#   B.errors.percent <- runif(years) ########################
#   phi.sd<-sqrt(log(phi.CV^2+1)) ########################
  
  # get process errors
  set.seed(process.noise.seed) 
  process.errors<-rnorm(years,mean=0,sd=process.noise)
  
  ## Solve for harvest control rule parameters
  Fo<- -(max.F*B.lim)/(Bmsy - B.lim)
  b<-max.F/(Bmsy - B.lim)
  
  for (i in 1:years){
    
    Byear<-B.vec[i]
    
    # SAMPLING MODEL
    Bhat.vec[i]<-Byear*B.errors[i]
    
                     
#     #SAMPLING MODEL FOR ADAPTIVE
#     #was an extra t (i) multiplying the sd here . check lognormal error formula
#     B.errors[i]<-exp(qnorm(B.errors.percent[i],mean=-phi.sd^2//2,sd=phi.sd) ########################
#     phi.CV[i]=ifelse(B[i]<B.crit,0.1,0.5)                 ########################
#                      
    # MANAGEMENT MODEL: Determine F based on Harvest Control Rule
    if (Bhat.vec[i]>=Bmsy) F<-max.F 
    if (Bhat.vec[i]<=Bmsy) F<-Fo+b*Bhat.vec[i]
    if (Bhat.vec[i]<=B.lim) F<-0
    
    Y.vec[i]<-min(F*Bhat.vec[i],Byear) 
    
    # OPERATING MODEL: determines the true biomass fluctions through time
    production<-r*(1-Byear/K)*(Byear/K-A/K)+rnorm(1,mean=0,sd=process.noise)
    B.vec[i+1]<-max(0.1,Byear+Byear*production-Y.vec[i])
  }
  
  # RESPONSE VARIABLES
  Value<-(Y.vec*p)-(c*Y.vec/B.vec[1:years])
  discount.vec<-1/((1+delta)^seq(0,(years-1)))
  NPV<-sum(Value*discount.vec)
  BB<- sum(length(which(B.vec<=(.25*median(B.vec)))),length(which(B.vec>=(2.25*median(B.vec)))))/length(B.vec) #Freq Bonanza or Bust years
  #TP <- length(which(B.vec[years]<A))/ length(B.vec) #did the final biomass dip below A 
  TP <- ifelse(B.vec[years]>A,0,1)
  
  return(list(NPV=NPV,Y=Y.vec,B=B.vec,Bhat=Bhat.vec,BB=BB,TP=TP))
}


#############################################################
#Repeat that calculates theh deviation of low and high investment monitoring investment record NPV
#############################################################

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
  return.invest<-unique(return.invest) #pull out unique values from simulaiton
  
  # Remove outliers
  outlier.index<-which(abs(scale(return.invest))>=3) #identify outliers
  result<-ifelse(length(outlier.index>1),mean(return.invest[-outlier.index],na.rm=T),mean(return.invest,na.rm=T))
  return(result)
  
}


#############################################################
#Additional  Repeat model (repeat.model2) that pulls NPV, Fraction of "Crashes" (FC), and prop tipped (TP) 
#############################################################

repeat.model2<-function(n.iters=500,B.start,B.lim,years,K,A,r,phi.CV,delta,process.noise,p,max.F,c,phi.seeds,process.seeds){
  value<-rep(NA,n.iters)
  BB<-rep(NA,n.iters) #Boom and Bust
  TP<-rep(NA,n.iters) #tipping points
  dB<-rep(NA,n.iters) #Deviation of estimation from true biomass
  B<-rep(NA,n.iters) #Biomass
  Y<-rep(NA,n.iters) #yield
  phi.CV.seed.save<-rep(NA,n.iters)
  
  for (i in 1:n.iters){
    
    phi.CV.seed<-phi.seeds[i]
    process.noise.seed<-process.seeds[i]
    
    phi.CV.seed.save[i]<-phi.CV.seed
    
    model.output<-est.NPV(years,K,A,r,phi.CV,delta,process.noise,p,B.start,B.lim,max.F,phi.CV.seed,process.noise.seed,c)
    
    value[i] <- model.output$NPV
    BB[i] <-model.output$BB 
    TP[i] <-model.output$TP
    dB[i] <-median(abs(model.output$B[-21]/model.output$Bhat))
    B[i] <-median(model.output$B)
    Y[i] <-median(model.output$Y)
  }
  
  return(list(value,BB,TP,dB,B,Y))
}

######
#Test repeat.model2
######

A = 10
start.B.list<-seq(20,100,by=1)
Return.invest.out<-rep(NA,length(start.B.list))
n.iters=1
rm(.Random.seed)
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

cv2 = 0.5
start.B.list<-seq(20,100,by=1)

value = repeat.model2(n.iters,B.start=start.B.list[9],B.lim,years,K,A,r,phi.CV=cv2,delta=.05,process.noise=0.0,p,max.F,c,phi.seeds,process.seeds)
return.value<-median(c(value[[1]]))
return.BB<-median(c(value[[2]]))
return.TP<-sum(value[[3]])/n.iters #fraction of the replicate runs where the population dips below A 
return.dB<-value[[4]]
return.B <-value[[5]]
value


# 
# 