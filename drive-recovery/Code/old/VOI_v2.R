library(lattice)
library(ggplot2)
library(reshape2)
library(directlabels)

setwd("/Users/adrianstier/Dropbox/Projects/In Progress/Value of Information/Code/Figures")

#####################
#Value of Information
#####################

# parameterize model duration and economics
years = 20 #time of simulation
delta<-.05 # discount rate
process.noise = 0 #temporally uncorrelated variance (sd) in little r 
p = 10 #price per unit biomass
c=200 # cost to achieve F

# parameterize the model based on A,BMSY and MSY
A = 10  #allee effect threshold 
Bmsy<- 70 #Biomass at MSY
MSY<-25 # MSY
K<-(3*Bmsy^2 - 2*A*Bmsy)/( 2*Bmsy-A) #Carrying Capacity of Focal population (derived by TE in matlab)
r<-MSY/(Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K)) #population growth rate 
Fmsy<-MSY/Bmsy #Fishing mortality that produces MSY
max.F<-Fmsy #maximum fishing defined as Fmsy
B.lim<-max(A,20) # lower biomass limit for harvest control rule

#############################################################
#Define the net present value for a fixed monitoring investment 
#############################################################

est.NPV<-function(years,K,A,r,phi.CV,delta,process.noise,p,B.start,B.lim,max.F,phi.CV.seed,process.noise.seed,c){
  
  # Figure out reference points given param inputs for K, A, MSY, and Bmsy
  Bmsy<- A/3 + K/3 + (A^2 - A*K + K^2)^(1/2)/3
  MSY<-r*Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K)
  Fmsy<-MSY/Bmsy
  
  B.vec<-rep(NA,years+1) #Biomass through time
  Bhat.vec<-rep(NA,years) #Estimated biomass through time
  Y.vec<-rep(NA,years) #yield through time
  phi<-Bmsy*phi.CV #CV determines precision of Bhat estimat
  B.vec[1]<-B.start #starting biomass
  
  # Get observation errors
  set.seed(phi.CV.seed) #observation errors depend on seed
  B.errors<-exp(rnorm(years,mean=(0-phi.CV^2/2),sd=phi.CV)) #lognormal observation error
    
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
  TP <- ifelse(btest[years]>A,0,1)
  
  return(list(NPV=NPV,Y=Y.vec,B=B.vec,Bhat=Bhat.vec,BB=BB,TP=TP))
}




#############################################################
#Repeat the above model a number of times (n.iters)  for low and high investment monitoring investment record NPV
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
#Simulate the return on investment for a range of starting values and 2 different Investments 
#############################################################

start.B.list<-seq(10,100,by=5)

Return.invest.out<-rep(NA,length(start.B.list))
n.iters=100
rm(.Random.seed)
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

for (i in 1:length(start.B.list)){
  
  Return.invest.out[i]<-repeat.model(n.iters,B.start=start.B.list[i],B.lim,lowCV=0.05,highCV=0.1,years,K,A,r,delta=.05,process.noise=0.0,p,max.F,c,phi.seeds,process.seeds)
  
}

par(las=1,mfrow=c(1,1))
plot(start.B.list,Return.invest.out,type="l",col="black",lwd=2,xlab="Initial Biomass",ylab="Return on Investment",xlim=c(10,100))
abline(v=A,lwd=2,lty="dotted")




#############################################################
#Write a Second Repeat model (repeat.model2) that pulls NPV, Fraction of "Crashes" (FC), and prop tipped (TP) 
#############################################################

repeat.model2<-function(n.iters=500,B.start,B.lim,years,K,A,r,phi.CV,delta,process.noise,p,max.F,c,phi.seeds,process.seeds){
  value<-rep(NA,n.iters)
  BB<-rep(NA,n.iters)
  TP<-rep(NA,n.iters)
  phi.CV.seed.save<-rep(NA,n.iters)
  
  for (i in 1:n.iters){
    
    phi.CV.seed<-phi.seeds[i]
    process.noise.seed<-process.seeds[i]
    
    phi.CV.seed.save[i]<-phi.CV.seed
    
    model.output<-est.NPV(years,K,A,r,phi.CV,delta,process.noise,p,B.start,B.lim,max.F,phi.CV.seed,process.noise.seed,c)
   
    value[i] <- model.output$NPV
    BB[i] <-model.output$BB 
    TP[i] <-model.output$TP 
    
  }
  
  return(list(value,BB,TP))
}

######
#Test 
# 
# A = 70
# start.B.list<-seq(10,100,by=1)
# Return.invest.out<-rep(NA,length(start.B.list))
# n.iters=500
# rm(.Random.seed)
# phi.seeds<-round(1000000*runif(n.iters),0)
# process.seeds<-round(1000000*runif(n.iters),0)
# 
# cv2 = 0.5
# start.B.list<-seq(10,100,by=1)
# 
# value = repeat.model2(n.iters,B.start=start.B.list[2],B.lim,years,K,A,r,phi.CV=cv2,delta=.05,process.noise=0.0,p,max.F,c,phi.seeds,process.seeds)
# return.value<-median(c(value[[1]]))
# return.BB<-median(c(value[[2]]))
# return.TP<-sum(value[[3]])/n.iters #fraction of the replicate runs where the population dips below A 
# 
# 



#############################################################
#Q: What is the prob of crossing a tipping point under different levels of population certainty and harvest thresholds
#Method: Determine How Different Values of FMSY and CV matter to probability of crossing tipping point for 4 different Allee Effect Strengths
#############################################################
B.start <- 75
avec <- seq(65,75,by = 2)
phivec <- seq(0.1,0.5,by=0.025)
FMSYvec <- seq(.1,1,by=.1)*max.F #manipulating FMSY max.F
ar <- array(dim=c(length(FMSYvec),length(phivec),length(avec)))
dimnames(ar) = list(FMSYvec,phivec,paste("A =",avec))

n.iters = 1
rm(.Random.seed)
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

                     
  for(a in seq(avec)){  
    for(j in seq(phivec)){
        for (i in seq(FMSYvec)){
              max.F = FMSYvec[i]
              value <-repeat.model2(n.iters,B.start,B.lim,years,K,A=avec[a],r,phi.CV=phivec[j],delta=.05,process.noise=0.0,p,max.F,c,phi.seeds,process.seeds)
          #ar[i,j,a]<-median(c(value[[3]])) #TP old
              ar[i,j,a]<- sum(value[[3]])/n.iters
      }
    }
  }
  
#plot the effects of increasing cv and different starting densities on value
df1 = melt(ar)
colnames(df1) = c("Fmsy","CV","TP","Prob.Cross.TP")
df1$pFmsy = round(df1$Fmsy/max.F,2)
range(df1$Prob.Cross.TP) #very low prob of tipping


ggplot(df1,aes(x = CV, y = pFmsy))+
geom_tile(aes(fill=Prob.Cross.TP,colour=Prob.Cross.TP))+
scale_fill_gradient(low="dodgerblue",high="firebrick")+
scale_colour_gradient(low="dodgerblue",high="firebrick")+
xlab("CV of Monitoring")+
ylab("Fmsy")+
facet_grid(.~TP)

df1$pFmsy2 = factor(df1$pFmsy)
ggplot(df1,aes(x=CV,y=Prob.Cross.TP,group=pFmsy2))+
  geom_line(aes(colour=pFmsy2))+
  facet_grid(TP~.)

#2d Slice 
df2 = df1[df1$pFmsy %in% c(0.70,0.90),]
ggplot(df2,aes(x=CV,y=Prob.Cross.TP,group=pFmsy2))+
  geom_line(aes(colour=pFmsy2))+
  facet_grid(TP~.)

####these are coming out so low in probability of crossing tipping point that the risk needs enhancing or the calculation for the prob of tipping needs help 




#############################################################
#Q: Does the value of information change as a population approaches a tipping point?
#Method: Determine Net Present Value (NPV) for a range of monitoring investments and range of starting biomasses
#############################################################
start.B.list <- seq(10,100,by=5)
phivec <- seq(0.1,0.5,by=0.025)
emat <- matrix(0,nrow=length(start.B.list),ncol=length(phivec)) #Make Empty Matrix
colnames(emat) = phivec 
rownames(emat) = start.B.list
n.iters = 400
rm(.Random.seed)
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

for(j in seq(phivec)){
  for (i in 1:length(start.B.list)){
    output <-repeat.model2(n.iters,B.start=start.B.list[i],B.lim,years,K,A,r,phi.CV=phivec[j],delta=.05,process.noise=0.0,p,max.F,c,phi.seeds,process.seeds)
    emat[i,j]<-median(c(output[[1]])) #NPV
  }
}

#plot the effects of increasing cv and different starting densities on value
df1 = melt(emat)
df1$NPV = df1$value
ggplot(df1,aes(x = Var1, y = Var2, z = NPV))+
  geom_tile(aes(fill=NPV,colour=NPV))+
  scale_fill_gradient(low="dodgerblue",high="firebrick")+
  scale_colour_gradient(low="dodgerblue",high="firebrick")+
  xlab("Initial Biomass")+
  ylab("CV of Monitoring")




#############################################################
#Q: Does infomration matter more when there is a stronger Allee effect?
#Method: Determine How Strength of Allee Effect of Fmsy and strenght of Allee Effect modify NPV
#############################################################

B.start <- 75

avec <- seq(-600,100,by = 10)
phivec <- seq(0.1,0.5,by=.1)
FMSYvec <- seq(.1,1,by=.1)*max.F #manipulating FMSY through MSY since Fmsy is derived above
ar <- array(dim=c(length(FMSYvec),length(phivec),length(avec)))
dimnames(ar) = list(paste("Fmsy = ",round(FMSYvec,2)),phivec,avec)

n.iters = 200
rm(.Random.seed)
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

for(a in seq(avec)){  
  for(j in seq(phivec)){
    for (i in seq(FMSYvec)){
      max.F = FMSYvec[i]
      value <-repeat.model2(n.iters,B.start,B.lim,years,K,A=avec[a],r,phi.CV=phivec[j],delta=.05,process.noise=0.0,p,max.F,c,phi.seeds,process.seeds)
      ar[i,j,a]<-median(c(value[[1]])) #NPV
    }
  }
}

#plot the effects of increasing cv and different starting densities on value
df1 = melt(ar)
colnames(df1) = c("Fmsy","CV","TP","NPV")
ggplot(df1,aes(x =TP , y = CV))+
geom_tile(aes(fill=NPV,colour=NPV))+
scale_fill_gradient(low="red")+
scale_colour_gradient(low="red")+
xlab("A: Strength of Allee Effect")+
ylab("CV")+
facet_grid(Fmsy~.)




#############################################################
#Q: How many bonanza and bust years (BB) happen as a population approaches a tipping point?
#Method: Bonanza and Bust for a range of monitoring investments and range of starting values
#############################################################
process.noise = 0
B.lim = 11

start.B.list <- seq(10,100,by=5)
phivec <- seq(0.1,0.9,by=0.025)
emat <- matrix(0,nrow=length(start.B.list),ncol=length(phivec)) #Make Empty Matrix
colnames(emat) = phivec 
rownames(emat) = start.B.list
n.iters = 50
rm(.Random.seed)
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

for(j in seq(phivec)){
  for (i in 1:length(start.B.list)){
    output <-repeat.model2(n.iters,B.start=start.B.list[i],B.lim,years,K,A,r,phi.CV=phivec[j],delta=.05,process.noise=0.0,p,max.F,c,phi.seeds,process.seeds)
    emat[i,j]<-median(c(output[[2]])) #frequency of Bonanza and bust
  }
}

#plot the effects of increasing cv and different starting densities on value
df1 = melt(emat)
df1$BB = df1$value
ggplot(df1,aes(x = Var1, y = Var2, z = BB))+
  geom_tile(aes(fill=BB,colour=BB))+
  scale_fill_gradient(low="red")+
  scale_colour_gradient(low="red")+
  xlab("Initial Biomass")+
  ylab("CV of Monitoring")

