#########
#This code simulates across vectors of tipping point (a), cv of monitoring (phi), and fishing strength (pFMSY)
#and outputs data focused on making safe operating space plots
#########


source("code/0_libraries.R") #load packages that are relevant
source("code/2_model_parameters.R") # base parameters
source("code/3_mse_model.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"


#Set Number of Iterations and Seeds for Phi and Process for each of the simulations below

n.iters = 5000
rm(.Random.seed)
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

years=50
B.start <- 75 #starting biomass
avec <- seq(10,30,by = 10) #vector of tipping points
phivec <- seq(0.0,0.5,by=.01) #vector of accuracy
FMSYvec <- seq(.1,2,by=0.01) #manipulating  max.F my a multiplier
ar1 <- array(dim=c(length(FMSYvec),8,length(phivec),length(avec)))
dimnames(ar1) = list(FMSYvec,c("NPV","Prob.Cross.TP","biomass","CumYield","SDBiomass","Ptip.MGMT","Fmsy","max.F.2"),phivec,paste("A =",avec))

# mfmat<-nrow(expand.grid(avec,phivec,FMSYvec))

for(a in 1:length(avec)){  
  for(j in 1:length(phivec)){
    for (i in 1:length(FMSYvec)){
      
      
      #dictate the monitoring investment as fixed
      phi.CV.low=phi.CV.high=phivec[j]
      
      #calculate maxF
      A=avec[a]
      Bmsy<- A/3 + K/3 + (A^2 - A*K + K^2)^(1/2)/3 #Biomass at MSY
      B.lim<-max(A,0.25*Bmsy) # lower biomass limit for harvest control rule 
      MSY<-r*Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K) #MSY
      Fmsy<-MSY/Bmsy #Fishing mortality that produces MSY
      max.F.2=FMSYvec[i]*Fmsy
      
      
      
      value <-repeat.model2(n.iters,B.start,B.lim,years,K,A,r,phi.CV,delta=.05,process.noise=0.0,p,max.F=max.F.2,phi.seeds,process.seeds)
      
      print(phivec[j])
      print(FMSYvec[i])
      
      ar1[i,1,j,a] <-median(c(value[[1]]))  #NPV
      ar1[i,2,j,a] <-sum(value[[3]])/n.iters #p tip
      ar1[i,3,j,a] <-median(value[[6]]) #biomass
      ar1[i,4,j,a] <-sum(value[[7]]) #Cumulative Yield
      ar1[i,5,j,a] <-sd(value[[6]]) #sd biomass
      ar1[i,6,j,a] <-sum(value[[4]])/n.iters #add one for number of times dip below mgmt threshold    
      ar1[i,7,j,a] <-Fmsy
      ar1[i,8,j,a] <-max.F.2
    }
  }
}


save(ar1,file=here("output/simulation",paste("safe-operating-space",Sys.Date(),n.iters,".Rdata")))
