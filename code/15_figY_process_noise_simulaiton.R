######
#Figure 5 Simulation - how does process noise alter the value of information in avoiding tipping points
######

source("code/0_libraries.R") #load packages that are relevant
source("code/2_model_parameters.R") # base parameters
source("code/3_mse_model.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"

#Set up iterations and vector\s of params to explore
n.iters=500
procnoisevec <- seq(0,2,by=0.1)
phivec <- seq(0.1,0.5,by=0.05) #uncertainty cv

phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

#all combinations 
cb <- as.matrix(crossing(procnoisevec,phivec))

emat <- matrix(NA,nrow=nrow(cb),ncol=8)
emat[,1]<-cb[,1]
emat[,2]<-cb[,2]

colnames(emat) <- c("process_noise","precision","NPV","ptip","biomass","cumulative_yield","sd-biomass","ptip_mgmt")


for(i in 1:nrow(cb)){
  
  phi.CV.low=phi.CV.high=emat[i,1]
  
  value <-repeat.model2(n.iters,B.start=75,B.lim,years,K,A,r,phi.CV,delta,process.noise=emat[i,2],p,max.=max.F,phi.seeds,process.seeds)
  
  emat[i,3] <-median(c(value[[1]]))  #NPV
  emat[i,4] <-sum(value[[3]])/n.iters #p tip
  emat[i,5] <-median(value[[6]]) #biomass
  emat[i,6] <-sum(value[[7]]) #Cumulative Yield
  emat[i,7] <-sd(value[[6]]) #sd biomass
  emat[i,8] <-sum(value[[4]])/n.iters #add one for number of times dip below mgmt threshold    
  
}

save(emat,file=paste("output/simulation/process_noise.Rdata"))




