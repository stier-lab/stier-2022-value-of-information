#########
#FIGURE 2 Simulation: how does value of information change when you spend more time near tipping point
#########

source("code/0_libraries.R") #load packages that are relevant
source("code/2_model_parameters.R") # base parameters
source("code/3_mse_model.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"

#set parameter range for surface of simulated parameters
years = 20
B.vec <-c(55,70,100)#seq(50,100, by = 10) #MCS: used to be by 5
avec <- c(10,20,30) #biomass at which allee effect occurs
phivec <- seq(0.1,0.5,by = 0.1) #uncertainty cv MCS: CV of biomass? or survey cv of biomass?
FMSYvec <- seq(.1,2,by = 0.2) #manipulating FMSY max.F

#create empty array with labels
ar <- array(dim=c(length(FMSYvec),13,length(phivec),length(avec),length(B.vec)))
dimnames(ar) = list(FMSYvec,c("NPV","Prob.Cross.TP","Biomass","CumulativeYield",
                              "SDBiomass","Ptip.MGMT","Fmsy","max.F.2",
                              "yrs.near.thresh1","yrs.near.thresh2","rescue","prob_rescue","prob_rescue2"),
                    phivec,paste("A =",avec),B.vec)

#set number of iterations 
n.iters = 300
rm(.Random.seed)
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

#write infinite for loop and dream of knowing how to use apply funcitons better

for(b in seq(B.vec)){
  for(a in seq(avec)){  
    for(j in seq(phivec)){
      for (i in seq(FMSYvec)){
        
        print(B.vec[b])
        
        #dictate the monitoring investment as fixed
        phi.CV.low=phi.CV.high=phivec[j]
        
        #calculate maxF
        A=avec[a]
        Bmsy<- A/3 + K/3 + (A^2 - A*K + K^2)^(1/2)/3 #Biomass at MSY
        B.lim<-max(A,0.25*Bmsy) # lower biomass limit for harvest control rule 
        MSY<-r*Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K) #MSY
        Fmsy<-MSY/Bmsy #Fishing mortality that produces MSY
        max.F.2=FMSYvec[i]*Fmsy
        
        
        value <- repeat.model2(n.iters,B.start=B.vec[b],B.lim,years,K,A=avec[a],r,phi.CV,delta=.05,process.noise=0.0,p,max.=max.F.2,phi.seeds,process.seeds)
        
        #MCS: why not name these with $'s so you can tell what they're called?
        ar[i,1,j,a,b] <-median(c(value[[1]]))  #NPV
        ar[i,2,j,a,b] <-sum(value[[3]])/n.iters #p tip
        ar[i,3,j,a,b] <-median(value[[6]]) #biomass
        ar[i,4,j,a,b] <-sum(value[[7]]) #Cumulative Yield
        ar[i,5,j,a,b] <-sd(value[[6]]) #sd biomass
        ar[i,6,j,a,b] <-sum(value[[4]])/n.iters #add one for number of times dip below mgmt threshold    
        ar[i,7,j,a,b] <-Fmsy
        ar[i,8,j,a,b] <-max.F.2
        ar[i,9,j,a,b] <-median(value$thresh1) #k/2 threshold for danger
        ar[i,10,j,a,b]<-median(value$thresh2) #k/4 threshold for danger
        ar[i,11,j,a,b]<-median(value$rescue) #number of rescues
        ar[i,12,j,a,b]<-length(which(value$rescue>0))/length(value$rescue) #probability of rescue
        ar[i,13,j,a,b]<-mean(value$rescue_prob,na.rm=TRUE)
      }
    }
  }
}

save(ar,file=here::here("output","simulation",paste("fig2_mcs_",Sys.Date(),"_",n.iters,".Rdata",sep="")))





############################################################################
#simulate for danger zone A=10 for lots of different fishing for proximity to tipping point 
############################################################################


#set parameter range for surface of simulated parameters
years = 100
B.vec <-c(70)#seq(50,100, by = 10) #MCS: used to be by 5
avec <- c(10) #biomass at which allee effect occurs
phivec <- seq(0.1,0.5,by = 0.1) #uncertainty cv MCS: CV of biomass? or survey cv of biomass?
FMSYvec <- seq(.1,2,by = 0.01) #manipulating FMSY max.F

#create empty array with labels
ar <- array(dim=c(length(FMSYvec),13,length(phivec),length(avec),length(B.vec)))
dimnames(ar) = list(FMSYvec,c("NPV","Prob.Cross.TP","Biomass","CumulativeYield",
                              "SDBiomass","Ptip.MGMT","Fmsy","max.F.2",
                              "yrs.near.thresh1","yrs.near.thresh2","rescue","prob_rescue","prob_rescue2"),
                    phivec,paste("A =",avec),B.vec)

#set number of iterations 
n.iters = 1000
rm(.Random.seed)
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

#write infinite for loop and dream of knowing how to use apply funcitons better

for(b in seq(B.vec)){
  for(a in seq(avec)){  
    for(j in seq(phivec)){
      for (i in seq(FMSYvec)){
        
        print(B.vec[b])
        
        #dictate the monitoring investment as fixed
        phi.CV.low=phi.CV.high=phivec[j]
        
        #calculate maxF
        A=avec[a]
        Bmsy<- A/3 + K/3 + (A^2 - A*K + K^2)^(1/2)/3 #Biomass at MSY
        B.lim<-max(A,0.25*Bmsy) # lower biomass limit for harvest control rule 
        MSY<-r*Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K) #MSY
        Fmsy<-MSY/Bmsy #Fishing mortality that produces MSY
        max.F.2=FMSYvec[i]*Fmsy
        
        
        value <- repeat.model2(n.iters,B.start=B.vec[b],B.lim,years,K,A=avec[a],r,phi.CV,delta=.05,process.noise=0.0,p,max.=max.F.2,phi.seeds,process.seeds)
        
        #MCS: why not name these with $'s so you can tell what they're called?
        ar[i,1,j,a,b] <-median(c(value[[1]]))  #NPV
        ar[i,2,j,a,b] <-sum(value[[3]])/n.iters #p tip
        ar[i,3,j,a,b] <-median(value[[6]]) #biomass
        ar[i,4,j,a,b] <-sum(value[[7]]) #Cumulative Yield
        ar[i,5,j,a,b] <-sd(value[[6]]) #sd biomass
        ar[i,6,j,a,b] <-sum(value[[4]])/n.iters #add one for number of times dip below mgmt threshold    
        ar[i,7,j,a,b] <-Fmsy
        ar[i,8,j,a,b] <-max.F.2
        ar[i,9,j,a,b] <-median(value$thresh1) #k/2 threshold for danger
        ar[i,10,j,a,b]<-median(value$thresh2) #k/4 threshold for danger
        ar[i,11,j,a,b]<-median(value$rescue) #number of rescues
        ar[i,12,j,a,b]<-length(which(value$rescue>0))/length(value$rescue) #probability of rescue
        ar[i,13,j,a,b]<-mean(value$rescue_prob,na.rm=TRUE)
      }
    }
  }
}

save(ar,file=here::here("output","simulation",paste("fig2_dz_acs_100yr",Sys.Date(),"_",n.iters,".Rdata",sep="")))






############################################################################
#simulate for rescue effect 
############################################################################


#set parameter range for surface of simulated parameters
years = 20
B.vec <-c(70)#seq(50,100, by = 10) #MCS: used to be by 5
avec <- c(10) #biomass at which allee effect occurs
phivec <- seq(0.1,0.5,by = 0.01) #uncertainty cv MCS: CV of biomass? or survey cv of biomass?
FMSYvec <- seq(.1,2,by = 0.01) #manipulating FMSY max.F

#create empty array with labels
ar <- array(dim=c(length(FMSYvec),13,length(phivec),length(avec),length(B.vec)))
dimnames(ar) = list(FMSYvec,c("NPV","Prob.Cross.TP","Biomass","CumulativeYield",
                              "SDBiomass","Ptip.MGMT","Fmsy","max.F.2",
                              "yrs.near.thresh1","yrs.near.thresh2","rescue","prob_rescue","prob_rescue2"),
                    phivec,paste("A =",avec),B.vec)

#set number of iterations 
n.iters = 500
rm(.Random.seed)
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

#write infinite for loop and dream of knowing how to use apply funcitons better

for(b in seq(B.vec)){
  for(a in seq(avec)){  
    for(j in seq(phivec)){
      for (i in seq(FMSYvec)){
        
        print(B.vec[b])
        
        #dictate the monitoring investment as fixed
        phi.CV.low=phi.CV.high=phivec[j]
        
        #calculate maxF
        A=avec[a]
        Bmsy<- A/3 + K/3 + (A^2 - A*K + K^2)^(1/2)/3 #Biomass at MSY
        B.lim<-max(A,0.25*Bmsy) # lower biomass limit for harvest control rule 
        MSY<-r*Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K) #MSY
        Fmsy<-MSY/Bmsy #Fishing mortality that produces MSY
        max.F.2=FMSYvec[i]*Fmsy
        
        
        value <- repeat.model2(n.iters,B.start=B.vec[b],B.lim,years,K,A=avec[a],r,phi.CV,delta=.05,process.noise=0.0,p,max.=max.F.2,phi.seeds,process.seeds)
        
        #MCS: why not name these with $'s so you can tell what they're called?
        ar[i,1,j,a,b] <-median(c(value[[1]]))  #NPV
        ar[i,2,j,a,b] <-sum(value[[3]])/n.iters #p tip
        ar[i,3,j,a,b] <-median(value[[6]]) #biomass
        ar[i,4,j,a,b] <-sum(value[[7]]) #Cumulative Yield
        ar[i,5,j,a,b] <-sd(value[[6]]) #sd biomass
        ar[i,6,j,a,b] <-sum(value[[4]])/n.iters #add one for number of times dip below mgmt threshold    
        ar[i,7,j,a,b] <-Fmsy
        ar[i,8,j,a,b] <-max.F.2
        ar[i,9,j,a,b] <-median(value$thresh1) #k/2 threshold for danger
        ar[i,10,j,a,b]<-median(value$thresh2) #k/4 threshold for danger
        ar[i,11,j,a,b]<-median(value$rescue) #number of rescues
        ar[i,12,j,a,b]<-length(which(value$rescue>0))/length(value$rescue) #probability of rescue
        ar[i,13,j,a,b]<-mean(value$rescue_prob,na.rm=TRUE)
      }
    }
  }
}

save(ar,file=here::here("output","simulation",paste("fig2_rescue_acs_",Sys.Date(),"_",n.iters,".Rdata",sep="")))




