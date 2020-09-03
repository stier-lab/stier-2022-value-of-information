######
#Figure 4 Simulation - is adaptive monitoring more cost effective that precautionary managmeent?
######

source("code/0_libraries.R") #load packages that are relevant
source("code/2_model_parameters.R") # base parameters
source("code/3_mse_model.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"

#Number of Iterations 
n.iters=1000
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

#Define Parameters 
max.F= max.F
B.start=100
A=10
years=20
B.crit=.25*Bmsy

#Set up Monitoring Regimes to Loop Through
cv <- c("fixedCV0.1","fixedCV0.5","PrecautionaryBufferCV","mean_PBCV")
phitab<-matrix(0,nrow=2,ncol=4)
colnames(phitab)<-cv

phitab[,1]<-0.1
phitab[,2]<-0.5
phitab[1,3]<-0.1
phitab[2,3]<-0.5

bcritvec<-c(0.25,0.5,1,1.25)
csvec<-c(1,5,10)
mfvec<-seq(0,2,by=0.01)

##Need to run and see how to melt right 
#Set up Empty data Frame
edf<-array(dim=c(4,5,length(bcritvec),length(csvec),length(mfvec)))
dimnames(edf) = list(cv,c("CV","NPV","Monitoring","NPV_minus_Monitoring","Prob_Tip"),bcritvec,csvec,mfvec)

# NPV_minus_Monitoring <- rep(0,4)
# NPV <- rep(0,4)
# Monitoring <-rep(0,4)
# Prob_Tip <-rep(0,4)
# edf <- data.frame(cv,NPV,Monitoring,NPV_minus_Monitoring,Prob_Tip)


for(c in 1:length(csvec)){  
  for(bc in 1:length(bcritvec)){
    for(i in 1:ncol(phitab)){
      for(mf in 1:length(mfvec)){
        
        phi.CV.low=phitab[1,i]
        phi.CV.high=phitab[2,i]
        B.crit<-bcritvec[bc]*Bmsy
        cs<-csvec[c]
        max.F=mfvec[mf]*Fmsy
        
        value = repeat.model2(n.iters,B.start=100,B.lim,years,K,A,r,phi.CV,delta=.05,process.noise=0.0,p,max.F=max.F,phi.seeds,process.seeds)
        
        edf[i,2,bc,c,mf]<-median(value$value)
        edf[i,3,bc,c,mf]<-median(value$cost.monitor)
        edf[i,4,bc,c,mf]<-median(value$NPV_minusCM)
        edf[i,5,bc,c,mf]<-sum(value$TP)/n.iters
        
        phitab[1,4]<-mean(value$phi.CV)
        phitab[2,4]<-mean(value$phi.CV)
        
      }
    }
  }
}

save(edf,file=paste("output/simulation/precautionary_buffer.Rdata"))
