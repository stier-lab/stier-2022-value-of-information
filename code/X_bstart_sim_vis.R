
source("code/0_libraries.R") #load packages that are relevant
source("code/2_model_parameters.R") # base parameters
source("code/3_mse_model.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"


#set parameter range for surface of simulated parameters
years = 50
B.vec <- c(15,50,85)#eq(50,100, by = 10) #MCS: used to be by 5
avec <- c(10) #biomass at which allee effect occurs
phivec <- seq(0.1,0.5,by = 0.1) #uncertainty cv MCS: CV of biomass? or survey cv of biomass?
FMSYvec <- seq(.1,2,by = 0.2) #manipulating FMSY max.F

#create empty array with labels
ar <- array(dim=c(length(FMSYvec),13,length(phivec),length(avec),length(B.vec)))
dimnames(ar) = list(FMSYvec,c("NPV","Prob.Cross.TP","Biomass","CumulativeYield",
                              "SDBiomass","Ptip.MGMT","Fmsy","max.F.2",
                              "yrs.near.thresh1","yrs.near.thresh2","rescue","prob_rescue","prob_rescue2"),
                    phivec,paste("A =",avec),B.vec)

#set number of iterations 
n.iters = 150
rm(.Random.seed)
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

#write infinite for loop and dream of knowing how to use apply funcitons better

for(b in 1:length(B.vec)){
  for(a in 1:length(avec)){
    for(j in 1:length(phivec)){
      for (i in 1:length(FMSYvec)){
        
        print(B.vec[b])
        
        #dictate the monitoring investment as fixed
        phi.CV.low=phi.CV.high=phivec[j]
        
        #calculate maxF
        A = avec[a]
        Bmsy<- A/3 + K/3 + (A^2 - A*K + K^2)^(1/2)/3 #Biomass at MSY
        B.lim<-max(A,0.25*Bmsy) # lower biomass limit for harvest control rule 
        MSY<-r*Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K) #MSY
        Fmsy<-MSY/Bmsy #Fishing mortality that produces MSY
        max.F.2=FMSYvec[i]*Fmsy
        
        
        value <- repeat.model2(n.iters,B.start=B.vec[b],B.lim,years,K,A=avec[a],
                               r,phi.CV,delta=.05,process.noise=0.0,p,max.=max.F.2,phi.seeds,process.seeds)
        
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
        ar[i,12,j,a,b]<-length(which(value$rescue>0))/length(value$rescue) #probability of rescue /#rescues
        ar[i,13,j,a,b]<-mean(value$rescue_prob,na.rm=TRUE)  #prob rescue 2 ignoring nas
      }
    }
  }
}

save(ar,file=here::here("output","simulation",paste("figX_bstart_values_50years",Sys.Date(),"_",n.iters,".Rdata",sep="")))




#heat map as function of years near threshold

df1 = melt(ar,varnames=names(dimnames(ar)))
colnames(df1) = c("pFmsy","metric","CV","A","B.start","value")
df1w <-pivot_wider(df1,names_from = metric)%>%
  filter(A == "A = 10")

#Probability of tipping
gg_tip<-ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=Prob.Cross.TP,colour=Prob.Cross.TP))+
  scale_colour_gradientn(colours = pal,name="P.tipping", guide = gc) + 
  scale_fill_gradientn(colours = pal,name="P.tipping") + 
  xlab("Monitoring Precision (CV)")+
  ylab("Max H (pHmsy)")+
  facet_wrap(~B.start)+
  theme_pubr(legend="right")

#fraction of time series where B < 0.8*Bmsy
gg_danger<-ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=yrs.near.thresh1,colour=yrs.near.thresh1))+
  scale_fill_gradient(name="% B<0.8Bmsy",low="dodgerblue",high="firebrick",guide = gc)+
  scale_colour_gradient(name="% B<0.8Bmsy",low="dodgerblue",high="firebrick",guide = gc)+
  xlab("Monitoring Precision (CV)")+
  ylab("Max H (pHmsy)")+
  facet_wrap(~B.start)+
  theme_pubr(legend="right")


#Net Present Value
gg_NPV<-ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=NPV,colour=NPV))+
  scale_fill_gradient(name="NPV",low="dodgerblue",high="firebrick",guide = gc)+
  scale_colour_gradient(name="NPV",low="dodgerblue",high="firebrick",guide = gc)+
  xlab("Monitoring Precision (CV)")+
  ylab("Max H (pHmsy)")+
  facet_wrap(~B.start)+
  theme_pubr(legend="right")

#Number of years where B goes from <0.8Bmsy to >0.8Bmsy
gg_rescue<-ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=rescue,colour=rescue))+
  scale_fill_gradient(name="# rescues",low="gray",high="dodgerblue",guide = gc)+
  scale_colour_gradient(name="# rescues",low="gray",high="dodgerblue",guide = gc)+
  xlab("Monitoring Precision (CV)")+
  ylab("Max H (pHmsy)")+
  facet_wrap(~B.start)+
  theme_pubr(legend="right")

#Proportion of times when B<0.8*Bmsy recovers above 
gg_rescue_prob2<-ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=prob_rescue2,colour=prob_rescue2))+
  scale_fill_gradient(name="p-rescue",low="dodgerblue",high="firebrick",guide=gc)+
  scale_colour_gradient(name="p-rescue",low="dodgerblue",high="firebrick",guide=gc)+
  xlab("Monitoring Precision (CV)")+
  ylab("Max H (pHmsy)")+
  facet_wrap(~B.start)+
  theme_pubr(legend="right")


#not sure what this is but it's some sort of weighted of prop rescues relative to proportion of time B<0.8Bmsy
df1w$rescue_rel_risk <- df1w$prob_rescue/df1w$yrs.near.thresh1

gg_rescue_rel_risk<-ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=rescue_rel_risk,colour=rescue_rel_risk))+
  scale_fill_gradient(low="dodgerblue",high="firebrick")+
  scale_colour_gradient(low="dodgerblue",high="firebrick")+
  xlab("Monitoring Precision (CV)")+
  ylab("Max H (pHmsy)")+
  facet_wrap(~B.start)+
  theme_pubr(legend="right")


# plot_grid(gg_tip,gg_danger,gg_NPV,gg_rescue,gg_rescue2,gg_rescue_prob2,ncol=2)



plot_grid(gg_tip,gg_NPV,gg_rescue,ncol=1,labels=c("A","B","C","D"))

all<-plot_grid(gg_tip,gg_NPV,gg_rescue,ncol=1,labels=c("A","B","C","D"))
save_plot("output/figures/tippingpoint/alevels_combined.pdf",all,base_width=6,base_height=7)




