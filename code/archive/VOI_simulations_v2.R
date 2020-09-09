##########################################
#Value of Information Simulations 
##########################################


source("code/0_libraries.R") #load packages that are relevant
source("code/2_model_parameters.R") # base parameters
source("code/3_mse_model.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"


gc <- guide_colorbar(
  frame.colour = "black",
  barheight = 8,
  frame.linewidth = 2,
  ticks.colour = "black",
  ticks.linewidth = 2
        )


##########################################################################################################################
#FIGURE 2: Prob tipping underdifferent scenarios of CV, A, and %Fmsy
##########################################################################################################################

#the following loops through a number of different possible combinations of fmsy cv and a to think about the importance of info
#across a surface. 

source("code/2_model_parameters.R") # base parameters

#Set Number of Iterations and Seeds for Phi and Process for each of the simulations below

n.iters = 100
rm(.Random.seed)
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

B.start <- 50 #starting biomass
avec <- seq(10,30,by = 10) #vector of tipping points
<<<<<<< HEAD:code/VOI_simulations_v2.R
phivec <- seq(0.0,0.5,by=.1) #vecor of accuracy
=======
phivec <- seq(0.0,0.5,by=.1) #vector of accuracy
>>>>>>> dfeea026928408acb30d6782364ca1c2c096979f:code/archive/VOI_simulations_v2.R
FMSYvec <- seq(.1,2,by=0.1) #manipulating  max.F my a multiplier
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


 save(ar1,file=here("output/simulation",paste("risk and heatmaps",Sys.Date(),n.iters,".Rdata")))
 load("output/simulation","risk and heatmaps 2019-03-13 3000 .Rdata") #this is ignored on github will need to produce

source("code/2_model_parameters.R") # base parameters
 

df1 = melt(ar,varnames=names(dimnames(ar1)))
colnames(df1) = c("pFmsy","metric","CV","A","value")

df1 = reshape(df1,
              timevar = "metric",
              idvar = c("pFmsy","A","CV"),
              direction = "wide")


colnames(df1) = c("pFmsy","CV","A","NPV","Prob.Cross.TP","Biomass","CumulativeYield","SDBiomass","Ptip.MGMT","Fmsy","max.F.2")
#df1$pFmsy = round((df1$max.F.2/df1$max.F.1),4)
range(df1$Prob.Cross.TP) #very low prob of tipping
range(df1$pFmsy)


########
#######
#Look at the effects of pFmsy and CV monitoring on NPV, ptip, biomass, ptipmgmt 
#######
########


av <- unique(df1$A)
rvec<-c("NPV","Prob.Cross.TP","Biomass","Ptip.MGMT")

for(j in 1:length(av)){

tdf1 <- filter(df1, A == av[j]) %>%
select("pFmsy","CV","NPV","Prob.Cross.TP","Biomass","Ptip.MGMT") %>%
gather("NPV","Prob.Cross.TP","Biomass","Ptip.MGMT",
       key="response",
       value = "value")

p<-list()

for(i in 1:4){

    tdf2 <- filter(tdf1, response == rvec[i])
    
    p[[i]]  <-  ggplot(tdf2,aes(x = CV, y = pFmsy))+
        geom_tile(aes(fill=value,colour=value))+
        scale_fill_gradient(low="dodgerblue",high="firebrick",name=rvec[i])+
        scale_colour_gradient(low="dodgerblue",high="firebrick",name=rvec[i])+
          xlab("CV of Monitoring")+
        ylab("pFmsy")+
        theme_pubr(legend="right")

          
}


p_temp <- plot_grid(p[[1]],p[[2]],p[[3]],p[[4]],labels="AUTO")
save_plot(here("output/figures/original",paste(av[j],"CV_pFMSY_conservative.pdf")),p_temp,base_width=8,base_height=5)
p<-list()

}


########################################
########################################
########################################


##These plots below are a mess and should be simplified using select/tidyverse to make sure comparing same A values 


#########
#FIGURE 1a: how does prob of tipping matter across tippoing point
#########


#probability of tipping at A=10
df1a10 <- subset(df1,A == "A = 10")


g1 = ggplot(df1a10,aes(x = CV, y = pFmsy))+
  geom_tile(aes(fill=Prob.Cross.TP,colour=Prob.Cross.TP))+
  scale_fill_gradient(low="dodgerblue",high="firebrick")+
  scale_colour_gradient(low="dodgerblue",high="firebrick")+
  xlab("CV of Monitoring")+
  ylab("Fmsy")+
  theme_pubr()
#   theme_pubr(legend="right")
print(g1)

# #Probability of crossing a tipping point accross Fmsy and CV monitoring - slice plot
# df2 <- subset(df1,CV %in% c(0.0,.1,.2,.3,.4,.5))
# df2 <- subset(df2,pFmsy %in% c(0.0,.1,.5,1,1.5,2))
# 
# ggplot(df2,aes(x=CV,y=Prob.Cross.TP,group=pFmsy))+
#   geom_line(aes(colour=pFmsy))+
#   scale_colour_gradient(low="#fee8c8",high="#e34a33")+
#   facet_grid(TP~.)+
#   xlab("CV of Monitoring")+
#   ylab("Probability of Crossing a Tipping Point")+
#   facet_grid(A~.)+
#     theme_pubr(legend="right")
# 
# #just subset out a=10
# df3 <- subset(df2,A == "A = 10")
# 
# g2 = ggplot(df3,aes(x=CV,y=Prob.Cross.TP,group=pFmsy))+
#   geom_line(aes(colour=pFmsy))+
#   scale_colour_gradient(low="#fee8c8",high="#e34a33")+
#   xlab("CV of Monitoring")+
#   ylab("Probability of Crossing a Tipping Point")+
#     theme_pubr(legend="right")
# 
# print(g2)


#########
#FIGURE 3: safe operating space for biological and managmeent tipping points
#########

#what is the cv necessary to get 5% chance of collapse for differen pFmys

df_3b <- subset(df1, A == "A = 10")

df.01 <- subset(df_3b,Prob.Cross.TP<0.01)
df.05 <- subset(df_3b,Prob.Cross.TP<0.05)
df.10 <- subset(df_3b,Prob.Cross.TP<0.1)
df.20 <- subset(df_3b,Prob.Cross.TP<0.2)

cvmax.01 <- tapply(df.01$CV,list(df.01$pFmsy),max)
cvmax.05 <- tapply(df.05$CV,list(df.05$pFmsy),max)
cvmax.1 <- tapply(df.10$CV,list(df.10$pFmsy),max)
cvmax.2 <- tapply(df.20$CV,list(df.20$pFmsy),max)

myls <- list(cvmax.2,cvmax.1,cvmax.05,cvmax.01)
max.rows <- max(nrow(cvmax.01),nrow(cvmax.05), nrow(cvmax.1),nrow(cvmax.2))
new_myls <- lapply(myls,function(x){x[1:max.rows]})

df4 <- data.frame(do.call(cbind, lapply(new_myls, `[`,)))
df4$pFmsy <- as.numeric(rownames(df4))
names(df4) = c("20%","10%","5%","1%","pFmsy")
df4 <- melt(df4,id.vars=c("pFmsy"))
names(df4) <- c("pFmsy","PercentRisk","value")
df4$PercentRisk <- factor(df4$PercentRisk, levels = c("20%","10%","5%","1%"))


g3 = ggplot(df4,aes(x=pFmsy,y=value,group=PercentRisk))+
  geom_line(aes(colour=PercentRisk,lty=PercentRisk))+
  xlab("pFmsy")+
  ylab("Max CV to Avoid Tipping point")+
    theme_pubr(legend="right")+
  scale_x_continuous(limits=c(0,2))+
  scale_y_continuous(limits=c(0,0.6),breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6))
  # scale_colour_Publication()
# ggtitle("Risk to Avoid Allee TP")


print(g3) 



#what is the cv necessary to get 5% chance of crossing mgmt threshold for differen pFmys

df_3c <- subset(df1, A == "A = 10")

df.01 <- subset(df_3c,Ptip.MGMT<0.01)
df.05 <- subset(df_3c,Ptip.MGMT<0.05)
df.10 <- subset(df_3c,Ptip.MGMT<0.1)
df.20 <- subset(df_3c,Ptip.MGMT<0.2)

cvmax.01 <- tapply(df.01$CV,list(df.01$pFmsy),max)
cvmax.05 <- tapply(df.05$CV,list(df.05$pFmsy),max)
cvmax.1 <- tapply(df.10$CV,list(df.10$pFmsy),max)
cvmax.2 <- tapply(df.20$CV,list(df.20$pFmsy),max)

myls <- list(cvmax.2,cvmax.1,cvmax.05,cvmax.01)
max.rows <- max(nrow(cvmax.01),nrow(cvmax.05), nrow(cvmax.1),nrow(cvmax.2))
new_myls <- lapply(myls,function(x){x[1:max.rows]})

df4b <- data.frame(do.call(cbind, lapply(new_myls, `[`,)))
df4b$pFmsy <- as.numeric(rownames(df4b))
names(df4b) = c("20%","10%","5%","1%","pFmsy")
df4b <- melt(df4b,id.vars=c("pFmsy"))
names(df4b) <- c("pFmsy","PercentRisk","value")
df4b$PercentRisk <- factor(df4b$PercentRisk, levels = c("20%","10%","5%","1%"))


g3_mg = ggplot(df4b,aes(x=pFmsy,y=value,group=PercentRisk))+
  geom_line(aes(colour=PercentRisk,lty=PercentRisk))+
  xlab("pFmsy")+
  ylab("Max CV to Avoid MGMT Tipping point")+
    theme_pubr(legend="right")+
  scale_x_continuous(limits=c(0,2))+
  scale_y_continuous(limits=c(0,0.6),breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6))


print(g3_mg)

risk_plot <- plot_grid(g3,g3_mg,ncol=1,labels="AUTO")

# grid.arrange(g3,g3_mg,ncol=1,heights = c(1.2,1.2))

#combine into single plot, too much? 
df4$group<-"Biological Tipping Point"
df4b$group<-"Managment Tipping Point"

dfall<-rbind(df4,df4b)

ggplot(dfall,aes(x=pFmsy,y=value))+
  geom_smooth(method="loess",aes(colour=PercentRisk,lty=group),se=F)+
  ylab("Max CV to Avoid MGMT Tipping point")+
  xlab("Harvest Pressure (pH-MSY)")+
    theme_pubr(legend="right")+
  scale_x_continuous(limits=c(0,2))+
  scale_y_continuous(limits=c(0,0.525),breaks=c(0.0,0.1,0.2,0.3,0.4,0.5))
ggtitle("Risk to Avoid MGMT TP")


#########
#FIGURE 1b: How does the value of information change accross a range of stock biomassses
#########


##now plot NPV acros pFmsy for different CVs

df_3b<-subset(df_3b,CV %in% c(0.0,0.1,0.2,0.3,0.4,0.5))

g4 = ggplot(df_3b,aes(x=pFmsy,y=NPV,group=CV))+
  geom_line(aes(colour=CV))+
  scale_colour_gradient(low="#e0f3db",high="#43a2ca")+
  xlab("pFmsy")+
  ylab("Net Present Value")+
  theme_pubr()

print(g4)

plot_key <- plot_grid(g1,g3,g4,ncol=1,labels="AUTO")
save_plot(here("output/figures/original",paste(df_3b$A[1],"aggregateplot_conservative.pdf")),plot_key,base_width=8,base_height=10)


##########################################################################################################################
#FIGURE 3: How does the value of information change accross a range of stock biomassses
##########################################################################################################################
#setwd("~/Dropbox/Projects/In Progress/Value of Information/Code/Value of Information")

#AS's code with "repeat.model2" function
source(here("code","ModelParameters_v1.R")) # base parameters

years = 20
B.vec <- seq(1,100, by = 5)
avec <- seq(10,30,by = 10) #biomass at which allee effect occurs
phivec <- seq(0.1,0.5,by=.1) #uncertainty cv
FMSYvec <- seq(.1,2,by = 0.2) #manipulating FMSY max.F

ar <- array(dim=c(length(FMSYvec),8,length(phivec),length(avec),length(B.vec)))
dimnames(ar) = list(FMSYvec,c("NPV","Prob.Cross.TP","Biomass","CumulativeYield","SDBiomass","Ptip.MGMT","Fmsy","max.F.2"),phivec,paste("A =",avec),B.vec)



n.iters = 200
rm(.Random.seed)
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

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
        
        
        value <-repeat.model2(n.iters,B.start=B.vec[b],B.lim,years,K,A=avec[a],r,phi.CV,delta=.05,process.noise=0.0,p,max.=max.F.2,phi.seeds,process.seeds)
        
        
        ar[i,1,j,a,b] <-median(c(value[[1]]))  #NPV
        ar[i,2,j,a,b] <-sum(value[[3]])/n.iters #p tip
        ar[i,3,j,a,b] <-median(value[[6]]) #biomass
        ar[i,4,j,a,b] <-sum(value[[7]]) #Cumulative Yield
        ar[i,5,j,a,b] <-sd(value[[6]]) #sd biomass
        ar[i,6,j,a,b] <-sum(value[[4]])/n.iters #add one for number of times dip below mgmt threshold    
        ar[i,7,j,a,b] <-Fmsy
        ar[i,8,j,a,b] <-max.F.2
        
      }
    }
  }
}

save(ar,file=here("output/simulations",paste("range_of_bstart_conservative",Sys.Date(),n.iters,".Rdata")))

#ar has 5 dimmensions: 1) pFmsy, 2) response variable, 3)  phi-uncertainty, 4) a values, 5) starting biomass 
#so divisoin or substractoin of the arry ais just the values of NPV 
#not any other variables, which are the col and rownames
load(here("output","simulation","range_of_bstart 2015-09-18 20000 .Rdata")) #this is the original simulatoin
load(here("output","simulation","range_of_bstart 2019-04-02 100 .Rdata")) #this is the conservative simulatoin - MCS: doesn't include pFMSY ==1 
source(here("code","ModelParameters_v1.R")) # reset base parameters after simulation (specifically max.F)

#########
#Biomass Plots to double check that you get higher equilibrium
#########

df1 = melt(ar)
colnames(df1) = c("pFmsy","metric","CV","A","B.start","value")

df1 = reshape(df1,
              timevar = "metric",
              idvar = c("pFmsy","A","CV","B.start"),
              direction = "wide")


colnames(df1) = c("pFmsy","CV","TP","B.start","NPV","Prob.Cross.TP","Biomass","CumulativeYield","SDBiomass","Ptip.MGMT","Fmsy","max.F.2")

unique(df1$pFmsy)
#where does boimass of fish peak relative to startig densities and fishing pressures
ggplot(df1,aes(x=B.start,y=pFmsy))+
  geom_tile(aes(fill=Biomass,colour=Biomass))+
  # scale_fill_gradient(low="dodgerblue",high="firebrick")+
  # scale_colour_gradient(low="dodgerblue",high="firebrick")+
  #   theme_pubr(legend="right")+
  facet_grid(CV~.)


#Median Biomass over time series for given tipping point (10) CV, an max F
df2 <- subset(df1,B.start ==91 & TP=="A = 10")

ggplot(df2,aes(x=pFmsy,y=Biomass,group=CV))+
  geom_line(aes(colour=CV))+
    theme_pubr(legend="right")+
  xlab("Proportion of Fmsy")+
  ylab("Median Biomass over time series")

ggsave(here("output","figures","original","Biomass_CV_Fmsy.pdf"))


#melt down and look at the different response variables

#########
#Biomass
#########
df3 <- subset(df1,TP=="A = 30")


ggplot(df3,aes(x=B.start,y=CV))+
  geom_tile(aes(fill=Biomass,colour=Biomass))+
  scale_fill_gradient(name="Biomass",low="dodgerblue",high="firebrick")+
  scale_colour_gradient(name="Biomass",low="dodgerblue",high="firebrick")+
    theme_pubr(legend="right")+
  facet_wrap(~pFmsy,ncol=2)

ggsave(here("output","figures","original","Biomass_bstart_cv_pfmsy.pdf"),width=10,height=20)

#########
#Biomass SD 
#########

ggplot(df3,aes(x=B.start,y=CV))+
  geom_tile(aes(fill=SDBiomass,colour=SDBiomass))+
  scale_fill_gradient(name="Biomass_SD",low="dodgerblue",high="firebrick")+
  scale_colour_gradient(name="Biomass_SD",low="dodgerblue",high="firebrick")+
    theme_pubr(legend="right")+
  facet_wrap(~pFmsy,ncol=2)


ggsave(here("output","figures","original","BiomassSD_bstart_cv_pfmsy.pdf"),width=10,height=20)


#########
#Ptip as function of SSB and CV 
#########

ggplot(df3,aes(x=B.start,y=CV))+
  geom_tile(aes(fill=Prob.Cross.TP,colour=Prob.Cross.TP))+
  scale_fill_gradient(name="Biomass_SD",low="dodgerblue",high="firebrick")+
  scale_colour_gradient(name="Biomass_SD",low="dodgerblue",high="firebrick")+
    theme_pubr(legend="right")+
  facet_wrap(~pFmsy,ncol=2)

ggsave(here("output","figures","original","ptip_bstart_cv_pfmsy.pdf"),width=10,height=20)


#########
#NPV as function of SSB and CV 
#########
ggplot(df3,aes(x=B.start,y=CV))+
  geom_tile(aes(fill=NPV,colour=NPV))+
  scale_fill_gradient(name="NPV",low="dodgerblue",high="firebrick")+
  scale_colour_gradient(name="NPV",low="dodgerblue",high="firebrick")+
    theme_pubr(legend="right")+
  facet_wrap(~pFmsy,ncol=2)

ggsave(here("output","figures","original","NPV_bstart_cv_pfmsy.pdf"),width=10,height=20)


#########
#Catch as a fcn of biomass and cv 
#########
ggplot(df3,aes(x=B.start,y=pFmsy))+
  geom_tile(aes(fill=CumulativeYield,colour=CumulativeYield))+
  scale_fill_gradient(name="CumulativeYield",low="dodgerblue",high="firebrick")+
  scale_colour_gradient(name="CumulativeYield",low="dodgerblue",high="firebrick")+
  theme_pubr(legend="right")+
  facet_wrap(~CV,ncol=2)


#slice out 0.1 and 0.5 and single pFmsy 
# npv_slice <- subset(df1,pFmsy == 1 & TP == "A = 20" & CV %in% c(0.1,0.5))
# npv_slice$CV <-factor(npv_slice$CV)
# 
# ggplot(npv_slice,aes(x=B.start,y=NPV,group=CV))+
#   geom_line(aes(colour=CV),size=1.5)+
#     theme_pubr(legend="right")+
#   xlab("Starting Biomass")+
#   ylab("Net Present Value (NPV)")+
#   annotate("text", x = avec[2], y = 1, label = "TP")+
#   annotate("text", x = B.lim, y = 1, label = "Blim")+
#   geom_vline(xintercept=avec[2],lty=2,colour="grey",size=1)+
#   geom_vline(xintercept=B.lim,lty=3,colour="darkgrey",size=1)
# 
# ggsave(here("output","figures","conservative","NPV_bstart_cv_pfmsy.pdf"),width=10,height=20)


#########
#Cumulative Yield as function of SSB and CV 
#########

df4<-subset(df1,pFmsy == 1 & TP == "A = 10" & CV %in% c(0.1,0.5)) # MCS: df1 doesn't have a pFMSY == 1 combo

ggplot(df4,aes(x=B.start,y=CV))+
  geom_tile(aes(fill=CumulativeYield,colour=CumulativeYield))+
  scale_fill_gradient(name="Cumulative Yield",low="dodgerblue",high="firebrick")+
  scale_colour_gradient(name="Cumulative Yield",low="dodgerblue",high="firebrick")+
    theme_pubr(legend="right")+
  facet_wrap(~pFmsy,ncol=2)

ggsave(here("output","figures","original","cyield_bstart_cv_pfmsy.pdf"),width=10,height=20)



#########
#Return on Investment Plots A = 10
#########

#NPV 0.1 - NPV 0.5

#entire fmsy vec cause you want acrross that
#1 is the NPV column 
#c(1) is the phivec but careful if you run it for longer
#1 is the avec so a=10 but could change with the vecot offered
#entire b vec cause simulating accoss that 

dimnames(ar) #look at dimemnsions of simulation 
# [[1]] is Fmsy vec
# [[2]] is output from simulation
# [[3]] i CV monitoring
# [[4]] is location of tipping point "A"
# [[5]] is starting biomass of simulation 

####ROI as difference of NPV 0.1 - NPV 0.5

#the second to last index is for a so 1 - A=10, 2- A=20, 3-A=30
npv_diff<-(ar[,1,c(1),3,]-ar[,1,c(5),3,])
npv_diff<-melt(npv_diff)
names(npv_diff)<-c("pFmsy","B.start","roi")

#calculate Return on Investment, the ratio between high and low CV

heat_diff <- ggplot(npv_diff,aes(x = B.start, y = pFmsy))+
  geom_tile(aes(fill=roi,colour=roi))+
  scale_fill_gradient(low="dodgerblue",high="firebrick")+
  scale_colour_gradient(low="dodgerblue",high="firebrick")+
  xlab("Starting Biomass")+
  ylab("pFmsy")+
  theme_pubr(legend="right")

print(heat_diff)


#Slice Plot DIFFERENCE 
gs_diff <- ggplot(npv_diff,aes(x=B.start,y=roi,group=pFmsy))+
  annotate("text", x = 20, y = 1, label = "TP")+
  # annotate("text", x = B.lim, y = 1, label = "Blim")+
  geom_vline(xintercept=30,lty=2,colour="grey",size=1.5)+
  # geom_vline(xintercept=B.lim,lty=3,colour="darkgrey",size=1.5)+
  geom_line(aes(colour=pFmsy))+
  scale_colour_gradient(low="#e0ecf4",high="#8856a7")+
  xlab("Starting Standing Stock Biomass")+
  ylab("ROI (NPV(CV.1) - NPV(CV.5)")+
    theme_pubr(legend="right")
  
print(gs_diff)


####ROI as Ratio of NPV 0.1 / NPV 0.5

#NPV 0.1 / NPV 0.5, 
npv_ratio<-(ar[,1,c(1),3,]/ar[,1,c(5),3,])
npv_ratio<-melt(npv_ratio)
names(npv_ratio)<-c("pFmsy","B.start","roi")

#heatmap of all simulations
heat_ratio <- ggplot(npv_ratio,aes(x = B.start, y = pFmsy))+
  geom_tile(aes(fill=roi,colour=roi))+
  scale_fill_gradient(low="dodgerblue",high="firebrick")+
  scale_colour_gradient(low="dodgerblue",high="firebrick")+
  xlab("Starting Biomass")+
  ylab("pFmsy")+
  theme_pubr(legend="right")

print(heat_ratio)


#Slice Plot just subset of lines  
gs_ratio <- ggplot(npv_ratio,aes(x=B.start,y=roi,group=pFmsy))+
  geom_vline(xintercept=10,lty=2,colour="grey",size=1)+
  geom_vline(xintercept=B.lim,lty=3,colour="darkgrey",size=1)+
  geom_line(aes(colour=pFmsy))+
  #annotate("text", x = avec[2], y = 1, label = "TP")+
  #annotate("text", x = B.lim, y = 1, label = "Blim")+
  scale_colour_gradient(low="dodgerblue",high="firebrick")+
  #scale_colour_gradient(low="#e0ecf4",high="#8856a7")+
  xlab("Starting Standing Stock Biomass")+
  ylab("ROI (NPV(CV.1) / NPV(CV.5)")+
    theme_pubr(legend="right")#+
  # scale_y_log10()

print(gs_ratio)

npv_ratio2<-subset(npv_ratio,pFmsy %in% c(0.0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0))
# npv_ratio2$roi<-log10(npv_ratio2$roi)

gs_ratio2 <- ggplot(npv_ratio2,aes(x=B.start,y=roi,group=pFmsy))+
  geom_vline(xintercept=10,lty=2,colour="grey",size=1)+
  geom_vline(xintercept=0.25*Bmsy,lty=3,colour="darkgrey",size=1)+
  geom_line(aes(colour=pFmsy))+
  #annotate("text", x = avec[2], y = 1, label = "TP")+
  #annotate("text", x = B.lim, y = 1, label = "Blim")+
  scale_colour_gradient(low="dodgerblue",high="firebrick",name="pHmsy")+
  # scale_y_log10(limits=c(1,10),breaks=c(1,5,10))+
  # scale_y_continuous(limits=c(0.5,8),breaks=c(1,2,3,4,5,6,7,8))+
  #scale_colour_gradient(low="#e0ecf4",high="#8856a7")+
  xlab("Starting Standing Stock Biomass")+
  ylab("Return on Investment (NPV(CV.1) / NPV(CV.5)")+
    theme_pubr(legend="right")
  
print(gs_ratio2)


# ggsave(here("output","figures","original","ROI_original.pdf"),width=5,height=5)
# multiplot(gs_diff,gs_ratio)


#Proximity to Tipping Point

# can be defined as abs(-bstart)

npv_ratio2$prox<- -1*(npv_ratio2$B.start)
npv_ratio2<-subset(npv_ratio2,pFmsy %in% c(0.0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0))

gs_ratio3 <- ggplot(npv_ratio2,aes(x=prox,y=roi,group=pFmsy))+
  geom_vline(xintercept=-10,lty=2,colour="grey",size=1)+
  geom_vline(xintercept=-0.25*Bmsy,lty=3,colour="darkgrey",size=1)+
  geom_line(aes(colour=pFmsy))+
  # geom_smooth(aes(colour=pFmsy),se=F)+
  #annotate("text", x = avec[2], y = 1, label = "TP")+
  #annotate("text", x = B.lim, y = 1, label = "Blim")+
  scale_colour_gradient(low="dodgerblue",high="firebrick",name="pHmsy")+
  # scale_y_log10(limits=c(1,10),breaks=c(1,5,10))+
  scale_x_continuous(limits=c(-100,-5),breaks=c(-100,-75,-50,-25))+
  # scale_y_continuous(limits=c(0.5,8),breaks=c(1,2,3,4,5,6,7,8))+
  #scale_colour_gradient(low="#e0ecf4",high="#8856a7")+
  xlab("Proximity to Tipping Point")+
  ylab("Return on Investment (NPV(CV.1) / NPV(CV.5)")+
    theme_pubr(legend="right")

print(gs_ratio3)



#combine but the dimmensions still need work to reproduce nice figure 
roi_plot <- plot_grid(heat_diff,heat_ratio,gs_diff,gs_ratio,ncol=2,labels="AUTO")
save_plot(here("output/figures/original","multiplot_diff_ratio_ROI_A=30_conservative.pdf"),roi_plot,base_width=8,base_height=5) 




ggsave("multiplot_diff_ratio_ROI.pdf")

##########################################################################################################################
#FIGURE Sx: Time series output for different CVs and 3 different starting biomasses for 
#1) Cumulative Yield through time 
#2) Biomass Through time 
#3) Yield through time
##########################################################################################################################

# parameterize model duration and economics
years = 20 #time of simulation
process.noise = 0 #temporally uncorrelated variance (sd) in little r 

#same parameters as in ModelparametersFile parameterize the model based on A,BMSY and MSY
# A = 10  #allee effect threshold 
# Bmsy<- 70 #Biomass at MSY
# MSY<-25 # MSY
# K<-(3*Bmsy^2 - 2*A*Bmsy)/( 2*Bmsy-A) #Carrying Capacity of Focal population (derived by TE in matlab)
# r<-MSY/(Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K)) #population growth rate 
# Fmsy<-MSY/Bmsy #Fishing mortality that produces MSY
# B.lim<-max(A,20) # lower biomass limit for harvest control rule
# B.lim<-20 

#Can play with this to show equilib goes up when Fmsy goes down
max.F<-Fmsy #maximum fishing defined as Fmsy 

B.vec <- seq(30,90, by = 30)
phivec <- seq(0.1,0.5,by=.1)

#make an emptry array and name it 
ts_ar <- array(dim=c(length(seq(1:years)),3,length(phivec),length(B.vec)))

dimnames(ts_ar) = list(seq(1:years),
                       c("Biomass","Yield","CumulativeY"),
                       phivec,
                       B.vec)


#set same seed for all simulations to focus on starting values
phi.seeds <- round(100000*runif(1),0)
proc.seeds <- round(100000*runif(1),0)

#set multiple seeds to allow variation among 
phi.seeds <- round(100000*runif(length(B.vec)),0)
proc.seeds <- round(100000*runif(length(B.vec)),0)


for(b in seq(B.vec)){
  for(j in seq(phivec)){
    
    #single seed for all simulations comment out next two lines if you want diff seed
    phi.CV.seed<-phi.seeds      
    process.noise.seed<-proc.seeds
    
    #comment the next two lines out if you want same seed 
    #phi.CV.seed<-phi.seeds[b]
    #process.noise.seed<-proc.seeds[b]
    
    m <- est.NPV(years,K,A,r,phi.CV=phivec[j],delta,process.noise,p,B.start=B.vec[b],B.lim,max.F,phi.CV.seed,process.noise.seed,c)
    
    ts_ar[,1,j,b] <- round(m$B[-21],1)
    ts_ar[,2,j,b] <- round(m$Y,1)
    ts_ar[,3,j,b] <- cumsum(m$Y)
  }
}


m1 <- melt(ts_ar)
names(m1) <-c("time","group", "CV","SSB","Response")
m1$CV2 = factor(m1$CV)
m1$CV = round(m1$CV,1)
m2 <- m1[m1$CV %in% c(0.1,0.5),]


ggplot(m2,aes(x=time,y=Response,group=CV2))+
  geom_line(aes(colour=CV2))+
  facet_grid(group~SSB,scales="free_y")+
  theme_pubr(legend="right")

ggsave("time_series_startdens.pdf")



#does the mean and var in biomass and yield differ among simuilations
tapply(m1$Response,list(m1$group,m1$CV2,m1$SSB),mean)
tapply(m1$Response,list(m1$group,m1$CV2),var)

m3 <- subset(m2,group!="CumulativeY")
ggplot(m3, aes(x=CV2, y=Response,fill=factor(CV2))) +
  stat_summary(colour="black",fun.y = median, geom = "bar", position=position_dodge(width =0.9))+
  facet_grid(group~SSB,scales="free")+
  theme_pubr(legend="right")
  ggtitle("Median response After 20 Steps")

ggsave("StartingDens_CV_barplot.pdf")



##########################################################################################################################
#NPV as a function of the strength of process noise vairation and uncertainty in tipping 
##########################################################################################################################


source(here("code","MSE_Model.R"))#l
source(here("code","ModelParameters_v1.R"))

n.iters=50
procnoisevec <- seq(0,2,by=0.1)
phivec <- seq(0.1,0.5,by=0.05) #uncertainty cv

phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

#all combianations 
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

noise_df_NPV <- gather(as.data.frame(emat),"NPV","ptip","biomass","cumulative_yield","sd-biomass","ptip_mgmt", 
                   key="response", 
                   value = "value") %>% filter(response =="NPV")


noise_NPV<-ggplot(noise_df_NPV,aes(x= process_noise, y=precision))+
             geom_tile(aes(fill=value,colour=value))+
             theme_pubr(legend="right")+
             ylab("monitoring CV")+
             xlab("Process Noise")+
             scale_fill_gradient(low="#80BE9E",high="#F98866",name="NPV", guide = gc)+
             scale_colour_gradient(low="#80BE9E",high="#F98866",name="NPV", guide = gc)

npv_l <- get_legend(noise_NPV)

noise_df_ptip <- gather(as.data.frame(emat),"NPV","ptip","biomass","cumulative_yield","sd-biomass","ptip_mgmt", 
                       key="response", 
                       value = "value") %>% filter(response =="ptip")


noise_ptip<-ggplot(noise_df_ptip,aes(x= process_noise, y=precision))+
  geom_tile(aes(fill=value,colour=value))+
  theme_pubr(legend="right")+
  ylab("monitoring CV")+
  xlab("Process Noise")+
  scale_fill_gradient(low="#80BE9E",high="#F98866",name="Ptip",guide= gc)+
  scale_colour_gradient(low="#80BE9E",high="#F98866",name="Ptip", guide = gc)

ptip_l <- get_legend(noise_ptip)


noise_df_ptipmgmt <- gather(as.data.frame(emat),"NPV","ptip","biomass","cumulative_yield","sd-biomass","ptip_mgmt", 
                        key="response", 
                        value = "value") %>% filter(response =="ptip_mgmt")


noise_ptipmgmt<-ggplot(noise_df_ptipmgmt,aes(x= process_noise, y=precision))+
  geom_tile(aes(fill=value,colour=value))+
  theme_pubr(legend="right")+
  ylab("monitoring CV")+
  xlab("Process Noise")+
  scale_fill_gradient(low="#80BE9E",high="#F98866",name="Ptip MGMT",guide= gc)+
  scale_colour_gradient(low="#80BE9E",high="#F98866",name="Ptip MGMT", guide = gc)

ptipm_l <- get_legend(noise_ptipmgmt)
  

#combine but the dimmensions still need work to reproduce nice figure 
conserv_plot <- plot_grid(noise_NPV+theme(legend.position="none"),noise_ptip+theme(legend.position="none"),noise_ptipmgmt+theme(legend.position="none"),ncol=1,labels="AUTO")
conserve_plot2 <- plot_grid(npv_l,ptip_l,ptipm_l,ncol=1,align="hv")
conserve_plot3 <- plot_grid(conserv_plot,conserve_plot2,axis="h",rel_widths=c(1,.3),align = 'h')
conserve_plot3
save_plot(here("output/figures","process_noise_cv_original_MSE_3_20_19.pdf"),conserve_plot3,base_width=6,base_height=10) 





# #############################################################
# #Q: How many bonanza and bust years (BB) happen as a population approaches a tipping point?
# #Method: Bonanza and Bust for a range of monitoring investments and range of starting values
# #############################################################
# process.noise = 0
# B.lim = 11
# 
# start.B.list <- seq(10,100,by=5)
# phivec <- seq(0.1,0.9,by=0.025)
# emat <- matrix(0,nrow=length(start.B.list),ncol=length(phivec)) #Make Empty Matrix
# colnames(emat) = phivec 
# rownames(emat) = start.B.list
# n.iters = 50
# rm(.Random.seed)
# phi.seeds<-round(1000000*runif(n.iters),0)
# process.seeds<-round(1000000*runif(n.iters),0)
# 
# for(j in seq(phivec)){
#   for (i in 1:length(start.B.list)){
#     output <-repeat.model2(n.iters,B.start=start.B.list[i],B.lim,years,K,A,r,phi.CV=phivec[j],delta=.05,process.noise=0.0,p,max.F,c,phi.seeds,process.seeds)
#     emat[i,j]<-median(c(output[[2]])) #frequency of Bonanza and bust
#   }
# }
# 
# #plot the effects of increasing cv and different starting densities on value
# df1 = melt(emat)
# df1$BB = df1$value
# ggplot(df1,aes(x = Var1, y = Var2, z = BB))+
#   geom_tile(aes(fill=BB,colour=BB))+
#   scale_fill_gradient(low="red")+
#   scale_colour_gradient(low="red")+
#   xlab("Initial Biomass")+
#   ylab("CV of Monitoring")
# 


#########################################################################################################################
#old Code for time series by response variable
##########################################################################################################################


# bdf <- rbind(subset(m1,group=="Biomass" & CV == c(0.1)),subset(m1,group=="Biomass" & CV == c(0.5)))
# ydf <-  rbind(subset(m1,group=="Yield" & CV == c(0.1)),subset(m1,group=="Yield" & CV == c(0.5)))
# cydf <-  rbind(subset(m1,group=="CumulativeY" & CV == c(0.1)),subset(m1,group=="CumulativeY" & CV == c(0.5)))
# 
# #BIOMASS
# gb = ggplot(bdf,aes(x=time,y=Response,group=CV2))+
#   geom_line(aes(colour=CV2),size=1)+
#   facet_grid(.~SSB)+
#   xlab("Time")+
#   ylab("Biomass")+
#   theme_acs()
# 
# 
# #YIELD
# gy = ggplot(ydf,aes(x=time,y=Response,group=CV2))+
#   geom_line(aes(colour=CV2),size=1)+
#   facet_grid(.~SSB)+
#   xlab("Time")+
#   ylab("Yield")+
#   theme_acs()
# 
# #CUMULATIVE YIELD 
# gcy = ggplot(cydf,aes(x=time,y=Response,group=CV2))+
#   geom_line(aes(colour=CV2),size=1)+
#   facet_grid(.~SSB)+
#   xlab("Time")+
#   ylab("Cumulative Yield")+
#   theme_acs()
# 
# multiplot(gb,gy,gcy)
