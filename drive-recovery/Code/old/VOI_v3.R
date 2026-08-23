##########################################
#Value of Information Simulations 
##########################################

#set directory
setwd("/Users/adrianstier/Dropbox/Projects/In Progress/Value of Information/Code/") #ACS Laptop

#load packages
library(ggplot2)
library(reshape2)

source("theme_acs.R")
source("multiplot.R")
source("ModelParameters_v1.R") # base parameters
source("MSE_Model.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"


##########################################################################################################################
#FIGURE 2: Prob tipping underdifferent scenarios of CV, A, and %Fmsy
##########################################################################################################################

B.start <- 50
avec <- seq(10,50,by = 10)
phivec <- seq(0.1,0.5,by=.01)
FMSYvec <- seq(.1,1,by=0.01)*max.F #manipulating FMSY max.F
ar <- array(dim=c(length(FMSYvec),2,length(phivec),length(avec)))
dimnames(ar) = list(FMSYvec,c("NPV","Prob.Cross.TP"),phivec,paste("A =",avec))

n.iters = 300
rm(.Random.seed)
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)


for(a in seq(avec)){  
  for(j in seq(phivec)){
    for (i in seq(FMSYvec)){
      max.F = FMSYvec[i]
      value <-repeat.model2(n.iters,B.start,B.lim,years,K,A=avec[a],r,phi.CV=phivec[j],delta=.05,process.noise=0.0,p,max.F,c,phi.seeds,process.seeds)
      
      ar[i,1,j,a] <-median(c(value[[1]]))  #NPV
      ar[i,2,j,a] <- sum(value[[3]])/n.iters #p tip
    }
  }
}


df1 = melt(ar,varnames=names(dimnames(ar)))
colnames(df1) = c("Fmsy","target","CV","A","value")

df1 = reshape(df1,
              timevar = "target",
              idvar = c("Fmsy","A","CV"),
              direction = "wide")

colnames(df1) = c("Fmsy","CV","TP","NPV","Prob.Cross.TP")
df1$pFmsy = round(df1$Fmsy/max.F,4)
range(df1$Prob.Cross.TP) #very low prob of tipping
range(df1$pFmsy)

######Probability of crossing a tipping point accross Fmsy and CV monitoring - heatmap

ggplot(df1,aes(x = CV, y = pFmsy))+
geom_tile(aes(fill=Prob.Cross.TP,colour=Prob.Cross.TP))+
scale_fill_gradient(low="dodgerblue",high="firebrick")+
scale_colour_gradient(low="dodgerblue",high="firebrick")+
xlab("CV of Monitoring")+
ylab("Fmsy")+
facet_grid(.~TP)+
  theme_acs()

df1a30 <- subset(df1,TP == "A = 30")
g1 = ggplot(df1a30,aes(x = CV, y = pFmsy))+
  geom_tile(aes(fill=Prob.Cross.TP,colour=Prob.Cross.TP))+
  scale_fill_gradient(low="dodgerblue",high="firebrick")+
  scale_colour_gradient(low="dodgerblue",high="firebrick")+
  xlab("CV of Monitoring")+
  ylab("Fmsy")+
  theme_acs()

#Probability of crossing a tipping point accross Fmsy and CV monitoring - slice plot

df2 <- subset(df1,CV %in% c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1))

ggplot(df2,aes(x=CV,y=Prob.Cross.TP,group=pFmsy))+
  geom_line(aes(colour=pFmsy))+
  scale_colour_gradient(low="#fee8c8",high="#e34a33")+
    facet_grid(TP~.)+
  xlab("CV of Monitoring")+
  ylab("Probability of Crossing a Tipping Point")+
  facet_grid(TP~.)+
  theme_acs()

#Just A = 30

df3 <- subset(df2,TP == "A = 30")

g2 = ggplot(df3,aes(x=CV,y=Prob.Cross.TP,group=pFmsy))+
  geom_line(aes(colour=pFmsy))+
  scale_colour_gradient(low="#fee8c8",high="#e34a33")+
  xlab("CV of Monitoring")+
  ylab("Probability of Crossing a Tipping Point")+
  theme_acs()

print(g2)


#what is the cv necessary to get 5% chance of collapse for differen pFmys

df_3b <- subset(df1, TP == "A = 30")

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
         geom_line(aes(colour=PercentRisk))+
         xlab("pFmsy")+
         ylab("Max CV to Avoid Tipping point")+
  theme_acs()

print(g3)

##now plot NPV acros pFmsy for different CVs

g4 = ggplot(df_3b,aes(x=pFmsy,y=NPV,group=CV))+
  geom_line(aes(colour=CV))+
  scale_colour_gradient(low="#e0f3db",high="#43a2ca")+
    xlab("pFmsy")+
  ylab("Net Present Value")+
  theme_acs()

print(g4)

multiplot(g1,g2,g3,g4, cols=2)



##########################################################################################################################
#FIGURE 3: How does the value of information change accross a range of stock biomassses
##########################################################################################################################

#TE's code with repeat.model function

start.B.list<-seq(10,100,by=5)
max.F = Fmsy


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

#AS's code with "repeat.model2" function


B.vec <- seq(10,100, by = 1)
avec <- seq(10,50,by = 10)
phivec <- seq(0.1,0.5,by=.1)
FMSYvec <- seq(.2,1,by=0.2)*max.F #manipulating FMSY max.F
ar <- array(dim=c(length(FMSYvec),2,length(phivec),length(avec),length(B.vec)))
dimnames(ar) = list(FMSYvec,c("NPV","Prob.Cross.TP"),phivec,paste("A =",avec),B.vec)


n.iters = 10
rm(.Random.seed)
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

for(b in seq(B.vec)){
  for(a in seq(avec)){  
    for(j in seq(phivec)){
      for (i in seq(FMSYvec)){
        max.F = FMSYvec[i]
        value <-repeat.model2(n.iters,B.start=B.vec[b],B.lim,years,K,A=avec[a],r,phi.CV=phivec[j],delta=.05,process.noise=0.0,p,max.F,c,phi.seeds,process.seeds)
        
        ar[i,1,j,a,b] <-median(c(value[[1]]))  #NPV
        ar[i,2,j,a,b] <- sum(value[[3]])/n.iters #p tip
        
      }
    }
  }
}


roi <- ar[,1,1,3,] - ar[,1,5,3,] #calculate Return on Investment, the deviation between high and low CV
df1 <- melt(roi)
names(df1) <-c("Fmsy","Biomass","ROI")
df1$pFmsy = round(df1$Fmsy/max.F,4)


ggplot(df1,aes(x=Biomass,y=ROI,group=pFmsy))+
  geom_vline(xintercept=avec[3],lty=2,colour="grey",size=1.5)+
  geom_line(aes(colour=pFmsy))+
  scale_colour_gradient(low="#e0ecf4",high="#8856a7")+
  xlab("Standing Stock Biomass")+
  ylab("Return on Investment (NPV(CV.1)-NPV(CV.5)")+
  theme_acs()

#explore the mechanisms underlying this humped pattern by looking at time series of output accross different CVs and 3 differetn starting biomasses
  #1) Cumulative Yield through time 
  #2) Biomass Through time 
  #3) Yield through time

# quick routine to see one model iteration result

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

B.lim<-20 
B.start<-50 

B.vec <- seq(30,90, by = 30)
phivec <- seq(0.1,0.5,by=.1)

ts_ar <- array(dim=c(length(seq(1:years)),3,length(phivec),length(B.vec)))
dimnames(ts_ar) = list(seq(1:years),
                       c("Biomass","Yield","CumulativeY"),
                       phivec,
                       B.vec
) 


  for(b in seq(B.vec)){
      for(j in seq(phivec)){
        
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
  

bdf <- rbind(subset(m1,group=="Biomass" & CV == c(0.1)),subset(m1,group=="Biomass" & CV == c(0.5)))
ydf <-  rbind(subset(m1,group=="Yield" & CV == c(0.1)),subset(m1,group=="Yield" & CV == c(0.5)))
cydf <-  rbind(subset(m1,group=="CumulativeY" & CV == c(0.1)),subset(m1,group=="CumulativeY" & CV == c(0.5)))

#BIOMASS
gb = ggplot(bdf,aes(x=time,y=Response,group=CV2))+
  geom_line(aes(colour=CV2),size=2)+
  facet_grid(.~SSB)+
  xlab("Time")+
  ylab("Biomass")+
  theme_acs()


#YIELD
gy = ggplot(ydf,aes(x=time,y=Response,group=CV2))+
  geom_line(aes(colour=CV2),size=2)+
  facet_grid(.~SSB)+
  xlab("Time")+
  ylab("Yield")+
  theme_acs()

#CUMULATIVE YIELD 
gcy = ggplot(cydf,aes(x=time,y=Response,group=CV2))+
  geom_line(aes(colour=CV2),size=2)+
  facet_grid(.~SSB)+
  xlab("Time")+
  ylab("Cumulative Yield")+
  theme_acs()

multiplot(gb,gy,gcy)




##########################################################################################################################
#Figure 4: adaptive monitoring
##########################################################################################################################
#wrote an additional piece of code to change monitoring depending on tipping






##########################################################################################################################
#Extra Code. Fun But maybe for another paper:  bonanza-bust year dmonstration
##########################################################################################################################


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
