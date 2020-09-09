
library(reshape2)
library(ggplot2)
library(ggpubr)

#load
source(here::here("code","archive","theme_publication.R")) #graphing hack 1
source(here::here("code","archive","multiplot.R")) #graphic hack 2
source(here::here("code","2_model_parameters.R")) # base parameters
source(here::here("code","3_mse_model.R")) #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"

#############################################################
#BELOW PLOT A 4 panel figure for the conceptiual description of the model 
#############################################################


#####################################
#Figure 1a AS Plot a bunch of A's on a single panel 
#####################################

#Allee model derived by TE 
dbdt.fun<-function(B,K,A,r,F) r*B*(1-B/K)*(B/K-A/K)-F

Blist<-seq(0,120,by=1)


A.list<-seq(0,40,by=10)
emat <- matrix(0,nrow=length(Blist),ncol=length(A.list))

for (i in 1:length(A.list)){
  A=A.list[i]
  Bmsy<- 70
  K<--(3*Bmsy^2 - 2*A*Bmsy)/(A - 2*Bmsy)
  print(Bmsy)
  MSY<-20
  r<-MSY/(Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K))
  Fmsy<-MSY/Bmsy
  
  dbdt<-sapply(Blist,FUN=dbdt.fun,K=K,A=A,r=r,F=0)
  emat[,i] <- dbdt
}


df1 <- data.frame(emat)
df1$time = Blist
colnames(df1) <- c(A.list,"time")

df2 <-melt(df1,id.vars="time")
colnames(df2) <- c("time","A","Biomass")
df2$A <-as.numeric(as.character(df2$A))

gga = ggplot(df2,aes(x=time,y=Biomass,group=A))+
  geom_line(aes(colour=A))+
  scale_colour_gradient(low="dodgerblue",high="firebrick")+
  geom_hline(yintercept=0)+
  xlab("Biomass")+
  ylab("Change in Biomass Per Unit Time (dB/dt)")+
  theme_pubr()+
  # theme_Publication()+
  theme(legend.position = "right")


print(gga)
ggsave("output/figures/dbdt.pdf",width=10,height=5)

#######
###Figure 1b Harvest control Rule for different Biomasses 
#######

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

Fo<- -(max.F*B.lim)/(Bmsy - B.lim)
b<-max.F/(Bmsy - B.lim)


bvec <-seq(0,100,by=1)
yvec <-rep(0,length(bvec))

for(i in 1:length(bvec)){
  
# MANAGEMENT MODEL: Determine F based on Harvest Control Rule
  if (bvec[i]>=Bmsy) F<-max.F 
  if (bvec[i]<=Bmsy) F<-Fo+b*bvec[i]
  if (bvec[i]<=B.lim) F<-0
  
  yvec[i]<-F*bvec[i]
  
}

df3 <- data.frame(bvec,yvec)
df3$prop <-df3$yvec/df3$bvec
plot(df3$prop,type="l")

ggb = ggplot(df3,aes(x=bvec,y=prop))+
  geom_line(colour="dodgerblue",size=1)+
  xlab("Standing Stock Biomass")+
  ylab("Proportion of Biomass Caught")+
  theme_Publication()

ggsave("managementmodel.pdf",width=5,height=2)

print(ggb)



######
#Fig 1c how does deviation of Bhat from B scale for different values of CV
#####

# parameterize the model based on A,BMSY and MSY
Bmsy<- A/3 + K/3 + (A^2 - A*K + K^2)^(1/2)/3 #Biomass at MSY
MSY<-r*Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K) #MSY
K<-(3*Bmsy^2 - 2*A*Bmsy)/( 2*Bmsy-A) #Carrying Capacity of Focal population (derived by TE in matlab)
r<-MSY/(Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K)) #population growth rate 
Fmsy<-MSY/Bmsy #Fishing mortality that produces MSY
max.F<-Fmsy #maximum fishing defined as Fmsy
B.lim<-max(A,0.25*Bmsy) # lower biomass limit for harvest control rule 


B.start <- 100
avec<-seq(30,30,1)
phivec <- seq(0.1,0.5,by=.01)
FMSYvec <- seq(.1,1,by=0.1)*max.F #manipulating FMSY max.F
ar <- array(dim=c(length(FMSYvec),2,length(phivec),length(A)))
dimnames(ar) = list(FMSYvec,c("NPV","Prob.Cross.TP"),phivec,paste("A =",avec))

emat<-matrix(0,nrow=length(phivec),ncol=2)
colnames(emat) <-c("Phi","dB")

n.iters = 100
rm(.Random.seed)
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

for(j in seq(phivec)){
  
  phi.CV.low=phi.CV.high=phivec[j]
  value <-repeat.model2(n.iters,B.start,B.lim,years,K,A=avec,r,phi.CV,delta=.05,process.noise=0.0,p,max.F,phi.seeds,process.seeds)
  emat[j,2] <-median(value[[5]])
  
}

emat[,1] <-phivec

df_cv <-data.frame(emat)

ggc = ggplot(df_cv,aes(x=Phi,y=dB))+
  geom_line(colour="dodgerblue",size=.5)+
  xlab("CV")+
  ylab("Median Sampling Error (abs(Biomass-Bhat))")+
  theme_Publication()

# ggsave("sampling_error.pdf",width=5,height=2)

print(ggc)


############
#What does the function that TE look like accross a range of CVs 
###########

years=10000
phivec2 <- seq(0.1,0.5,by=.1)
#phivec2 <-c(0.001,0.1,0.2,0.5)
emat2 =matrix(nrow=length(seq(1:years)),ncol=length(phivec2))
colnames(emat2) <-factor(phivec2)
rownames(emat2) <-c(seq(1:years))


for(j in seq(phivec2)){
  B.errors<-exp(rnorm(years,mean=(0-phivec2[j]^2/2),sd=phivec2[j]))
  emat2[,j] <-B.errors
}

df_cv2<- data.frame(melt(emat2))
df_cv2$Var2 <-factor(df_cv2$Var2)

ggplot(df_cv2,aes(x=value))+
  geom_histogram(aes(fill=Var2,colour=Var2))+
  facet_grid(Var2~.)+
  theme_Publication()

tapply(df_cv2$value,list(df_cv2$Var2),median)
tapply(df_cv2$value,list(df_cv2$Var2),sd)


#What does the function that TE look like accross a range of CVs 
years=10000
phivec <- seq(0.1,0.5,by=.1)
emat2 =matrix(nrow=length(seq(1:years)),ncol=length(phivec))
colnames(emat2) <-factor(phivec)
rownames(emat2) <-c(seq(1:years))


for(j in seq(phivec)){
  B.errors<-rlnorm(years,mean=(phivec[j]),sd=phivec[j])
  emat2[,j] <-B.errors
}

emat2[,1] <-phivec
df_cv2<- data.frame(melt(emat2))
df_cv2$Var2 <-factor(df_cv2$Var2)

ggplot(df_cv2,aes(x=value))+
  geom_histogram(aes(fill=Var2,colour=Var2))+
  facet_grid(Var2~.,scales="free_y")+
  theme_Publication()

tapply(df_cv2$value,list(df_cv2$Var2),median)


#############################################################
###Example data output of Yield, Biomass, Bhat for two different CVs
#############################################################

source("ModelParameters_v1.R") # base parameters
rm(.Random.seed)
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

B.vec <- seq(50,100, by = 50)
phivec <- seq(0.1,0.5,by=.1)


ts_ar <- array(dim=c(length(seq(1:years)),3,length(phivec),length(B.vec)))
dimnames(ts_ar) = list(seq(1:years),
                       c("Biomass","Yield","Bhat"),
                       phivec,
                       B.vec
) 


for(b in seq(B.vec)){
  for(j in seq(phivec)){
    
    phi.CV.low=phi.CV.high=phivec[j]
    m <- est.NPV(years,K,A,r,phi.CV.low,phi.CV.high,delta,process.noise,p,B.start=B.vec[b],B.crit,B.lim,max.F,phi.CV.seed,process.noise.seed,c)
    
    ts_ar[,1,j,b] <- round(m$B[-21],1)
    ts_ar[,2,j,b] <- round(m$Y,1)
    ts_ar[,3,j,b] <- round(m$Bhat[-1],1)
  }
}


m1 <- melt(ts_ar)
names(m1) <-c("time","group", "CV","SSB","Response")
m1$CV2 = factor(m1$CV)
m1$CV = round(m1$CV,1)

bdf <- rbind(subset(m1,group=="Biomass" & CV == c(0.1)),subset(m1,group=="Biomass" & CV == c(0.5)))
bhatdf <-  rbind(subset(m1,group=="Bhat" & CV == c(0.1)),subset(m1,group=="Bhat" & CV == c(0.5)))
ydf <-  rbind(subset(m1,group=="Yield" & CV == c(0.1)),subset(m1,group=="Yield" & CV == c(0.5)))

df_all<- rbind(bdf,bhatdf,ydf)
df_100<- subset(df_all,SSB==100)
df_100$ttt <-paste(df_100[,2],df_100[,6])
df_100$group2 <-c(rep("Biomass",80),
                  rep("Yield",40))
df_100$group3 <-c(rep("Actual",40),
                  rep("Estimated",40),
                  rep("Actual",40))


ggd = ggplot(df_100,aes(x=time,y=Response,group=ttt))+
  geom_line(aes(colour=group2,lty=group3),size=.5)+
  facet_grid(.~CV2,scales = "free_y")+
  xlab("Time")+
  ylab("Biomass of Fish")+
  theme_Publication()

print(ggd)

df_b<-subset(df_100,group2=="Biomass")

ggplot(df_b,aes(x=time,y=Response,group=ttt))+
  geom_line(aes(colour=CV2,lty=group3),size=.5)+
  facet_grid(.~CV2,scales = "free_y")+
  geom_hline(yintercept=B.lim)+
  scale_y_continuous(limits=c(0,300))+
  xlab("Time")+
  ylab("Biomass of Fish")+
  theme_Publication()

df_100b<-subset(df_100,CV==0.1)

ggplot(df_100b,aes(x=time,y=Response,group=ttt))+
  geom_line(aes(colour=group2,lty=group3),size=.5)+
  facet_wrap(~group2,scales = "free_y",ncol=1)+
  geom_hline(yintercept=B.lim)+
  geom_hline(yintercept=A,lty=2)+
  xlab("Time")+
  ylab("Biomass of Fish")+
  theme_Publication()

ggsave("assessment model.pdf",width=5,height=2)

multiplot(gga,ggb,ggc,ggd,cols=2)


