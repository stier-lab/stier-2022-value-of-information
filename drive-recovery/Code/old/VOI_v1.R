#########
##Value of Information
#########

#Load Packages
library(ggplot2)
library(primer)
library(reshape)


############################################################################################
############################################################################################
############################################################################################
#ALLEE G-S Model
############################################################################################
############################################################################################
############################################################################################


##################################################
#Visualize Logistic Growth With Allee Effect - Courchamp 1999
##################################################
dallee <- function(k = 100,k_a = 20, rd = .5, N0 = 50, t = 50){
  N <- c(N0, numeric(t))
  for (i in 1:t) N[i + 1] <- {
    N[i] + rd * N[i] * (1 - N[i]/k) * (N[i]/k_a-1)
  }
  return(N)
}

Nts <- dallee()
plot(0:t, Nts,type="l")



##################################################
#Function 2: Allee Effect, Gordon-Schaefer discrete logistic population growth with fishing 
##################################################

years = 50 #time of simulation
N <- matrix(0,nrow=years+1,ncol=10) #Make Empty Matrix
N[,1] = seq(0:years+1) #Fill out year column in emat 

k = 100 #carrying capacity of the fishery
k_a = 40  #allee effect threshold below which poulation declines does weird stuff below 20 but that should be okay
r = .5 #population growth parameter 
phi=2 #variance estimator for a constant investment - particularly sensitive to phi
rnoise = 0 #temporally uncorrelated variance (sd) in little r 
ppp = 10 #price per pound (dollars per biomass)
com = 10 #cost of monitoring (com) is the investment phi times a made up constant per unit of monitoring effort

N[1,2] = 60 #starting density

colnames(N) = c("Time","Number","Estimated Number","Actual Yield","Possible Yield","Lost or Gained Yield","Monitoring Investment","Revenue","Cost","Profit")

for(t in 1:years){
  
  Nhat <- rnorm(1,mean = N[t,2],sd = phi) #Estimate of population from previous time step
  N[t+1,3] <- Nhat
  
  Y <- ifelse(Nhat>k/2,Nhat-k/2,0) #Yield is anything over k/2 i.e. msy given estimated biomass
  N[t+1,4] <- Y
  
  Nt1 <- N[t,2] + rnorm(1,mean = r, sd = rnoise) * N[t,2] * (1-N[t,2]/k) * (N[t,2]/k_a-1) - Y #population grows logistically with allee minus some yield
  N[t+1,2] <- Nt1
}

N[,5] = N[,2] - k/2 #possible yield if perfect knowledge
N[,6] = N[,5] - N[,4] #difference in actual yield versus appropriate yield - sometimes negative i.e. overyielding
N[,7] = phi
N[,8] = N[,4]*ppp  #revenue as a function of yield * price per pound (ppp)
N[,9] = phi*com #cost is the price per monitoring effort
N[,10] = N[,8] - N[,9]


ndf = data.frame(N)
ndf = ndf[,c(1,4,5)]
ndf = melt(ndf,id.vars=c("Time"))

ggplot(ndf,aes(x=Time,value,group=variable))+
  geom_point(aes(colour=variable))+
  geom_line(aes(colour=variable))+
  theme(
    text = element_text(colour="black"),
    line = element_line(colour="black",size=1),
    axis.text = element_text(colour="black",size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA,colour = "black",size=2),
    axis.text.x=element_text(angle = 0),
    strip.background = element_rect(fill="black"),
    strip.text = element_text(colour="white",size=12)
  )+
  labs(x="Time",y="Yield")



ndf = data.frame(N)
ndf = ndf[,c(1,10)]
ndf = melt(ndf,id.vars=c("Time"))
ndf = subset(ndf,Time>2)

ggplot(ndf,aes(x=Time,value,group=variable))+
  geom_point(aes(colour=variable))+
  geom_line(aes(colour=variable))+
  theme(
    text = element_text(colour="black"),
    line = element_line(colour="black",size=1),
    axis.text = element_text(colour="black",size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA,colour = "black",size=2),
    axis.text.x=element_text(angle = 0),
    strip.background = element_rect(fill="black"),
    strip.text = element_text(colour="white",size=12)
  )+
  labs(x="Time",y="Yield")



#do some time series summary tests with the arima function 
# acf(ndf$value) #negative autocorrelation for lag 1
# a1 = arima(ndf$value,c(1,0,0))
# a2 = arima(ndf$value,c(2,0,0))
# a3 = arima(ndf$value,c(3,0,0))
# AIC(a1,a2,a3)


##################################################
#Evaluate GS Allee (GSA)
##################################################
####ARRAY DIMMENSIONS 
#1-time
#2-range of fixed monitoring investments

##Add 2 new harvest control rules as columns to ouput matrix includiing measuring only every even year
#add way to start at different densities

#determine even and odd years
is.odd <- function(x) x %% 2 != 0

#starting values
years = 50 #time of simulation
phivec = 3 #range of 
k = 100 #carrying capacity of the fishery
k_avec = c(35,49,55) #a range of thresholds for different levels at which the poulation crosses a threshold 
r = .5 #population growth parameter 
rnoise = 0 #temporally uncorrelated variance (sd) in little r 
ppp = 10 #price per pound (dollars per biomass)
com = 10 #cost of monitoring (com) is the investment phi times a made up constant per unit of monitoring effort

startvec = c(k,.75*k,.5*k)

#Make Empty Array
GSA <- array(dim=c(length(seq(0:years+1)),10,length(seq(1:phivec)),length(k_avec),length(startvec)))


#time step and starting densities
GSA[,1,,,] = rep(seq(0:years),3) #fill time in row 
#GSA[1,2,,,] = rep(k,3) #starting density at msy

colnames(GSA) = c("Time","Number","Estimated Number","Actual Yield","Possible Yield Rel K/2","Lost or Gained Yield Rel K/2","Monitoring Investment","Revenue k/2","Cost k/2","Profit k/2")

for(startindex in seq(startvec)) {
  
  for(kaindex in seq(k_avec)){
    
    for(phindex in 1:phivec){
      
      for(t in 1:years){
        
        #STARTING DENSITIES
        GSA[1,2,,,] = rep(startvec[startindex],3)
        
        
        #SAMPLING MODEL
        Nhat <- rnorm(1,mean = GSA[t,2,phindex,kaindex,],sd = 1/phindex) #Estimate of population from previous time step 1/j is hyperbolic
        Nhatodd <- ifelse(is.odd(t)==TRUE,rnorm(1,mean = GSA[t,2,phindex,kaindex,],sd = 1/phindex),GSA[t-1,3,phindex,kaindex,]) #if odd sample, if even go back 2 years and assume same 
              
        GSA[t+1,3,phindex,kaindex,] <- Nhat
        
        #MANAGEMENT & ASSESSMENT MODEL HARVEST CONTROL RULES
        Y.5b0 <- ifelse(Nhat>k/2,Nhat-k/2,0) #Yield is take all of excess over k/2 i.e. MSY given estimated biomass if above k/2 else take none that year -
        Y.75b0 <- ifelse(Nhat>(.75*k),Nhat-(.75*k),0) #Yield is take all of excess over .75 none i.e. MEY given estimated biomass - more risky than y.5b0 
        Yprec <- ifelse(Nhat>k/2,.2*Nhat,0) #Yield is 20%  if above k/2  otherwise take none
        Yprec2 <- ifelse(Nhat>k/2,.2*(Nhat-k/2),0) #Yield is 20%  if above k/2  otherwise take none
        
        GSA[t+1,4,phindex,kaindex,] <- Y.5b0 #saved into array, others above are calculated but not saved (for now)
        
        #OPERATING MODEL
        Nt1 <- GSA[t,2,phindex,kaindex,] + r * GSA[t,2,phindex,kaindex,] * (1-GSA[t,2,phindex,kaindex,]/k) * (GSA[t,2,phindex,kaindex,]/k_avec[kaindex]-1) - Y.5b0 #GS - allee model k_a is where dn/dt goes negative  
        #could simultaneously run a different model here
        
        GSA[t+1,2,phindex,kaindex,] <- Nt1
        
        
      }
      
      GSA[,5,phindex,kaindex,] = GSA[,2,phindex,kaindex,] - k/2 #possible yield if perfect knowledge
      GSA[,6,phindex,kaindex,] = GSA[,5,phindex,kaindex,] - GSA[,4,phindex,kaindex,] #difference in actual yield versus appropriate yield - sometimes negative i.e. overyielding
      GSA[,7,phindex,kaindex,] = phindex #how much of the population was measured
      GSA[,8,phindex,kaindex,] = GSA[,4,phindex,kaindex,]*ppp  #revenue as a function of yield * price per pound (ppp)
      GSA[,9,phindex,kaindex,] = phindex*com #cost is the price per monitoring effort
      GSA[,10,phindex,kaindex,] = GSA[,8,phindex,kaindex,] - GSA[,9,phindex,kaindex,] #Profit
      
    }
      
  }
}

#########
### Pull out and plot deviation from optimal yield for low medium and high investment at different k_a's
########


gsadf=data.frame(GSA)
#adf = melt(A,id.vars=c("Time"))

#pull out three different values
# gsadf2 = gsadf[,c(1,6,16,26)]
# gsadf2 = gsadf[,c(1,36,46,56)]
# gsadf2 = gsadf[,c(1,66,76,86)]
gsadf2 =gsadf[,c(1,6,16,26,36,46,56,66,76,86)]


gsadf2 = melt(gsadf2,id.vars=c("Time.1.1.1"))
gsadf2[,4] = c(rep("Low Investment",length(gsadf[,1])),rep("Med Investment",length(gsadf[,1])),rep("High Investment",length(gsadf[,1])))
gsadf2[,5] = c(rep(paste("K_a = ",paste(k_avec[1])),length(gsadf[,1])*3),rep(paste("K_a = ",paste(k_avec[2])),length(gsadf[,1])*3),rep(paste("K_a = ",paste(k_avec[3])),length(gsadf[,1])*3))
  
  
names(gsadf2) = c("Time","Output","Amount","Investment","K_a")
                                                      
gsadf2$Investment <- factor(gsadf2$Investment, levels = c("Low Investment", "Med Investment", "High Investment"))

gsadf2 = subset(gsadf2,Time>3)


ggplot(gsadf2,aes(x=Time,y=Amount,group=Investment))+
  geom_point(aes(colour=Investment))+
  geom_line(aes(colour=Investment))+
  facet_grid(K_a~.,scales="free_y")+
  theme(
    text = element_text(colour="black"),
    line = element_line(colour="black",size=1),
    axis.text = element_text(colour="black",size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA,colour = "black",size=2),
    axis.text.x=element_text(angle = 0),
    strip.background = element_rect(fill="black"),
    strip.text = element_text(colour="white",size=12)
  )+
  labs(x="Time",y="Deviation from Optimal Yield (Optimal - Actual Yield)")+
  ggtitle("Start at K")


#########
### Pull out and Profit data
########

gsadf2 =gsadf[,c(1,10,20,30,40,50,60,70,80,90)]


gsadf2 = melt(gsadf2,id.vars=c("Time.1.1.1"))
gsadf2[,4] = c(rep("Low Investment",length(gsadf[,1])),rep("Med Investment",length(gsadf[,1])),rep("High Investment",length(gsadf[,1])))
gsadf2[,5] = c(rep(paste("K_a = ",paste(k_avec[1])),length(gsadf[,1])*3),rep(paste("K_a = ",paste(k_avec[2])),length(gsadf[,1])*3),rep(paste("K_a = ",paste(k_avec[3])),length(gsadf[,1])*3))


names(gsadf2) = c("Time","Output","Amount","Investment","K_a")

gsadf2$Investment <- factor(gsadf2$Investment, levels = c("Low Investment", "Med Investment", "High Investment"))

gsadf2 = subset(gsadf2,Time>3)


ggplot(gsadf2,aes(x=Time,y=Amount,group=Investment))+
  geom_point(aes(colour=Investment))+
  geom_line(aes(colour=Investment))+
  facet_grid(K_a~.,scales="free_y")+
  theme(
    text = element_text(colour="black"),
    line = element_line(colour="black",size=1),
    axis.text = element_text(colour="black",size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA,colour = "black",size=2),
    axis.text.x=element_text(angle = 0),
    strip.background = element_rect(fill="black"),
    strip.text = element_text(colour="white",size=12)
  )+
  labs(x="Time",y="Profit (Revenue from Yield - Monitoring Costs)")+
  ggtitle("Start at K")


######
#Estimate Some Volitility and Boom Bust  with ARIMA function for Time Series
######
  
gsadf_vol =gsadf[,c(1,10,20,30,40,50,60,70,80,90)]
ncol(gsadf_vol)

emat = matrix(ncol=8,nrow=ncol(gsadf_vol))

for(i in 2:ncol(gsadf_vol)){
emat [i,1] <- tryCatch(arima(gsadf_vol[4:51,i],c(1,0,0))$coef[2],error=function(e) NA) #mean from arma
emat[i,3] <- mean(gsadf_vol[4:51,i],na.rm=TRUE) #regular mean
emat [i,2] <- tryCatch(arima(gsadf_vol[4:51,i],c(1,0,0))$sigma2 ,error=function(e) NA)#var from arma
emat[i,4] <- var(gsadf_vol[4:51,i],na.rm=TRUE) #regular var
emat[i,5] <- length(which(gsadf_vol[4:51,i]>(gsadf_vol[4:51,i]+2*sd(gsadf_vol[4:51,i])))) #boom >2SD
emat[i,6] <- length(which(gsadf_vol[4:51,i]<(gsadf_vol[4:51,i]-2*sd(gsadf_vol[4:51,i])))) #bust <2SD
emat[i,7] <- length(which(gsadf_vol[4:51,i]>abs(2.5*mean(gsadf_vol[4:51,i]))))  #boom >250% of mean
emat[i,8] <- length(which(gsadf_vol[4:51,i]<abs(.25*mean(gsadf_vol[4:51,i])))) #bust <2SD
}

voldf = data.frame(emat[-1,]) 
voldf$Investment = rep(c("Low Investment","Medium Investment","High Investment"),3)
voldf$Threshold = c(rep(paste("K_a = ",paste(k_avec[1])),3),rep(paste("K_a = ",paste(k_avec[2])),3),rep(paste("K_a = ",paste(k_avec[3])),3))
names(voldf) = c("ARIMA_Mean","ARIMA_var","Mean","Var","BoomOutlier","BustOutlier","Boom250","Bust25","Investment","K_avec")



#Test
#tryCatch(arima(gsadf_vol[4:51,9],c(1,0,0),method="ML")$coef[2],error = function(e) NA)
#var(gsadf_vol[4:51,4])
#mean(gsadf_vol[4:51,9])



#########
### HARVEST CONTROL RULES - Pull out and plot deviation from optimal yield - this is sloppy and should be in array
########


gsadf=data.frame(GSA)
#adf = melt(A,id.vars=c("Time"))

#pull out three different values
# gsadf2 = gsadf[,c(1,6,16,26)]
# gsadf2 = gsadf[,c(1,36,46,56)]
# gsadf2 = gsadf[,c(1,66,76,86)]
gsadf2 =gsadf[,c(1,6,16,26)]


gsadf2 = melt(gsadf2,id.vars=c("Time.1.1.1"))
gsadf2[,4] = c(rep("Low Investment",length(gsadf[,1])),rep("Med Investment",length(gsadf[,1])),rep("High Investment",length(gsadf[,1])))

names(gsadf2) = c("Time","Output","Amount","Investment")

gsadf2$Investment <- factor(gsadf2$Investment, levels = c("Low Investment", "Med Investment", "High Investment"))

gsadf2 = subset(gsadf2,Time>3)

#gsadf_prec = gsadf2
#gsadf_prec2 = gsadf2
#gsadf_5k = gsadf2
#gsadf_25k = gsadf2

dd3 = rbind(gsadf_prec,gsadf_25k,gsadf_5k,gsadf_prec2)
dd3$HCR=c(rep("20%B0 if over MSY",144),rep("MEY",144),rep("MSY",144),rep("Buffer",144))
dd3$HCR <- factor(dd3$HCR, levels = c("Buffer", "MSY", "MEY","20%B0 if over MSY"))

#write.csv(dd3,"temp_HSCdf.csv")


ggplot(dd3,aes(x=Time,y=Amount,group=Investment))+
  geom_point(aes(colour=Investment))+
  geom_line(aes(colour=Investment))+
  facet_grid(.~HCR,scales="free_y")+
  theme(
    text = element_text(colour="black"),
    line = element_line(colour="black",size=1),
    axis.text = element_text(colour="black",size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA,colour = "black",size=2),
    axis.text.x=element_text(angle = 0),
    strip.background = element_rect(fill="black"),
    strip.text = element_text(colour="white",size=12)
  )+
  labs(x="Time",y="Deviation from Optimal Yield (Optimal - Actual Yield)")+
  ggtitle("")





############################################################################################
############################################################################################
############################################################################################
############################################################################################
######Below are function Visualizations for the allee model(s)
############################################################################################
############################################################################################
############################################################################################
############################################################################################

##################################################
#Visualize Logistic Growth
##################################################

dlogistic <- function(k = 100, rd = 1, N0 = 2, t = 15){
  N <- c(N0, numeric(t))
  for (i in 1:t) N[i + 1] <- {
    N[i] + rd * N[i] * (1 - N[i]/k)
  }
return(N)
}

Nts <- dlogistic()
t <- 15
a <- 0.01
plot(0:t, Nts,type="l")
abline(h = k, lty = 3)




##################################################
#Visualize Logistic Growth With Allee Effect - Courchamp 1999
##################################################
#effectiv r greater than 2 = stable oscillations 
#.5*(50/20)

dallee <- function(k = 100,k_a = 50, rd = .5, N0 = 51, t = 50){
  N <- c(N0, numeric(t))
  for (i in 1:t) N[i + 1] <- {
    N[i] + rd * N[i] * (1 - N[i]/k) * (N[i]/k_a-1)
  }
  return(N)
}


Nts <- dallee()
t <- 50
k <- 100
plot(0:t, Nts,type="l")
abline(h = k, lty = 3)



# draw in continuous time
allee.c <- expression(r*N*(1-N/k)*(N/k_a-1))

af <- function (N,r,k,k_a) {
  r*N*(1-N/k)*(N/k_a-1)
}
  
k = 100 #carrying capacity of the fishery
k_a = 20 #allee effect threshold below which poulation declines
r = 1 #population growth parameter 
N <- 0:100

plot(N, eval(allee.c), type = "l", ylab = "Population Growth Rate (dN/dt)", xlab = "N")

k_a = 30 #allee effect threshold below which poulation declines
curve(r*x*(1-x/k)*(x/k_a-1),from=0,to=100,col=2,add=TRUE)


k_a = 40 #allee effect threshold below which poulation declines
curve(r*x*(1-x/k)*(x/k_a-1),from=0,to=100,col=3,add=TRUE)


abline(h = 0,lty = 3); legend("topright", "r=1", lty = 3)
abline(v = k/2,lty = 2); legend("bottomright", "K/2", lty = 2)


##################################################
#Alternative Parameterization of Allee effect 2 with theta and a param - Stephens and Southerland 1999
##################################################

N0 = 130 #starting densities  
t = 50 #time of simulation
k = 1000 #carrying capacity
r = .5 #intrinsic rate of increase
theta = 200 #parameter describing when allee effect kicks in
a = .7 #parameter describing how strong the allee effect is

dallee2 <- function(){
  N <- c(N0, numeric(t))
  for (i in 1:t) N[i + 1] <- {
    N[i]+ r*N[i] - (r*(N[i]^2)/k) - a*theta*N[i]/(theta+N[i])
  }
  return(N)
}

Nts <- dallee2()
plot(0:t, Nts,type="l")
abline(h = k, lty = 3)



# draw in continuous time
allee.c2 <- expression(N*r - (r*N^2/k)-a*N*theta/(theta+N))

r <- 1.5
theta <- 100
a <- 4
k <-1000
N <- 0:1000
  
plot(N, eval(allee.c2), type = "p", ylab = "Population Growth Rate (dN/dt)", xlab = "N")
abline(h = 0); legend("topright", "r=1", lty = 1)



##################################################
#Visualize Negative Logistic for Monitoring Function
##################################################

# draw in continuous time
loga <- expression(1/(50+exp(-.75*(-i*x))))

i =.5
x <- 0:25

plot(x, eval(loga), type = "l", ylab = "Decrease in Variance", xlab = "N")




############################################################################################
############################################################################################
############################################################################################

#Hard Code Logistic funciton but having hard time getting output of more than one response var 
# 
# ft <- function (N,r,k,phi) {
#   Nhat.t1 <- rnorm(1,mean = N[t,2],sd = phi)
#   Y <- ifelse(Nhat>k/2,Nhat-k/2,0)
#   Nt1 <- N[t,2] + r * N[t,2] * (1-N[t,2]/k)- Y
# c(Nhat.t1,Y,Nt1)
# }
# 
# N <- matrix(NA, nrow = t+1, ncol = 3)
# N[1,] <- c(0,0,40)
# for(i in 1:t) N[i+1,] <- ft(N[i,],r=.5,k=100,phi=3)
# 




############################################################################################
############################################################################################
############################################################################################
############################################################################################

# To Do 
# 1) Evaluate with time varying investment 
# 2) Incorporate economics and cost functions



##################################################
#Function 1: Gordon-Schaefer discrete logistic population growth with fishing and fixed investment in evaal
##################################################

#set simualtion length 
years = 50 #time of simulation

#Make Empty Matrix
N <- matrix(0,nrow=years+1,ncol=10)
N[,1] = seq(0:years+1)

#set parameters and starting values
k = 100 #carrying capacity of the fishery
r = .5 #population growth parameter
rnoise = .1
phi= 2 #variance estimator for a constant investment through time, as phi increases variance goes down
ppp = 10 #price per pound (dollars per biomass)
com = 10 #cost of monitoring (com) is the investment phi times a made up constant per unit of monitoring effort
mgmt = k/2

N[1,2] = k/2 #starting density at msy

colnames(N) = c("Time","Number","Estimated Number","Actual Yield","Possible Yield","Lost or Gained Yield","Monitoring Investment","Revenue","Cost","Profit")


for(t in 1:years){
  
  #SAMPLING Model
  Nhat <- rnorm(1,mean = N[t,2],sd = 1/phi) #Estimate of population from previous time step
  N[t+1,3] <- Nhat
  
  #MANAGEMENT MODE & ASSESSMENT Model 
  Y <- ifelse(Nhat>k/2,Nhat-mgmt,0) #Yield is anything over mgmt threshold (e.g. msy given estimated biomass)
  N[t+1,4] <- Y
  
  #OPERATING Model
  Nt1 <- N[t,2] + rnorm(1,mean=r,sd=rnoise) * N[t,2] * (1-N[t,2]/k)- Y #population grows logistically minus some yield  - could make 
  N[t+1,2] <- Nt1
}


N[,5] = N[,2] - mgmt #possible yield if perfect knowledge
N[,6] = N[,5] - N[,4] #difference in actual yield versus appropriate yield - sometimes negative i.e. overyielding
N[,7] = phi
N[,8] = N[,4]*ppp  #revenue as a function of yield * price per pound (ppp)
N[,9] = phi*com #cost is the price per monitoring effort
N[,10] = N[,8] - N[,9]

mean(N[,6])
mean(N[,5])
mean(N[,7])
mean(N[,8])


ndf = data.frame(N)
ndf = ndf[,c(1,4,5)]
ndf = melt(ndf,id.vars=c("Time"))
ndf = subset(ndf,Time>2)

#plot actual and possible yield 

ggplot(ndf,aes(x=Time,value,group=variable))+
  geom_point(aes(colour=variable))+
  geom_line(aes(colour=variable))+
  theme(
    text = element_text(colour="black"),
    line = element_line(colour="black",size=1),
    axis.text = element_text(colour="black",size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA,colour = "black",size=2),
    axis.text.x=element_text(angle = 0),
    strip.background = element_rect(fill="black"),
    strip.text = element_text(colour="white",size=12)
  )+
  labs(x="Time",y="Yield")

#plot profit

ndf = data.frame(N)
ndf = ndf[,c(1,10)]
ndf = melt(ndf,id.vars=c("Time"))
ndf = subset(ndf,Time>2)

#plot revenue 

ggplot(ndf,aes(x=Time,value,group=variable))+
  geom_point(aes(colour=variable))+
  geom_line(aes(colour=variable))+
  theme(
    text = element_text(colour="black"),
    line = element_line(colour="black",size=1),
    axis.text = element_text(colour="black",size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA,colour = "black",size=2),
    axis.text.x=element_text(angle = 0),
    strip.background = element_rect(fill="black"),
    strip.text = element_text(colour="white",size=12)
  )+
  labs(x="Time",y="Profit (Yield Revenu - Monitoring Cost)")



##################################################
#Evaluate Logistic Growth Acrross a Range of Investments
##################################################

#starting values
years = 50 #time of simulation
phivec = 3 #range of monitoring  ivestment. Higher numbers 
k = 100 #carrying capacity of the fishery
r = .5 #population growth parameter 
rnoise = .05 #temporally uncorrelated variance (sd) in little r 
ppp = 10 #price per pound (dollars per biomass)
com = 10 #cost of monitoring (com) is the investment phi times a made up constant per unit of monitoring effort

#Make Empty Array
GS <- array(dim=c(length(seq(0:years+1)),10,length(seq(1:kvec))))

GS[,1,] = rep(seq(0:years),3) #fill time in row 
GS[1,2,] = rep(k/2,3) #starting density at msy

colnames(GS) = c("Time","Number","Estimated Number","Actual Yield","Possible Yield","Lost or Gained Yield","Monitoring Investment","Revenue","Cost","Profit")



for(j in 1:phivec){
  
  for(t in 1:years){
    
    Nhat <- rnorm(1,mean = GS[t,2,j],sd = 1/j) #Estimate of population from previous time step
    GS[t+1,3,j] <- Nhat
    
    Y <- ifelse(Nhat>k/2,Nhat-k/2,0) #Yield is anything over k/2 i.e. msy given estimated biomass
    GS[t+1,4,j] <- Y
    
    Nt1 <- GS[t,2,j] + rnorm(1,mean = r,sd = rnoise) * GS[t,2,j] * (1-GS[t,2,j]/k)- Y #population grows logistically minus some yield  - could make 
    GS[t+1,2,j] <- Nt1
  }
  
  GS[,5,j] = GS[,2,j] - k/2 #possible yield if perfect knowledge
  GS[,6,j] = GS[,5,j] - GS[,4,j] #difference in actual yield versus appropriate yield - sometimes negative i.e. overyielding
  
  
  GS[,7,j] = j
  GS[,8,j] = GS[,4,j]*ppp  #revenue as a function of yield * price per pound (ppp)
  GS[,9,j] = j*com #cost is the price per monitoring effort
  GS[,10,j] = GS[,8,j] - GS[,9,j] #Profit
  
}



### Pull out and plot deviation from optimal yield for low medium and high investment
gdf=data.frame(GS)
gdf2 = gdf[2:51,c(1,6,16,26)]
gdf2 = melt(gdf2,id.vars=c("Time.1"))
gdf2[,4] = c(rep("Low Investment",50),rep("Med Investment",50),rep("High Investment",50))
names(gdf2) = c("Time","Output","Amount","Investment")
gdf2$Investment <- factor(gdf2$Investment, levels = c("Low Investment", "Med Investment", "High Investment"))                  
gdf2 = subset(gdf2,Time>3)

ggplot(gdf2,aes(x=Time,Amount,group=Investment))+
  geom_point(aes(colour=Investment))+
  geom_line(aes(colour=Investment))+
  theme(
    text = element_text(colour="black"),
    line = element_line(colour="black",size=1),
    axis.text = element_text(colour="black",size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA,colour = "black",size=2),
    axis.text.x=element_text(angle = 0),
    strip.background = element_rect(fill="black"),
    strip.text = element_text(colour="white",size=12)
  )+
  labs(x="Time",y="Deviation from Optimal Yield (Optimal - Actual Yield)")


### Pull out and plot profit for three levels of investment
gdf=data.frame(GS)
gdf2 = gdf[2:51,c(1,10,20,30)]
gdf2 = melt(gdf2,id.vars=c("Time.1"))
gdf2[,4] = c(rep("Low Investment",50),rep("Med Investment",50),rep("High Investment",50))
names(gdf2) = c("Time","Output","Amount","Investment")
gdf2$Investment <- factor(gdf2$Investment, levels = c("Low Investment", "Med Investment", "High Investment"))                  
gdf2 = subset(gdf2,Time>3)

ggplot(gdf2,aes(x=Time,Amount,group=Investment))+
  geom_point(aes(colour=Investment))+
  geom_line(aes(colour=Investment))+
  theme(
    text = element_text(colour="black"),
    line = element_line(colour="black",size=1),
    axis.text = element_text(colour="black",size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA,colour = "black",size=2),
    axis.text.x=element_text(angle = 0),
    strip.background = element_rect(fill="black"),
    strip.text = element_text(colour="white",size=12)
  )+
  labs(x="Time",y="Profit (Profit from Yield - Cost of Monitoring)")

