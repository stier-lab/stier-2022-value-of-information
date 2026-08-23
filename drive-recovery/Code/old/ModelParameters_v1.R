# parameterize model duration and economics

years = 20 #time of simulation
delta<-.05 # discount rate
process.noise = 0 #temporally uncorrelated variance (sd) in little r 
p = 10 #price per unit biomass
c=200 # cost to achieve F
B.start <- 100

# parameterize the model based on A,BMSY and MSY
A = 10  #allee effect threshold 
Bmsy<- 70 #Biomass at MSY
MSY<-25 # MSY
K<-(3*Bmsy^2 - 2*A*Bmsy)/( 2*Bmsy-A) #Carrying Capacity of Focal population (derived by TE in matlab)
r<-MSY/(Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K)) #population growth rate 
Fmsy<-MSY/Bmsy #Fishing mortality that produces MSY
max.F<-Fmsy #maximum fishing defined as Fmsy *****double check an older version of this 
B.lim<-max(A,30) # lower biomass limit for harvest control rule 
B.crit <-40 #for adaptive monitoring B.crit describes Biomass value below which CV of monitoring decreases

#this B.lim is the one you can change for the 

