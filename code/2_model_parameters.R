# parameterize model duration and economics

years <- 20 #time of simulation
B.start <- 100 #starting biomass
delta <- 0.05 # discount rate
process.noise <- 0 #temporally uncorrelated variance (sd) in little r 

####pricing stuff here is pulled out of the air 
p <- 20 #price per unit biomass
c <- 200 # cost to achieve F

#cm = 5# old cost of monitoring. higher cm, more expensive monitoriing

ci <- 100 #intecept of decay in cost of monitoring function
cs <- 5 #slope in cost of monitoring function

# parameterize the model based on A,BMSY and MSY
A <- 30  #allee effect threshold 
Bmsy <- 70 #Biomass at MSY
MSY <- 25

K<-(3*Bmsy^2 - 2*A*Bmsy)/( 2*Bmsy-A) # Carrying Capacity of Focal population (derived by TE in matlab)
r<-MSY/(Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K)) # population growth rate 

Fmsy <- MSY/Bmsy # Fishing mortality that produces MSY
max.F <- Fmsy # maximum fishing defined as Fmsy *****double check an older version of this 
B.lim <- max(A,0.25*Bmsy) # lower biomass limit for harvest control rule 
B.crit <- 50 

phi.CV.low <- 0.1 #low cv associated with high montioring investment
phi.CV.high <- 0.5 #high cv associated with low monitoring investment

#Set up iterations for repeat model
rm(.Random.seed)
phi.CV.seed<-round(100000*runif(1),0)
process.noise.seed<-round(100000*runif(1),0)