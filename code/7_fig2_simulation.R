#########
#FIGURE 2 Simulation: how does value of information change when you spend more time near tipping point
#########

source("code/0_libraries.R") #load packages that are relevant
source("code/2_model_parameters.R") # base parameters
source("code/3_mse_model.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"

#set parameter range for surface of simulated parameters
years = 20
B.vec <- seq(1,100, by = 10) #MCS: used to be by 5
avec <- seq(10,30,by = 10) #biomass at which allee effect occurs
phivec <- seq(0.1,0.5,by = 0.1) #uncertainty cv MCS: CV of biomass? or survey cv of biomass?
FMSYvec <- seq(.1,2,by = 0.2) #manipulating FMSY max.F

#create empty array with labels
ar <- array(dim=c(length(FMSYvec),10,length(phivec),length(avec),length(B.vec)))
dimnames(ar) = list(FMSYvec,c("NPV","Prob.Cross.TP","Biomass","CumulativeYield",
                              "SDBiomass","Ptip.MGMT","Fmsy","max.F.2",
                              "yrs.near.thresh1","yrs.near.thresh2"),
                    phivec,paste("A =",avec),B.vec)

#set number of iterations 
n.iters = 100
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
        ar[i,9,j,a,b] <-median(value$thresh1)
        ar[i,10,j,a,b]<-median(value$thresh2)
        
      }
    }
  }
}

save(ar,file=here::here("output","simulation",paste("fig2_mcs_",Sys.Date(),"_",n.iters,".Rdata",sep="")))

#melt down ar and look for what values might make sense 
 
sim_out<-melt(ar)
colnames(sim_out)<-c("maxF","response","cv","A","b.start","value")

atab10<-filter(sim_out,response == "NPV" & A=="A = 10")
atab20<-filter(sim_out,response == "Prob.Cross.TP" & A=="A = 20")
atab30<-filter(sim_out,response == "NPV" & A=="A = 30")

ggplot(atab10,aes(x=b.start,y=value,colour=maxF,group=maxF))+
  geom_point()+
  geom_line()+
  facet_wrap(~cv)+
  geom_vline(xintercept=10)+
  scale_colour_gradient2(low="darkblue",midpoint=1,high="red",mid="darkgray")+
  theme_classic()

ggplot(atab30,aes(x=b.start,y=value,colour=maxF,group=maxF))+
  geom_point()+
  geom_line()+
  facet_wrap(~cv)+
  geom_vline(xintercept=30)+
  scale_colour_gradient2(low="darkblue",midpoint=1,high="red",mid="darkgray")+
  theme_classic()+
  ylab("NPV")


compare_NPV <- full_join(atab10,atab30) %>%
  filter(cv %in% c(0.1,0.5))%>%
  filter(maxF %in% c(0.1,0.9,1.9))

compare_NPV$cv <- as.factor(compare_NPV$cv)


ggplot(compare_NPV,aes(x=b.start,y=value,colour=cv,shape=A,group=cv))+
  geom_point()+
  geom_line()+
  facet_grid(maxF~A)+
  theme_pubr()+
  ylab("NPV")






# try plotting out data ROI-dangerzone

atab10<-
  sim_out%>%
  filter(response %in% c("yrs.near.thresh1","NPV","Prob.Cross.TP")  & A %in% c("A = 10","A = 30"))%>%
  pivot_wider(names_from = response,values_from=value)%>%
  filter(cv %in% c(0.1,0.5))%>%
  filter(Prob.Cross.TP<0.75)%>%
  # pivot_wider(names_from = cv,values_from = NPV,names_prefix="NPVCV")%>% 
  # pivot_longer(cols = c('NPVCV0.1','NPVCV0.5')) %>% drop_na() %>% pivot_wider()%>%
  # mutate(ROI=NPVCV0.1/NPVCV0.5)%>%
  filter(b.start>10)

ggplot(atab10,aes(x=yrs.near.thresh1,y=ROI,colour=maxF))+
  geom_point()

  atab10$cv<-as.factor(atab10$cv)
  
  ggplot(atab10,aes(x=yrs.near.thresh1,y=NPV,colour=maxF,group=b.start))+
    geom_line()
  
  
  select(-Prob.Cross.TP)
  
  
# repro example

x <- data.frame(group = c("a","b","c","a","b","c"),
           treatment = c("control","control","nutrients","control","control","nutrients"),
           response1 = c(1,2,3,NA,NA,NA),
           response2 = c(NA,NA,NA,1,2,3)
)

#solution
data.frame(group = c("a","b","c"),
           treatment = c("control","control","nutrients"),
           response1 = c(1,2,3),
           response2 = c(1,2,3)
           
)


x %>% pivot_longer(cols = c('response1','response2')) %>% drop_na() %>% pivot_wider()

#example1
x %>% 
  mutate(response_both = coalesce(response1,response2)) %>% 
  distinct(group, treatment, response_both)


