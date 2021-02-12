
#load packages, parameters, and MSE model
source("code/0_libraries.R") #load packages that are relevant
source("code/2_model_parameters.R") # base parameters
source("code/3_mse_model.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"

# load("output/simulation/fig2_dz_acs_50yr2021-01-22_5000.Rdata") 
# load("output/simulation/fig2_rescue_acs_2020-11-30_500.Rdata")
load("output/simulation/fig4_rescue_acs_2021-01-26_5000.Rdata") #this is ignored on github will need to produce


###Just the solo figure with prob_rescue2

df1 = melt(ar,varnames=names(dimnames(ar)))
colnames(df1) = c("pFmsy","metric","CV","A","B.start","value")
df1w <-pivot_wider(df1,names_from = metric)%>%
  filter(B.start == 70 & A == "A = 10")

#probability of rescue occurring when overharvest happeneds
#this is problematic qualitatitvely because tipping happens a lot, so the scenarios
#shown are ones where tipping didn't occur 

ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=prob_rescue2,colour=prob_rescue2))+
  scale_fill_gradient(low="#BA2F00",high="#7ACCD7",guide = gc,name=str_wrap("Probability of Rescue",14))+
  scale_colour_gradient(low="#BA2F00",high="#7ACCD7",guide = gc,name=str_wrap("Probability of Rescue",14))+
  xlab("Monitoring Precision")+
  ylab("Havest Rate (pHmsy)")+
  theme_pubr(legend="right")


#this is just the number of rescues out of times there was a dip

ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=rescue,colour=rescue))+
  scale_fill_gradient(low="#BA2F00",high="#7ACCD7",guide = gc,name=str_wrap("Rescues",14))+
  scale_colour_gradient(low="#BA2F00",high="#7ACCD7",guide = gc,name=str_wrap("Rescues",14))+
  xlab("Monitoring Precision")+
  ylab("Havest Rate (pHmsy)")+
  theme_pubr(legend="right")

ggsave("output/figures/rescuemap.pdf",width=5,height=4)



#heat map as function of years near threshold

df1 = melt(ar,varnames=names(dimnames(ar)))
colnames(df1) = c("pFmsy","metric","CV","A","B.start","value")
df1w <-pivot_wider(df1,names_from = metric)%>%
  filter(B.start ==70)


#Probability of tipping
gg_tip<-ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=Prob.Cross.TP,colour=Prob.Cross.TP))+
  scale_fill_gradient(name="prob tip",low="dodgerblue",high="firebrick",guide = gc)+
  scale_colour_gradient(name="prob tip",low="dodgerblue",high="firebrick",guide = gc)+
  xlab("CV of Monitoring")+
  ylab("pFmsy")+
  # facet_wrap(~A)+
  theme_pubr(legend="right")

#fraction of time series where B < 0.8*Bmsy
gg_danger<-ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=yrs.near.thresh1,colour=yrs.near.thresh1))+
  scale_fill_gradient(name="% B<0.8Bmsy",low="dodgerblue",high="firebrick",guide = gc)+
  scale_colour_gradient(name="% B<0.8Bmsy",low="dodgerblue",high="firebrick",guide = gc)+
  xlab("CV of Monitoring")+
  ylab("pFmsy")+
  # facet_wrap(~A)+
  theme_pubr(legend="right")

#Net Present Value
gg_NPV<-ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=NPV,colour=NPV))+
  scale_fill_gradient(name="NPV",low="dodgerblue",high="firebrick",guide = gc)+
  scale_colour_gradient(name="NPV",low="dodgerblue",high="firebrick",guide = gc)+
  xlab("CV of Monitoring")+
  ylab("pFmsy")+
  # facet_wrap(~A)+
  theme_pubr(legend="right")

#Number of years where B goes from <0.8Bmsy to >0.8Bmsy
gg_rescue<-ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=rescue,colour=rescue))+
  scale_fill_gradient(name="# rescues",low="dodgerblue",high="firebrick",guide = gc)+
  scale_colour_gradient(name="# rescues",low="dodgerblue",high="firebrick",guide = gc)+
  xlab("CV of Monitoring")+
  ylab("pFmsy")+
  # facet_wrap(~A)+
  theme_pubr(legend="right")

#Proportion of times when B<0.8*Bmsy recovers above 
gg_rescue_prob2<-ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=prob_rescue2,colour=prob_rescue2))+
  scale_fill_gradient(name="p-rescue",low="dodgerblue",high="firebrick",guide=gc)+
  scale_colour_gradient(name="p-rescue",low="dodgerblue",high="firebrick",guide=gc)+
  xlab("CV of Monitoring")+
  ylab("pFmsy")+
  # facet_wrap(~A)+
  theme_pubr(legend="right")


#not sure what this is but it's some sort of weighted of prop rescues relative to proportion of time B<0.8Bmsy
df1w$rescue_rel_risk <- df1w$prob_rescue/df1w$yrs.near.thresh1

gg_rescue_rel_risk<-ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=rescue_rel_risk,colour=rescue_rel_risk))+
  scale_fill_gradient(low="dodgerblue",high="firebrick")+
  scale_colour_gradient(low="dodgerblue",high="firebrick")+
  xlab("CV of Monitoring")+
  ylab("pFmsy")+
  # facet_wrap(~A)+
  theme_pubr(legend="right")


# plot_grid(gg_tip,gg_danger,gg_NPV,gg_rescue,gg_rescue2,gg_rescue_prob2,ncol=2)



plot_grid(gg_tip,gg_danger,gg_NPV,gg_rescue,gg_rescue_prob2,ncol=2)

