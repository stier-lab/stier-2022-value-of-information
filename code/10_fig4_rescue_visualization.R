
#load packages, parameters, and MSE model
source("code/0_libraries.R") #load packages that are relevant
source("code/2_model_parameters.R") # base parameters
source("code/3_mse_model.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"

# load("output/simulation/fig2_dz_acs_50yr2021-01-22_5000.Rdata") #this is ignored on github will need to produce
# load("output/simulation/fig2_rescue_acs_2020-11-30_500.Rdata")
load("output/simulation/fig4_rescue_acs_2021-01-25_3000.Rdata") #this is ignored on github will need to produce


###Just the solo figure with prob_rescue2

df1 = melt(ar,varnames=names(dimnames(ar)))
colnames(df1) = c("pFmsy","metric","CV","A","B.start","value")
df1w <-pivot_wider(df1,names_from = metric)%>%
  filter(B.start ==70 & A == "A = 10")


ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=prob_rescue2,colour=prob_rescue2))+
  scale_fill_gradient(low="#7ACCD7",high="#BA2F00")+
  scale_colour_gradient(low="#7ACCD7",high="#BA2F00")+
  xlab("Monitoring Precision")+
  ylab("Havest Rate (pHmsy)")+
  theme_pubr(legend="right")

# 
#   scale_colour_gradient(name = "Rescue Probability",
#                       labels = c("High (20%)", "Low (1%)"))+
#   scale_fill_gradient(name = "Risk Tolerance",
#                     labels = c("High (20%)", "Low (1%)"))
 





#heat map as function of years near threshold

df1 = melt(ar,varnames=names(dimnames(ar)))
colnames(df1) = c("pFmsy","metric","CV","A","B.start","value")
df1w <-pivot_wider(df1,names_from = metric)%>%
  filter(B.start ==70)

#rescues relative to danger metric
df1w$rescue_rel_risk <- df1w$prob_rescue/df1w$yrs.near.thresh1

gg_tip<-ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=Prob.Cross.TP,colour=Prob.Cross.TP))+
  scale_fill_gradient(low="dodgerblue",high="firebrick",)+
  scale_colour_gradient(low="dodgerblue",high="firebrick")+
  xlab("CV of Monitoring")+
  ylab("pFmsy")+
  facet_wrap(~A)+
  theme_pubr(legend="right")

gg_danger<-ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=yrs.near.thresh1,colour=yrs.near.thresh1))+
  scale_fill_gradient(low="dodgerblue",high="firebrick",)+
  scale_colour_gradient(low="dodgerblue",high="firebrick")+
  xlab("CV of Monitoring")+
  ylab("pFmsy")+
  facet_wrap(~A)+
  theme_pubr(legend="right")

gg_NPV<-ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=NPV,colour=NPV))+
  scale_fill_gradient(low="dodgerblue",high="firebrick")+
  scale_colour_gradient(low="dodgerblue",high="firebrick")+
  xlab("CV of Monitoring")+
  ylab("pFmsy")+
  facet_wrap(~A)+
  theme_pubr(legend="right")

gg_rescue<-ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=rescue,colour=rescue))+
  scale_fill_gradient(low="dodgerblue",high="firebrick")+
  scale_colour_gradient(low="dodgerblue",high="firebrick")+
  xlab("CV of Monitoring")+
  ylab("pFmsy")+
  facet_wrap(~A)+
  theme_pubr(legend="right")


gg_rescue2<-ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=prob_rescue,colour=prob_rescue))+
  scale_fill_gradient(low="dodgerblue",high="firebrick")+
  scale_colour_gradient(low="dodgerblue",high="firebrick")+
  xlab("CV of Monitoring")+
  ylab("pFmsy")+
  facet_wrap(~A)+
  theme_pubr(legend="right")


gg_rescue_rel_risk<-ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=rescue_rel_risk,colour=rescue_rel_risk))+
  scale_fill_gradient(low="dodgerblue",high="firebrick")+
  scale_colour_gradient(low="dodgerblue",high="firebrick")+
  xlab("CV of Monitoring")+
  ylab("pFmsy")+
  facet_wrap(~A)+
  theme_pubr(legend="right")



gg_rescue_prob2<-ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=prob_rescue2,colour=prob_rescue2))+
  scale_fill_gradient(low="dodgerblue",high="firebrick")+
  scale_colour_gradient(low="dodgerblue",high="firebrick")+
  xlab("CV of Monitoring")+
  ylab("pFmsy")+
  facet_wrap(~A)+
  theme_pubr(legend="right")


plot_grid(gg_tip,gg_danger,gg_NPV,gg_rescue,gg_rescue2,gg_rescue_prob2,ncol=2)

plot_grid(gg_tip,gg_NPV,gg_rescue_prob2,ncol=1)

