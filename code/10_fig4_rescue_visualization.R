
#load packages, parameters, and MSE model
source("code/0_libraries.R") #load packages that are relevant
source("code/2_model_parameters.R") # base parameters
source("code/3_mse_model.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"


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

###Just the solo figure with prob_rescue2

df1 = melt(ar,varnames=names(dimnames(ar)))
colnames(df1) = c("pFmsy","metric","CV","A","B.start","value")
df1w <-pivot_wider(df1,names_from = metric)%>%
  filter(B.start ==70 & A == "A = 10")

c("#7ACCD7", "#115896", "#7C6C65", "#4C4C53", "#BA2F00", "#21282F")

ggplot(df1w,aes(x=CV,y=pFmsy))+
  geom_tile(aes(fill=prob_rescue2,colour=prob_rescue2))+
  scale_fill_gradient(low="#7ACCD7",high="#BA2F00")+
  scale_colour_gradient(low="#7ACCD7",high="#BA2F00")+
  xlab("CV of Monitoring")+
  ylab("Havest Rate (pFmsy)")+
  theme_pubr(legend="right")