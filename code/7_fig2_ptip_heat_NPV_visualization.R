#########
#FIGURE 1: how does prob of tipping matter across tipping point and what does th
#########

source("code/0_libraries.R") #load packages that are relevant
source("code/2_model_parameters.R") # base parameters
source("code/3_mse_model.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"

#pull output file
load("output/simulation/risk and heatmaps 2021-01-22 5000 .Rdata") #this is ignored on github will need to produce

#rearrange and label for plotting
df1 = melt(ar1,varnames=names(dimnames(ar)))
colnames(df1) = c("pFmsy","metric","CV","A","value")

df1 = reshape(df1,
              timevar = "metric",
              idvar = c("pFmsy","A","CV"),
              direction = "wide")


colnames(df1) = c("pFmsy","CV","A","NPV","Prob.Cross.TP","Biomass","CumulativeYield","SDBiomass","Ptip.MGMT","Fmsy","max.F.2")
range(df1$Prob.Cross.TP) #very low prob of tipping
range(df1$pFmsy)



#probability of tipping at A=10
df1a10 <- subset(df1,A == "A = 10")

g_ptip = ggplot(df1a10,aes(x = CV, y = pFmsy))+
  geom_tile(aes(fill=Prob.Cross.TP,colour=Prob.Cross.TP))+
  scale_colour_gradientn(colours = pal,name="Probability of tipping", guide = gc) + 
  scale_fill_gradientn(colours = pal,name="Probability of tipping") + 
  xlab("CV of Monitoring")+
  ylab("Harvest rate (pFmsy)")+
  theme_pubr(legend="right")+
  theme(
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title = element_text(size = 14))

print(g_ptip)


df_3b <- subset(df1, A == "A = 10")
df_3b<-subset(df_3b,CV %in% c(0.1,0.2,0.3,0.4,0.5))

g_npv = ggplot(df_3b,aes(x=pFmsy,y=NPV,group=CV))+
  # geom_line(aes(colour=CV))+
  # geom_point(aes(colour=CV))+
  geom_smooth(aes(colour=CV),se=F,size=1)+
  scale_colour_gradient(low="#e0f3db",high="#43a2ca", name = "CV of Monitoring",guide=gc)+
  xlab("Harvest rate pHmsy")+
  ylab("Net present value")+
  theme_pubr(legend="right")+
  theme(
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title = element_text(size = 14))
  
print(g_npv)


risk_plot <- plot_grid(g_ptip,g_npv,nrow=1,labels="AUTO")

print(risk_plot)

save_plot(plot=risk_plot,"output/figures/NPV_risk/fig2-NPV-ptip.pdf",
          base_width=12,base_height=4)
