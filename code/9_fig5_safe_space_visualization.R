#########
#FIGURE 3: safe operating space for biological and management tipping points
#########

#load libraries, parameters, and MSE model
source("code/0_libraries.R") #load packages that are relevant
source("code/2_model_parameters.R") # base parameters
source("code/3_mse_model.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"

#pull output file
load("output/simulation/safe-operating-space 2021-01-25 10 .Rdata") #this is ignored on github will need to produce

#rearrange and label for plotting
df1 = melt(ar1,varnames=names(dimnames(ar)))
colnames(df1) = c("pFmsy","metric","CV","A","value")

df1 = reshape(df1,
              timevar = "metric",
              idvar = c("pFmsy","A","CV"),
              direction = "wide")


colnames(df1) = c("pFmsy","CV","A","NPV","Prob.Cross.TP","Biomass","CumulativeYield","SDBiomass","Ptip.MGMT","Fmsy","max.F.2")


#what is the cv necessary to get 5% chance of collapse for differen pFmys

df_3b <- subset(df1, A == "A = 10")

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
df5<-df4%>%
  filter(PercentRisk == c("1%","20%"))

#main pub fig

ggplot(df5,aes(x=pFmsy,y=value,group=PercentRisk))+
  geom_line(aes(colour=PercentRisk,lty=PercentRisk))+
  geom_area(aes(fill=PercentRisk),alpha=0.2)+
  xlab("Harvest Rate (pFmsy)")+
  ylab("Minimum monitoring precision to avoid threshold")+
  theme_pubr(legend="right")+
  ylim(0,0.65)+
  # scale_fill_manual(values = alpha(c("#ff1212","#1212ff"), .25)) 
  scale_colour_manual(name = "Risk Tolerance",
                      labels = c("High (20%)", "Low (1%)"),
                      values=wes_palette("Zissou1", 2, type = "continuous"))+
  scale_fill_manual(name = "Risk Tolerance",
                    labels = c("High (20%)", "Low (1%)"),
                    values=wes_palette("Zissou1", 2, type = "continuous"))+
  scale_linetype_discrete(name = "Risk Tolerance",
                          labels = c("High (20%)", "Low (1%)"))





#figures for supplement of more risk profiles 

g_ptip = ggplot(df4,aes(x=pFmsy,y=value,group=PercentRisk))+
  geom_line(aes(colour=PercentRisk,lty=PercentRisk))+
  # geom_area(aes(fill=PercentRisk),alpha=0.5)+
  xlab("Harvest Rate (pFmsy)")+
  ylab("Max CV to Avoid Tipping point")+
  theme_pubr(legend="right")+
  scale_x_continuous(limits=c(0,2))+
  scale_y_continuous(limits=c(0,0.6),breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6))
# scale_colour_Publication()
# ggtitle("Risk to Avoid Allee TP")


print(g_ptip) 




  # scale_x_continuous(limits=c(0,2))+
  # scale_y_continuous(limits=c(0,0.6),breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6))
# scale_colour_Publication()
# ggtitle("Risk to Avoid Allee TP")


#what is the cv necessary to get 5% chance of crossing mgmt threshold for differen pFmys

df_3c <- subset(df1, A == "A = 10")

df.01 <- subset(df_3c,Ptip.MGMT<0.01)
df.05 <- subset(df_3c,Ptip.MGMT<0.05)
df.10 <- subset(df_3c,Ptip.MGMT<0.1)
df.20 <- subset(df_3c,Ptip.MGMT<0.2)

cvmax.01 <- tapply(df.01$CV,list(df.01$pFmsy),max)
cvmax.05 <- tapply(df.05$CV,list(df.05$pFmsy),max)
cvmax.1 <- tapply(df.10$CV,list(df.10$pFmsy),max)
cvmax.2 <- tapply(df.20$CV,list(df.20$pFmsy),max)

myls <- list(cvmax.2,cvmax.1,cvmax.05,cvmax.01)
max.rows <- max(nrow(cvmax.01),nrow(cvmax.05), nrow(cvmax.1),nrow(cvmax.2))
new_myls <- lapply(myls,function(x){x[1:max.rows]})

df4b <- data.frame(do.call(cbind, lapply(new_myls, `[`,)))
df4b$pFmsy <- as.numeric(rownames(df4b))
names(df4b) = c("20%","10%","5%","1%","pFmsy")
df4b <- melt(df4b,id.vars=c("pFmsy"))
names(df4b) <- c("pFmsy","PercentRisk","value")
df4b$PercentRisk <- factor(df4b$PercentRisk, levels = c("20%","10%","5%","1%"))


g_mgmt_tip = ggplot(df4b,aes(x=pFmsy,y=value,group=PercentRisk))+
  geom_line(aes(colour=PercentRisk,lty=PercentRisk))+
  # geom_area(aes(fill=PercentRisk), alpha=0.4) +
  xlab("Harvest Rate (pFmsy)")+
  ylab("Max CV to Avoid MGMT Tipping point")+
  theme_pubr(legend="right")+
  scale_x_continuous(limits=c(0,2))+
  scale_y_continuous(limits=c(0,0.6),breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6))


print(g_mgmt_tip)

risk_plot <- plot_grid(g_ptip,g_mgmt_tip,ncol=1,labels="AUTO")

print(risk_plot)


