
#########
#FIGURE 3: How does the value of information change as a function of 
#########

#load libraries
source("code/0_libraries.R") #load packages that are relevant
source("code/2_model_parameters.R") # base parameters
source("code/3_mse_model.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"

#import simulation output
load("output/simulation/precautionary_buffer.Rdata")

##Univariate Response
mm<-melt(edf[,-1,,,])
names(mm)<-c("cv","metric","Bcrit","cs","mf","value")
mm$metric <-factor(mm$metric,levels=c("NPV","Monitoring","NPV_minus_Monitoring","Prob_Tip"))
mm$cv <-factor(mm$cv,levels=c("fixedCV0.1","fixedCV0.5","PrecautionaryBufferCV","mean_PBCV"))

#subset out npv minus monitoring 
mm3<- subset(mm,metric!="NPV_minus_Monitoring")
mm3<- drop.levels(subset(mm3,cv!="mean_PBCV"))

csvec <-unique(mm$cs)

temp<-subset(mm3,cs==csvec[1])

temp2npv<-subset(temp,metric=="NPV")
temp2mon<-subset(temp,metric=="Monitoring")
temp2tip<-subset(temp,metric=="Prob_Tip")

temp3<-temp2npv
temp3$cost<-temp2mon$value
temp3$ratio<-temp3$value/temp2mon$value
temp3$ptip<-temp2tip$value

names(temp3) <-c("cv","metric","Bcrit","cs","mf","NPV","cost","ratio","ptip")

temp3b<-subset(temp3,Bcrit==0.50)
temp3b$metric <-factor(temp3b$metric,levels=c("NPV","Monitoring","Prob_Tip"))

gsmooth<-ggplot(temp3b,aes(x=mf,y=ratio,group=cv))+
  geom_point(aes(colour=ptip,pch=cv))+
  geom_smooth(aes(lty=cv),se=F,colour="black")+
  theme_pubr(legend="right")+
  scale_colour_gradientn(colours = pal,name="Probability of tipping") + 
  xlab("Maximum Fishing Effort")+
  ylab("Value Ratio: NPV/Cost")+
  #$ facet_grid(.~cv)+
  ggtitle(paste("Cost Function Slope (cs) =",csvec[1]))


print(gsmooth)


# 
# #code below (i think) does some sensitivity to different cost functions
# 
# pdf(paste("Cost_Benefit",Sys.Date(),".pdf"), width=12,onefile = TRUE)
# 
# 
# for(i in 1:length(csvec)){
#   
#   temp<-subset(mm3,cs==csvec[i])
#   temp_b<-subset(temp,mf==1.5) 
#   temp_b<-subset(temp_b,Bcrit==0.5)
#   temp_b$metric <-factor(temp_b$metric,levels=c("NPV","Monitoring","Prob_Tip"))
#   
#   
#   guni<-ggplot(temp_b,aes(cv,y=value))+
#     geom_bar(stat="identity",aes(fill=metric),width=0.75)+
#     facet_grid(metric~Bcrit,scales="free")+
#     theme_pubr(legend="right")+
#     xlab("Monitoring Strategy")+
#     ylab("Value")+
#     ggtitle(paste("Cost Function Slope (cs) =",csvec[i]))+
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   
#   print(guni)
#   # ggsave(paste("Univariate_Cost_Benefit_Tip.pdf","costfucntionslope=",csvec[i],".pdf"),guni)
#   
#   temp2npv<-subset(temp,metric=="NPV")
#   temp2mon<-subset(temp,metric=="Monitoring")
#   temp2tip<-subset(temp,metric=="Prob_Tip")
#   
#   temp3<-temp2npv
#   temp3$cost<-temp2mon$value
#   temp3$ratio<-temp3$value/temp2mon$value
#   temp3$ptip<-temp2tip$value
#   
#   names(temp3) <-c("cv","metric","Bcrit","cs","mf","NPV","cost","ratio","ptip")
#   
#   temp3b<-subset(temp3,Bcrit==0.50)
#   temp3b$metric <-factor(temp3b$metric,levels=c("NPV","Monitoring","Prob_Tip"))
#   
#   gsmooth<-ggplot(temp3b,aes(x=mf,y=ratio,group=cv))+
#     geom_point(aes(colour=ptip,pch=cv))+
#     geom_smooth(aes(lty=cv),se=F,colour="black")+
#     theme_pubr(legend="right")+
#     scale_colour_gradientn(colours = pal,name="Probability of tipping") + 
#     xlab("Maximum Fishing Effort")+
#     ylab("Value Ratio: NPV/Cost")+
#     #$ facet_grid(.~cv)+
#     ggtitle(paste("Cost Function Slope (cs) =",csvec[i]))
#   
#   
#   print(gsmooth)
#   
#   # ggsave(paste("Biplot_NPV_Cost_Tip_nofacet.pdf","costfucntionslope=",csvec[i],".pdf"),gsmooth)
#   
#   #alterantively, plot cost versus benefit
#   
#   gcb<-ggplot(temp3b,aes(x=cost,y=NPV))+
#     geom_point(aes(colour=ptip,pch=cv,size=mf))+
#     theme_pubr(legend="right")+
#     scale_colour_gradientn(colours = pal,name="Probability of tipping") + 
#     xlab("Monitoring Cost")+
#     ylab("Benefit NPV")+
#     facet_grid(.~cv)+
#     ggtitle(paste("Cost Function Slope (cs) =",csvec[i]))
#   
#   print(gcb)
#   
# 
# 
# 
# ###this looks funny now that you sampled across different fishing efforts, check subset
# 
# t2<-subset(mm3,cs==5 & Bcrit==1)
# guni<-ggplot(t2,aes(cv,y=value))+
#   geom_bar(stat="identity",aes(fill=metric),width=0.75)+
#   facet_grid(metric~Bcrit,scales="free")+
#   theme_pubr(legend="right")+
#   xlab("Monitoring Strategy")+
#   ylab("Value")+
#   ggtitle(paste("Cost Function Slope (cs) = 5"))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# print(guni)
# ggsave(paste("Univariate_Cost_Benefit_Tip_5only.pdf","costfucntionslope=",csvec[2],".pdf"),guni)
# 
# # Biplot of NPV and Monitoring Cost 
# 
# mm2 = reshape(mm,
#               timevar = "metric",
#               idvar = c("cv","Bcrit","cs"),
#               direction = "wide")
# 
# names(mm2)<-c("cv","Bcrit","cs","NPV","Monitoring","NPV-Monitoring","Prob_Tip")
# mm2$Bcrit<-factor(paste("Bcrit=",mm2$Bcrit,"*Bmsy"))
# mm2$cs<-factor(paste("CostFuncSlope=",mm2$cs))
# 
# mm2$cs <-factor(mm2$cs,levels=c("CostFuncSlope= 1","CostFuncSlope= 5","CostFuncSlope= 10"))
# 
# mm3<-subset(mm2,cv!="mean_PBCV")
# 
# ggto<-ggplot(mm3,aes(x=Monitoring,y=NPV))+
#   geom_jitter(aes(shape=cv,colour=Prob_Tip),size=5)+
#   theme_Publication()+
#   facet_grid(cs~Bcrit,scales="free")+
#   scale_colour_gradient(low="dodgerblue",high="red")
# print(ggto)
# ggsave("Trade_Off_Cost_Benefit.pdf",ggto)
# 
# ####
# 
# 
# # tmp<-data.frame("NPV"=value$cost.monitor,"mCost"=value$value)
# # ggplot(tmp,aes(x=mCost,y=NPV))+
# # geom_point()+
# # geom_density2d()+
# # stat_density2d(aes(fill=..level..),geom="polygon")
# # stat_density2d(aes(fill = ..level..), geom="polygon")
