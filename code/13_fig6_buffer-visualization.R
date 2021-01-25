
#########
#FIGURE 3: How does the value of information change as a function of 
#########

#load libraries
source("code/0_libraries.R") #load packages that are relevant
source("code/2_model_parameters.R") # base parameters
source("code/3_mse_model.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"

#import simulation output
load("output/simulation/precautionary_buffer 2021-01-21 5000 .Rdata")

##Univariate Response
mm<-melt(edf[,-1,,,])
names(mm)<-c("cv","metric","Bcrit","cs","mf","value")
mm$metric <-factor(mm$metric,levels=c("NPV","Monitoring","NPV_minus_Monitoring","Prob_Tip"))
mm$cv <-factor(mm$cv,levels=c("fixedCV0.1","fixedCV0.5","PrecautionaryBufferCV","mean_PBCV"))

#subset out npv minus monitoring 
mm3<- subset(mm,metric!="NPV_minus_Monitoring")
mm3<- drop.levels(subset(mm3,cv!="mean_PBCV"))

csvec <-unique(mm$cs)

temp<-subset(mm3,cs==csvec[2])

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


ggplot(temp3b,aes(x=mf,y=ratio,group=cv))+
    geom_smooth(aes(lty=cv),colour="gray60",se=F)+
    geom_point(aes(colour=ptip,pch=cv),alpha=0.5,size=3)+
    geom_line(aes(colour=ptip))+
    theme_pubr(legend="right")+
    scale_colour_gradient(low="#00AFBB",high="#FC4E07")+
    scale_shape_discrete(
      name = "Monitoring strategy",
      labels=c("High Precision (CV=0.1)",
               "Low Precision ((CV=0.5)",
               "Precautionary Buffer"))+
    scale_linetype_discrete(name = "Monitoring strategy",
                            labels=c("High Precision (CV=0.1)",
                                     "Low Precision ((CV=0.5)",
                                     "Precautionary Buffer"))+
    scale_shape_discrete(
      name = "Monitoring strategy",
      labels=c("High Precision (CV=0.1)",
               "Low Precision ((CV=0.5)",
               "Precautionary Buffer"))+
    xlab("Maximum Fishing Effort pHMSY")+
    ylab("Value Ratio: NPV/Monitoring Cost")+
    labs(colour="Probability of tipping")+
    theme(strip.text = element_text(size = 10))+
    theme(strip.background = element_blank())+
    # theme(legend.position = c(.90,.90))+
    # theme(legend.title=element_blank())+
    theme(axis.title.x= element_text(color= "black", size=14),
          axis.title.y= element_text(color= "black", size=14))+
    theme(legend.text=element_text(size=10))+
    theme(legend.background = element_rect(
      size=0.5, linetype ="solid"))+
    theme(axis.text = element_text(size = 12))+
    theme(legend.text=element_text(size=12))

  ggsave("output/figures/Buffer/buffer_cs_5.pdf",width=7,height=5)


  
  
  
#Archive# 
  
  # gsmooth<-ggplot(temp3b,aes(x=mf,y=ratio,group=cv))+
  #   geom_point(aes(colour=ptip,pch=cv))+
  #   geom_smooth(aes(lty=cv),se=F,colour="black")+
  #   theme_pubr(legend="right")+
  #   scale_colour_gradientn(colours = pal,name="Probability of tipping") + 
  #   xlab("Maximum Fishing Effort")+
  #   ylab("Value Ratio: NPV/Cost")
  # 
  # 
  # print(gsmooth)
  
  
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


# 
# 
# 
# 
# load("output/simulation/precautionary_buffer 2021-01-22 5000 .Rdata")
# 
# 
# ##Univariate Response
# 
# mm<-melt(edf[,,,,])
# names(mm)<-c("cv","metric","Bcrit","cs","mf","value")
# mm$metric <-factor(mm$metric,levels=c("NPV","Monitoring","NPV_minus_Monitoring","Prob_Tip"))
# mm$cv <-factor(mm$cv,levels=c("fixedCV0.1","fixedCV0.5","PrecautionaryBufferCV","mean_PBCV"))
# 
# #subset out npv minus monitoring 
# mm3<- subset(mm,metric!="NPV_minus_Monitoring")
# mm3<- drop.levels(subset(mm3,cv!="mean_PBCV"))
# 
# 
# pdf(paste("output/figures/Buffer/Cost_Benefit",Sys.Date(),".pdf"), width=12,onefile = TRUE)
# 
# 
# for(i in 1:length(csvec)){
#   
#   temp<-subset(mm3,cs==csvec[i])
#   temp_b<-subset(temp,mf==1) 
#   temp_b<-subset(temp_b,Bcrit==1)
#   temp_b$metric <-factor(temp_b$metric,levels=c("NPV","Monitoring","Prob_Tip"))
#   
#   
#   guni<-ggplot(temp_b,aes(cv,y=value))+
#     geom_bar(stat="identity",aes(fill=metric),width=0.75)+
#     facet_grid(metric~Bcrit,scales="free")+
#     theme_classic()+
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
#     geom_line(aes(colour=ptip))+
#     geom_smooth(aes(lty=cv),colour="gray60",se=F)+
#     theme_classic()+
#     scale_colour_gradient(low="dodgerblue",high="red")+
#     xlab("Maximum Fishing Effort")+
#     ylab("Value Ratio: NPV/Cost")+
#     #$ facet_grid(.~cv)+
#     ggtitle(paste("Cost Function Slope (cs) =",csvec[i]))
#   
#   print(gsmooth)
#   
#   ggplot(temp3b,aes(x=mf,y=ratio,group=cv))+
#     geom_smooth(aes(lty=cv),colour="gray60",se=F)+
#     geom_point(aes(colour=ptip,pch=cv),alpha=0.5,size=3)+
#     geom_line(aes(colour=ptip))+
#     theme_pubr(legend="right")+
#     scale_colour_gradient(low="#00AFBB",high="#FC4E07")+
#     scale_shape_discrete(
#       name = "Monitoring strategy",
#       labels=c("High Precision (CV=0.1)",
#                "Low Precision ((CV=0.5)",
#                "Precautionary Buffer"))+
#     scale_linetype_discrete(name = "Monitoring strategy",
#                             labels=c("High Precision (CV=0.1)",
#                                      "Low Precision ((CV=0.5)",
#                                      "Precautionary Buffer"))+
#     scale_shape_discrete(
#       name = "Monitoring strategy",
#       labels=c("High Precision (CV=0.1)",
#                "Low Precision ((CV=0.5)",
#                "Precautionary Buffer"))+
#     xlab("Maximum Fishing Effort pHMSY")+
#     ylab("Value Ratio: NPV/Monitoring Cost")+
#     labs(colour="Probability of tipping")+
#     theme(strip.text = element_text(size = 10))+
#     theme(strip.background = element_blank())+
#     # theme(legend.position = c(.90,.90))+
#     # theme(legend.title=element_blank())+
#     theme(axis.title.x= element_text(color= "black", size=14),
#           axis.title.y= element_text(color= "black", size=14))+
#     theme(legend.text=element_text(size=10))+
#     theme(legend.background = element_rect( 
#       size=0.5, linetype ="solid"))+
#     theme(axis.text = element_text(size = 12))+
#     theme(legend.text=element_text(size=12))
#   
#   ggsave("output/figures/Buffer/buffer_cs_5.pdf",width=7,height=5)  
#   
#   
#   cv0.1<-
#     temp3b%>%
#     filter(cv=="fixedCV0.1")
#   
#   line0.1<-predict(loess(mf~ratio,data=cv0.1))
#   stat_smooth(mf~ratio,data=cv0.1)
#   
#   cv0.5<-
#     temp3b%>%
#     filter(cv=="fixedCV0.5")
#   
#   line0.5<-predict(loess(mf~ratio,data=cv0.5))
#   
#   cv0.1_0.5<-
#     temp3b%>%
#     filter(cv=="PrecautionaryBufferCV")
#   
#   line0.1_0.5<-predict(loess(ratio~mf,data=cv0.1_0.5))
#   
#   # plot(cm1,type="l",xlim=c(0,0.6),ylim=c(0,100),ylab="monitoring cost",xlab="monitoring precision")
#   # lines(cvec,cm5,type="l",col=2)
#   # lines(cvec,cm10,type="l",col=4)
#   # 
#   
#   pred_df<-data.frame(line0.1,line0.5,line0.1_0.5)
#   names(pred_df)<-c("High Monitoring","Low Monitoring","Precautionary Buffer")
#   pred_df$mf<-cv0.5$mf
#   
#   plot(pred_df$mf,pred_df$`High Monitoring`,type="l")
#   
#   pred_df2<-pivot_longer(pred_df,!mf)
#   pred_df2<- arrange(pred_df2,name,mf)
#   pred_df2$tip<-c(cv0.1$ptip,cv0.5$ptip,cv0.1_0.5$ptip)
#   pred_df2$name<-as.factor(pred_df2$name)
#   
#   gsmooth2<-
#     ggplot(pred_df2,aes(x=mf,y=value))+
#     geom_line(aes(group=name,colour=tip))+
#     scale_colour_gradient(low="dodgerblue",high="red")
#   
#   # facet_wrap(~name)
#   print(gsmooth2)
#   
#   pred_smooth<-gs$data[[3]]
#   
#   gs<-ggplot_build(gsmooth)
#   ggplot(data=,aes(x=x,y=y,group=group))+
#     geom_line()
#   
#   #add colors
#   
#   
#   
#   
#   # ggsave(paste("Biplot_NPV_Cost_Tip_nofacet.pdf","costfucntionslope=",csvec[i],".pdf"),gsmooth)
#   
#   #alterantively, plot cost versus benefit
#   
#   gcb<-ggplot(temp3b,aes(x=cost,y=NPV))+
#     geom_point(aes(colour=ptip,pch=cv,size=mf))+
#     theme_classic()+
#     scale_colour_gradient(low="dodgerblue",high="red")+
#     xlab("Monitoring Cost")+
#     ylab("Benefit NPV")+
#     facet_grid(.~cv)+
#     ggtitle(paste("Cost Function Slope (cs) =",csvec[i]))
#   
#   print(gcb)
#   
# }
# 
# dev.off()
# 
# ###this loooks funny now tha tyou sampeld across different fishing efforts, check subset
# 
# t2<-subset(mm3,cs==5 & Bcrit==1)
# guni<-ggplot(t2,aes(cv,y=value))+
#   geom_bar(stat="identity",aes(fill=metric),width=0.75)+
#   facet_grid(metric~Bcrit,scales="free")+
#   theme_classic()+
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
#   theme_classic()+
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
# 
# 

