##
#Adaptive Monitoring
#The goal of this code is to run simulations where you have both high and low CV depending on the estimated biomass (Bhat)
##

source("code/0_libraries.R") #load packages that are relevant
source("code/2_model_parameters.R") # base parameters
source("code/3_mse_model.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"

#Number of Iterations 
n.iters=5000
phi.seeds<-round(1000000*runif(n.iters),0)
process.seeds<-round(1000000*runif(n.iters),0)

#Define Parameters 
max.F= max.F
B.start=100
A=10
years=50
# B.crit=.25*Bmsy

#Set up Monitoring Regimes to Loop Through
cv <- c("fixedCV0.1","fixedCV0.5","PrecautionaryBufferCV","mean_PBCV")
phitab<-matrix(0,nrow=2,ncol=4)
colnames(phitab)<-cv

phitab[,1]<-0.1
phitab[,2]<-0.5
phitab[1,3]<-0.1
phitab[2,3]<-0.5

bcritvec<-c(0.25,0.5,1,1.25) #different thresholds for when more precise monitoring kicks in
csvec<-c(1,5,10) #different costs of monitoring
mfvec<-seq(0,2,by=0.05) #

###################################################Need to run and see how to melt right 
#Set up Empty data Frame
edf<-array(dim=c(4,5,length(bcritvec),length(csvec),length(mfvec)))
dimnames(edf) = list(cv,c("CV","NPV","Monitoring","NPV_minus_Monitoring","Prob_Tip"),bcritvec,csvec,mfvec)

# NPV_minus_Monitoring <- rep(0,4)
# NPV <- rep(0,4)
# Monitoring <-rep(0,4)
# Prob_Tip <-rep(0,4)
# edf <- data.frame(cv,NPV,Monitoring,NPV_minus_Monitoring,Prob_Tip)

for(c in 1:length(csvec)){  
  for(bc in 1:length(bcritvec)){
    for(i in 1:ncol(phitab)){
      for(mf in 1:length(mfvec)){
        
        phi.CV.low=phitab[1,i]
        phi.CV.high=phitab[2,i]
        B.crit<-bcritvec[bc]*Bmsy
        cs<-csvec[c]
        max.F=mfvec[mf]*Fmsy
        
        value = repeat.model2(n.iters,B.start=100,B.lim,years,K,A,r,phi.CV,delta=.05,
                              process.noise=0.0,p,max.F=max.F,phi.seeds,process.seeds)
        
        edf[i,2,bc,c,mf]<-median(value$value)
        edf[i,3,bc,c,mf]<-median(value$cost.monitor)
        edf[i,4,bc,c,mf]<-median(value$NPV_minusCM)
        edf[i,5,bc,c,mf]<-sum(value$TP)/n.iters
        
        phitab[1,4]<-mean(value$phi.CV)
        phitab[2,4]<-mean(value$phi.CV)
        
        print(max.F)
      }
    }
  }
}

save(edf,file=here("output/simulation",paste("precautionary_buffer",Sys.Date(),n.iters,".Rdata")))


load("output/simulation/precautionary_buffer 2021-01-22 5000 .Rdata")


##Univariate Response

mm<-melt(edf[,,,,])
names(mm)<-c("cv","metric","Bcrit","cs","mf","value")
mm$metric <-factor(mm$metric,levels=c("NPV","Monitoring","NPV_minus_Monitoring","Prob_Tip"))
mm$cv <-factor(mm$cv,levels=c("fixedCV0.1","fixedCV0.5","PrecautionaryBufferCV","mean_PBCV"))

#subset out npv minus monitoring 
mm3<- subset(mm,metric!="NPV_minus_Monitoring")
mm3<- drop.levels(subset(mm3,cv!="mean_PBCV"))


pdf(paste("output/figures/Buffer/Cost_Benefit",Sys.Date(),".pdf"), width=12,onefile = TRUE)


for(i in 1:length(csvec)){
  
  temp<-subset(mm3,cs==csvec[i])
  temp_b<-subset(temp,mf==1) 
  temp_b<-subset(temp_b,Bcrit==1)
  temp_b$metric <-factor(temp_b$metric,levels=c("NPV","Monitoring","Prob_Tip"))
  
  
  guni<-ggplot(temp_b,aes(cv,y=value))+
    geom_bar(stat="identity",aes(fill=metric),width=0.75)+
    facet_grid(metric~Bcrit,scales="free")+
    theme_classic()+
    xlab("Monitoring Strategy")+
    ylab("Value")+
    ggtitle(paste("Cost Function Slope (cs) =",csvec[i]))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(guni)
  # ggsave(paste("Univariate_Cost_Benefit_Tip.pdf","costfucntionslope=",csvec[i],".pdf"),guni)
  
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
    geom_line(aes(colour=ptip))+
    geom_smooth(aes(lty=cv),colour="gray60",se=F)+
    theme_classic()+
    scale_colour_gradient(low="dodgerblue",high="red")+
    xlab("Maximum Fishing Effort")+
    ylab("Value Ratio: NPV/Cost")+
    #$ facet_grid(.~cv)+
    ggtitle(paste("Cost Function Slope (cs) =",csvec[i]))
  
  print(gsmooth)
  
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

  
  cv0.1<-
  temp3b%>%
    filter(cv=="fixedCV0.1")
    
  line0.1<-predict(loess(mf~ratio,data=cv0.1))
  stat_smooth(mf~ratio,data=cv0.1)
  
  cv0.5<-
    temp3b%>%
    filter(cv=="fixedCV0.5")
  
  line0.5<-predict(loess(mf~ratio,data=cv0.5))
  
  cv0.1_0.5<-
    temp3b%>%
    filter(cv=="PrecautionaryBufferCV")
  
  line0.1_0.5<-predict(loess(ratio~mf,data=cv0.1_0.5))
  
  # plot(cm1,type="l",xlim=c(0,0.6),ylim=c(0,100),ylab="monitoring cost",xlab="monitoring precision")
  # lines(cvec,cm5,type="l",col=2)
  # lines(cvec,cm10,type="l",col=4)
  # 
  
  pred_df<-data.frame(line0.1,line0.5,line0.1_0.5)
  names(pred_df)<-c("High Monitoring","Low Monitoring","Precautionary Buffer")
  pred_df$mf<-cv0.5$mf
  
  plot(pred_df$mf,pred_df$`High Monitoring`,type="l")
  
  pred_df2<-pivot_longer(pred_df,!mf)
  pred_df2<- arrange(pred_df2,name,mf)
  pred_df2$tip<-c(cv0.1$ptip,cv0.5$ptip,cv0.1_0.5$ptip)
  pred_df2$name<-as.factor(pred_df2$name)
  
  gsmooth2<-
  ggplot(pred_df2,aes(x=mf,y=value))+
    geom_line(aes(group=name,colour=tip))+
    scale_colour_gradient(low="dodgerblue",high="red")
    
    # facet_wrap(~name)
  print(gsmooth2)
  
  pred_smooth<-gs$data[[3]]
  
  gs<-ggplot_build(gsmooth)
  ggplot(data=,aes(x=x,y=y,group=group))+
    geom_line()
  
  #add colors
  
  
  
  
  # ggsave(paste("Biplot_NPV_Cost_Tip_nofacet.pdf","costfucntionslope=",csvec[i],".pdf"),gsmooth)
  
  #alterantively, plot cost versus benefit
  
  gcb<-ggplot(temp3b,aes(x=cost,y=NPV))+
    geom_point(aes(colour=ptip,pch=cv,size=mf))+
    theme_classic()+
    scale_colour_gradient(low="dodgerblue",high="red")+
    xlab("Monitoring Cost")+
    ylab("Benefit NPV")+
    facet_grid(.~cv)+
    ggtitle(paste("Cost Function Slope (cs) =",csvec[i]))
  
  print(gcb)
  
}

dev.off()

###this loooks funny now tha tyou sampeld across different fishing efforts, check subset

t2<-subset(mm3,cs==5 & Bcrit==1)
guni<-ggplot(t2,aes(cv,y=value))+
  geom_bar(stat="identity",aes(fill=metric),width=0.75)+
  facet_grid(metric~Bcrit,scales="free")+
  theme_classic()+
  xlab("Monitoring Strategy")+
  ylab("Value")+
  ggtitle(paste("Cost Function Slope (cs) = 5"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(guni)
ggsave(paste("Univariate_Cost_Benefit_Tip_5only.pdf","costfucntionslope=",csvec[2],".pdf"),guni)

# Biplot of NPV and Monitoring Cost 

mm2 = reshape(mm,
              timevar = "metric",
              idvar = c("cv","Bcrit","cs"),
              direction = "wide")

names(mm2)<-c("cv","Bcrit","cs","NPV","Monitoring","NPV-Monitoring","Prob_Tip")
mm2$Bcrit<-factor(paste("Bcrit=",mm2$Bcrit,"*Bmsy"))
mm2$cs<-factor(paste("CostFuncSlope=",mm2$cs))

mm2$cs <-factor(mm2$cs,levels=c("CostFuncSlope= 1","CostFuncSlope= 5","CostFuncSlope= 10"))

mm3<-subset(mm2,cv!="mean_PBCV")

ggto<-ggplot(mm3,aes(x=Monitoring,y=NPV))+
  geom_jitter(aes(shape=cv,colour=Prob_Tip),size=5)+
  theme_classic()+
  facet_grid(cs~Bcrit,scales="free")+
  scale_colour_gradient(low="dodgerblue",high="red")
print(ggto)
ggsave("Trade_Off_Cost_Benefit.pdf",ggto)

####


# tmp<-data.frame("NPV"=value$cost.monitor,"mCost"=value$value)
# ggplot(tmp,aes(x=mCost,y=NPV))+
# geom_point()+
# geom_density2d()+
# stat_density2d(aes(fill=..level..),geom="polygon")
# stat_density2d(aes(fill = ..level..), geom="polygon")


