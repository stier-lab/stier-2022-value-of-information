#########
#FIGURE 2: How does the value of information change as a function of 
#########

#load packages, parameters, and MSE model
source("code/0_libraries.R") #load packages that are relevant
source("code/2_model_parameters.R") # base parameters
source("code/3_mse_model.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"



#ar has 5 dimensions: 1) pFmsy, 2) response variable, 3)  phi-uncertainty, 4) a values, 5) starting biomass 
#so division or subtraction of the array is just the values of NPV 


#####################################################################
#####################################################################
#Plot time below A+K/2 for cv=0.5 against ROI or just raw NPV for cv=0.1 and 0.5
#####################################################################
#####################################################################


# load("output/simulation/fig2_dz_acs_2020-11-30_300.Rdata") #this is the original simulation. dataframe = ar
load("output/simulation/fig2_dz_acs_100yr2021-01-22_10.Rdata") #this is the original simulation. dataframe = ar


df1 = melt(ar,varnames=names(dimnames(ar)))
colnames(df1) = c("pFmsy","metric","CV","A","B.start","value")
df1w <-pivot_wider(df1,names_from = metric)%>%
  filter(B.start == 70, CV %in% c(0.1,0.5))

df1w$CV = as.factor(df1w$CV)

df1w$pFmsy<-100*df1w$pFmsy

#thin the data? this doen'st work yet...
# df1wb<-df1w%>%
# slice(which(row_number() %% 5 == 1))

# n=5
# dfthin<- df1w[-seq(n, NROW(df1w), by = n),]


gg_abs_npv <-ggplot(df1w,aes(x=yrs.near.thresh1,y=NPV,shape=CV,colour=pFmsy,group=pFmsy))+
  geom_point(alpha=0.5)+
  geom_line(alpha=0.8)+
  scale_shape_discrete(name = "Monitoring Precision",
                       labels = c("High precision", "Low precision"))+
  scale_colour_gradientn(colours = pal,name=str_wrap("Maximum Harvest (%H_MSY)",20), guide = gc)+
    xlab("% Time in Overharvested (<0.8Bmsy)") +
  ylab("Net Present Value") +
  theme_pubr(legend="right")+
  theme(
    axis.text.x=element_text(size=10),
    axis.text.y=element_text(size=10),
    axis.title = element_text(size = 14),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

print(gg_abs_npv)
#this looks as a function of time below a threshold /i.e.dangerzone and ROI

ggsave("output/figures/ROI/ROI-1-25-2021.pdf",width=6,height=4)

#order of ar dimensions 
#1-fmsyvec, #2-response variable dimension, #3-phivec, #4-A values, #5-b.start

outputs<-ar

  npv_ratio <- (outputs[,1, 1 ,1,] / outputs[,1,5,1,])  # ROI = NPV(CV=0.1)/NPV(CV=0.5)
  t_near_tp <- outputs[,9, 5 ,1,] #time spent near tipping point at cv=0.5
  ptip <-outputs[,2, 5 ,1,]
  pFmsy<-as.numeric(names(ptip))
  npv_ratio3 <- data.frame(t_near_tp,ptip,npv_ratio,pFmsy)
  
gg_roi<-  ggplot(npv_ratio3,aes(npv_ratio3,x=t_near_tp,y=npv_ratio)) +
    geom_point(aes(colour=pFmsy)) +
    geom_smooth(se=F,color="black")+
    # facet_wrap(~B.start, scales = "free_y") +
    scale_colour_gradient(low="dodgerblue",high="firebrick",name="pHmsy") +
    xlab("%Time in Danger Zone (below A + K/2) for CV=0.5") +
    ylab("Return on Investment (NPV(CV.1) / NPV(CV.5)") +
    theme_pubr(legend="right")+
    geom_hline(yintercept=1,lty=2)
  

  plot_grid(gg_abs_npv,gg_roi,ncol=1)
  

#another way to see this? 
  
  ggplot(npv_ratio3,aes(npv_ratio3,x=pFmsy,y=npv_ratio)) +
    geom_point(aes(colour=pFmsy)) +
    geom_smooth(se=F,color="black")+
    # facet_wrap(~B.start, scales = "free_y") +
    scale_colour_gradient(low="dodgerblue",high="firebrick",name="pHmsy") +
    xlab("%Time in Danger Zone (below A + K/2) for CV=0.5") +
    ylab("Return on Investment (NPV(CV.1) / NPV(CV.5)") +
    theme_pubr(legend="right")+
    geom_hline(yintercept=1,lty=2)
  















#####################################################################
#####################################################################
#Plos focused on NPV as a function of starting biomass
#####################################################################
#####################################################################


load("output/simulation/fig2_mcs_2021-01-22_10.Rdata") #this is the original simulation. dataframe = ar

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






#=========GRAVEYARD===================#

# # get simulation outputs
# load("output/simulation/fig2_dz_acs_2020-11-30_300.Rdata") #this is the original simulation. dataframe = ar
# 
# #order of ar dimensions 
# #1-fmsyvec, #2-response variable dimension, #3-phivec, #4-A values, #5-b.start
# 
# ###This figure shows how the ROI changes as a function of b.start 
# 
# Fig2 <- function(outputs = ar){
#   npv_ratio <- (outputs[, 1, 1 ,1,] / outputs[, 1, 5 ,1,])  # ROI = NPV(CV=0.1)/NPV(CV=0.5)
#   npv_ratio <- melt(npv_ratio)
#   names(npv_ratio) <- c("pFmsy","B.start","roi")
#   
#   #npv_ratio2 <- subset(npv_ratio,pFmsy %in% c(0.0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0))
#   npv_ratio2 <- npv_ratio
#   npv_ratio2$prox <- -1*(npv_ratio2$B.start) # starting biomass
#   #npv_ratio2 <- subset(npv_ratio2,pFmsy %in% c(0.0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0))
#   # npv_ratio3<-npv_ratio2 %>%
#   # filter(pFmsy<0.8)%>%
#   # filter(B.start>30) for A=30
#   
#   
#   gs_ratio3 <- npv_ratio2 %>%
#     ggplot(aes(x=prox,y=roi,group=pFmsy)) +
#     geom_vline(xintercept=-10,lty=2,colour="grey",size=1) +
#     geom_vline(xintercept=-0.25*Bmsy,lty=3,colour="darkgrey",size=1) +
#     geom_line(aes(colour=pFmsy)) +
#     scale_colour_gradient(low="dodgerblue",high="firebrick",name="pHmsy") +
#     # scale_x_continuous(limits=c(-100,-5),breaks=c(-100,-75,-50,-25)) +
#     xlab("starting density") +
#     ylab("Return on Investment (NPV(CV.1) / NPV(CV.5)") +
#     theme_pubr(legend="right")
#   
#   gs_ratio3
# }
# 
# Fig2(outputs = ar)



# #####################################################################
# #####################################################################
# #Plots focused on time in danger zone and ROI 
# #####################################################################
# #####################################################################
# 
# load("output/simulation/fig2_mcs_2020-11-16_300.Rdata") #this is the original simulation. dataframe = ar
# 
# 
# ###This figure shows how  ROI changes as funciton of time below A+k/4 and thins out the data based on different starting values 
# #1-fmsyvec, #2-response variable dimension, #3-phivec, #4-A values, #5-b.start
# 
# 
# Fig2c <- function(outputs = ar){
#   npv_ratio <- (outputs[,1, 1 ,3,] / outputs[,1,5,3,])  # ROI = NPV(CV=0.1)/NPV(CV=0.5)
#   t_near_tp <- outputs[,9, 5 ,3,]
#   ptip <-outputs[,2, 5 ,3,]
#   
#   t_near_tp <- melt(t_near_tp)
#   names(t_near_tp) <- c("pFmsy","B.start","time.in.dangerzone")
#   
#   npv_ratio <- melt(npv_ratio)
#   names(npv_ratio) <- c("pFmsy","B.start","roi")
#   
#   t_ptip <-melt(ptip)
#   names(t_ptip) <- c("pFmsy","B.start","ptip")
#   
#   
#   #npv_ratio2 <- subset(npv_ratio,pFmsy %in% c(0.0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0))
#   npv_ratio2 <- npv_ratio %>% 
#     left_join(t_near_tp,by = c('pFmsy','B.start'))
#   
#   npv_ratio3<- npv_ratio2 %>%
#     left_join(t_ptip,by=c('pFmsy','B.start'))
#   
#   # npv_ratio3$pFmsy<-as.factor(npv_ratio3$pFmsy)
#   
#   #npv_ratio2$prox <- -1*(npv_ratio2$B.start) # starting biomass
#   #npv_ratio2 <- subset(npv_ratio2,pFmsy %in% c(0.0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0))
#   
#   timeplot2 <- npv_ratio3 %>%
#     filter(!is.na(roi)) %>%
#     filter(B.start>50) %>%
#     # filter(pFmsy %in% c(0.5))%>%
#     filter(ptip <0.25)%>%
#     ggplot(aes(x=time.in.dangerzone,y=roi,group=pFmsy)) +
#     geom_point(aes(colour=pFmsy)) +
#     facet_wrap(~pFmsy, scales = "free_y") +
#     scale_colour_gradient(low="dodgerblue",high="firebrick",name="pFMSY") +
#     xlab("Time below (A + K/2) for CV=0.5") +
#     ylab("Return on Investment (NPV(CV.1) / NPV(CV.5)") +
#     theme_pubr(legend="right")
#   timeplot2
# }
# 
# Fig2c(outputs = ar)
# 

