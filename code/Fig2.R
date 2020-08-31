# Figure 2 code

# get simulation outputs
load(here("output","simulation","range_of_bstart 2015-09-18 20000 .Rdata")) #this is the original simulation. dataframe = ar
source(here("code","ModelParameters_v1.R")) # reset base parameters after simulation (specifically max.F)


Fig2 <- function(outputs = ar){
  npv_ratio <- (outputs[,1,c(1),3,]/outputs[,1,c(5),3,])
  npv_ratio <- melt(npv_ratio)
  names(npv_ratio) <- c("pFmsy","B.start","roi")
  
  npv_ratio2 <- subset(npv_ratio,pFmsy %in% c(0.0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0))
  npv_ratio2$prox<- -1*(npv_ratio2$B.start)
  npv_ratio2 <- subset(npv_ratio2,pFmsy %in% c(0.0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0))
  
  gs_ratio3 <- npv_ratio2 %>%
                ggplot(aes(x=prox,y=roi,group=pFmsy))+
                geom_vline(xintercept=-10,lty=2,colour="grey",size=1)+
                geom_vline(xintercept=-0.25*Bmsy,lty=3,colour="darkgrey",size=1)+
                geom_line(aes(colour=pFmsy))+
                scale_colour_gradient(low="dodgerblue",high="firebrick",name="pHmsy")+
                scale_x_continuous(limits=c(-100,-5),breaks=c(-100,-75,-50,-25))+
                xlab("Proximity to Tipping Point")+
                ylab("Return on Investment (NPV(CV.1) / NPV(CV.5)")+
                theme_pubr(legend="right")
  gs_ratio3
}

Fig2(outputs = npv_ratio2)

