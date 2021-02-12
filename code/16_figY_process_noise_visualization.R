######
#Figure 5 visualizaition - how does process noise alter the value of information in avoiding tipping points
######

source("code/0_libraries.R") #load packages that are relevant
source("code/2_model_parameters.R") # base parameters
source("code/3_mse_model.R") #load MSE model "est.NPV" and wrapper to repeat model "repeat.model2"

#load data
load("output/simulation/process_noise.Rdata") #this is ignored on github will need to produce

noise_df_NPV <- gather(as.data.frame(emat),"NPV","ptip","biomass","cumulative_yield","sd-biomass","ptip_mgmt", 
                       key="response", 
                       value = "value") %>% filter(response =="NPV")


noise_NPV<-ggplot(noise_df_NPV,aes(x= process_noise, y=precision))+
  geom_tile(aes(fill=value,colour=value))+
  theme_pubr(legend="right")+
  ylab("monitoring CV")+
  xlab("Process Noise")+
  scale_fill_gradient(low="#80BE9E",high="#F98866",name="NPV", guide = gc)+
  scale_colour_gradient(low="#80BE9E",high="#F98866",name="NPV", guide = gc)

npv_l <- get_legend(noise_NPV)

noise_df_ptip <- gather(as.data.frame(emat),"NPV","ptip","biomass","cumulative_yield","sd-biomass","ptip_mgmt", 
                        key="response", 
                        value = "value") %>% filter(response =="ptip")


noise_ptip<-ggplot(noise_df_ptip,aes(x= process_noise, y=precision))+
  geom_tile(aes(fill=value,colour=value))+
  theme_pubr(legend="right")+
  ylab("monitoring CV")+
  xlab("Process Noise")+
  scale_fill_gradient(low="#80BE9E",high="#F98866",name="Ptip",guide= gc)+
  scale_colour_gradient(low="#80BE9E",high="#F98866",name="Ptip", guide = gc)

ptip_l <- get_legend(noise_ptip)


noise_df_ptipmgmt <- gather(as.data.frame(emat),"NPV","ptip","biomass","cumulative_yield","sd-biomass","ptip_mgmt", 
                            key="response", 
                            value = "value") %>% filter(response =="ptip_mgmt")


noise_ptipmgmt<-ggplot(noise_df_ptipmgmt,aes(x= process_noise, y=precision))+
  geom_tile(aes(fill=value,colour=value))+
  theme_pubr(legend="right")+
  ylab("monitoring CV")+
  xlab("Process Noise")+
  scale_fill_gradient(low="#80BE9E",high="#F98866",name="Ptip MGMT",guide= gc)+
  scale_colour_gradient(low="#80BE9E",high="#F98866",name="Ptip MGMT", guide = gc)

ptipm_l <- get_legend(noise_ptipmgmt)


#combine but the dimmensions still need work to reproduce nice figure 
conserv_plot <- plot_grid(noise_NPV+theme(legend.position="none"),noise_ptip+theme(legend.position="none"),noise_ptipmgmt+theme(legend.position="none"),ncol=1,labels="AUTO")
conserve_plot2 <- plot_grid(npv_l,ptip_l,ptipm_l,ncol=1,align="hv")
conserve_plot3 <- plot_grid(conserv_plot,conserve_plot2,axis="h",rel_widths=c(1,.3),align = 'h')
conserve_plot3
save_plot(here("output/figures/process_noise_cv_original_MSE_3_20_19.pdf"),conserve_plot3,base_width=6,base_height=10) 

