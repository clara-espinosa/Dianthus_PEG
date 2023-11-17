 library (tidyverse); library (seedr); library (stringr); library(GerminaR)
 
 # join bioclim data ibuttons + microlog
rbind(dianthus_bioclim_ibuttons, dianthus_bioclim_microlog) -> bioclim 

#####    IMMEDIATE ##########
#adapt dataset for germinaR analysis
 read.csv ("data/dianthus_germ_data.csv", sep= ";") %>%
   #filter(sowing_time =="Immediate_sowing")%>%
   mutate(viable = N_seeds - empty - fungus, 
          sowing_ID = paste(sowing_time, ID))%>%
   dplyr::select (sowing_ID, WP_treatment, petri, viable, D1:D28)%>% 
   mutate(across(c(sowing_ID, WP_treatment), as.factor))-> GermR
str(GermR)

#GerminaR package to obtain indices   
ger_summary(SeedN = "viable", evalName = "D", data=GermR[4:28]) -> germ_indices

#complete info of treatment, ID, petri....
germ_indices <- bind_cols(GermR[,1:3], germ_indices)
str(germ_indices)

# choose germination indices 
#              GRP - germination percentage (from 0 to 100%)
#              MGR - mean germination rate (time units)
#              SYN - syncronization index (from 0 to 1)
germ_indices %>%
  dplyr::select(sowing_ID, WP_treatment, petri, viable, grp, mgr, syn) %>%
  mutate(WP_treatment = factor(WP_treatment))%>%
  mutate(WP_treatment = fct_relevel(WP_treatment,"0", "-0.2", "-0.4", "-0.6", "-0.8", "-1", "-1.2" ))%>%
  separate_wider_delim(sowing_ID, delim = " ", names = c("sowing_time", "ID"))%>%
  merge(bioclim, by= c("ID")) %>%
  merge(read.csv("data/Dianthus_header.csv", sep = ";"), by= c("ID"))->germ_indices   %>%
  #as.data.frame() %>%
  #mutate_all(~ifelse(is.nan(.), 0, .))  # replace NAN to 0->
str(germ_indices)

germ_indices%>%
  ggplot (aes(x=GDD, y= grp, color= WP_treatment)) +
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, level = 0.9)+
  facet_wrap(~sowing_time)+
  labs( title= "Final germination per GDD")
