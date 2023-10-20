 library (tidyverse); library (seedr); library (stringr); library(GerminaR)
 
 # join bioclim data ibuttons + microlog
rbind(dianthus_bioclim_ibuttons, dianthus_bioclim_microlog) -> bioclim 

#####    IMMEDIATE ##########
#adapt dataset for germinaR analysis
 read.csv ("data/dianthus_germ_data.csv", sep= ";") %>%
   filter(sowing_time =="Immediate_sowing")%>%
   mutate(viable = N_seeds - empty - fungus)%>%
   dplyr::select (ID, WP_treatment, petri, viable, D1:D28)%>% 
   mutate(across(c(ID, WP_treatment), as.factor))-> GermR
str(GermR)

#GerminaR package to obtain indices   
ger_summary(SeedN = "viable", evalName = "D", data=GermR[4:28]) -> ind_immediate

#complete info of treatment, ID, petri....
ind_immediate <- bind_cols(GermR[,1:3], ind_immediate)

# choose germination indices 
#              GRP - germination percentage (from 0 to 100%)
#              MGR - mean germination rate (time units)
#              SYN - syncronization index (from 0 to 1)
ind_immediate %>%
  dplyr::select(ID, WP_treatment, petri, viable, grp, mgr, syn) %>%
  mutate_all(~replace(., is.nan(.), 0)) %>% # replace NAN to 0
  merge(read.csv("data/Dianthus_header.csv", sep = ";"))%>%
  merge(bioclim)-> graph
str(graph)
  
ggplot (graph, aes(x=Snw, y= grp, color= WP_treatment)) +
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, level = 0.9)+
  labs( title= "Final germination per GDD")
 
##### after ripenning UNTIL DAY 16 ONLY ######## 
#adapt dataset for germinaR analysis
read.csv ("data/dianthus_germ_data.csv", sep= ";") %>%
  filter(sowing_time =="After_ripening")%>%
  mutate(viable = N_seeds)%>%
  dplyr::select (ID, WP_treatment, petri, viable, D1:D16)%>% 
  mutate(across(c(ID, WP_treatment), as.factor))-> GermR
str(GermR)

#GerminaR package to obtain indices   
ger_summary(SeedN = "viable", evalName = "D", data=GermR[4:16]) -> ind_after

#complete info of treatment, ID, petri....
ind_after <- bind_cols(GermR[,1:3], ind_after)

# choose germination indices 
#              GRP - germination percentage (from 0 to 100%)
#              MGR - mean germination rate (time units)
#              SYN - syncronization index (from 0 to 1)
ind_after %>%
  dplyr::select(ID, WP_treatment, petri, viable, grp, mgr, syn) %>%
  mutate_all(~replace(., is.nan(.), 0)) %>% # replace NAN to 0
  merge(read.csv("data/Dianthus_header.csv", sep = ";"))%>%
  merge(bioclim)-> graph
str(graph)

ggplot (graph, aes(x=GDD, y= grp, color= WP_treatment)) +
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, level = 0.9)+
  labs( title= "Final germination per GDD")

 