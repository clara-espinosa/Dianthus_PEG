 library (tidyverse); library (stringr); library(GerminaR)
 library(glmmTMB); library (DHARMa); library (rstatix)

#adapt dataset for germinaR analysis
 read.csv ("data/dianthus_germ_data.csv", sep= ",") %>%
   mutate(viable = N_seeds - empty - fungus, 
          sowing_ID = paste(storage_treatment, ID))%>%
   dplyr::select (sowing_ID, WP_treatment, petri, viable, D1:D28)%>% 
   mutate(across(c(sowing_ID), as.factor))-> GermR
str(GermR)

#GerminaR package to obtain indices   
ger_summary(SeedN = "viable", evalName = "D", data=GermR[4:28]) -> germ_indices

#complete info of treatment, ID, petri....
germ_indices <- bind_cols(GermR[,1:3], germ_indices)
str(germ_indices)

# we choose MGT - mean germination time (time units, i.e. days) as germination speed index 
# comparison only at 0 MPa (control treatment) as requirement of reviewer
germ_indices %>%
  dplyr::select(sowing_ID, WP_treatment, petri, viable, mgt) %>%
  separate_wider_delim(sowing_ID, delim = " ", names = c("storage_treatment", "ID"))%>%
  convert_as_factor(storage_treatment, ID, WP_treatment)%>%
  filter(WP_treatment=="0")  %>%
  merge(read.csv("data/Dianthus_header.csv", sep = ","), by= c("ID")) %>%
  dplyr::select (Site, ID, storage_treatment, WP_treatment, petri, mgt) -> mgt

str(mgt)


# test differences in germination speed
a <- glmmTMB(mgt ~ storage_treatment + (1|Site/ID) , family = Gamma(link="log"),  data= mgt) 
summary(a)# everything is significant 
residuals <- simulateResiduals (a) ; plot(residuals) #with gamma family residuals looks good

# visualization
x11()


mgt%>%
  convert_as_factor(storage_treatment, ID, WP_treatment)%>%
  mutate(storage_treatment = fct_relevel(storage_treatment, "Fresh_seeds", "After_ripened" ))%>%
  mutate(storage_treatment = recode (storage_treatment, "Fresh_seeds" = "Fresh", "After_ripened" = "After ripened"))%>%
  mutate(WP_treatment = recode (WP_treatment,  "0" = "0 MPa"))%>%
  ggplot (aes()) +
  geom_boxplot(aes(x= storage_treatment, y= mgt, fill = storage_treatment), color = "black")+
  geom_point(aes(x= storage_treatment, y= mgt, fill = storage_treatment), show.legend = F, color = "black", shape= 21, size = 3, alpha = 0.5,position = "jitter")+
  geom_segment (aes(x= 1.1,xend =1.9,  y = 13, yend= 13), color = "black", linewidth = 1, show.legend = F)+
  geom_text ( aes(x= 1.5, y= 14, label = "***"), size= 4.5)+
  scale_fill_manual (name= "Storage treatment", values = c("forestgreen","gold" )) +
  scale_y_continuous(limits = c(0,15), breaks = seq(0, 15, by = 5))+
  theme_classic(base_size = 12)+
  labs( title= "Mean Germination Time", y = "Mean Germination Time (days)")+
  theme(strip.text = element_text(size= 15),
        text = element_text(family = "sans"),
        plot.margin = margin(0,0.15,0.5,0, unit = "cm"),
        panel.background = element_rect(color = "black", fill = NULL),
        axis.text.x = element_blank(),  #, angle = 20, vjust = 0.8
        axis.ticks.x= element_blank(),
        axis.text.y = element_text(size =12),
        axis.title.y= element_text(size =14),
        axis.title.x = element_blank(), 
        legend.text = element_text(size=12),
        legend.title = element_text (size = 14),
        legend.position = "bottom", # 
        legend.key = element_rect(fill = NA, color = NA))
