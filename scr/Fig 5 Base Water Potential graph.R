library(tidyverse);library (seedr)

#FIG 5 base water potential

regrpvalues <- data.frame (storage_treatment= c("Fresh", "After ripened"), lbl = c("p = 0.22", "p = 0.03")) # from glm model
regrpvalues %>%
  mutate(storage_treatment = factor(storage_treatment))%>%
  mutate(storage_treatment = fct_relevel(storage_treatment, "Fresh", "After ripened" ))-> regrpvalues
bWP_summary %>%
  merge(bioclim, by= c("ID")) %>%
  merge(summary_seedmass, by= c("ID"))%>%
  mutate(psib50 = ifelse( psib50 > 0, 0, psib50))%>%
  #merge(read.csv("data/Dianthus_header.csv", sep = ";"), by= "ID")%>%
  mutate(storage_treatment = factor(storage_treatment))%>%
  mutate(storage_treatment = recode (storage_treatment, "Fresh_seeds" = "Fresh", "After_ripened" = "After ripened"))%>%
  mutate(storage_treatment = fct_relevel(storage_treatment, "Fresh", "After ripened" ))%>%
  ggplot (aes(x=GDD, y= psib50, fill= storage_treatment), color = "black") + #, color = site
  geom_point(size= 3, shape=21)+
  geom_smooth(method = "lm", se=FALSE, level = 0.9, color = "black")+
  scale_fill_manual (name= "Storage treatment", values = c("forestgreen", "gold") ) +
  scale_color_manual (name= "Storage treatment", values = c("forestgreen", "gold") ) +
  geom_text(data= regrpvalues, aes(y= 0.1, x= 1550,  label=lbl), color= "black", size = 4)+
  facet_wrap(~storage_treatment)+
  theme_classic(base_size = 12) +
  #ggthemes::theme_tufte(base_size = 16) + 
  geom_hline(yintercept=0, linetype ="dashed", linewidth =1, colour = "black")+
  theme (text = element_text(family = "sans"),
         panel.background = element_rect(color = "black", fill = NULL), #hjust = 0.5,
         plot.title = element_text ( size = 14), #
         strip.text = element_text (size = 12),
         strip.background =element_rect(fill="white"),
         axis.title = element_text (size=10),  
         legend.title = element_text(size = 12),
         legend.text = element_text (size =10), 
         legend.position = "none")+
  labs(y = expression(paste(Psi,"b (MPa)")), x= "Growing degree days (ºC)") -> Fig5; Fig5

ggsave(filename = "Figure 5.png", plot =Fig5, path = "results/figures", 
       scale =1, width = 180, height = 100,units = "mm",device = "png", dpi = 600)
