library(tidyverse);library (seedr)

#FIG 5 base water potential

regrpvalues <- data.frame (sowing_time= c("Fresh", "After ripened"), lbl = c("p = 0.19", "p = 0.04")) # from glm model
regrpvalues %>%
  mutate(sowing_time = factor(sowing_time))%>%
  mutate(sowing_time = fct_relevel(sowing_time, "Fresh", "After ripened" ))-> regrpvalues
bWP_summary %>%
  merge(bioclim, by= c("ID")) %>%
  merge(summary_seedmass, by= c("ID"))%>%
  #merge(read.csv("data/Dianthus_header.csv", sep = ";"), by= "ID")%>%
  mutate(sowing_time = factor(sowing_time))%>%
  mutate(sowing_time = recode (sowing_time, "Immediate" = "Fresh", "After_ripening" = "After ripened"))%>%
  mutate(sowing_time = fct_relevel(sowing_time, "Fresh", "After ripened" ))%>%
  ggplot (aes(x=GDD, y= psib50, fill= sowing_time, color = sowing_time)) + #, color = site
  geom_point(size= 4, shape=21)+
  geom_smooth(method = "lm", se=FALSE, level = 0.9)+
  scale_fill_manual (name= "Storage treatment", values = c("forestgreen", "gold") ) +
  scale_color_manual (name= "Storage treatment", values = c("forestgreen", "gold") ) +
  geom_text(data= regrpvalues, aes(y= 0.1, x= 1550,  label=lbl), color= "black", size = 5)+
  facet_wrap(~sowing_time)+
  ggthemes::theme_tufte(base_size = 16) + 
  geom_hline(yintercept=0, linetype ="dashed", size =1, colour = "black")+
  theme (text = element_text(family = "sans"),
         panel.background = element_rect(color = "black", fill = NULL), #hjust = 0.5,
         plot.title = element_text ( size = 18), #
         strip.text = element_text (size = 16),
         strip.background =element_rect(fill="white"),
         axis.title.y = element_text (size=14), 
         axis.title.x = element_text (size=14), 
         legend.title = element_text(size = 14),
         legend.text = element_text (size =14), 
         legend.position = "none")+
  labs(y = expression(paste(Psi,"b (MPa)")), x= "Growing degree days (ºC)") -> Fig5; Fig5
