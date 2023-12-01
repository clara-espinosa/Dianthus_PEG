library(tidyverse);library (seedr)

#base water potential
bWP_summary %>%
  merge(bioclim, by= c("ID")) %>%
  merge(summary_seedmass, by= c("ID"))%>%
  #merge(read.csv("data/Dianthus_header.csv", sep = ";"), by= "ID")%>%
  mutate(sowing_time = factor(sowing_time))%>%
  mutate(sowing_time = fct_relevel(sowing_time, "Immediate", "After_ripening" ))%>%
  mutate(sowing_time = recode (sowing_time, "Immediate" = "Immediate", "After_ripening" = "After ripening"))%>%
  ggplot (aes(x=GDD, y= psib50)) + #, color = site
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, level = 0.9)+
  facet_wrap(~sowing_time)+
  ggthemes::theme_tufte() + 
  geom_hline(yintercept=0, linetype ="dashed", size =1, colour = "red")+
  theme (text = element_text(family = "sans"),
         panel.background = element_rect(color = "black", fill = NULL), #hjust = 0.5,
         plot.title = element_text ( size = 20), #
         strip.text = element_text (size = 18),
         strip.background =element_rect(fill="white"),
         axis.title.y = element_text (size=14), 
         axis.title.x = element_text (size=14), 
         legend.title = element_text(size = 14),
         legend.text = element_text (size =12))+
  labs(title= "Base water potential per GDD", y = "Base Water Potential (MPa)") -> Fig4; Fig4
