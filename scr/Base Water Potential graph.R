library(tidyverse);library (seedr)

#base water potential
bWP_summary %>%
  merge(bioclim, by= c("ID")) %>%
  merge(summary_seedmass, by= c("ID"))%>%
  merge(read.csv("data/Dianthus_header.csv", sep = ";"), by= c("ID", "site"))%>%
  mutate(sowing_time = factor(sowing_time))%>%
  mutate(sowing_time = fct_relevel(sowing_time, "Immediate", "After_ripening" ))%>%
  mutate(sowing_time = recode (sowing_time, "Immediate" = "Immediate", "After_ripening" = "After ripening"))%>%
  ggplot (aes(x=GDD, y= psib50)) + #, color = site
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, level = 0.9)+
  facet_wrap(~sowing_time)+
  theme_bw(base_size = 16) +
  geom_hline(yintercept=0, linetype ="dashed", size =1, colour = "red")+
  theme (plot.title = element_text ( size = 26), #hjust = 0.5,
         strip.text = element_text (size = 20),
         strip.background =element_rect(fill="white"),
         panel.background = element_blank(),
         axis.title.y = element_text (size=14), 
         axis.title.x = element_text (size=14), 
         legend.title = element_text(size = 14),
         legend.text = element_text (size =12))+
  labs(title= "Base water potential per GDD", y = "Base Water Potential (MPa)") -> fig3;fig3
