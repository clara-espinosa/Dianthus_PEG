library(tidyverse);library(wesanderson); library (binom) ;library(ggpubr)

#final germination x treatment x ID graph
x11()
germination_summary %>%
  mutate(WP_treatment = factor(treatment))%>%
  mutate(WP_treatment = fct_relevel(WP_treatment,"0", "-0.2", "-0.4", "-0.6", "-0.8", "-1", "-1.2" ))%>%
  merge(bioclim, by = "ID") %>%
  merge(read.csv("data/Dianthus_header.csv", sep = ";"), by = c("ID", "site")) %>%
  ggplot() +
  geom_point(aes(x= treatment, y =germination.mean, color =WP_treatment), size = 3) +
  #geom_errorbar(aes(x= treatment, ymin = germination.lower, 
  #ymax = germination.upper, color = WP_treatment),width = 0.2, size =1.2)  +
  coord_cartesian(ylim = c(0,1))+
  facet_wrap(~sowing_time)+
  theme_bw(base_size = 16) +
  theme (plot.title = element_text ( size = 26), #hjust = 0.5,
         strip.text = element_text (size = 20),
         axis.title.y = element_text (size=14), 
         axis.title.x = element_text (size=14), 
         legend.title = element_text(size = 14),
         legend.text = element_text (size =12))+
  labs (title = "Final germination percentage x plot", y= "Mean Final Germination", x = "WP Treatment") 

# seed mass x plot graph
summary_seedmass%>%
  rename (Subpopulation=ID) %>%
  ggplot(aes(x= Subpopulation, y =mean, ymin = min, ymax = max, color = Subpopulation))+
  geom_point( size = 3) +
  geom_errorbar (width = 0.2, size =1.2)+
  theme_bw(base_size = 14) +
  theme (plot.title = element_text ( size = 26), #hjust = 0.5,
         strip.text = element_text (size = 20),
         axis.title.y = element_text (size=14), 
         axis.title.x = element_text (size=14),
         axis.text.x = element_blank(),
         legend.title = element_text(size = 14),
         legend.text = element_text (size =12))+
  #scale_color_manual (name= "Subpopulation", values = c("forestgreen", "gold") ) +
  labs (title = "Mean seed mass per plot", y= "Seed mass (mg)", x = "Subpopulation ID") 

# seed mass x GDD graph
summary_seedmass %>%
  merge(bioclim)%>%
  ggplot(aes(x=GDD, y=mean, ymin = min, ymax = max)) +
  geom_point(size = 3)+
  geom_errorbar (width = 0.2, size =1.2)+
  geom_smooth() +
  theme_bw(base_size = 14) +
  theme (plot.title = element_text ( size = 26), #hjust = 0.5,
         strip.text = element_text (size = 20),
         axis.title.y = element_text (size=14), 
         axis.title.x = element_text (size=14),
         axis.text.x = element_text (size=14),
         legend.title = element_text(size = 14),
         legend.text = element_text (size =12))+
  labs (title = "Seed mass correlation with GDD", y= "Seed mass (mg)", x = "Growing degree days")

# seed mass x base water potential population both storage treatments
summary_seedmass %>%
  merge(bWP_summary)%>%
  merge(read.csv("data/Dianthus_header.csv", sep = ";"), by= c("ID"))%>%
  filter(Immediate == "Yes")%>%
  filter(After_ripening == "Yes") %>%
  mutate(sowing_time = factor(sowing_time))%>%
  mutate(sowing_time = fct_relevel(sowing_time, "Immediate", "After_ripening" ))%>%
  mutate(sowing_time = recode (sowing_time, "Immediate" = "Fresh", "After_ripening" = "After ripened"))%>%
  ggplot(aes(x= mean, y=psib50, color = sowing_time ))+
  geom_point(size = 3)+
  geom_smooth(method = "lm") +
  #ylim(-0.5, 0.1) +
  #xlim(0.6, 1.7)+
  #facet_wrap(~sowing_time)+
  theme_bw(base_size = 14) +
  theme (plot.title = element_text ( size = 24), #hjust = 0.5,
         strip.text = element_text (size = 20),
         axis.title.y = element_text (size=14), 
         axis.title.x = element_text (size=14),
         legend.title = element_text(size = 14),
         legend.text = element_text (size =12))+
  scale_color_manual (name= "Storage treatment", values = c("forestgreen", "gold") ) +
  labs (title = "Base Water Potential vs Seed mass", x= "Seed mass (mg)",  y = expression(paste(Psi,"b (MPa)")))
