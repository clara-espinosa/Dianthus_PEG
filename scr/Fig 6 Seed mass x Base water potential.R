library(tidyverse);

# FIG 6 seed mass x Base water potential

regrpvalues2 <- data.frame (sowing_time= c("Fresh", "After ripened"), lbl = c("p = 0.53", "p = 0.058")) # from glms
regrpvalues2 %>%
  mutate(sowing_time = factor(sowing_time))%>%
  mutate(sowing_time = fct_relevel(sowing_time, "Fresh", "After ripened" ))-> regrpvalues2

summary_seedmass %>%
  merge(bWP_summary)%>%
  merge(read.csv("data/Dianthus_header.csv", sep = ";"), by= c("ID")) %>%
  mutate(site= as.factor(site))%>%
  mutate(sowing_time = factor(sowing_time))%>%
  mutate(sowing_time = fct_relevel(sowing_time, "Immediate", "After_ripening" ))%>%
  mutate(sowing_time = recode (sowing_time, "Immediate" = "Fresh", "After_ripening" = "After ripened"))%>%
  mutate(site = fct_relevel(site,
                            "Rabinalto", "Cañada",
                            "Solana", "Penouta")) %>%
  ggplot()+
  geom_point(aes(x= mean, y=psib50,color = site ), size = 3)+
  geom_smooth(aes(x= mean, y=psib50), method = "lm") +
  #ylim(-0.5, 0.1) +
  #xlim(0.6, 1.7)+
  facet_wrap(~sowing_time)+
  scale_color_manual(name= "Summit",values = c("green3", "#551A8B","orange",   "deepskyblue3")) +
  geom_text(data= regrpvalues2, aes(y= 0.1, x= 0.62,  label=lbl), size = 5)+
  #scale_fill_manual(name= "", values = c("green3", "#551A8B","orange",   "deepskyblue3"))+
  ggthemes::theme_tufte(base_size = 16) + 
  theme (text = element_text(family = "sans"),
         panel.background = element_rect(color = "black", fill = NULL), #hjust = 0.5,
         plot.title = element_text ( size = 20), #
         strip.text = element_text (size = 18),
         strip.background =element_rect(fill="white"),
         axis.title.y = element_text (size=14), 
         axis.title.x = element_text (size=14), 
         legend.title = element_text(size = 14),
         legend.text = element_text (size =14))+
  labs (title = "Base Water Potential vs Seed mass", x= "Seed mass (mg)", y = expression(paste(Psi,"b (MPa)"))) -> Fig6;Fig6
