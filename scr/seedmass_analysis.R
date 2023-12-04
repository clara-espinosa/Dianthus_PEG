library(tidyverse);library (seedr)
library (stringr);library (rstatix)
library(glmmTMB); library (DHARMa)


##### seed mass subpopulations variability #####
read.csv("data/ind_seeds_weight.csv", sep = ",") ->seed_mass
str(seed_mass)
seed_mass%>%
  mutate(ID= as.factor(ID))%>%
  group_by(ID)%>%
  get_summary_stats(weight)%>%
  as.data.frame()-> summary_seedmass
str(summary_seedmass)

write.csv(summary_seedmass, "results/summary_seed_mass.csv")

# glmm
str(bWP_summary)
hist(summary_seedmass$mean)
seed_mass %>%
  merge(bioclim, by= c("ID")) %>%
  merge(bWP_summary, by= c("ID"))%>%
  as.data.frame()-> glm
a <- glmmTMB(abs(psib50) ~ weight * sowing_time + (1|site),  family = Gamma(link="log"),  data= glm) #Gamma(link="log")// gaussian
summary(a)# not significant interaction term , only sowing time (i.e. immediate sowing has higher bWP)   
# same results without C00
residuals <- simulateResiduals (a) ; plot(residuals)#gaussian family DO not mets assumptions

seed_mass %>%
  merge(bioclim, by= c("ID")) %>%
  merge(bWP_summary, by= c("ID"))%>%
  filter(sowing_time=="After_ripening")%>%
  as.data.frame()-> glm

a <- glmmTMB(abs(psib50) ~ weight + (1|site), family = Gamma(link="log"),  data= glm) #gaussian
summary(a) # marginal significant effrects of seed weight
residuals <- simulateResiduals (a) ; plot(residuals)#gaussian family DO not mets assumptions

# seed mass x plot graph
ggplot(summary_seedmass, aes(x= ID, y =mean, ymin = min, ymax = max, color = ID))+
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
  labs (title = "Mean seed mass per plot", y= "Seed mass (mg)", x = "Plot ID") 
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
         axis.text.x = element_blank(),
         legend.title = element_text(size = 14),
         legend.text = element_text (size =12))+
  labs (title = "Seed mass correlation with GDD", y= "Seed mass (mg)", x = "GDD")

# seed mass x Base water potential
summary_seedmass %>%
  merge(bWP_summary)%>%
  merge(read.csv("data/Dianthus_header.csv", sep = ";"), by= c("ID")) %>%
  mutate(site= as.factor(site))%>%
  mutate(sowing_time = factor(sowing_time))%>%
  mutate(sowing_time = fct_relevel(sowing_time, "Immediate", "After_ripening" ))%>%
  mutate(sowing_time = recode (sowing_time, "Immediate" = "Immediate", "After_ripening" = "After ripening"))%>%
  mutate(site = fct_relevel(site,
                            "Rabinalto", "Cañada",
                            "Solana", "Penouta")) %>%
  ggplot()+
  geom_point(aes(x= mean, y=psib50,color = site ), size = 3)+
  geom_smooth(aes(x= mean, y=psib50), method = "lm") +
  #ylim(-0.5, 0.1) +
  #xlim(0.6, 1.7)+
  facet_wrap(~sowing_time)+
  scale_color_manual(name= "",values = c("green3", "#551A8B","orange",   "deepskyblue3")) +
  scale_fill_manual(name= "", values = c("green3", "#551A8B","orange",   "deepskyblue3"))+
  ggthemes::theme_tufte() + 
  theme (text = element_text(family = "sans"),
         panel.background = element_rect(color = "black", fill = NULL), #hjust = 0.5,
         plot.title = element_text ( size = 20), #
         strip.text = element_text (size = 18),
         strip.background =element_rect(fill="white"),
         axis.title.y = element_text (size=14), 
         axis.title.x = element_text (size=14), 
         legend.title = element_text(size = 14),
         legend.text = element_text (size =12))+
  labs (title = "Base Water Potential vs Seed mass", x= "Seed mass (mg)", y = "Base Water Potential (Mpa)")

summary_seedmass %>%
  merge(bWP_summary)%>%
  merge(read.csv("data/Dianthus_header.csv", sep = ";"), by= c("ID"))%>%
  filter(Immediate == "Yes")%>%
  filter(After_ripening == "Yes") %>%
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
  labs (title = "Base Water Potential vs Seed mass", x= "Seed mass (mg)", y = "Base Water Potential (Mpa)")
