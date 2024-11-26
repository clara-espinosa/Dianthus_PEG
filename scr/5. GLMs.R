library(tidyverse);library(MCMCglmm)
library(glmmTMB); library (DHARMa) 
library(binom);library(effects)
library (stringr);library (rstatix)

###################################### GLMM germination analysis #######################################
# all subpopulations ####
data%>% # from script 3 seedr analisis
  group_by(ID, storage_treatment, WP_treatment, petri)%>%
  summarise(germinated = sum(germinated), viable = min (viable)) %>%
  merge(read.csv("data/Dianthus_header.csv", sep = ","), by= c("ID"))%>%  #
  merge(summary_seedmass,by= c("ID") ) %>% 
  dplyr::select(ID, storage_treatment, WP_treatment, petri, germinated, viable, Site, mean)%>% 
  as.data.frame()->final_germ 

a <- glmmTMB(cbind(germinated, viable - germinated) ~ WP_treatment* storage_treatment + (1|Site/ID),  family = binomial, data= final_germ) 
summary(a) # every thing significant including interaction 

# subpopulations with both storage treatments ####
final_germ %>%
  filter(ID %in% c("A00", "B03", "C00", "C19", "D00", "D19"))%>%
  as.data.frame()->final_germ_pop_both 

a <- glmmTMB(cbind(germinated, viable - germinated) ~ WP_treatment* storage_treatment + (1|Site/ID),  family = binomial, data= final_germ_pop_both ) 
summary(a) # every thing significant including interaction 

###################################### bWP glm analysis ###############################################
# all subpopulations ####
bWP_summary %>%
  merge(bioclim, by= c("ID")) %>% # merge data from ibuttons and microlog sensors from climate handling script
  merge(summary_seedmass, by= c("ID"))%>% # merge seed mass mean values x populations
  mutate(psib50 = ifelse( psib50 > 0, 0, psib50))%>% # remove those base water potential that were positive
  as.data.frame()-> glm
hist(glm$psib50) # check data distribution

a <- glmmTMB(psib50 ~ GDD * storage_treatment+ (1|Site) , family = gaussian,  data= glm) 
summary(a)# significant interaction term suggest analyzing both sowing times separately
          # same results without B00
residuals <- simulateResiduals (a) ; plot(residuals)#gaussian family mets assumptions

# effect of storage treatment in base water potential
a <- glmmTMB(psib50 ~ storage_treatment+ (1|Site) , family = gaussian,  data= glm) 
summary(a)# significant effect of storage treatment
residuals <- simulateResiduals (a) ; plot(residuals)#gaussian family mets assumptions

glm %>%
  filter(storage_treatment == "Fresh_seeds")%>%
  as.data.frame()-> glm_im
b <- glmmTMB(psib50 ~ GDD + (1|Site), family = gaussian,  data= glm_im) #
summary(b)# No significant effect of GDD 
    
residuals <- simulateResiduals (b) ; plot(residuals)#gaussian family mets assumptions

glm %>%
  filter(storage_treatment == "After_ripened")%>%
  as.data.frame()-> glm_af
c <- glmmTMB(psib50 ~ GDD + (1|Site) , family = gaussian,  data= glm_af) ###  
summary(c)# significant effect of GDD
residuals <- simulateResiduals (c) ; plot(residuals)#gaussian family mets assumptions
# subpopulations with both storage treatments ####
glm %>%
  filter(ID %in% c("A00", "B03", "C00", "C19", "D00", "D19"))%>%
  as.data.frame()->glm_both 
hist(glm_both $psib50) # check data distribution NOT looking too NORMAL
unique(glm_both $Site)

a <- glmmTMB(psib50 ~ GDD * storage_treatment+ (1|Site) , family = gaussian,  data= glm_both ) #
summary(a)# significant interaction term suggest analyzing both sowing times separately

# effect of storage treatment in base water potential
a <- glmmTMB(psib50 ~ storage_treatment+ (1|Site) , family = gaussian,  data= glm_both) #+ (1|site)
summary(a)# significant effect of storage treatment
residuals <- simulateResiduals (a) ; plot(residuals)#gaussian family mets assumptions



glm_both %>%
  filter(storage_treatment == "Fresh_seeds")%>%
  as.data.frame()-> glm_im_both
b <- glmmTMB(psib50 ~ GDD + (1|Site), family = gaussian,  data= glm_im_both) #
summary(b)# No significant effect of GDD 

residuals <- simulateResiduals (b) ; plot(residuals)#gaussian family mets assumptions

glm_both  %>%
  filter(storage_treatment == "After_ripened")%>%
  as.data.frame()-> glm_af_both
c <- glmmTMB(psib50 ~ GDD + (1|Site) , family = gaussian,  data= glm_af_both) ###  
summary(c)# significant effect of GDD
residuals <- simulateResiduals (c) ; plot(residuals)#gaussian family mets assumptions

############################# seed mass subpopulations as a covariate of GLMM models BWP ##############
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
  merge(bWP_summary, by= c("ID", "storage_treatment"))%>%
  mutate(psib50 = ifelse( psib50 > 0, 0.0000000001, psib50))%>% # problem with 0s in glmmtmb with gamma family
  as.data.frame()-> glm
str(glm)
unique(glm$Site)
hist(glm$weight) # look normally distributed
hist(glm$GDD)

glm%>%
  get_summary_stats(weight)

# 1. check if seed mass differ between  populations storage treatments 
a <- glmmTMB(weight ~ storage_treatment + (1|Site),  family = gaussian,  data= glm) #Gamma(link="log")// gaussian
summary(a)# not significant differences between storage treatments   
residuals <- simulateResiduals (a) ; plot(residuals)#gaussian family DO not mets assumptions
glm%>%
  mutate(storage_treatment = recode (storage_treatment, "Fresh_seeds" = "Fresh", "After_ripened" = "After ripened"))%>%
  mutate(storage_treatment = fct_relevel(storage_treatment, "Fresh", "After ripened" ))%>%
  ggplot(aes(y=weight, x= storage_treatment, fill = storage_treatment))+
  geom_boxplot()+
  scale_fill_manual (name= "Storage treatment", values = c("forestgreen", "gold") ) +
  labs (y= "Seed mass (mg)", x = "Storage treatment")+
  ggthemes::theme_tufte(base_size = 16)+
  theme (text = element_text(family = "sans"),
         panel.background = element_rect(color = "black", fill = NULL), #hjust = 0.5,
         plot.title = element_text ( size = 18), #
         strip.text = element_text (size = 16),
         strip.background =element_rect(fill="white"),
         axis.title.y = element_text (size=14), 
         axis.title.x = element_text (size=14), 
         legend.title = element_text(size = 14),
         legend.text = element_text (size =14), 
         legend.position = "none")

# 2 check if base water potential differ according only to weight
hist(abs(glm$psib50))
glm%>%
  get_summary_stats(psib50)
a <- glmmTMB(abs(psib50) ~ weight + (1|Site),  family = Gamma(link="log"),  data= glm) #Gamma(link="log")// gaussian
summary(a)# not significant seed mass effect on base water potential
residuals <- simulateResiduals (a) ; plot(residuals)#gaussian family DO not mets assumptions
glm%>%
  ggplot(aes(y=psib50, x= weight))+
  geom_point()+
  geom_smooth(method = "lm")

# 3 check if seed mass differ across GDD
a <- lm(weight ~ GDD ,  data= glm) 
summary(a)# significant relationship with GDD 

glm%>%
  ggplot(aes(y=weight, x= GDD))+
  geom_point()+
  geom_smooth()

# 4 check if base water potential differ according to weight and GDD and storage treatment
# problem 1 bWP and 1 GDD but 10 seeds weights. some warnings appear
str(bWP_summary)
hist(summary_seedmass$mean)
seed_mass %>%
  merge(bioclim, by= c("ID")) %>%
  merge(bWP_summary, by= c("ID", "storage_treatment"))%>%
  #mutate(psib50 = ifelse( psib50 > 0, 0.0000000001, psib50))%>% # problem with 0s in glmmtmb with gamma family
  as.data.frame()-> glm
a <- glmmTMB(abs(psib50) ~ weight * GDD *storage_treatment + (1|Site),  family = Gamma(link="log"),  data= glm) #Gamma(link="log")// gaussian
summary(a)# not significant seed mass effect across both storage treatments
residuals <- simulateResiduals (a) ; plot(residuals)#gaussian family DO not mets assumptions
