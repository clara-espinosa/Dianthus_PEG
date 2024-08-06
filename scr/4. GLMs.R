library(tidyverse);library(MCMCglmm)
library(glmmTMB); library (DHARMa) # paquetes para los GLMs
library(binom);library(effects)
library (stringr);library (rstatix)
library(emmeans) # v. 1.7.0
library(magrittr) # v. 2.0.1

# seed mass data ####
read.csv("data/ind_seeds_weight.csv", sep = ",") %>% # -> seed_mass
  mutate(ID= as.factor(ID))%>%
  group_by(ID)%>%
  get_summary_stats(weight)%>%
  as.data.frame()-> summary_seedmass
str(summary_seedmass)

write.csv(summary_seedmass, "results/summary_seed_mass.csv")
t.test(weight~storage_treatment, data = seed_mass) # no significant differences between storage treatments

#### GLM germination analysis ####
# all subpopulations ####
# glm para final germination necesitamos dataframe con semillas germinadas y viables
# podríamos usar los datos brutos y transformarlos o directamente usar el objeto germ_indices
unique(data$ID)
unique(final_germ$ID)
data%>%
  group_by(ID, storage_treatment, WP_treatment, petri)%>%
  summarise(germinated = sum(germinated), viable = min (viable)) %>%
  merge(read.csv("data/Dianthus_header.csv", sep = ","), by= c("ID"))%>%  #
  merge(summary_seedmass,by= c("ID") ) %>% 
  dplyr::select(ID, storage_treatment, WP_treatment, petri, germinated, viable, Site, mean)%>% 
  as.data.frame()->final_germ 

str(final_germ)
final_germ%>%
  summarise(viable = sum(viable))

a <- glmmTMB(cbind(germinated, viable - germinated) ~ WP_treatment* storage_treatment + (1|Site/ID),  family = binomial, data= final_germ) 
summary(a) # every thing significant including interaction (check sowing time separately)

#https://cran.r-project.org/web/packages/emmeans/vignettes/AQuickStart.html
# WP_treatment as numeric doesn't work well in emmeans
EMM<-emmeans (a, ~ WP_treatment * storage_treatment)
### Simple pairwise comparisons...
pairs(EMM, simple = "WP_treatment")    # compare WP_treatment for each sowing time 
pairs(EMM, simple = "storage_treatment")     # compare sowing time for each treatment

# subpopulations with both storage treatments ####
final_germ %>%
  filter(ID %in% c("A00", "B03", "C00", "C19", "D00", "D19"))%>%
  as.data.frame()->final_germ_pop_both 

a <- glmmTMB(cbind(germinated, viable - germinated) ~ WP_treatment* storage_treatment + (1|Site/ID),  family = binomial, data= final_germ_pop_both ) 
summary(a) # every thing significant including interaction (check sowing time separately)
#https://cran.r-project.org/web/packages/emmeans/vignettes/AQuickStart.html
EMM<-emmeans (a, ~ WP_treatment * storage_treatment)
### Simple pairwise comparisons...
pairs(EMM, simple = "WP_treatment")    # compare WP_treatment for each sowing time
pairs(EMM, simple = "storage_treatment")     # compare sowing time for each treatment 


##### bWP glm analysis ####
str(bWP_summary) # summary object from seedr_analisis script
# psib50 corresponds with base water potential of the 50th percentil
# all subpopulations ####
bWP_summary %>%
  merge(bioclim, by= c("ID")) %>% # merge data from ibuttons and microlog sensors from climate handling script
  merge(summary_seedmass, by= c("ID"))%>% # merge seed mass mean values x populations
  mutate(psib50 = ifelse( psib50 > 0, 0, psib50))%>% # remove those base water potential that were positive
  as.data.frame()-> glm
hist(glm$psib50) # check data distribution
unique(glm$Site)

a <- glmmTMB(psib50 ~ GDD * storage_treatment+ (1|Site) , family = gaussian,  data= glm) #+ (1|site)
summary(a)# significant interaction term suggest analyzing both sowing times separately
          # same results without B00
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

residuals <- simulateResiduals (a) ; plot(residuals)#gaussian family do not mets assumptions
# PROBLEM to use gamma family, needed value different than 0, if we change that manually nothing becomes significant

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

#### nlme package and EMMEANS posthoc NOT WORKING WITH CONTINUOS VARIABLE (GDD) #####
library(nlme)
d <- lme(psib50 ~ GDD * storage_treatment, random = ~1|Site ,  data= glm) #
summary(d)
anova(d)

# assumptions lme
#Existence of variance: Do not need to check, in practice, it is always true.
#Linearity: 
ggplot(glm, aes(y=psib50, x= GDD))+
  geom_point()+
  geom_smooth(aes(y=psib50, x= GDD),method ="lm")+
  facet_grid(~storage_treatment)
#Homogeneity: Need to Check by plotting residuals vs predicted values.
plot(residuals(d), predict(d))
#Normality of error term: need to check by histogram, QQplot of residuals, even Kolmogorov-Smirnov test.
qqnorm(residuals(d))
qqline(residuals(d))
#Normality of random effect: Get the estimate of random effect (in your case random intercepts), and check them as check the residual. But it is not efficient because you just have 7 random intercepts.
#https://cran.r-project.org/web/packages/emmeans/vignettes/AQuickStart.html
EMM<-emmeans (d, ~ GDD * storage_treatment)
### Simple pairwise comparisons...
pairs(EMM, simple = "GDD")    # compare GDD for each storage treatment
pairs(EMM, simple = "storage_treatment")     # compare storage treatment for each GDD

test(pairs(EMM, by="GDD"), by = NULL, adjust = "mvt")
# https://aosmith.rbind.io/2019/03/25/getting-started-with-emmeans/
EMM2<-emmeans (d, specs = pairwise ~ GDD:storage_treatment, type = "response")
EMM2$emmeans

# checking from stackoverflow https://stackoverflow.com/questions/52381434/emmeans-continuous-independant-variable
ref_grid(d)
summary(.Last.value)
emmeans (d, "storage_treatment")
emmeans (d, "GDD")

emmip(d, storage_treatment ~ GDD, cov.reduce = range)
##### seed mass subpopulations as a covariate of GLMM models BWP #####
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

# 1 check if seed mass differ between  populations storage treatments 
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

# 2 check if seed mass differ across GDD
a <- lm(weight ~ GDD ,  data= glm) 
summary(a)# significant relationship with GDD 

glm%>%
  ggplot(aes(y=weight, x= GDD))+
  geom_point()+
  geom_smooth()

# 3 check if base water potential differ according only to weight
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

# 4 check if base water potential differ according to weight and storage treatment
a <- glmmTMB(abs(psib50) ~ weight * storage_treatment + (1|Site),  family = Gamma(link="log"),  data= glm) #Gamma(link="log")// gaussian
summary(a)# not significant effect of seed mass (weight) and neither a significant interaction term 
residuals <- simulateResiduals (a) ; plot(residuals)#gaussian family DO not mets assumptions

glm%>%
  mutate(storage_treatment = recode (storage_treatment, "Fresh_seeds" = "Fresh", "After_ripened" = "After ripened"))%>%
  mutate(storage_treatment = fct_relevel(storage_treatment, "Fresh", "After ripened" ))%>%
  ggplot()+
  geom_point(aes(y=psib50, x= weight, fill = storage_treatment), color = "black", shape = 21, size = 3)+
  geom_smooth (aes(y=psib50, x= weight),method = "lm", color ="black")+
  facet_wrap (~storage_treatment)+
  scale_fill_manual (name= "Storage treatment", values = c("forestgreen", "gold") ) +
  labs (x= "Seed mass (mg)", y = expression(paste(Psi,"b (MPa)")))+
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

# 5 check if base water potential differ according to weight and GDD 
# problem 1 bWP and 1 GDD but 10 seeds weights. some warnings appear
str(bWP_summary)
hist(summary_seedmass$mean)
seed_mass %>%
  merge(bioclim, by= c("ID")) %>%
  merge(bWP_summary, by= c("ID", "storage_treatment"))%>%
  #mutate(psib50 = ifelse( psib50 > 0, 0.0000000001, psib50))%>% # problem with 0s in glmmtmb with gamma family
  as.data.frame()-> glm
a <- glmmTMB(abs(psib50) ~ weight * GDD + (1|Site),  family = Gamma(link="log"),  data= glm) #Gamma(link="log")// gaussian
summary(a)# not significant seed mass effect on base water potential and GDD
residuals <- simulateResiduals (a) ; plot(residuals)#gaussian family DO not mets assumptions

glm%>%
  mutate(storage_treatment = recode (storage_treatment, "Fresh_seeds" = "Fresh", "After_ripened" = "After ripened"))%>%
  mutate(storage_treatment = fct_relevel(storage_treatment, "Fresh", "After ripened" ))%>%
  ggplot()+
  geom_point(aes(y=psib50, x= GDD, fill = storage_treatment), color = "black", shape = 21, size = 3)+
  geom_smooth (aes(y=psib50, x= GDD),method = "lm", color ="black")+
  facet_wrap (~storage_treatment)+
  scale_fill_manual (name= "Storage treatment", values = c("forestgreen", "gold") ) +
  labs (x= "Seed mass (mg)", y = expression(paste(Psi,"b (MPa)")))+
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
# 6 check if base water potential differ according to weight and GDD and storage treatment
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

glm%>%
  mutate(storage_treatment = recode (storage_treatment, "Fresh_seeds" = "Fresh", "After_ripened" = "After ripened"))%>%
  mutate(storage_treatment = fct_relevel(storage_treatment, "Fresh", "After ripened" ))%>%
  ggplot()+
  geom_point(aes(y=psib50, x= GDD, fill = storage_treatment), color = "black", shape = 21, size = 3)+
  geom_smooth (aes(y=psib50, x= GDD),method = "lm", color ="black")+
  facet_wrap (~storage_treatment)+
  scale_fill_manual (name= "Storage treatment", values = c("forestgreen", "gold") ) +
  labs (x= "Seed mass (mg)", y = expression(paste(Psi,"b (MPa)")))+
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

#### seed mass subpopulations as a covariate of GLMMs models germinatin ####
final_germ 
a <- glmmTMB(cbind(germinated, viable - germinated) ~ WP_treatment* storage_treatment + (1|Site/ID),  family = binomial, data= final_germ) 
summary(a) # every thing significant including interaction (check sowing time separately)

# 1 check if germination differ according only to weight
a <- glmmTMB(cbind(germinated, viable - germinated) ~ mean + (1|Site/ID),  family = binomial, data= final_germ) 
summary(a)# not significant seed mass effect on germination
residuals <- simulateResiduals (a) ; plot(residuals)

final_germ%>%
  mutate(germpro=germinated/viable)%>%
  mutate(WP_treatment= as.factor(WP_treatment))%>%
  ggplot(aes(y=germpro, x= mean))+
  geom_point()+
  geom_smooth(method = "lm")

# 2 check if germination differs according to weight and storage treatment
a <- glmmTMB(cbind(germinated, viable - germinated) ~ mean*storage_treatment + (1|Site/ID),  family = binomial, data= final_germ) 
summary(a)# not significant seed mass effect on germination and not interacting, only fresh seeds = lower germination
residuals <- simulateResiduals (a) ; plot(residuals)

final_germ%>%
  mutate(germpro=germinated/viable)%>%
  mutate(WP_treatment= as.factor(WP_treatment))%>%
  mutate(storage_treatment = recode (storage_treatment, "Fresh_seeds" = "Fresh", "After_ripened" = "After ripened"))%>%
  mutate(storage_treatment = fct_relevel(storage_treatment, "Fresh", "After ripened" ))%>%
  ggplot(aes(y=germpro, x= mean))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap (~storage_treatment)+
  scale_fill_manual (name= "Storage treatment", values = c("forestgreen", "gold") )+
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

# 3 check if germination differs according to weight, storage treatment and WP
a <- glmmTMB(cbind(germinated, viable - germinated) ~ mean*storage_treatment*WP_treatment + (1|Site/ID),  family = binomial, data= final_germ) 
summary(a)# significant effect of seed mass, complex interactions check with Eduardo
residuals <- simulateResiduals (a) ; plot(residuals)

final_germ%>%
  mutate(germpro=germinated/viable)%>%
  mutate(WP_treatment= as.factor(WP_treatment))%>%
  mutate(storage_treatment = recode (storage_treatment, "Fresh_seeds" = "Fresh", "After_ripened" = "After ripened"))%>%
  mutate(storage_treatment = fct_relevel(storage_treatment, "Fresh", "After ripened" ))%>%
  ggplot(aes(y=germpro, x= mean, color = WP_treatment))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap (~storage_treatment)+
  scale_fill_manual (name= "Storage treatment", values = c("forestgreen", "gold") )+
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
         legend.position = "right")

