library(tidyverse);library(MCMCglmm)
library(glmmTMB); library (DHARMa) # paquetes para los GLMs
library(binom);library(effects);library(emmeans)

#### GLM germination analysis ####
# glm para final germination necesitamos dataframe con semillas germinadas y viables
# podríamos usar los datos brutos y transformarlos o directamente usar el objeto germ_indices
unique(data$ID)
unique(final_germ$ID)
data%>%
  group_by(ID, sowing_time, WP_treatment, petri)%>%
  summarise(germinated = sum(germinated), viable = sum(viable)) %>%
  merge(read.csv("data/Dianthus_header.csv", sep = ";"), by= c("ID"))%>%  #
  #merge(bioclim, by= c("ID")) %>% # C00 not bioclim data so it dissapears
  #mutate(WP_treatment = as.factor(WP_treatment)) %>%
  as.data.frame()->final_germ 
str(final_germ)

a <- glmmTMB(cbind(germinated, viable - germinated) ~ WP_treatment* sowing_time + (1|site/ID),  family = binomial, data= final_germ) 
summary(a) # every thing significant including interaction (check sowing time separately)
#https://cran.r-project.org/web/packages/emmeans/vignettes/AQuickStart.html
EMM<-emmeans (a, ~ WP_treatment * sowing_time)
### Simple pairwise comparisons...
pairs(EMM, simple = "WP_treatment")    # compare WP_treatment for each sowing time
pairs(EMM, simple = "sowing_time")     # compare sowing time for each treatment

final_germ %>%
  filter(sowing_time == "Immediate")%>%
  merge(bioclim, by= c("ID", "site"))-> final_germ_im
unique(final_germ_im$ID)
str(final_germ_im)
a <- glmmTMB(cbind(germinated, viable - germinated) ~ WP_treatment*GDD + (1|site/ID),  family = binomial, data= final_germ_im) 
summary(a) # only WP_treatment significant, no differences according to GDD

final_germ %>%
  filter(sowing_time == "After_ripening")%>%
  merge(bioclim, by= c("ID", "site"))-> final_germ_af
unique(final_germ_af$ID)
str(final_germ_af)
a <- glmmTMB(cbind(germinated, viable - germinated) ~ WP_treatment*GDD + (1|ID),  family = binomial, data= final_germ_af) 
summary(a) # only WP_treatment significant, no differences according to GDD

# glm para mean germination rate mgr (parecido al base water potential)

##### bWP glm analysis ####
# para bwp datos usaremos el summary creado por seedr donde tenemos calculado
# por cada sowing_time y ID el bWP = "psib50"
# quizás deberíamos intentar calcular el bWP por cada petri dentro de cada ID 
# probar glm y preguntar a eduardo
# mirar la distribución de los datos para saber si usar gaussian, gamma u otra distribución
# sigo el scrip de Adri con glmmTMB
str(bWP_summary)
hist(bWP_summary$psib50)
bWP_summary %>%
  merge(bioclim, by= c("ID")) %>%
  merge(summary_seedmass, by= c("ID"))%>%
  #filter(!ID == "C00")%>%
  as.data.frame()-> glm
a <- glmmTMB(psib50 ~ GDD * sowing_time , family = gaussian,  data= glm) #+ (1|site)
summary(a)# significant interaction term suggest analyzing both sowing times separately
          # same results without C00
residuals <- simulateResiduals (a) ; plot(residuals)#gaussian family mets assumptions

glm %>%
  filter(sowing_time == "Immediate")%>%
  as.data.frame()-> glm_im
b <- glmmTMB(psib50 ~ GDD + (1|site), family = gaussian,  data= glm_im) #
summary(b)# No significant effect of GDD 
          # same results withou C00
residuals <- simulateResiduals (b) ; plot(residuals)#gaussian family mets assumptions

glm %>%
  filter(sowing_time == "After_ripening")%>%
  as.data.frame()-> glm_af
c <- glmmTMB(psib50 ~ GDD + (1|site), family = gaussian,  data= glm_af) ###  with random effect doesn´t work probably singular fit!!!
summary(c)# significant effect of GDD
residuals <- simulateResiduals (c) ; plot(residuals)#gaussian family mets assumptions

