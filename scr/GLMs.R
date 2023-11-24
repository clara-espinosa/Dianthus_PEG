library(tidyverse);library(MCMCglmm); library(blme)
library(lme4); library(glmmTMB); library (DHARMa) # paquetes para los GLMs
library(binom);library(effects)

#### GLM germination analysis ####
# glm para final germination necesitamos dataframe con semillas germinadas y viables
# podríamos usar los datos brutos y transformarlos o directamente usar el objeto germ_indices
# donde ya tenemos calculado por cada sowing_time, ID y petri 
# el num de semillas germinadas al final = "grs"
# el num de semillas viables = "viable"
str(germ_indices)
a <- glmmTMB(cbind(grs, viable - grs) ~ WP_treatment* sowing_time + (1|site/ID),  family = binomial, data= germ_indices) 
summary(a)# significant interaction term suggest analyzing both sowing times separatey

germ_indices %>%
  filter(sowing_time == "Immediate") -> germ_ind_im
a <- glmmTMB(cbind(grs, viable - grs) ~ WP_treatment* GDD+ (1|site/ID),  family = binomial, data= germ_ind_im) 
summary(a) # marginal WP_treatment significant

germ_indices %>%
  filter(sowing_time == "After_ripening") -> germ_ind_af
a <- glmmTMB(cbind(grs, viable - grs) ~ WP_treatment* GDD+ (1|site/ID),  family = binomial, data= germ_ind_af) 
summary(a) # WP_treatment significant and GDD marginal (no interaction)

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
  as.data.frame()-> glm
a <- glmmTMB(psib50 ~ GDD * sowing_time , family = gaussian,  data= glm) #+ (1|site)
summary(a)# significant interaction term suggest analyzing both sowing times separatey
residuals <- simulateResiduals (a) ; plot(residuals)#gaussian family mets assumptions

glm %>%
  filter(sowing_time == "Immediate")%>%
  as.data.frame()-> glm_im
b <- glmmTMB(psib50 ~ GDD + (1|site), family = gaussian,  data= glm_im) #
summary(b)# No significant effect of GDD
residuals <- simulateResiduals (b) ; plot(residuals)#gaussian family mets assumptions

glm %>%
  filter(sowing_time == "After_ripening")%>%
  as.data.frame()-> glm_af
c <- glmmTMB(psib50 ~ GDD + (1|site), family = gaussian,  data= glm_af) ###  with random effect doesn´t work probably singular fit!!!
summary(c)# significant effect of GDD
residuals <- simulateResiduals (c) ; plot(residuals)#gaussian family mets assumptions

