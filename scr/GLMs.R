library(tidyverse);library(MCMCglmm)
library(lme4); library(glmmTMB); library (DHARMa) # paquetes para los GLMs
library(binom);library(effects)

#### GLM germination analysis ####
# glm para germination necesitamos dataframe con semillas germinadas y viables
# podríamos usar los datos brutos y transformarlos o directamente usar el objeto germ_indices
# donde ya tenemos calculado por cada sowing_time, ID y petri 
# el num de semillas germinadas al final = "grs"
# el num de semillas viables = "viable"
str(germ_indices)
glm(cbind(grs, viable - grs) ~ GDD * sowing_time,  family = binomial, data= germ_indices) 
# Error in family$linkfun(mustart) : Value 1.02083 out of range (0, 1)

MCMCglmm::MCMCglmm(cbind(seeds_germ, viable - seeds_germ) ~ incubator*community, # 
                   random = ~ animal + code:ID,
                   family = "multinomial2", pedigree = nnls_orig, prior = priors, data = df,
                   nitt = nite, thin = nthi, burnin = nbur,
                   verbose = FALSE, saveX = FALSE, saveZ = FALSE, saveXL = FALSE, pr = FALSE, pl = FALSE) -> m1

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
a <- glmmTMB(psib50 ~ GDD * sowing_time , family = gaussian,  data= glm) 
summary(a)# significant interaction term suggest analyzing both sowing times separatey
residuals <- simulateResiduals (a) ; plot(residuals)#gaussian family mets assumptions

glm %>%
  filter(sowing_time == "Immediate")%>%
  as.data.frame()-> glm_im
b <- glmmTMB(psib50 ~ GDD, family = gaussian,  data= glm_im) 
summary(b)# No significant effect of GDD
residuals <- simulateResiduals (b) ; plot(residuals)#gaussian family mets assumptions

glm %>%
  filter(sowing_time == "After_ripening")%>%
  as.data.frame()-> glm_af
c <- glmmTMB(psib50 ~ GDD, family = gaussian,  data= glm_af) 
summary(c)# significant effect of GDD
residuals <- simulateResiduals (c) ; plot(residuals)#gaussian family mets assumptions
