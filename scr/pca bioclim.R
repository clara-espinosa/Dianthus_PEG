library(tidyverse);library(zoo);library(dplyr)
library (lubridate);library(ggrepel)

###### PCA 80 IBUTTONS + microlog #############
dianthus_bioclim_microlog %>%
  rename(site=Site)%>%
  rbind(bioclim_ibuttons) %>%
  as.data.frame()-> bioclim_80 
str(bioclim)
bioclim_80  %>%
  mutate(across(c(ID, site), as.factor))%>%
  select(site, ID, bio1:GDD)%>%
  mutate(site = fct_recode(site, "Penouta" = "Penauta")) ->bioclim_80 

bioclim_80 [, 3:8] %>%cor()

bioclim_80 [, 3:8] %>%
  FactoMineR::PCA() -> pca1

cbind((bioclim_80  %>%  dplyr::select(site ,ID)), data.frame(pca1$ind$coord[, 1:2])) %>%
  mutate(site = fct_relevel(site,
                            "Rabinalto", "Cañada",
                            "Solana", "Penouta"))-> pcaInds

pca1$var$coord[, 1:2] %>%
  data.frame %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Variable = fct_recode(Variable, "Snow" = "Snw"))-> pcaVars

pca1$eig
pca1$var
pca1



