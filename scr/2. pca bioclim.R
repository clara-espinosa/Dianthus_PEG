library(tidyverse);library(zoo);library(dplyr)
library (lubridate);library(ggrepel)

###### PCA 80 IBUTTONS + microlog #############

# indices calculation (80 plots for PCA)
read.csv("data/temperature_ibuttons.csv", sep =";") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% 
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  merge(read.csv("data/ibuttons plots coordinates.csv", sep =",")) %>%
  group_by(Site,ID, Day = lubridate::floor_date(Time, "day")) %>% #
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time)) %>% # Daily mean, max, min
  mutate(Snow = ifelse(X < 0.5 & N > -0.5, 1, 0)) %>% # Day is snow day or not
  mutate(FreezeThaw = ifelse(X > 0.5 & N < -0.5, 1, 0)) %>% # Day with freeze-thaw cycles
  mutate(FDD = ifelse(T < 0, T, 0)) %>% # Freezing degrees per day
  mutate(GDD = ifelse(T >= 5, T, 0)) %>% # Growing degrees day per month https://link.springer.com/article/10.1007/s00035-021-00250-1
  group_by(Site, ID, Month = lubridate::floor_date(Day, "month")) %>% #
  summarise(T = mean(T), X = mean(X), N = mean(N), # Daily mean, max, min
            Snow = sum(Snow), # Snow days per month
            FreezeThaw = sum(FreezeThaw), # Freeze-thaw days per month
            FDD = sum(FDD), # FDD per month
            GDD = sum(GDD)) %>% # GDD per month
  group_by(Site, ID) %>% #
  summarise(bio1 = mean(T), # Annual Mean Temperature
            bio2 = mean(X - N), # Mean Diurnal Range (Mean of monthly (max temp - min temp))
            bio7 = max(X) - min(N), # Temperature Annual Range (BIO5-BIO6)
            Snw = sum(Snow),
            FDD = abs(sum(FDD)), # FDD per year
            GDD = sum(GDD)) -> # GDD per year
  bioclim_all_ibuttons 

bioclim_all_ibuttons %>% write.csv("results/bioclim_all_ibuttons.csv", row.names = FALSE)


dianthus_bioclim_microlog %>% # from climate data handling script
  rbind(bioclim_all_ibuttons) %>%
  as.data.frame()-> bioclim_pca 

bioclim_pca %>%
  mutate(across(c(ID, Site), as.factor))%>%
  dplyr::select(Site, ID, bio1:GDD) ->bioclim_pca 

bioclim_pca [, 3:8] %>%cor()

bioclim_pca  [, 3:8] %>%
  FactoMineR::PCA() -> pca1

cbind((bioclim_pca   %>%  dplyr::select(Site ,ID)), data.frame(pca1$ind$coord[, 1:2])) %>%
  mutate(Site = fct_relevel(Site,
                            "Rabinalto", "Canada",
                            "Solana", "Penouta"))-> pcaInds

pca1$var$coord[, 1:2] %>%
  data.frame %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Variable = fct_recode(Variable, "Snow" = "Snw"))-> pcaVars

pca1$eig
pca1$var
pca1
