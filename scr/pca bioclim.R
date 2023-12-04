library(tidyverse);library(zoo);library(dplyr);library (lubridate)
library(ggrepel)

##### join ibuttons with DIANTHUS bioclim data ibuttons + microlog####
dianthus_bioclim_microlog %>%
  rename(site=Site)%>%
  rbind(dianthus_bioclim_ibuttons) %>%
  as.data.frame()-> bioclim 
str(bioclim)
bioclim %>%
  mutate(across(c(ID, site), as.factor))%>%
  select(site, ID, bio1:GDD)%>%
  mutate(site = fct_recode(site, "Penouta" = "Penauta")) ->bioclim

### PCA

bioclim[, 3:8] %>%
  FactoMineR::PCA() -> pca1

cbind((bioclim %>%  dplyr::select(site ,ID)), data.frame(pca1$ind$coord[, 1:2])) %>%
   mutate(site = fct_relevel(site,
                            "Rabinalto", "Cañada",
                            "Solana", "Penouta"))-> pcaInds

pca1$var$coord[, 1:2] %>%
  data.frame %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Variable = fct_recode(Variable, "Snow" = "Snw"))-> pcaVars

### Plot PCA

ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  coord_fixed() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_segment(data = pcaVars, aes(x = 0, y = 0, xend = 3*Dim.1, yend = 3*Dim.2)) +
  geom_point(aes(fill = site, color= site), size = 4) +
  geom_label(data = pcaVars, aes(x = 3*Dim.1, y = 3*Dim.2, label = Variable),  show.legend = FALSE, size = 4) +
  #geom_label_repel (data =pcaInds, aes(x=Dim.1, y = Dim.2, label = ID ), show.legend = FALSE, size = 4)+
  ggthemes::theme_tufte() + 
  theme(text = element_text(family = "sans"),
        legend.position = "right", 
        legend.title = element_blank(),
        legend.text = element_text(size = 12, color = "black"),
        panel.background = element_rect(color = "black", fill = NULL),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12, color = "black"),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm")) +
  #guides(fill = guide_legend(override.aes = list(shape = 22))) +
  scale_x_continuous(name = paste("Axis 1 (", round(pca1$eig[1, 2], 0),
                                  "% variance explained)", sep = "")) + 
  scale_y_continuous(name = paste("Axis 2 (", round(pca1$eig[2, 2], 0), 
                                  "% variance explained)", sep = "")) +
  scale_color_manual(values = c("gold", "#B3EE3A",  "#40E0D0", "#551A8B")) +
  scale_fill_manual(values = c("gold", "#B3EE3A",  "#40E0D0", "#551A8B")) -> pca_bioclim;pca_bioclim

pca1$eig
pca1$var

### Save figure

ggsave(S2, file = "results/figures/S2- PCA of the bioclimatic indices filtered.png", 
       path = NULL, scale = 1, width = 182, height = 120, units = "mm", dpi = 600)
# ggsave(f1, file = "results/figures/pca-temperatures.tiff", device = grDevices::tiff, 
#        path = NULL, scale = 1, width = 182, height = 182, units = "mm", dpi = 600, compression = "lzw")

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

### Plot PCA

ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  coord_fixed() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_segment(data = pcaVars, aes(x = 0, y = 0, xend = 3*Dim.1, yend = 3*Dim.2)) +
  geom_point(aes(fill = site, color= site), size = 4) +
  geom_label(data = pcaVars, aes(x = 3*Dim.1, y = 3*Dim.2, label = Variable),  show.legend = FALSE, size = 4) +
  #geom_label_repel (data =pcaInds, aes(x=Dim.1, y = Dim.2, label = ID ), show.legend = FALSE, size = 4)+
  ggthemes::theme_tufte() + 
  theme(text = element_text(family = "sans"),
        plot.title = element_text ( size = 18), #hjust = 0.5,
        legend.title = element_text(size =10),
        legend.position = "bottom", 
        legend.text = element_text(size = 11, color = "black"),
        panel.background = element_rect(color = "black", fill = NULL),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 11, color = "black")) +
  scale_x_continuous(name = paste("Axis 1 (", round(pca1$eig[1, 2], 0),
                                  "% variance explained)", sep = "")) + 
  scale_y_continuous(name = paste("Axis 2 (", round(pca1$eig[2, 2], 0), 
                                  "% variance explained)", sep = "")) +
  scale_color_manual(name= "",values = c("green3", "#551A8B","orange",   "deepskyblue3")) +
  scale_fill_manual(name= "", values = c("green3", "#551A8B","orange",   "deepskyblue3"))+
  labs(title = "PCA of bioclimatic indices") -> Fig2B; Fig2B


pca1$eig
pca1$var




