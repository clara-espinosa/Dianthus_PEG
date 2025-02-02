library(tidyverse);library(wesanderson); library (binom) ;library(ggpubr)
# supplementary fig 1
# combine individual subpopulations cumulative germination plots into 1 big

# create/extent colorpalette (https://www.datanovia.com/en/blog/easy-way-to-expand-color-palettes-in-r/)
WPcolors <- wes_palette("Zissou1", 7, type = "continuous")
cust_label <-as_labeller(c("Fresh B00" ="B00", "Fresh A00" = "A00", "After ripened A00" ="A00", "After ripened A02"="A02", 
                "Fresh B07"="B07", "Fresh B03"="B03", "After ripened B03"="B03", "After ripened A11"="A11", 
                "Fresh B17"="B17", "Fresh C00"="C00", "After ripened C00"="C00", "After ripened B19"="B19", 
                "Fresh B20"="B20", "Fresh C19"="C19", "After ripened C19"="C19", "After ripened C18"="C18", 
                "Fresh C06"="C06", "Fresh D00"="D00", "After ripened D00"="D00", "After ripened C20"="C20", 
                "Fresh D12"="D12", "Fresh D19"="D19", "After ripened D19"="D19", "After ripened D11"="D11"))
x11()
data %>%
  mutate(WP_treatment = factor(WP_treatment))%>%
  mutate(WP_treatment = fct_relevel(WP_treatment,"0", "-0.2", "-0.4", "-0.6", "-0.8", "-1", "-1.2" ))%>%
  mutate(storage_treatment = factor(storage_treatment))%>%
  mutate(storage_treatment = fct_relevel(storage_treatment, "Fresh_seeds", "After_ripened" ))%>%
  mutate(storage_treatment = recode (storage_treatment, "Fresh_seeds" = "Fresh", "After_ripened" = "After ripened"))%>%
  select(storage_treatment, ID, WP_treatment, petri,viable,  days, germinated)%>%
  group_by(storage_treatment, WP_treatment, ID, petri, days)%>% #ID,
  summarise(viable = sum(viable), germinated = sum(germinated))%>%
  group_by(storage_treatment, WP_treatment, ID, petri)%>%
  mutate(cumsum= cumsum(germinated),
         viable = last(viable) )%>%
  mutate(germPER = (cumsum/viable))%>%
  group_by(storage_treatment, WP_treatment, ID, days)%>%
  summarise(germPER_mean = mean(germPER), min= min (germPER), max = max(germPER))%>%
  mutate(Id_storage = paste(storage_treatment, ID))%>%
  mutate(Id_storage = factor(Id_storage))%>%
  mutate(Id_storage = fct_relevel(Id_storage, "Fresh B00","Fresh A00","After ripened A00", "After ripened A02",
                                  "Fresh B07","Fresh B03","After ripened B03", "After ripened A11",
                                  "Fresh B17","Fresh C00","After ripened C00", "After ripened B19",
                                  "Fresh B20","Fresh C19","After ripened C19", "After ripened C18",
                                  "Fresh C06","Fresh D00","After ripened D00", "After ripened C20",
                                  "Fresh D12","Fresh D19","After ripened D19", "After ripened D11"))%>%
  ggplot()+
  geom_line(aes(x=days, y=germPER_mean, group = WP_treatment, color= WP_treatment),size = 1.2) +
  #geom_line (aes (x=days, y=min, colour = WP_treatment), linewidth =1.25) + #order for sort as experiment
  #geom_line (aes (x=days, y=max, colour = WP_treatment), linewidth =1.25) +
  geom_ribbon (aes (x=days, ymin =min, ymax=max, colour = WP_treatment, fill = WP_treatment), alpha =0.3) +
  #scale_color_manual (name = "Microsites", values = c ("Fellfield site"= "chocolate2" , "Snowbed site" = "deepskyblue3")) +
  #scale_fill_manual (name = "Microsites", values = c ("Fellfield site"= "chocolate2" , "Snowbed site" = "deepskyblue3")) +
  facet_wrap(~Id_storage, ncol = 4, labeller = cust_label)+
  scale_color_manual(name = "WP treatments (MPa)", values = WPcolors, 
                     guide = guide_legend (direction = "horizontal", nrow = 1)) +#title.position = "top",
  scale_fill_manual(name = "WP treatments (MPa)", values = WPcolors, 
                     guide = guide_legend (direction = "horizontal", nrow = 1)) +
  coord_cartesian(ylim = c(0,1))+
  theme_classic(base_size = 12) +
  theme (plot.margin = margin(0, 10, 10, 10, "pt"),
         text = element_text(family = "sans"),
         #panel.background = element_rect(color = "black", fill = NULL),
         plot.title = element_text ( size = 18,), #hjust = 0.5
         plot.subtitle = element_text (size = 16),
         strip.text = element_text (size = 10, face = "bold"),
         strip.background = element_rect(fill="white", color= "white"),
         axis.title.y = element_text (size=12), 
         axis.title.x = element_text (size=12), 
         legend.title = element_text(size =12),
         legend.text = element_text (size =12), 
         #legend.margin=margin(0,0,0,0),
         legend.box.margin=margin(0,0,0,0),
         legend.position = "bottom")+
  labs (tag = "", title = "Subpopulations cumulative germination curves", 
        subtitle= "                  Fresh seeds                              After ripened seeds", 
        y= "Germination proportion", x = "Days") -> FigS1;FigS1
library(cowplot)
ggdraw()+
  draw_label(size= 8,label ="Functional intraspecific variation in the base water potential for seed germination along soil microclimatic gradients. 
             Espinosa del Alba, C., Cruz-Tejada, D., Jiménez-Alfaro, B., and E. Fernández-Pascual. (2025). Functional Ecology.")->label

plot_grid(FigS1,label,  ncol = 1, align = "v", rel_heights = c(12,1))->FigS1;FigS1

ggsave(filename = "FEEspinosadelAlbaSF1.png", plot =FigS1, path = "results/supplementary", 
       device = "png", dpi = 600)
