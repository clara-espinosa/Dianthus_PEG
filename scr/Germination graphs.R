library(tidyverse);library(wesanderson); library (binom) ;library(ggpubr)
####### FIG 2A germination bars comparing final germination #####
# mean final germination x sowing time and WP treatment with binomial errors
data%>%
  mutate(WP_treatment = factor(WP_treatment))%>%
  mutate(WP_treatment = fct_relevel(WP_treatment,"0", "-0.2", "-0.4", "-0.6", "-0.8", "-1", "-1.2" ))%>%
  mutate(sowing_time = factor(sowing_time))%>%
  mutate(sowing_time = fct_relevel(sowing_time, "Immediate", "After_ripening" ))%>%
  mutate(sowing_time = recode (sowing_time, "Immediate" = "Immediate", "After_ripening" = "After ripening"))%>%
  select(sowing_time, ID, WP_treatment, petri,viable,  days, germinated)%>%
  group_by(sowing_time, WP_treatment, days)%>% #ID,
  summarise(viable = sum(viable), germinated = sum(germinated))%>%
  group_by(sowing_time, WP_treatment)%>%
  summarise(seeds_germ= sum(germinated),
         viable = last(viable))%>%
  mutate (binom.confint(seeds_germ, viable, methods = "wilson")) %>%
  ggplot( aes(WP_treatment, mean, fill=sowing_time))+
  geom_col(position = position_dodge(0.7), width = 0.75, color = "black") +
  geom_errorbar(aes(WP_treatment, mean, ymin = lower, ymax = upper), 
                position=position_dodge(0.7), color = "black",width = 0.2, size =1) +
  scale_fill_manual (name= "Sowing time", values = c("forestgreen", "gold") ) +
  theme_classic(base_size = 16) +
  theme (plot.title = element_text ( size = 26), #hjust = 0.5,
         axis.title.y = element_text (size=14), 
         axis.title.x = element_text (size=14), 
         legend.title = element_text(size = 14),
         legend.text = element_text (size =12))+
  labs (title = "Final germination per sowing time", y= "Final germination proportion", x = "WP Treatments (MPa)") -> fig2A;fig2A

####### FIG 2B cumulative germination curve with ggplot ##############
# create/extent colorpalette (https://www.datanovia.com/en/blog/easy-way-to-expand-color-palettes-in-r/)
WPcolors <- wes_palette("Zissou1", 7, type = "continuous")

data %>%
  mutate(WP_treatment = factor(WP_treatment))%>%
  mutate(WP_treatment = fct_relevel(WP_treatment,"0", "-0.2", "-0.4", "-0.6", "-0.8", "-1", "-1.2" ))%>%
  mutate(sowing_time = factor(sowing_time))%>%
  mutate(sowing_time = fct_relevel(sowing_time, "Immediate", "After_ripening" ))%>%
  mutate(sowing_time = recode (sowing_time, "Immediate" = "Immediate", "After_ripening" = "After ripening"))%>%
  select(sowing_time, ID, WP_treatment, petri,viable,  days, germinated)%>%
  group_by(sowing_time, WP_treatment, days)%>% #ID,
  summarise(viable = sum(viable), germinated = sum(germinated))%>%
  mutate(cumsum= cumsum(germinated),
         viable = last(viable) )%>%
  mutate(germPER = (cumsum/viable))%>%
  ggplot(aes(x=days, y=germPER, group = WP_treatment, color= WP_treatment))+
  geom_line(size = 2) +
  facet_wrap(~sowing_time)+
  scale_color_manual(name = "WP Treatments\n (MPa)", values = WPcolors) +
  coord_cartesian(ylim = c(0,1))+
  theme_classic(base_size = 16) +
  theme (plot.title = element_text ( size = 26), #hjust = 0.5,
         strip.text = element_text (size = 20),
         panel.background = element_blank(), 
         axis.title.y = element_text (size=14), 
         axis.title.x = element_text (size=14), 
         legend.title = element_text(size = 14),
         legend.text = element_text (size =12))+
  labs (title = "Cumulative germination curves", y= "Germination proportion", x = "Days")->fig2B;fig2B 


Fig2 <- ggarrange(fig2A, fig2B, labels = c("A", "B"), ncol = 1, nrow = 2) 
Fig2                
                  
ggsave(filename = "results/figures/Fig2.png", Fig2, path = NULL, 
       scale = 1, width = 600, height = 800, units = "mm", dpi = 600)

#### extra ####

#final germination x treatment x ID graph
x11()
germination_summary %>%
  mutate(WP_treatment = factor(treatment))%>%
  mutate(WP_treatment = fct_relevel(WP_treatment,"0", "-0.2", "-0.4", "-0.6", "-0.8", "-1", "-1.2" ))%>%
  merge(bioclim, by = "ID") %>%
  merge(read.csv("data/Dianthus_header.csv", sep = ";"), by = c("ID", "site")) %>%
  ggplot() +
  geom_point(aes(x= treatment, y =germination.mean, color =WP_treatment), size = 3) +
  #geom_errorbar(aes(x= treatment, ymin = germination.lower, 
  #ymax = germination.upper, color = WP_treatment),width = 0.2, size =1.2)  +
  coord_cartesian(ylim = c(0,1))+
  facet_wrap(~sowing_time)+
  theme_bw(base_size = 16) +
  theme (plot.title = element_text ( size = 26), #hjust = 0.5,
         strip.text = element_text (size = 20),
         axis.title.y = element_text (size=14), 
         axis.title.x = element_text (size=14), 
         legend.title = element_text(size = 14),
         legend.text = element_text (size =12))+
  labs (title = "Final germination percentage x plot", y= "Mean Final Germination", x = "WP Treatment") 
