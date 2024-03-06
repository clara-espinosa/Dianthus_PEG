library(tidyverse);library(wesanderson); library (binom) ;library(ggpubr)
####### FIG 4A germination bars comparing final germination #####
# mean final germination x sowing time and WP treatment with binomial errors
data%>%
  mutate(WP_treatment = factor(WP_treatment))%>%
  mutate(WP_treatment = fct_relevel(WP_treatment,"0", "-0.2", "-0.4", "-0.6", "-0.8", "-1", "-1.2" ))%>%
  mutate(sowing_time = factor(sowing_time))%>%
  mutate(sowing_time = fct_relevel(sowing_time, "Immediate", "After_ripening" ))%>%
  mutate(sowing_time = recode (sowing_time, "Immediate" = "Fresh", "After_ripening" = "After ripened"))%>%
  select(sowing_time, ID, WP_treatment, petri,viable,  days, germinated)%>%
  group_by(sowing_time, WP_treatment, days)%>% #ID,
  summarise(viable = sum(viable), germinated = sum(germinated))%>%
  group_by(sowing_time, WP_treatment)%>%
  summarise(seeds_germ= sum(germinated),
         viable = last(viable))%>%
  mutate (binom.confint(seeds_germ, viable, methods = "wilson")) %>%
  ggplot( aes(WP_treatment, mean, fill=sowing_time))+
  geom_col(position = position_dodge(0.7), width = 0.75, color = "black") +
  ylim(0,1)+
  geom_errorbar(aes(WP_treatment, mean, ymin = lower, ymax = upper), 
                position=position_dodge(0.7), color = "black",width = 0.2, size =1) +
  scale_fill_manual (name= "Storage treatment", values = c("forestgreen", "gold") ) +
  ggthemes::theme_tufte(base_size = 16) + 
  theme (plot.margin = margin(5, 5, 5, 5, "pt"),
         text = element_text(family = "sans"),
         panel.background = element_rect(color = "black", fill = NULL),
         plot.title = element_text ( size = 18), #hjust = 0.5,
         axis.title.y = element_text (size=13), 
         axis.title.x = element_text (size=13), 
         legend.title = element_text(size = 13),
         legend.text = element_text (size =13),
         #legend.background = element_rect(fill="white",size=0.5, linetype="solid",colour ="black"),
         legend.position = c(0.82,0.82))+
  labs (tag = "A", title = "Final germination per storage treatment", y= "Final germination proportion", x = "WP treatments (MPa)") -> Fig4A;Fig4A

####### FIG 4B cumulative germination curve with ggplot ##############
# create/extent colorpalette (https://www.datanovia.com/en/blog/easy-way-to-expand-color-palettes-in-r/)
WPcolors <- wes_palette("Zissou1", 7, type = "continuous")

data %>%
  mutate(WP_treatment = factor(WP_treatment))%>%
  mutate(WP_treatment = fct_relevel(WP_treatment,"0", "-0.2", "-0.4", "-0.6", "-0.8", "-1", "-1.2" ))%>%
  mutate(sowing_time = factor(sowing_time))%>%
  mutate(sowing_time = fct_relevel(sowing_time, "Immediate", "After_ripening" ))%>%
  mutate(sowing_time = recode (sowing_time, "Immediate" = "Fresh", "After_ripening" = "After ripened"))%>%
  select(sowing_time, ID, WP_treatment, petri,viable,  days, germinated)%>%
  group_by(sowing_time, WP_treatment, days)%>% #ID,
  summarise(viable = sum(viable), germinated = sum(germinated))%>%
  mutate(cumsum= cumsum(germinated),
         viable = last(viable) )%>%
  mutate(germPER = (cumsum/viable))%>%
  ggplot(aes(x=days, y=germPER, group = WP_treatment, color= WP_treatment))+
  geom_line(size = 2) +
  facet_wrap(~sowing_time)+
  scale_color_manual(name = "WP treatments (MPa)", values = WPcolors, 
                     guide = guide_legend (title.position = "top",direction = "horizontal", nrow = 1)) +
  coord_cartesian(ylim = c(0,1))+
  theme_classic(base_size = 16) +
  theme (plot.margin = margin(5, 5, 5, 5, "pt"),
           text = element_text(family = "sans"),
         panel.background = element_rect(color = "black", fill = NULL),
         plot.title = element_text ( size = 18), #hjust = 0.5,
         strip.text = element_text (size = 16),
         axis.title.y = element_text (size=13), 
         axis.title.x = element_text (size=13), 
         legend.title = element_text(size =13),
         legend.text = element_text (size =13), 
         legend.margin=margin(0,0,0,0),
         legend.box.margin=margin(-10,0,0,0),
         legend.position = "bottom")+
  labs (tag = "B", title = "Cumulative germination curves", y= "Germination proportion", x = "Days")->Fig4B;Fig4B

ggarrange(Fig4A, Fig4B, heights = c(2,3), ncol = 1, nrow = 2)->Fig4;Fig4 
               
                  
ggsave(filename = "results/figures/Fig4.png", Fig2, path = NULL, 
       scale = 1, width = 600, height = 800, units = "mm", dpi = 600)