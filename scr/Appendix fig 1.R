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
  mutate(sowing_time = factor(sowing_time))%>%
  mutate(sowing_time = fct_relevel(sowing_time, "Immediate", "After_ripening" ))%>%
  mutate(sowing_time = recode (sowing_time, "Immediate" = "Fresh", "After_ripening" = "After ripened"))%>%
  select(sowing_time, ID, WP_treatment, petri,viable,  days, germinated)%>%
  group_by(sowing_time, WP_treatment, ID, days)%>% #ID,
  summarise(viable = sum(viable), germinated = sum(germinated))%>%
  mutate(cumsum= cumsum(germinated),
         viable = last(viable) )%>%
  mutate(germPER = (cumsum/viable))%>%
  mutate(Id_storage = paste(sowing_time, ID))%>%
  mutate(Id_storage = factor(Id_storage))%>%
  mutate(Id_storage = fct_relevel(Id_storage, "Fresh B00","Fresh A00","After ripened A00", "After ripened A02",
                                  "Fresh B07","Fresh B03","After ripened B03", "After ripened A11",
                                  "Fresh B17","Fresh C00","After ripened C00", "After ripened B19",
                                  "Fresh B20","Fresh C19","After ripened C19", "After ripened C18",
                                  "Fresh C06","Fresh D00","After ripened D00", "After ripened C20",
                                  "Fresh D12","Fresh D19","After ripened D19", "After ripened D11"))%>%
  ggplot(aes(x=days, y=germPER, group = WP_treatment, color= WP_treatment))+
  geom_line(size = 1.2) +
  facet_wrap(~Id_storage, ncol = 4, labeller = cust_label)+
  scale_color_manual(name = "WP treatments (MPa)", values = WPcolors, 
                     guide = guide_legend (direction = "horizontal", nrow = 1)) +#title.position = "top",
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
        subtitle= "                Fresh seeds                         After ripened seeds", 
        y= "Germination proportion", x = "Days") -> FigS1;FigS1
library(cowplot)
ggdraw(FigS1) +
  draw_line(x= c(0,10), y = c(10,1), lwd = 5)
