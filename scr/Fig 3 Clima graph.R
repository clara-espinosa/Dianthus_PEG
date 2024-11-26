library(tidyverse);library(zoo); library(dplyr);library (lubridate);library(ggpubr)
library(cowplot);library(patchwork); library(ggrepel)

### FIG 3A mean climograma villa from Penauta, Rabinalto and Solana central from 2021 to 2024####
read.csv("data/wp_villa_2020_2024.csv", sep = ",") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% #specify format of Time variable
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  mutate(Year = lubridate::year(Time)) %>% 
  mutate(Month = lubridate::month(Time)) %>% 
  mutate(Day = lubridate::day(Time)) %>% 
  mutate(Hour = lubridate::hour(Time)) %>% 
  dplyr::filter(Micro == "central") %>% # keep logger from center of the plot
  filter(!Site =="Canada") %>% # logger with broken sensors
  mutate (Site_year = paste (Site, Year)) %>%
  mutate(WP = rowMeans((cbind(wp1, wp2)))) %>% # calculate mean value between 3 gypsum sensors
  dplyr::select(! c(wp1, wp2)) %>% 
  group_by(Community, Site, Month, Day) %>% # calculate mean values x day
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time), 
            WPmean = mean(WP), WPmax = max(WP), WPmin = max (WP), abs_WP = sum(WP)) %>%
  mutate(GDD = ifelse(T >= 5, T, 0)) %>%
  group_by(Community, Site,  Month) %>% # calculate mean values x month 
  summarise(T = mean(T), X = max(X), N = min(N), abs_WP = sum(abs_WP), GDD = sum(GDD),n = length(n),
            WPmean = mean(WPmean), WPmax = mean(WPmax), WPmin = mean(WPmin)) -> villa_clima

x11()
villa_clima %>%
  group_by(Month)%>%
  summarise_at(vars(T:WPmin), mean, na.rm = TRUE)%>%
  ggplot() +
  geom_col(aes (x= Month, y=WPmax*2), colour = "black", fill = "azure4")+
  geom_line (aes (x=Month, y=N), colour = "darkred", linewidth =1.25) + 
  geom_line (aes (x=Month, y=X), colour = "darkred", linewidth =1.25) + 
  geom_ribbon (aes (x=Month, ymin =N, ymax=X), fill = "darkred", alpha =0.3) + 
  scale_x_continuous (limits = c(0.5,12.5), breaks = seq (1, 12, by= 1))+
  scale_y_continuous(limits = c(-5, 45), breaks = seq (-5, 45, by = 10),
                     sec.axis = sec_axis(transform = ~./20, name = expression(paste("Mean max ",Psi," (MPa)")))) + 
  geom_hline(yintercept=0, linetype ="dashed", linewidth =1, colour = "black")+
  ggthemes::theme_tufte(base_size = 12) + 
  theme (plot.margin = margin(0, 0, 0, 0, "pt"),
         text = element_text(family = "sans"),
         panel.background = element_rect(color = "black", fill = NULL),
         plot.title = element_text (size = 12, margin=margin(0,0,0,0)), #hjust = 0.5,
         axis.title.y = element_text (size=10), 
         axis.text = element_text(size = 8, color = "black"),
         legend.title = element_text(size =10),
         legend.text = element_text (size =8)) +
  labs (title = "A", y= "Temperature (ºC)", x = "Month")-> Fig3A;Fig3A 
##### FIG 3B Water potential x GDD #####
read.csv("data/wp_villa_2020_2024.csv", sep = ",") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% #specify format of Time variable
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  mutate(Year = lubridate::year(Time)) %>% 
  mutate(Month = lubridate::month(Time)) %>% 
  mutate(Day = lubridate::day(Time)) %>% 
  mutate(Hour = lubridate::hour(Time)) %>% 
  filter (! (Year %in% 2021 & Month < 7))%>% # remove missing june days of 2021
  mutate (Site_year = paste (Site, Year)) %>%
  mutate(WP = rowMeans((cbind(wp1, wp2)))) %>% 
  dplyr::select(! c(wp1, wp2)) %>% 
  group_by(Community, Site, Micro, Year, Month, Day) %>%
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time), 
            WPmean = mean(WP), WPmax = max(WP), WPmin = max (WP), abs_WP = sum(WP)) %>%
  mutate(GDD = ifelse(T >= 5, T, 0)) %>%
  group_by(Community, Site, Micro, Year, Month) %>%
  summarise(T = mean(T), X = max(X), N = min(N), abs_WP = sum(abs_WP), GDD = sum(GDD),n = length(n),
            WPmean = mean(WPmean), WPmax = mean(WPmax), WPmin = mean(WPmin))-> monthly_villa_clima  

monthly_villa_clima %>%
  filter(n >27)%>% # filter those months with less than 27 days of data
  filter (! Year == 2021)%>%
  filter (!(Year %in% 2022 & Month < 4))%>% # keep data only from growing season april-september
  filter (!(Year %in% 2022 & Month >9))%>%# keep data only from growing season april-september
  filter (!(Year %in% 2023 & Month < 4))%>%# keep data only from growing season april-september
  filter (!(Year %in% 2023 & Month >9))%>%# keep data only from growing season april-september
  mutate(abs_WP = abs_WP/10, # change units from bar to Mpa
         WPmean= WPmean/10, 
         WPmax= WPmax/10,
         WPmin= WPmin/10)%>%
  group_by(Year, Site, Micro)%>%
  summarise(abs_WP = sum(abs_WP),
            GDD = sum (GDD), 
            WPmean= sum(WPmean),
            n = length(n)) %>%
  filter(n >5)%>%
  as.data.frame() -> lm

summary(lm(abs_WP ~ GDD, data=lm)) # p= 0.009
summary(lm(abs_WP ~ GDD, data=lm))$adj.r.squared  # r2 = 0.66

# FIG 3B scatter plot GDD vs abs WP (only growing season, april-september )
lm%>%
  mutate(Site = fct_relevel(Site,
                            "Rabinalto", "Canada",
                            "Solana", "Penouta")) %>%
  ggplot()+  #abs_WP
  geom_point(aes(x=GDD, y=abs_WP, fill=Site), size=3, shape=21) +
  scale_fill_manual(name= "Site",labels = c("Rabinalto", "Cañada","Solana", "Penouta"), 
                    values = c( "green3","#551A8B","orange","deepskyblue3")) + # ,
  geom_smooth(aes(x=GDD, y=abs_WP), method = "lm", color = "black")+
  annotate(geom="text",x=1800, y=2400,label = "R² = 0.66",fontface="italic", size =4)+
  scale_x_continuous (limits =c(1600, 2600), breaks = c(1700,1900, 2100, 2300, 2500))+
  ggthemes::theme_tufte(base_size = 12) + 
  theme (plot.margin = margin(0,0,0,0, "cm"),
         text = element_text(family = "sans"),
         panel.background = element_rect(color = "black", fill = NULL),
         plot.title = element_text ( size = 12, margin=margin(0,0,0,0)), #hjust = 0.5,
         axis.title = element_text (size=10), 
         axis.text = element_text(size = 8, color = "black"),
         legend.title = element_text(size =10),
         legend.text = element_text (size =8),
         legend.position= "none") +
  labs (title = "B", y= expression(paste(Sigma," ", Psi," (MPa)")), x = "Growing degree days (ºC)") -> Fig3B;Fig3B

### FIG 3C PCA from 78 plots (central with micrlog + iButtons) ####
# origin data frame in pca bioclim script
ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  coord_fixed() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(fill = Site), size = 2, shape= 21) +
  geom_segment(data = pcaVars, aes(x = 0, y = 0, xend = 3*Dim.1, yend = 3*Dim.2), arrow = arrow(length = unit(0.3,"cm"))) +
  geom_label_repel(data = pcaVars, aes(x = 3*Dim.1, y = 3*Dim.2, label = Variable),  show.legend = FALSE, size = 4, segment.size= 1,
                   point.padding = 0.2, nudge_x = .15, nudge_y = .5,segment.curvature = -1e-20, segment.linetype = 1, segment.color = "red", arrow = arrow(length = unit(0.015, "npc")))+
  ggthemes::theme_tufte(base_size = 12) + 
  theme(plot.margin = margin(0,0,0,0, "cm"),
        text = element_text(family = "sans"),
        plot.title = element_text ( size = 12, margin=margin(0,0,0,0)), #hjust = 0.5,
        legend.title = element_blank(),
        legend.position = "bottom", 
        legend.margin=margin(0, 0, 0, 0),
        legend.box.margin=margin(-1,-1,-1,-1),
        legend.text = element_text(size = 8, color = "black", margin=margin(0,0,0,0)),
        panel.background = element_rect(color = "black", fill = NULL),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8, color = "black")) +
  scale_x_continuous(name = paste("Axis 1 (", round(pca1$eig[1, 2], 0),
                                  "% variance explained)", sep = ""), limits = c(-4.5,4.5)) + 
  scale_y_continuous(name = paste("Axis 2 (", round(pca1$eig[2, 2], 0), 
                                  "% variance explained)", sep = "")) +
  labs(title = "C")+
  scale_fill_manual(labels = c("Rabinalto", "Cañada","Solana", "Penouta"), values = c("green3", "#551A8B","orange",   "deepskyblue3")) -> Fig3C; Fig3C

#### figure combination ####
# using patchwork ##
design = 
"AB
AC"
(Fig3A+Fig3B+Fig3C) + 
  plot_layout(nrow=2, design = design, heights = c(2.5,2.5,5))-> Fig3;Fig3 # move width of plot panel to adjust sizes

ggsave(filename = "Figure 3.png", plot =Fig3, path = "results/figures", 
       scale =1, width = 180, height = 130,units = "mm",device = "png", dpi = 600) #
