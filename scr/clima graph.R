library(tidyverse);library(zoo); library(dplyr);library (lubridate);library(ggpubr)
library(cowplot);library(patchwork); library(ggrepel)

### FIG mean climograma villa from Penauta, Rabinalto and Solana central ####
read.csv("data/wp_villa_2021_2023.csv", sep = ",") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% #specify format of Time variable
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  mutate(Year = lubridate::year(Time)) %>% 
  mutate(Month = lubridate::month(Time)) %>% 
  mutate(Day = lubridate::day(Time)) %>% 
  mutate(Hour = lubridate::hour(Time)) %>% 
  dplyr::filter(Micro == "central") %>%
  filter(!Site =="Cañada") %>%
  filter (! (Year %in% 2021 & Month < 7))%>% # remove missing june days of 2021
  filter (! (Year %in% 2022 & Month > 6))%>% # remove missing july days
  mutate(site = fct_recode(Site, "Penouta" = "Penauta"))%>%
  mutate (Site_year = paste (site, Year)) %>%
  mutate(WP = rowMeans((cbind(wp1, wp2)))) %>% 
  select(! c(wp1, wp2)) %>% 
  group_by(Community, site, Month, Day) %>%
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time), 
            WPmean = mean(WP), WPmax = max(WP), WPmin = max (WP), abs_WP = sum(WP)) %>%
  mutate(GDD = ifelse(T >= 5, T, 0)) %>%
  group_by(Community, site,  Month) %>%
  summarise(T = mean(T), X = max(X), N = min(N), abs_WP = sum(abs_WP), GDD = sum(GDD),n = length(n),
            WPmean = mean(WPmean), WPmax = mean(WPmax), WPmin = mean(WPmin)) -> villa_clima

x11()
villa_clima %>%
  #mutate(Month = as.factor(Month))%>%
  group_by(Month)%>%
  summarise_at(vars(T:WPmin), mean, na.rm = TRUE)%>%
  ggplot() +
  geom_col(aes (x= Month, y=WPmax*2), colour = "black", fill = "azure4")+
  geom_line (aes (x=Month, y=N), colour = "darkred", linewidth =1.25) + 
  geom_line (aes (x=Month, y=X), colour = "darkred", linewidth =1.25) + 
  geom_ribbon (aes (x=Month, ymin =N, ymax=X), fill = "darkred", alpha =0.3) + 
  scale_x_continuous (limits = c(0.5,12.5), breaks = seq (1, 12, by= 1))+
  scale_y_continuous(limits = c(-5, 45), breaks = seq (-5, 45, by = 10),
                     sec.axis = sec_axis(trans = ~./20, name = expression(paste("Mean max ",Psi," (MPa)")))) + 
  #facet_grid(~site) +
  geom_hline(yintercept=0, linetype ="dashed", linewidth =1, colour = "black")+
  #geom_hline(yintercept=30, linetype ="dashed", size =1, colour = "navyblue")+
  #scale_fill_viridis (discrete=TRUE) +
  #scale_color_viridis (discrete = TRUE) +
  #geom_hline(yintercept=0, linetype ="dashed", size =1, colour = "red") +
  ggthemes::theme_tufte(base_size = 16) + 
  theme (plot.margin = margin(0, 0, 0, 0, "pt"),
         text = element_text(family = "sans"),
         panel.background = element_rect(color = "black", fill = NULL),
         plot.title = element_text (size = 20, margin=margin(0,0,0,0)), #hjust = 0.5,
         axis.title.y = element_text (size=14), 
         axis.text.y = element_text (size=14), 
         axis.text.x = element_text (size=14), 
         legend.title = element_text(size =14),
         legend.text = element_text (size =14)) +
  labs (title = "A", y= "Temperature (ºC)", x = "Month")-> Fig3A;Fig3A 
# 
# first try out to circulat bar plot with coord_polar()

##### FIG Water potential x GDD #####
read.csv("data/wp_villa_2021_2023.csv", sep = ",") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% #specify format of Time variable
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  mutate(Year = lubridate::year(Time)) %>% 
  mutate(Month = lubridate::month(Time)) %>% 
  mutate(Day = lubridate::day(Time)) %>% 
  mutate(Hour = lubridate::hour(Time)) %>% 
  #dplyr::filter(Micro == "central") %>%
  #filter(!Site =="Cañada") %>%
  filter (! (Year %in% 2021 & Month < 7))%>% # remove missing june days of 2021
  mutate(site = fct_recode(Site, "Penouta" = "Penauta"))%>%
  mutate (Site_year = paste (site, Year)) %>%
  mutate(WP = rowMeans((cbind(wp1, wp2)))) %>% 
  select(! c(wp1, wp2)) %>% 
  group_by(Community, site, Micro, Year, Month, Day) %>%
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time), 
            WPmean = mean(WP), WPmax = max(WP), WPmin = max (WP), abs_WP = sum(WP)) %>%
  mutate(GDD = ifelse(T >= 5, T, 0)) %>%
  group_by(Community, site, Micro, Year, Month) %>%
  summarise(T = mean(T), X = max(X), N = min(N), abs_WP = sum(abs_WP), GDD = sum(GDD),n = length(n),
            WPmean = mean(WPmean), WPmax = mean(WPmax), WPmin = mean(WPmin))-> monthly_villa_clima  

View(monthly_villa_clima)

monthly_villa_clima %>%
  filter(n >27)%>% # filtra aquellos meses con menos de 27 días con datos
  filter (! Year == 2021)%>%
  filter (!(Year %in% 2022 & Month < 3))%>%
  filter (!(Year %in% 2022 & Month >11))%>%
  filter (!(Year %in% 2023 & Month < 3))%>%
  filter (!(Year %in% 2023 & Month >11))%>%
  mutate(abs_WP = abs_WP/10, # cambiar de bar a Mpa
         WPmean= WPmean/10, 
         WPmax= WPmax/10,
         WPmin= WPmin/10)%>%
  group_by(Year, site, Micro)%>%
  summarise(abs_WP = sum(abs_WP),
            GDD = sum (GDD), 
            WPmean= sum(WPmean),
            n = length(n)) %>%
  filter(n >5)%>%
  as.data.frame() -> lm

summary(lm(abs_WP ~ GDD, data=lm))$adj.r.squared  
# scatter plot GDD vs abs WP (only growing season)
lm%>%
  mutate(site = fct_relevel(site,
                            "Rabinalto", "Cañada",
                            "Solana", "Penouta")) %>%
  ggplot()+  #abs_WP
  geom_point(aes(x=GDD, y=abs_WP, fill=site), size=4, shape=21) +
  scale_fill_manual(name= "Site",values = c( "green3","orange","deepskyblue3")) + #"#551A8B",,
  geom_smooth(aes(x=GDD, y=abs_WP), method = "lm")+
  annotate(geom="text",x=2050, y=2800,label = "R² = 0.69",fontface="italic", size =6)+
  ggthemes::theme_tufte(base_size = 16) + 
  theme (plot.margin = margin(0,0,0,0, "pt"),
         text = element_text(family = "sans"),
         panel.background = element_rect(color = "black", fill = NULL),
         plot.title = element_text ( size = 20, margin=margin(0,0,0,0)), #hjust = 0.5,
         axis.title.y = element_text (size=14), 
         axis.text.y = element_text (size=14), 
         axis.title.x = element_text (size=14), 
         axis.text.x = element_text (size=14), 
         legend.title = element_text(size =14),
         legend.text = element_text (size =14),
         legend.position= "none") +
  labs (title = "B", y= expression(paste(Sigma," ", Psi," (MPa)")), x = "Growing degree days (ºC)") -> Fig3B;Fig3B
#y = expression(paste("b (MPa)"))
### FIG PCA from 78 plots central + iButtons ####
# origin data frame in pca bioclim script
ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  coord_fixed() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_segment(data = pcaVars, aes(x = 0, y = 0, xend = 3*Dim.1, yend = 3*Dim.2)) +
  geom_point(aes(fill = site), size = 4, shape= 21) +
  #geom_label(data = pcaVars, aes(x = 3*Dim.1, y = 3*Dim.2, label = Variable),  show.legend = FALSE, size = 4, position=position_jitter(width=pcaVars$jit,height=pcaVars$jit)) +
  geom_label_repel(data = pcaVars, aes(x = 3*Dim.1, y = 3*Dim.2, label = Variable),  show.legend = FALSE, size = 5, segment.size= 1,
                   point.padding = 0.2, nudge_x = .15, nudge_y = .5,segment.curvature = -1e-20, segment.linetype = 1, segment.color = "red", arrow = arrow(length = unit(0.015, "npc")))+
  #geom_label_repel (data =pcaInds, aes(x=Dim.1, y = Dim.2, label = ID ), show.legend = FALSE, size = 4)+
  ggthemes::theme_tufte(base_size = 16) + 
  theme(plot.margin = margin(0,0,0,0, "pt"),
        text = element_text(family = "sans"),
        plot.title = element_text ( size = 20, margin=margin(0,0,0,0)), #hjust = 0.5,
        legend.title = element_blank(),
        legend.position = "bottom", 
        legend.margin=margin(0, 0, 0, 0),
        legend.box.margin=margin(-1,-1,-1,-1),
        legend.text = element_text(size = 14, color = "black", margin=margin(0,0,0,0)),
        panel.background = element_rect(color = "black", fill = NULL),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14, color = "black")) +
  scale_x_continuous(name = paste("Axis 1 (", round(pca1$eig[1, 2], 0),
                                  "% variance explained)", sep = ""), limits = c(-4,4)) + 
  scale_y_continuous(name = paste("Axis 2 (", round(pca1$eig[2, 2], 0), 
                                  "% variance explained)", sep = "")) +
  labs(title = "C")+
  #scale_color_manual(name= "",values = c("green3", "#551A8B","orange",   "deepskyblue3")) +
  scale_fill_manual( values = c("green3", "#551A8B","orange",   "deepskyblue3")) -> Fig3C; Fig3C

#### figure combination ####
# using patchwork
design = 
"AB
AC"
(Fig3A+Fig3B+Fig3C) + 
  plot_layout(nrow=2, design = design, heights = c(2.5,2.5,5))-> Fig3;Fig3 # move width of plot panel to adjust sizes

Fig2A|Fig2B/Fig2C +
  plot_layout(widths= c(1,1), heights = unit(c(5,5), c("cm", "null")))
