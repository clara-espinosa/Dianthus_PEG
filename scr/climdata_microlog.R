library(tidyverse);library(zoo); library(dplyr);library (lubridate);library(ggpubr)


###dataframe ####
read.csv("data/wp_villa_2021_2023.csv", sep = ",") -> microlog_df 
microlog_df
str(microlog_df)

######  climatic indices calculation NO FILTERED with data ibuttons! #####
read.csv("data/wp_villa_2021_2023.csv", sep =",") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% 
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  mutate(Year = lubridate:: year(Time)) %>%
  mutate(Month = lubridate::month(Time)) %>% 
  mutate(Day = lubridate::day(Time)) %>% 
  mutate(Hour = lubridate::hour(Time)) %>% 
  dplyr::filter(Micro == "central") %>%
  #merge(read.csv("data/Dianthus_header.csv", sep =";")) %>%
  group_by(Site, Year, Month, Day) %>%
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time)) %>% # Daily mean, max, min
  mutate(Snow = ifelse(X < 0.5 & N > -0.5, 1, 0)) %>% # Day is snow day or not
  mutate(FreezeThaw = ifelse(X > 0.5 & N < -0.5, 1, 0)) %>% # Day with freeze-thaw cycles
  mutate(FDD = ifelse(T < 0, T, 0)) %>% # Freezing degrees per day
  mutate(GDD = ifelse(T >= 5, T, 0))   %>% # Growing degrees day per month https://link.springer.com/article/10.1007/s00035-021-00250-1
  group_by(Site, Year, Month) %>%
  summarise(T = mean(T), X = mean(X), N = mean(N), # Daily mean, max, min
            Snow = sum(Snow), # Snow days per month
            FreezeThaw = sum(FreezeThaw), # Freeze-thaw days per month
            FDD = sum(FDD), # FDD per month
            GDD = sum(GDD))%>% # GDD per month
  group_by(Site, Year) %>%
  summarise(bio1 = mean(T), # Annual Mean Temperature
            bio2 = mean(X - N), # Mean Diurnal Range (Mean of monthly (max temp - min temp))
            bio7 = max(X) - min(N), # Temperature Annual Range (BIO5-BIO6)
            Snw = sum(Snow),
            FDD = abs(sum(FDD)), # FDD per year
            GDD = sum(GDD)) -> # GDD per year
  dianthus_bioclim_microlog_no_filtered

dianthus_bioclim_microlog_no_filtered %>% write.csv("results/dianthus_bioclim_microlog_no_filtered.csv", row.names = FALSE)

####### indices calculation FILTERED with data ibuttons!######
read.csv("data/wp_villa_2021_2023.csv", sep =",") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% 
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  mutate(Year = lubridate::year(Time)) %>%
  mutate(Month = lubridate::month(Time)) %>% 
  mutate(Day = lubridate::day(Time)) %>% 
  mutate(Hour = lubridate::hour(Time)) %>% 
  dplyr::filter(Micro == "central") %>%
  filter(!Site =="Cañada") %>%
  filter(Hour %in% c(0, 4, 8, 12, 16, 20)) %>% # Keep same recording hours as ibuttons
  filter(!Year %in% 2023)%>%
  filter (! (Year %in% 2021 & Month < 7))%>% # remove missing june days of 2021
  filter (! (Year %in% 2022 & Month > 6))%>% # remove missing july days
  #filter(! Month %in% 6) %>% # Remove missing June days
  filter(! (Month %in% 5 & Day > 29)) %>% # Remove missing May days
  filter(! (Month %in% 7 & Day < 12)) %>% # Remove missing July days 
  #merge(read.csv("data/Dianthus_header.csv", sep =";")) %>%
  group_by(Site, Day = lubridate::floor_date(Time, "day")) %>%
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time)) %>% # Daily mean, max, min
  mutate(Snow = ifelse(X < 0.5 & N > -0.5, 1, 0)) %>% # Day is snow day or not
  mutate(FreezeThaw = ifelse(X > 0.5 & N < -0.5, 1, 0)) %>% # Day with freeze-thaw cycles
  mutate(FDD = ifelse(T < 0, T, 0)) %>% # Freezing degrees per day
  mutate(GDD = ifelse(T >= 5, T, 0)) %>% # Growing degrees day per month https://link.springer.com/article/10.1007/s00035-021-00250-1
  group_by(Site, Month = lubridate::floor_date(Day, "month")) %>%
  summarise(T = mean(T), X = mean(X), N = mean(N), # Daily mean, max, min
            Snow = sum(Snow), # Snow days per month
            FreezeThaw = sum(FreezeThaw), # Freeze-thaw days per month
            FDD = sum(FDD), # FDD per month
            GDD = sum(GDD)) %>% # GDD per month
  group_by(Site) %>%
  summarise(bio1 = mean(T), # Annual Mean Temperature
            bio2 = mean(X - N), # Mean Diurnal Range (Mean of monthly (max temp - min temp))
            bio7 = max(X) - min(N), # Temperature Annual Range (BIO5-BIO6)
            Snw = sum(Snow),
            FDD = abs(sum(FDD)), # FDD per year
            GDD = sum(GDD))%>%
  mutate(ID= c("D00", "A00", "C00"))%>% ##"B00", 
  mutate(Site = as.factor(Site)) %>%
  rename(Site= Site)-> # GDD per year
  dianthus_bioclim_microlog

dianthus_bioclim_microlog %>% write.csv("results/dianthus_bioclim_microlog_filtered.csv", row.names = FALSE)

###############    growing season determination #####################
read.csv("data/wp_villa_2021_2023.csv", sep = ",") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% #specify format of Time variable
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  mutate(Year = lubridate::year(Time)) %>% 
  mutate(Month = lubridate::month(Time)) %>% 
  mutate(Day = lubridate::day(Time)) %>% 
  mutate(Hour = lubridate::hour(Time)) %>% 
  #dplyr::filter(Micro == "central") %>%
  mutate(WP = rowMeans((cbind(wp1, wp2)))) %>% 
  group_by(Community, Site, Micro,  Year, Day = lubridate::floor_date(Time, "day")) %>% #Micro,
  summarise(T = mean(Temperature)) %>% # Daily mean
  mutate (data_start = first(Day), data_end = last(Day)) %>% # get starting and ending data points x year
  mutate(t5 = ifelse(T>=5, 1, 0))%>% # days with Tmean>= 5 =1
  mutate(length = rollsumr(t5, k = 3, fill= 0)) %>% #sum the 2 previous rows of t5 
  filter(! (length < 3)) %>% # Filter date  with 2 consecutive days with Tmean>5ºC (Körner limit) 
  group_by(Community, Site,  Micro, Year) %>% #separate x year   Micro,
  summarise(GS_start = first (Day), GS_end = last(Day), # get the first and last day of the growing season
            data_start = first(data_start), data_end = last(data_end )) %>% 
  mutate (GS_length = GS_end - GS_start) %>%
  mutate (data_days = data_end - data_start) %>%
  mutate (Site_year = paste (Site, Micro, Year))%>%
  #select(Community, Site, Year, data_days, data_start, data_end, GS_length, GS_start, GS_end, Site_year)%>%
  as.data.frame()-> grow #length of growing season in days
str(grow)
library (viridis)
x11()
grow %>%
  mutate (Community = as.factor(Community), 
          Site = as.factor(Site), 
          Year = as.numeric(Year)) %>%
  #filter (! (data_days <260)) %>% 
  ggplot(aes(x=Year, y=GS_length, color = Micro, fill = Micro)) +
  geom_bar( stat = "identity", position = position_dodge())  +
  scale_fill_viridis (discrete=TRUE) +
  facet_wrap(~Site)+
  scale_color_viridis (discrete = TRUE) +
  labs( title = "Growing season length x Site",x = "Year", y = " Growing season (days)") + #
  #ggthemes::theme_tufte(base_size = 16) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text (hjust = 0.5, size = 30),
        #strip.text = element_blank(),
        strip.text.x = element_text(face = "bold", size = 22),
        strip.text.y = element_text(size = 14, angle = 360),
        legend.position = "right",
        panel.background = element_rect(color = "black", fill = NULL),
        axis.title.y = element_text (size=16), 
        axis.title.x = element_text (size=16))
# scale_color_manual (values = c("#AC1926", "#891171", "#33407D", "#077395", "#00BC7F", "#AADB41", "#FDE333"))


###################### WP traits during growing season (ONLY FOR WHOLE YEAR DATA?) #################

# function to filter for growing season  (NOW NOT WORKING PROPERLY)
GS_filter <-function (grow) {
  grow %>%
    pull (Site_year)%>%
    unique() -> unic
  grow  %>%
    pull(GS_start) -> date1 #convierte columna en vector
  grow %>%
    pull(GS_end) -> date2
  df  %>% # df needs to be a dataframe with the raw data 
    mutate (Time = as.Date(Time)) %>%
    filter(Site_year == unic) %>%
    filter(Time >= date1 & Time <= date2)  
}

read.csv("data/wp_villa_2021_2023.csv", sep = ",") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% #specify format of Time variable
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  mutate(Year = lubridate::year(Time)) %>% 
  mutate(Month = lubridate::month(Time)) %>% 
  mutate(Day = lubridate::day(Time)) %>% 
  mutate(Hour = lubridate::hour(Time)) %>% 
  mutate (Site_year = paste (Site, Micro, Year)) %>% # do -> df before runing the whole script
  #dplyr::filter(Micro == "central") %>% 
  merge(grow) %>%
  #filter (! (data_days <260)) %>% # 260 to include data from 2022 (check calendar in github)
  group_by(Community, Site, Micro, Year) %>%
  do (GS_filter(.)) %>% # filter data for only growing season
  mutate(WP = rowMeans((cbind(wp1, wp2)))) %>% 
  select(! c(wp1, wp2)) %>% # micro
  group_by(Community, Site, Micro, Year, Hour = lubridate::floor_date(Time, "hour")) %>%
  mutate(stress = ifelse(WP >=14, 1, 0)) %>% # Hour with water stress 1
  group_by(Community, Site, Micro,Year, Day = lubridate::floor_date(Time, "day")) %>%
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), #n = length(Time), 
            WPmean = mean(WP), WPmax = max(WP), WPmin = min (WP), WPtotal = sum(WP), 
            hour_stress = sum(stress)) %>%
  mutate (day_stress = ifelse(hour_stress>=12, 1, 0))%>% # if more than 12 hours with stress = stressed day
  mutate (stress_length = rollsumr(day_stress, k=4, fill = 0))  %>% # sum how many stressed days in the last four days
  filter(! (stress_length < 4)) %>% # filter dates in order to identify the first day with 4 previous days stressed
  group_by(Community, Site, Micro, Year) %>% #separate x year
  summarise(WS_start = first (Day), WS_end = last(Day), # get the first and last day of the with water stress 
            hour_stress = sum(hour_stress), WPtotal = sum(WPtotal), day_stress = sum(day_stress)) %>%
  mutate (WS_length = as.Date(WS_end) - as.Date(WS_start)) -> WP_traits

grow%>%
  merge(WP_traits) %>%
  write.csv("results/growing season and WP traits.csv")

str(WP_traits)
library (viridis)
WP_traits %>%
  mutate (Community = as.factor(Community), 
          Site = as.factor(Site), 
          Year = as.numeric(Year)) %>%
  #filter ( Year > 2017)%>%
  ggplot(aes(x=Year, y=WS_length, color = Micro, fill = Micro)) +
  geom_bar( stat = "identity", position = position_dodge())  +
  facet_wrap(~Site)+
  scale_fill_viridis (discrete=TRUE) +
  scale_color_viridis (discrete = TRUE) +
  labs( title = "Water stress length",x = "Year", y = " Water stress (days)") + #
  #ggthemes::theme_tufte(base_size = 16) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text (hjust = 0.5, size = 30),
        #strip.text = element_blank(),
        strip.text.x = element_text(face = "bold", size = 22),
        strip.text.y = element_text(size = 14, angle = 360),
        legend.position = "right",
        panel.background = element_rect(color = "black", fill = NULL),
        axis.title.y = element_text (size=16), 
        axis.title.x = element_text (size=16))

### get mean climograma villa from Penauta, Rabinalto and Solana central ####


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

# prueba visualización con doble eje Y y WP mean
x11()
villa_clima %>%
  #mutate(Month = as.factor(Month))%>%
  group_by(Month)%>%
  summarise_at(vars(T:WPmin), mean, na.rm = TRUE)%>%
  ggplot() +
  geom_col(aes (x= Month, y=WPmax*2), colour = "black", fill = "deepskyblue3")+
  geom_line (aes (x=Month, y=N), colour = "chocolate2", linewidth =1.25) + 
  geom_line (aes (x=Month, y=X), colour = "chocolate2", linewidth =1.25) + 
  geom_ribbon (aes (x=Month, ymin =N, ymax=X), fill = "chocolate2", alpha =0.3) + 
  scale_x_continuous (limits = c(0.5,12.5), breaks = seq (1, 12, by= 1))+
  scale_y_continuous(limits = c(-5, 45), breaks = seq (-5, 45, by = 10),
                     sec.axis = sec_axis(trans = ~./20, name = "Water potential (-MPa)")) +
  #facet_grid(~site) +
  geom_hline(yintercept=0, linetype ="dashed", size =1.3, colour = "darkred")+
  geom_hline(yintercept=30, linetype ="dashed", size =1, colour = "navyblue")+
  #scale_fill_viridis (discrete=TRUE) +
  #scale_color_viridis (discrete = TRUE) +
  #geom_hline(yintercept=0, linetype ="dashed", size =1, colour = "red") +
  ggthemes::theme_tufte() + 
  theme (text = element_text(family = "sans"),
         panel.background = element_rect(color = "black", fill = NULL),
         plot.title = element_text ( size = 20), #hjust = 0.5,
         axis.title.y = element_text (size=12), 
         axis.title.x = element_text (size=12), 
         legend.title = element_text(size =14),
         legend.text = element_text (size =12)) +
  labs (title = "Study area climogram", y= "Temperature ºC", x = "Month")-> Fig2A;Fig2A 

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
            WPmean = mean(WPmean), WPmax = mean(WPmax), WPmin = mean(WPmin))-> monthly_villa_clima  %>%
  filter(n >27)

# scatter plot GDD vs abs WP (all year looks weird, only growing season?)

monthly_villa_clima%>%
  filter(!site =="Cañada") %>%
  filter(!(site== "Solana" & Micro=="central"))%>%
  filter (! Year == 2021)%>%
  filter (!(Year %in% 2022 & Month < 3))%>%
  filter (!(Year %in% 2022 & Month >11))%>%
  filter (!(Year %in% 2023 & Month < 3))%>%
  filter (!(Year %in% 2023 & Month >11))%>%
  #mutate(Month = as.factor(Month))%>%
  group_by(Year, site, Micro)%>%
  summarise(abs_WP = sum(abs_WP),
            GDD = sum (GDD), 
            WPmean= sum(WPmean)) %>%
  ggplot()+  #abs_WP
  geom_point(aes(x=GDD, y=WPmean, color=site), size=4) +
  scale_color_manual(name= "Site",values = c( "deepskyblue3","green3","orange" )) + #"#551A8B",,
  geom_smooth(aes(x=GDD, y=WPmean))+
  ggthemes::theme_tufte() + 
  theme (text = element_text(family = "sans"),
         panel.background = element_rect(color = "black", fill = NULL),
         plot.title = element_text ( size = 20), #hjust = 0.5,
         axis.title.y = element_text (size=12), 
         axis.title.x = element_text (size=12), 
         legend.title = element_text(size =14),
         legend.text = element_text (size =12),
         legend.position= "right") +
  labs (title = "Yearly Water Potential x GDD", y= "Absolute WP (MPa)", x = "Growing Degree Days (ºC)") -> Fig2C;Fig2C
x11()
Fig2 <- ggarrange(Fig2A, Fig2B, Fig2C, labels = c("A", "B", "C"), ncol = 1, nrow = 3) 
Fig2                

ggsave(filename = "results/figures/Fig2.png", Fig2, path = NULL, 
       scale = 1, width = 600, height = 800, units = "mm", dpi = 600)

