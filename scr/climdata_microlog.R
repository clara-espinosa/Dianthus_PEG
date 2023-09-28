library(tidyverse);library(zoo);library(dplyr);library (lubridate)

###dataframe ####
read.csv("data/WP_gooddata_villa.csv", sep = ";") -> microlog_df 
microlog_df
str(microlog_df)


microlog_df %>%
  data.frame() %>%
  dplyr::filter(Micro == "central") %>%
  #filter(!site =="Cañada") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% #specify format of Time variable
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  mutate(Year = lubridate::year(Time)) %>% 
  mutate(Month = lubridate::month(Time)) %>% 
  mutate(Day = lubridate::day(Time)) %>% 
  mutate(Hour = lubridate::hour(Time)) %>% 
  #mutate(Time= as.Date(Time)) %>%
  mutate (site_year = paste (site, Year)) -> df

# indices calculation NO FILTERED with data ibuttons!
read.csv("data/WP_gooddata_villa.csv", sep =";") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% 
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  dplyr::filter(Micro == "central") %>%
  merge(read.csv("data/Dianthus_header.csv", sep =";")) %>%
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
            GDD = sum(GDD)) -> # GDD per year
  dianthus_bioclim_microlog
dianthus_bioclim_microlog %>%
  mutate(ID = c("B00", "D00", "A00", "C00"))%>%
  mutate (ID = as.factor (ID))%>%
  rename(site = Site)-> dianthus_bioclim_microlog

dianthus_bioclim_microlog %>% write.csv("results/dianthus_bioclim_microlog.csv", row.names = FALSE)

###############    growing season determination #####################
read.csv("data/WP_gooddata_villa.csv", sep = ";") %>%
  #filter(!site =="Canada") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% #specify format of Time variable
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  mutate(Year = lubridate::year(Time)) %>% 
  mutate(Month = lubridate::month(Time)) %>% 
  mutate(Day = lubridate::day(Time)) %>% 
  mutate(Hour = lubridate::hour(Time)) %>% 
  mutate(WP = rowMeans((cbind(sensor1, sensor2)))) %>% 
  group_by(community, site, Year, Day = lubridate::floor_date(Time, "day")) %>%
  summarise(T = mean(temperature)) %>% # Daily mean
  mutate (data_start = first(Day), data_end = last(Day)) %>% # get starting and ending data points x year
  mutate(t5 = ifelse(T>=5, 1, 0))%>% # days with Tmean>= 5 =1
  mutate(length = rollsumr(t5, k = 3, fill= 0)) %>% #sum the 2 previous rows of t5 
  filter(! (length < 3)) %>% # Filter date  with 2 consecutive days with Tmean>5ºC (Körner limit) 
  group_by(community, site, Year) %>% #separate x year
  summarise(GS_start = first (Day), GS_end = last(Day), # get the first and last day of the growing season
            data_start = first(data_start), data_end = last(data_end )) %>% 
  mutate (GS_length = difftime(GS_end , GS_start, units = "days")) %>%
  mutate (data_days = data_end - data_start) %>%
  mutate (site_year = paste (site, Year))-> grow #length of growing season in days

library (viridis)
x11()
grow %>%
  mutate (community = as.factor(community), 
          site = as.factor(site), 
          Year = as.numeric(Year)) %>%
  filter (! (data_days <360)) %>% 
  filter ( Year > 2017)%>%
  ggplot(aes(x=Year, y=GS_length, color = site, fill = site)) +
  geom_bar( stat = "identity", position = position_dodge())  +
  scale_fill_viridis (discrete=TRUE) +
  scale_color_viridis (discrete = TRUE) +
  labs( title = "Growing season length x site",x = "Year", y = " Growing season (days)") + #
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
    pull (site_year)%>%
    unique() -> unic
  grow  %>%
    pull(GS_start) -> date1 #convierte columna en vector
  grow %>%
    pull(GS_end) -> date2
  df %>% 
    mutate (Time = as.Date(Time)) %>%
    filter(site_year == unic) %>%
    filter(Time >= date1 & Time <= date2)  
}

read.csv("data/WP_gooddata_villa.csv", sep = ";") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% #specify format of Time variable
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  mutate(Year = lubridate::year(Time)) %>% 
  mutate(Month = lubridate::month(Time)) %>% 
  mutate(Day = lubridate::day(Time)) %>% 
  mutate(Hour = lubridate::hour(Time)) %>% 
  mutate (site_year = paste (site, Year)) %>%
  merge(grow) %>%
  filter (! (data_days <300)) %>% # 300 to include data from PICOS in 2019 (check calendar in github)
  group_by(community, site, Year) %>%
  do (GS_filter(.)) %>% # filter data for only growing season
  mutate(WP = rowMeans((cbind(sensor1, sensor2)))) %>% 
  select(! c(micro, sensor1, sensor2)) %>% 
  group_by(community, site, Year, Hour = lubridate::floor_date(Time, "hour")) %>%
  mutate(stress = ifelse(WP >=14, 1, 0)) %>% # Hour with water stress 1
  group_by(community, site, Year, Day = lubridate::floor_date(Time, "day")) %>%
  summarise(T = mean(temperature), X = max(temperature), N = min(temperature), #n = length(Time), 
            WPmean = mean(WP), WPmax = max(WP), WPmin = min (WP), WPtotal = sum(WP), 
            hour_stress = sum(stress)) %>%
  mutate (day_stress = ifelse(hour_stress>=12, 1, 0))%>% # if more than 12 hours with stress = stressed day
  mutate (stress_length = rollsumr(day_stress, k=4, fill = 0))  %>% # sum how many stressed days in the last four days
  filter(! (stress_length < 4)) %>% # filter dates in order to identify the first day with 4 previous days stressed
  group_by(community, site, Year) %>% #separate x year
  summarise(WS_start = first (Day), WS_end = last(Day), # get the first and last day of the with water stress 
            hour_stress = sum(hour_stress), WPtotal = sum(WPtotal)) %>%
  mutate (WS_length = diffTime(WS_end , WS_start, units = "days")) -> WP_traits


library (viridis)
WP_traits %>%
  mutate (community = as.factor(community), 
          site = as.factor(site), 
          Year = as.numeric(Year)) %>%
  #filter ( Year > 2017)%>%
  ggplot(aes(x=Year, y=WS_length, color = site, fill = site)) +
  geom_bar( stat = "identity", position = position_dodge())  +
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

### get mean value x day across years####
read.csv("data/WP_gooddata_villa.csv", sep = ";") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% #specify format of Time variable
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  mutate(Year = lubridate::year(Time)) %>% 
  mutate(Month = lubridate::month(Time)) %>% 
  mutate(Day = lubridate::day(Time)) %>% 
  mutate(Hour = lubridate::hour(Time)) %>% 
  mutate (site_year = paste (site, Year)) %>%
  merge(grow) %>%
  filter (! (data_days <300)) %>% # 300 to include data from PICOS in 2019 (check calendar in github)
  group_by(community, site, Year) %>%
  do (GS_filter(.)) %>% # filter data for only growing season
  mutate(WP = rowMeans((cbind(sensor1, sensor2)))) %>% 
  select(! c(micro, sensor1, sensor2)) %>% 
  group_by(community, site, Year, Month, Day) %>%
  summarise(T = mean(temperature), X = max(temperature), N = min(temperature), n = length(Time), 
            WPmean = mean(WP), WPmax = max(WP), WPmin = min (WP)) %>% # Daily mean, max, min
  #mutate(stress = ifelse(WPmax >=14, 1, 0)) %>% # Day with water stress stress=1
  #mutate (daily_Trange = X-N) %>% #daily temperature range
  group_by(community, site, Year, Month) %>%
  summarise(T = mean(T), X = mean(X), N = mean(N), # Daily mean, max, min
            WPmean = mean(WPmean), WPmax = max(WPmax), WPmin = min (WPmin)) %>% 
  group_by(community, site, Year, Month) %>%
  summarise_all(mean, na.rm = TRUE) -> year_month_site_GS
x11()
ggplot(year_month_site_GS) +
  geom_line (aes (x=Month, y=N, colour = site), size =1.25) + #order for sort as experiment
  geom_line (aes (x=Month, y=X, colour = site), size =1.25) + 
  geom_ribbon (aes (x=Month, ymin =N, ymax=X, colour = site, fill = site), alpha =0.2) + 
  scale_x_continuous (limits = c(3,11), breaks = seq (3, 11, by= 1))+
  facet_grid(~Year) +
  scale_fill_viridis (discrete=TRUE) +
  scale_color_viridis (discrete = TRUE) +
  #geom_hline(yintercept=0, linetype ="dashed", size =1, colour = "red") +
  theme_classic(base_size = 16) +
  theme (plot.title = element_text ( size = 30), #hjust = 0.5,
         axis.title.y = element_text (size=18), 
         axis.title.x = element_text (size=18), 
         legend.title = element_text(size = 20),
         legend.text = element_text (size =16)) +
  labs (title = "Temperature range during growing season", y= "temperature ºC", x = "Month") 

ggplot(year_month_site_GS) +
  geom_line (aes (x= Month, y= WPmean, colour = site), size = 1.25) +
  #geom_line (aes (x=Month, y=WPmin, colour = site), size =1.25) + #order for sort as experiment
  #geom_line (aes (x=Month, y=WPmax, colour = site), size =1.25) + 
  #geom_ribbon (aes (x=Month, ymin =WPmin, ymax=WPmax, colour = site, fill = site), alpha =0.2) + 
  scale_x_continuous (limits = c(3,11), breaks = seq (3, 11, by= 1))+
  scale_fill_viridis (discrete=TRUE) +
  scale_color_viridis (discrete = TRUE) +
  facet_grid(~Year) +
  geom_hline(yintercept=14, linetype ="dashed", size =1, colour = "red") +
  theme_classic(base_size = 16) + 
  theme (plot.title = element_text ( size = 30), #hjust = 0.5,
         axis.title.y = element_text (size=18), 
         axis.title.x = element_text (size=18), 
         legend.title = element_text(size = 20),
         legend.text = element_text (size =16)) +
  labs (title = "Mean Water Potential during growing season", y= "WP", x = "Month") 
