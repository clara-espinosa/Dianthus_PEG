library(tidyverse);library(zoo); library(dplyr);library (lubridate);library(ggpubr)
library(cowplot);library(patchwork)


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



