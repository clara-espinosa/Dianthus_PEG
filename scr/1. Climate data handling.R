library(tidyverse); library(urca); library(raster); library(tibbletime); library(lubridate)
library(zoo); library(dplyr);library(ggpubr);library(cowplot);library(patchwork)
Sys.setlocale("LC_TIME", "English")

write.csv(bioclim, "results/bioclim.csv")
#### iButtons ####
# indices calculation (14 plots for dianthus subpopulations from iButtons loggers)
read.csv("data/temperature_ibuttons.csv", sep =";") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% 
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  merge(read.csv("data/Dianthus_header.csv", sep =",")) %>%
  group_by(Site,ID, Day = lubridate::floor_date(Time, "day")) %>% #
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time)) %>% # Daily mean, max, min
  mutate(Snow = ifelse(X < 0.5 & N > -0.5, 1, 0)) %>% # Day is snow day or not
  mutate(FreezeThaw = ifelse(X > 0.5 & N < -0.5, 1, 0)) %>% # Day with freeze-thaw cycles
  mutate(FDD = ifelse(T < 0, T, 0)) %>% # Freezing degrees per day
  mutate(GDD = ifelse(T >= 5, T, 0)) %>% # Growing degrees day per month https://link.springer.com/article/10.1007/s00035-021-00250-1
  group_by(Site, ID, Month = lubridate::floor_date(Day, "month")) %>% #
  summarise(T = mean(T), X = mean(X), N = mean(N), # Daily mean, max, min
            Snow = sum(Snow), # Snow days per month
            FreezeThaw = sum(FreezeThaw), # Freeze-thaw days per month
            FDD = sum(FDD), # FDD per month
            GDD = sum(GDD)) %>% # GDD per month
  group_by(Site, ID) %>% #
  summarise(bio1 = mean(T), # Annual Mean Temperature
            bio2 = mean(X - N), # Mean Diurnal Range (Mean of monthly (max temp - min temp))
            bio7 = max(X) - min(N), # Temperature Annual Range (BIO5-BIO6)
            Snw = sum(Snow),
            FDD = abs(sum(FDD)), # FDD per year
            GDD = sum(GDD)) -> # GDD per year
  dianthus_bioclim_ibuttons 

dianthus_bioclim_ibuttons %>% write.csv("results/dianthus_bioclim_ibuttons.csv", row.names = FALSE)

# visualization data ibuttons loggers from 14 subpopulations 
x11()
read.csv("data/temperature_ibuttons.csv", sep =";") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% #specify format of TIME variable
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  merge(read.csv("data/dianthus_header.csv", sep= ",")) %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  ggplot(aes(Time, Temperature, color = ID)) + 
  facet_wrap(ID ~ Site, nrow = 5) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  xlab("Time (4-h recording inverval)") +
  ylab("Temperature (ºC)") +
  ggthemes::theme_tufte() +
  theme(strip.background = element_rect(colour = "grey96", fill = "grey96"),
        legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 11, color = "black")) -> ibuttons_clima;ibuttons_clima


### Save figure

ggsave(ibuttons_clima, file = "results/ibuttons_clima(2).png", 
       path = NULL, scale = 1, width = 182, height = 182, units = "mm", dpi = 600)

#### Microlog data ####
# indices calculation FILTERED with data ibuttons!##
read.csv("data/wp_villa_2020_2024.csv", sep =",") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% 
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  mutate(Year = lubridate::year(Time)) %>%
  mutate(Month = lubridate::month(Time)) %>% 
  mutate(Day = lubridate::day(Time)) %>% 
  mutate(Hour = lubridate::hour(Time)) %>% 
  dplyr::filter(Micro == "central") %>%
  filter(!Site =="Canada") %>%
  filter(Hour %in% c(0, 4, 8, 12, 16, 20)) %>% # Keep same recording hours as ibuttons
  filter(!Year %in% 2023)%>% # exclude 2023 data
  filter(!Year %in% 2024)%>% # exclude 2024 data
  filter (! (Year %in% 2021 & Month < 7))%>% # remove missing june days of 2021
  filter (! (Year %in% 2022 & Month > 5))%>% # remove missing july days
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
  rename(Site= Site) -> # GDD per year
  dianthus_bioclim_microlog

dianthus_bioclim_microlog %>% write.csv("results/dianthus_bioclim_microlog_filtered.csv", row.names = FALSE)

## merge ibuttons and microlog microclimate data ####

dianthus_bioclim_ibuttons%>%
  rbind(dianthus_bioclim_microlog)-> bioclim
write.csv(bioclim, "results/bioclim.csv")

# Field water potential data to investigate reviewer idea ####
library(dplyr)
read.csv("data/wp_villa_2020_2024.csv", sep =",") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% 
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  mutate(Year = lubridate::year(Time)) %>%
  mutate(Month = lubridate::month(Time)) %>% 
  mutate(Day = lubridate::day(Time)) %>% 
  mutate(Hour = lubridate::hour(Time)) %>% 
  mutate(WP = rowMeans((cbind(wp1, wp2)))) %>% # mean value between WP sensors
  filter((Month < 10 & Month > 6))%>% # change month 6 or 7 depending if we focus on july or august
  group_by(Site, Micro, Year, ID, Hour = lubridate::floor_date(Time, "hour")) %>%
  summarise(WP = mean (WP), n = length(Time)) %>% # mean WP value x day
  mutate(minBWP = ifelse(WP<=3.5, 1, 0))%>%
  filter((minBWP>0))%>%
  group_by(Site, ID, Year)%>%
  summarise(minBWP_date = first(Hour))-> minBWP_date

read.csv("data/wp_villa_2020_2024.csv", sep =",") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% 
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  mutate(Year = lubridate::year(Time)) %>%
  mutate(Month = lubridate::month(Time)) %>% 
  mutate(Day = lubridate::day(Time)) %>% 
  mutate(Hour = lubridate::hour(Time)) %>% 
  mutate(WP = rowMeans((cbind(wp1, wp2)))) %>% # mean value between WP sensors
  filter((Month < 10 & Month > 6))%>%# change month 6 or 7 depending if we focus on july or august
  group_by(Site, Micro, Year, ID, Hour = lubridate::floor_date(Time, "hour")) %>%
  summarise(WP = mean (WP), n = length(Time)) %>% # mean WP value x day
  mutate(maxBWP = ifelse(WP<=5.5, 1, 0))%>%
  filter((maxBWP>0))%>%
  group_by(Site, ID, Year)%>%
  summarise(maxBWP_date = first(Hour))->maxBWP_date

minBWP_date%>%
  merge(maxBWP_date, by=c("Site", "ID", "Year"))%>%
  mutate (time_diff = difftime(minBWP_date , maxBWP_date, units = "hour")) %>% 
  write.csv("results/field_WP_july.csv")
