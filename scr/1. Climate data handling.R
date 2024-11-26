library(tidyverse); library(urca); library(raster); library(tibbletime); library(lubridate)
library(zoo); library(dplyr);library(ggpubr);library(cowplot);library(patchwork)
Sys.setlocale("LC_TIME", "English")

write.csv(bioclim, "results/bioclim.csv")
#### iButtons ####
# indices calculation (14 plots for dianthus subpopulations from iButtons loggers)
read.csv("data/temperature_ibuttons.csv", sep =";") %>%  # read raw data
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% # specify time formar
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  merge(read.csv("data/Dianthus_header.csv", sep =",")) %>% 
  group_by(Site,ID, Day = lubridate::floor_date(Time, "day")) %>% #
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time)) %>% # Daily mean, max, min
  mutate(Snow = ifelse(X < 0.5 & N > -0.5, 1, 0)) %>% # to calculate if a day is under snow or not
  mutate(FDD = ifelse(T < 0, T, 0)) %>% # Freezing degrees per day
  mutate(GDD = ifelse(T >= 5, T, 0)) %>% # Growing degrees day per month https://link.springer.com/article/10.1007/s00035-021-00250-1
  group_by(Site, ID, Month = lubridate::floor_date(Day, "month")) %>% #
  summarise(T = mean(T), X = mean(X), N = mean(N), # Daily mean, max, min
            Snow = sum(Snow), # Snow days per month
            FDD = sum(FDD), # FDD per month
            GDD = sum(GDD)) %>% # GDD per month
  group_by(Site, ID) %>% #
  summarise(bio1 = mean(T), # Annual Mean Temperature
            bio2 = mean(X - N), # Mean Diurnal Range (Mean of monthly (max temp - min temp))
            bio7 = max(X) - min(N), # Temperature Annual Range (BIO5-BIO6)
            Snw = sum(Snow),# snow per year
            FDD = abs(sum(FDD)), # FDD per year
            GDD = sum(GDD)) -> # GDD per year
  dianthus_bioclim_ibuttons 

dianthus_bioclim_ibuttons %>% write.csv("results/dianthus_bioclim_ibuttons.csv", row.names = FALSE)

#### Microlog data ####
# indices calculation FILTERED to match  ibuttons data
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
  filter(! (Month %in% 5 & Day > 29)) %>% # Remove missing May days
  filter(! (Month %in% 7 & Day < 12)) %>% # Remove missing July days 
  group_by(Site, Day = lubridate::floor_date(Time, "day")) %>%
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time)) %>% # Daily mean, max, min
  mutate(Snow = ifelse(X < 0.5 & N > -0.5, 1, 0)) %>% # Day is snow day or not
  mutate(FDD = ifelse(T < 0, T, 0)) %>% # Freezing degrees per day
  mutate(GDD = ifelse(T >= 5, T, 0)) %>% # Growing degrees day per month https://link.springer.com/article/10.1007/s00035-021-00250-1
  group_by(Site, Month = lubridate::floor_date(Day, "month")) %>%
  summarise(T = mean(T), X = mean(X), N = mean(N), # Daily mean, max, min
            Snow = sum(Snow), # Snow days per month
            FDD = sum(FDD), # FDD per month
            GDD = sum(GDD)) %>% # GDD per month
  group_by(Site) %>%
  summarise(bio1 = mean(T), # Annual Mean Temperature
            bio2 = mean(X - N), # Mean Diurnal Range (Mean of monthly (max temp - min temp))
            bio7 = max(X) - min(N), # Temperature Annual Range (BIO5-BIO6)
            Snw = sum(Snow), # snow per year
            FDD = abs(sum(FDD)), # FDD per year
            GDD = sum(GDD))%>%# GDD per year
  mutate(ID= c("D00", "A00", "C00"))%>% # keep codes to join with iButtons data 
  mutate(Site = as.factor(Site)) %>%
  rename(Site= Site) -> 
  dianthus_bioclim_microlog

dianthus_bioclim_microlog %>% write.csv("results/dianthus_bioclim_microlog_filtered.csv", row.names = FALSE)

## merge ibuttons and microlog microclimate data ####

dianthus_bioclim_ibuttons%>%
  rbind(dianthus_bioclim_microlog)-> bioclim
write.csv(bioclim, "results/bioclim.csv")

bioclim

# Field water potential data to investigate how long might take to trigger germination in he field####
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
  group_by(Site, Micro, Year, ID, Hour = lubridate::floor_date(Time, "hour")) %>% # group data by hour
  summarise(WP = mean (WP), n = length(Time)) %>% # mean WP value x day
  mutate(minBWP = ifelse(WP<=3.5, 1, 0))%>% # WP values in bar 3.5 bars = -0.35 Mpa
  filter((minBWP>0))%>%
  group_by(Site, ID, Year)%>%
  summarise(minBWP_date = first(Hour))-> minBWP_date # dates to when the  minimum germination base water potential is reached

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
  mutate(maxBWP = ifelse(WP<=5.5, 1, 0))%>%# WP values in bar 5.5 bars = -0.55 Mpa
  filter((maxBWP>0))%>%
  group_by(Site, ID, Year)%>%
  summarise(maxBWP_date = first(Hour))->maxBWP_date # dates to when the maximum germination base water potential is reached

minBWP_date%>%
  merge(maxBWP_date, by=c("Site", "ID", "Year"))%>%
  mutate (time_diff = difftime(minBWP_date , maxBWP_date, units = "hour")) %>% # differences in hours between minBWP and maxBWP 
  write.csv("results/field_WP_july.csv")
