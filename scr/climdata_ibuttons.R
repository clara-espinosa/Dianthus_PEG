library(tidyverse); library(urca); library(raster); library(tibbletime); library(lubridate)
Sys.setlocale("LC_TIME", "English")


read.csv("data/Dianthus_header.csv", sep =";") %>%
  filter(ID %in% c("A00", "A02", "A11",
                   "B00", "B03", "B07", "B17", "B19","B20", 
                   "C00", "C06", "C18", "C19","C20",
                   "D00", "D11", "D12", "D19")) %>%
  pull(ID) -> dianthusplots

# visualization data ibuttons
x11()
read.csv("data/temperature_ibuttons.csv", sep =";") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% #specify format of TIME variable
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  merge(read.csv("data/dianthus_header.csv", sep= ";")) %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  ggplot(aes(Time, Temperature, color = ID)) + 
  facet_wrap(ID ~ site, nrow = 5) + 
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

# indices calculation (80 vs 18 plots)
read.csv("data/temperature_ibuttons.csv", sep =";") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% 
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  merge(read.csv("data/ibuttons plots coordinates.csv", sep =";")) %>%
  #merge(read.csv("data/Dianthus_header.csv", sep =";")) %>%
  group_by(site,ID, Day = lubridate::floor_date(Time, "day")) %>% #
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time)) %>% # Daily mean, max, min
  mutate(Snow = ifelse(X < 0.5 & N > -0.5, 1, 0)) %>% # Day is snow day or not
  mutate(FreezeThaw = ifelse(X > 0.5 & N < -0.5, 1, 0)) %>% # Day with freeze-thaw cycles
  mutate(FDD = ifelse(T < 0, T, 0)) %>% # Freezing degrees per day
  mutate(GDD = ifelse(T >= 5, T, 0)) %>% # Growing degrees day per month https://link.springer.com/article/10.1007/s00035-021-00250-1
  group_by(site, ID, Month = lubridate::floor_date(Day, "month")) %>% #
  summarise(T = mean(T), X = mean(X), N = mean(N), # Daily mean, max, min
            Snow = sum(Snow), # Snow days per month
            FreezeThaw = sum(FreezeThaw), # Freeze-thaw days per month
            FDD = sum(FDD), # FDD per month
            GDD = sum(GDD)) %>% # GDD per month
  group_by(site, ID) %>% #
  summarise(bio1 = mean(T), # Annual Mean Temperature
            bio2 = mean(X - N), # Mean Diurnal Range (Mean of monthly (max temp - min temp))
            bio7 = max(X) - min(N), # Temperature Annual Range (BIO5-BIO6)
            Snw = sum(Snow),
            FDD = abs(sum(FDD)), # FDD per year
            GDD = sum(GDD)) -> # GDD per year
  bioclim_ibuttons #dianthus_bioclim_ibuttons

dianthus_bioclim_ibuttons %>% write.csv("results/bioclim_ibuttons.csv", row.names = FALSE)

