library(tidyverse)
library (seedr)
library (stringr)

##### IMEMDIATE SOWING DATA + PLEIMINARY ANALYSIS #####
#load data + transformation data
read.csv ("data/dianthus_germ_data.csv", sep= ";") %>%
  mutate(viable = N_seeds - empty - fungus)%>%
  gather ("days", "germinated", D1:D28)%>% #transform from wide to long format
  mutate (days = gsub("D", "", days)) %>% # remove "D" from time column
  mutate (days = as.numeric(days),
          ID = as.factor (ID), 
          WP_treatment = as.numeric (WP_treatment), 
          sowing_time = as.factor (sowing_time)) %>%
  na.omit %>% 
  filter(sowing_time =="Immediate_sowing")%>%
  dplyr::filter (WP_treatment >= -1.2) -> df
str(df) 

# create physiodata object
test_df <- physiodata (d = df, # the name of your data object
                       t = "days", # the column with the scoring days
                       x = "WP_treatment", # the column with the experimental WP_treatment,
                       groups = c("species", "ID"), # this dataset has X different species/populations
                       g = "germinated", # the column with the number of germinated seeds,
                       pg = "viable") # the column with the total number of viable seeds

immediate_germsummary <- summary(test_df)
write.csv(immediate_germsummary , "results/dianthus_immediate_germsummary.csv")

plot(test_df) # plot of the cumulative germination curves
barplot(test_df) #plot of the final germination proportions and the median germination rate

o1 <- physiotime (d = df, # the name of your data object
                            t = "days", # the column with the scoring days
                            x = "WP_treatment", # the column with the experimental WP_treatment,
                            groups = c("species", "ID"), # this dataset has X different species/populations
                            g = "germinated", # the column with the number of germinated seeds,
                            pg = "N_seeds")  # the column with the total number of viable seeds

plot(o1)
immediate_bwpsummary <- summary(o1)
write.csv(immediate_bwpsummary, "results/dianthus_immediate_bwpsummary.csv")
o1

###### TEST FOR AFTER_RIPENNING GERMINATION DATA #####
read.csv ("data/dianthus_germ_data.csv", sep= ";") %>%
  gather ("days", "germinated", D1:D28)%>% #transform from wide to long format
  mutate (days = gsub("D", "", days)) %>% # remove "D" from time column
  mutate (days = as.numeric(days),
          ID = as.factor (ID), 
          WP_treatment = as.numeric (WP_treatment), 
          sowing_time = as.factor (sowing_time))%>%
  filter(sowing_time =="After_ripening")%>%
  dplyr::select(species, ID, sowing_time, WP_treatment, petri, N_seeds, days, germinated)%>%
  na.omit()-> df2 
str(df2)
# create physiodata object
test_df2 <- physiodata (d = df2, # the name of your data object
                       t = "days", # the column with the scoring days
                       x = "WP_treatment", # the column with the experimental WP_treatment,
                       groups = c("species", "ID"), # this dataset has X different species/populations
                       g = "germinated", # the column with the number of germinated seeds,
                       pg = "N_seeds") # the column with the total number of viable seeds
after_germsummary <- summary(test_df2)
write.csv(after_germsummary , "results/dianthus_after_germsummary.csv")

plot(test_df2) # plot of the cumulative germination curves
barplot(test_df2) #plot of the final germination proportions and the median germination rate

o2 <- physiotime (d = df2, # the name of your data object
                  t = "days", # the column with the scoring days
                  x = "WP_treatment", # the column with the experimental WP_treatment,
                  groups = c("species", "ID"), # this dataset has X different species/populations
                  g = "germinated", # the column with the number of germinated seeds,
                  pg = "N_seeds")  # the column with the total number of viable seeds

plot(o2)
after_bwpsummary <- summary(o2)
write.csv(after_bwpsummary, "results/dianthus_after_bwpsummary.csv")
o2

######## EDU EXTRA ANALYSIS #######
# Populations with positive Wb!!

df %>%
  filter(ID == "B03") -> df1

test_df <- physiodata (d = df1, # the name of your data object
                       t = "days", # the column with the scoring days
                       x = "WP_treatment", # the column with the experimental WP_treatment,
                       groups = c("species", "ID"), # this dataset has X different species/populations
                       g = "germinated", # the column with the number of germinated seeds,
                       pg = "N_seeds") # the column with the total number of viable seeds

physiotime (d = df1, # the name of your data object
            t = "days", # the column with the scoring days
            x = "WP_treatment", # the column with the experimental WP_treatment,
            groups = c("species", "ID"), # this dataset has X different species/populations
            g = "germinated", # the column with the number of germinated seeds,
            pg = "N_seeds") -> o1 # the column with the total number of viable seeds

plot(o1)
plot(test_df)

# Remove lowest potentials!!

df %>%
  filter(ID == "B03") %>%
  filter(WP_treatment > -0.5) -> df1

test_df <- physiodata (d = df1, # the name of your data object
                       t = "days", # the column with the scoring days
                       x = "WP_treatment", # the column with the experimental WP_treatment,
                       groups = c("species", "ID"), # this dataset has X different species/populations
                       g = "germinated", # the column with the number of germinated seeds,
                       pg = "N_seeds") # the column with the total number of viable seeds

physiotime (d = df1, # the name of your data object
            t = "days", # the column with the scoring days
            x = "WP_treatment", # the column with the experimental WP_treatment,
            groups = c("species", "ID"), # this dataset has X different species/populations
            g = "germinated", # the column with the number of germinated seeds,
            pg = "N_seeds") -> o1 # the column with the total number of viable seeds

plot(o1)
plot(test_df)
o1

# Other method!!

df %>%
  filter(ID == "B03") -> df1

test_df <- physiodata (d = df1, # the name of your data object
                       t = "days", # the column with the scoring days
                       x = "WP_treatment", # the column with the experimental WP_treatment,
                       groups = c("species", "ID"), # this dataset has X different species/populations
                       g = "germinated", # the column with the number of germinated seeds,
                       pg = "N_seeds") # the column with the total number of viable seeds

physiotime (d = df1, # the name of your data object
            t = "days", # the column with the scoring days
            x = "WP_treatment", # the column with the experimental WP_treatment,
            groups = c("species", "ID"), # this dataset has X different species/populations
            g = "germinated", # the column with the number of germinated seeds,
            pg = "N_seeds",
            method = "huidobro") -> o1 # the column with the total number of viable seeds

plot(o1)
plot(test_df)
o1

df %>%
  filter(ID == "B03") %>%
  group_by(petri, WP_treatment) %>%
  mutate(g = cumsum(germinated)) %>%
  arrange(petri, WP_treatment) %>%
  mutate(level = paste(WP_treatment, petri)) %>%
  ggplot(aes(days, g, color = as.factor(WP_treatment), group = level)) +
  geom_line(size = 2)
