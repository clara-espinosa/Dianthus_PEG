library(tidyverse);library (seedr)
library (stringr);library (rstatix) # paquete para la funcion get_summary_stats

###### analysis with seedr package #####
# load and transform data
read.csv ("data/dianthus_germ_data.csv", sep= ",") %>%
  mutate(viable = N_seeds - empty - fungus)%>%
  gather ("days", "germinated", D0:D28)%>% #transform from wide to long format
  mutate (days = gsub("D", "", days)) %>% # remove "D" from time column
  mutate (days = as.numeric(days),
          ID = as.factor (ID), 
          WP_treatment = as.numeric (WP_treatment), 
          storage_treatment = as.factor (storage_treatment))%>%
          na.omit -> data


# create physiodata object for germination summary
germ_df <- physiodata (d = data, # the name of your data object
                       t = "days", # the column with the scoring days
                       x = "WP_treatment", # the column with the experimental WP_treatment,
                       groups = c("storage_treatment", "ID"), # this dataset has X different species/populations
                       g = "germinated", # the column with the number of germinated seeds,
                       pg = "viable") # the column with the total number of viable seeds

germination_summary <- summary(germ_df)
write.csv(germination_summary , "results/germination_summary.csv")

plot(germ_df) # plot of the cumulative germination curves
barplot(geerm_df) #plot of the final germination proportions and the median germination rate

# create physyiotime object for WP summary
o1 <- physiotime (d = data, # the name of your data object
                  t = "days", # the column with the scoring days
                  x = "WP_treatment", # the column with the experimental WP_treatment,
                  groups = c("storage_treatment", "ID"), # this dataset has X different species/populations
                  g = "germinated", # the column with the number of germinated seeds,
                  pg = "viable")  # the column with the total number of viable seeds
o1
plot(o1)
bWP_summary <- summary(o1)
write.csv(bWP_summary, "results/bWP_summary.csv")
 