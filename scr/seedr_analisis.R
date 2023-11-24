library(tidyverse);library (seedr)
library (stringr);library (rstatix) # paquete para la funcion get_summary_stats

###### analysis with seedr package #####
# load and transform data
read.csv ("data/dianthus_germ_data.csv", sep= ";") %>%
  mutate(viable = N_seeds - empty - fungus)%>%
  gather ("days", "germinated", D0:D28)%>% #transform from wide to long format
  mutate (days = gsub("D", "", days)) %>% # remove "D" from time column
  mutate (days = as.numeric(days),
          ID = as.factor (ID), 
          WP_treatment = as.numeric (WP_treatment), 
          sowing_time = as.factor (sowing_time))%>%
          na.omit -> data


# create physiodata object for germination summary
test_df <- physiodata (d = data, # the name of your data object
                       t = "days", # the column with the scoring days
                       x = "WP_treatment", # the column with the experimental WP_treatment,
                       groups = c("sowing_time", "ID"), # this dataset has X different species/populations
                       g = "germinated", # the column with the number of germinated seeds,
                       pg = "viable") # the column with the total number of viable seeds

germination_summary <- summary(test_df)
write.csv(germination_summary , "results/germination_summary.csv")

plot(test_df) # plot of the cumulative germination curves
barplot(test_df) #plot of the final germination proportions and the median germination rate

# create physyiotime object for WP summary
o1 <- physiotime (d = data, # the name of your data object
                  t = "days", # the column with the scoring days
                  x = "WP_treatment", # the column with the experimental WP_treatment,
                  groups = c("sowing_time", "ID"), # this dataset has X different species/populations
                  g = "germinated", # the column with the number of germinated seeds,
                  pg = "viable")  # the column with the total number of viable seeds
o1
plot(o1)
bWP_summary <- summary(o1)
write.csv(bWP_summary, "results/bWP_summary.csv")
 
##### seed mass subpopulations variability #####
read.csv("data/50_seeds_weight.csv", sep = ";") ->seed_mass
str(seed_mass)
seed_mass%>%
  mutate(ID= as.factor(ID))%>%
  filter(species == "Dianthus langeanus")%>%
  mutate(seed_weight= weight/num_seeds)%>%
  group_by(ID)%>%
  get_summary_stats(seed_weight)%>%
  as.data.frame()-> summary_seedmass
str(summary_seedmass)
  
write.csv(summary_seedmass, "results/summary_seed_mass.csv")

# seed mass x plot graph
ggplot(summary_seedmass, aes(x= ID, y =mean, ymin = min, ymax = max, color = ID))+
  geom_point( size = 3) +
  geom_errorbar (width = 0.2, size =1.2)+
  theme_bw(base_size = 14) +
  theme (plot.title = element_text ( size = 26), #hjust = 0.5,
         strip.text = element_text (size = 20),
         axis.title.y = element_text (size=14), 
         axis.title.x = element_text (size=14),
         axis.text.x = element_blank(),
         legend.title = element_text(size = 14),
         legend.text = element_text (size =12))+
  labs (title = "Mean seed mass per plot", y= "Seed mass (mg)", x = "Plot ID") 
# seed mass x GDD graph
summary_seedmass %>%
  merge(bioclim)%>%
  ggplot(aes(x=GDD, y=mean, ymin = min, ymax = max)) +
  geom_point(size = 3)+
  geom_errorbar (width = 0.2, size =1.2)+
  geom_smooth() +
  theme_bw(base_size = 14) +
  theme (plot.title = element_text ( size = 26), #hjust = 0.5,
         strip.text = element_text (size = 20),
         axis.title.y = element_text (size=14), 
         axis.title.x = element_text (size=14),
         axis.text.x = element_blank(),
         legend.title = element_text(size = 14),
         legend.text = element_text (size =12))+
  labs (title = "Seed mass correlation with GDD", y= "Seed mass (mg)", x = "GDD")

# seed mass x Base water potential
summary_seedmass %>%
  merge(bWP_summary)%>%
  ggplot(aes(x= mean, y=psib50 ))+
  geom_point(size = 3)+
  geom_smooth(method = "lm") +
  ylim(-0.5, 0.1) +
  xlim(0.6, 1.7)+
  facet_wrap(~sowing_time)+
  theme_bw(base_size = 14) +
  theme (plot.title = element_text ( size = 24), #hjust = 0.5,
         strip.text = element_text (size = 20),
         axis.title.y = element_text (size=14), 
         axis.title.x = element_text (size=14),
         legend.title = element_text(size = 14),
         legend.text = element_text (size =12))+
  labs (title = "Base Water Potential vs Seed mass", x= "Seed mass (mg)", y = "Base Water Potential (Mpa)")

summary_seedmass %>%
  merge(bWP_summary)%>%
  merge(read.csv("data/Dianthus_header.csv", sep = ";"), by= c("ID"))%>%
  filter(Immediate == "Yes")%>%
  filter(After_ripening == "Yes") %>%
  ggplot(aes(x= mean, y=psib50, color = sowing_time ))+
  geom_point(size = 3)+
  geom_smooth(method = "lm") +
  #ylim(-0.5, 0.1) +
  #xlim(0.6, 1.7)+
  #facet_wrap(~sowing_time)+
  theme_bw(base_size = 14) +
  theme (plot.title = element_text ( size = 24), #hjust = 0.5,
         strip.text = element_text (size = 20),
         axis.title.y = element_text (size=14), 
         axis.title.x = element_text (size=14),
         legend.title = element_text(size = 14),
         legend.text = element_text (size =12))+
  labs (title = "Base Water Potential vs Seed mass", x= "Seed mass (mg)", y = "Base Water Potential (Mpa)")
