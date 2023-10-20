library(tidyverse)
library (seedr)
library (stringr)
library (rstatix) # paquete para la funci?n get_summary_stats


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

ggplot(summary_seedmass, aes(x= ID, y =mean, ymin = min, ymax = max, color = ID))+
  geom_point( size = 3) +
  geom_errorbar (width = 0.2, size =1.2)

summary_seedmass %>%
  merge(bioclim)%>%
  ggplot(aes(x=GDD, y=mean)) +
  geom_point()+
  geom_smooth()
  
##### IMEMDIATE SOWING DATA + PREELIMINARY ANALYSIS #####
#load data + transformation data
read.csv ("data/dianthus_germ_data.csv", sep= ";") %>%
  mutate(viable = N_seeds - empty - fungus)%>%
  gather ("days", "germinated", D0:D28)%>% #transform from wide to long format
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

#exploratory visualization
x11()
str(immediate_germsummary) 
immediate_germsummary%>%
  mutate(treatment = factor(treatment))%>%
  mutate(treatment = fct_relevel(treatment,"0", "-0.2", "-0.4", "-0.6", "-0.8", "-1", "-1.2" ))%>%
  as.data.frame ()-> graph
str(graph)  

#final germination x treatment x ID graph
ggplot(graph) +
  geom_point(aes(x= treatment, y =germination.mean, color = treatment), size = 3) +
  #geom_errorbar(aes(x= treatment, ymin = germination.lower, 
                   # ymax = germination.upper, color = treatment),width = 0.2, size =1.2)  +
  coord_cartesian(ylim = c(0,1))+
  theme_minimal(base_size = 16) +
  theme (plot.title = element_text ( size = 30), #hjust = 0.5,
         axis.title.y = element_text (size=18), 
         axis.title.x = element_text (size=18), 
         legend.title = element_text(size = 20),
         legend.text = element_text (size =16))+
  labs (title = "Immediate sowing", y= "Mean Germination", x = "WP Treatment") 
  

# cumulative germination curve with ggplot
df%>%
  mutate(WP_treatment = factor(WP_treatment))%>%
  mutate(WP_treatment = fct_relevel(WP_treatment,"0", "-0.2", "-0.4", "-0.6", "-0.8", "-1", "-1.2" ))%>%
  select(ID, WP_treatment, petri,viable,  days, germinated)%>%
  group_by( WP_treatment, days)%>% #ID,
  summarise(viable = sum(viable), germinated = sum(germinated))%>%
  mutate(cumsum= cumsum(germinated))%>%
  mutate(germPER = (cumsum/viable)*100)%>%
  #filter(ID=="A00") %>%
  ggplot(aes(x=days, y=germPER, group = WP_treatment, color= WP_treatment))+
  geom_line(size = 2) +
  coord_cartesian(ylim = c(0,100))+
  theme_minimal(base_size = 16) +
  theme (plot.title = element_text ( size = 30), #hjust = 0.5,
         axis.title.y = element_text (size=18), 
         axis.title.x = element_text (size=18), 
         legend.title = element_text(size = 20),
         legend.text = element_text (size =16))+
  labs (title = "", y= "Germination %", x = "Days") 

#base water potential
immediate_bwpsummary %>%
  merge(bioclim, by= c("ID")) %>%
  as.data.frame()-> graph
str(graph)

ggplot (graph, aes(x=GDD, y= psib50)) +
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, level = 0.9)+
  labs( title= "WPb per GDD")

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
#exploratory visualization
x11()
str(after_germsummary) 
after_germsummary%>%
  mutate(treatment = factor(treatment))%>%
  mutate(treatment = fct_relevel(treatment,"0", "-0.2", "-0.4", "-0.6", "-0.8", "-1", "-1.2" ))%>%
  as.data.frame ()-> graph
str(graph)  

#final germination x treatment x ID graph
ggplot(graph) +
  geom_point(aes(x= treatment, y =germination.mean, color = treatment), size = 3) +
  #geom_errorbar(aes(x= treatment, ymin = germination.lower, 
                    #ymax = germination.upper, color = treatment),width = 0.2, size =1.2)  +
  coord_cartesian(ylim = c(0,1))+
  theme_minimal(base_size = 16) +
  theme (plot.title = element_text ( size = 30), #hjust = 0.5,
         axis.title.y = element_text (size=18), 
         axis.title.x = element_text (size=18), 
         legend.title = element_text(size = 20),
         legend.text = element_text (size =16))+
  labs (title = "After-ripening sowing", y= "Mean Germination", x = "WP Treatment")
# cumulative germination curve with ggplot
df2%>%
  mutate(WP_treatment = factor(WP_treatment))%>%
  mutate(WP_treatment = fct_relevel(WP_treatment,"0", "-0.2", "-0.4", "-0.6", "-0.8", "-1", "-1.2" ))%>%
  select(ID, WP_treatment, petri, N_seeds,  days, germinated)%>% #viable,
  filter(!days>16)%>%
  group_by(WP_treatment, days)%>% #ID,
  summarise(viable = sum(N_seeds), germinated = sum(germinated))%>%
  mutate(cumsum= cumsum(germinated))%>%
  mutate(germPER = (cumsum/viable)*100)%>%
  ggplot(aes(x=days, y=germPER, group = WP_treatment, color= WP_treatment))+
  geom_line(size = 2) +
  coord_cartesian(ylim = c(0,100))+
  theme_minimal(base_size = 16) +
  theme (plot.title = element_text ( size = 30), #hjust = 0.5,
         axis.title.y = element_text (size=18), 
         axis.title.x = element_text (size=18), 
         legend.title = element_text(size = 20),
         legend.text = element_text (size =16))+
  labs (title = "", y= "Germination %", x = "Days") 

#base water potential
after_bwpsummary %>%
  merge(bioclim, by= c("ID")) %>%
  as.data.frame()-> graph

str(graph)

ggplot (graph, aes(x=GDD, y= psib50)) +
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, level = 0.9)+
  labs( title= "WPb per GDD")

# Visual comparison both sowings data
after_bwpsummary %>%
  mutate(sowing = "After_ripening") -> after_bwpsummary

immediate_bwpsummary%>%
  mutate(sowing = "Immediate") -> immediate_bwpsummary
rbind(immediate_bwpsummary, after_bwpsummary)%>%
  merge(read.csv("data/Dianthus_header.csv", sep=";"))%>%
  filter((Immediate=="Yes" & After_ripening =="Yes"))%>%
  select(ID, sowing, theta, psib50, sigma, R2)%>%
  ggplot(aes(x=ID, y= psib50, group=sowing, color=sowing)) +
  geom_point (size= 3)+
  theme_minimal(base_size = 16) +
  theme (plot.title = element_text ( size = 30), #hjust = 0.5,
         axis.title.y = element_text (size=18), 
         axis.title.x = element_text (size=18), 
         legend.title = element_text(size = 20),
         legend.text = element_text (size =16))
  

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
