library(tidyverse)
# install.packages("devtools")
# devtools::install_github("efernandezpascual/seedr")
library (seedr)
library (stringr)

#load data + transformation data
read.csv ("data/data290823.csv", sep= ";") %>%
  gather ("times", "germinated", D1:D10)%>% #transform from wide to long format
  mutate (times = gsub("D", "", times)) %>%
  mutate (times = as.numeric(times),
          ID = as.factor (ID), 
          treatment = as.numeric (treatment)) %>%
  na.omit %>% 
  filter (treatment >= -1.2) -> df # remove "D" from time column
str(df)  

# create physiodata object
test_df <- physiodata (d = df, # the name of your data object
                        t = "times", # the column with the scoring times
                        x = "treatment", # the column with the experimental treatment,
                        groups = c("species", "ID"), # this dataset has X different species/populations
                        g = "germinated", # the column with the number of germinated seeds,
                        pg = "N_seeds") # the column with the total number of viable seeds
#plot(test_df)


physiotime (d = df, # the name of your data object
            t = "times", # the column with the scoring times
            x = "treatment", # the column with the experimental treatment,
            groups = c("species", "ID"), # this dataset has X different species/populations
            g = "germinated", # the column with the number of germinated seeds,
            pg = "N_seeds") -> o1 # the column with the total number of viable seeds

#plot(o1)

o1

# Populations with positive Wb!!

df %>%
  filter(ID == "B03") -> df1

test_df <- physiodata (d = df1, # the name of your data object
                       t = "times", # the column with the scoring times
                       x = "treatment", # the column with the experimental treatment,
                       groups = c("species", "ID"), # this dataset has X different species/populations
                       g = "germinated", # the column with the number of germinated seeds,
                       pg = "N_seeds") # the column with the total number of viable seeds

physiotime (d = df1, # the name of your data object
            t = "times", # the column with the scoring times
            x = "treatment", # the column with the experimental treatment,
            groups = c("species", "ID"), # this dataset has X different species/populations
            g = "germinated", # the column with the number of germinated seeds,
            pg = "N_seeds") -> o1 # the column with the total number of viable seeds

plot(o1)
plot(test_df)

# Remove lowest potentials!!

df %>%
  filter(ID == "B03") %>%
  filter(treatment > -0.5) -> df1

test_df <- physiodata (d = df1, # the name of your data object
                       t = "times", # the column with the scoring times
                       x = "treatment", # the column with the experimental treatment,
                       groups = c("species", "ID"), # this dataset has X different species/populations
                       g = "germinated", # the column with the number of germinated seeds,
                       pg = "N_seeds") # the column with the total number of viable seeds

physiotime (d = df1, # the name of your data object
            t = "times", # the column with the scoring times
            x = "treatment", # the column with the experimental treatment,
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
                       t = "times", # the column with the scoring times
                       x = "treatment", # the column with the experimental treatment,
                       groups = c("species", "ID"), # this dataset has X different species/populations
                       g = "germinated", # the column with the number of germinated seeds,
                       pg = "N_seeds") # the column with the total number of viable seeds

physiotime (d = df1, # the name of your data object
            t = "times", # the column with the scoring times
            x = "treatment", # the column with the experimental treatment,
            groups = c("species", "ID"), # this dataset has X different species/populations
            g = "germinated", # the column with the number of germinated seeds,
            pg = "N_seeds",
            method = "huidobro") -> o1 # the column with the total number of viable seeds

plot(o1)
plot(test_df)
o1

df %>%
  filter(ID == "B03") %>%
  group_by(dish, treatment) %>%
  mutate(g = cumsum(germinated)) %>%
  arrange(dish, treatment) %>%
  mutate(level = paste(treatment, dish)) %>%
  ggplot(aes(times, g, color = as.factor(treatment), group = level)) +
  geom_line(size = 2)
