library(tidyverse)
install.packages("devtools")
devtools::install_github("efernandezpascual/seedr")
library (seedr)
library (stringr)

#load data + transformation data
read.csv ("data/data290823.csv", sep= ";") %>%
  gather ("times", "germinated", D1:D11)%>% #transform from wide to long format
  mutate (times = gsub("D", "", times)) %>%
  mutate (times = as.numeric(times),
          ID = as.factor (ID), 
          treatment = as.numeric (treatment)) %>%
  filter(species == "Dianthus langeanus") %>%
  filter (treatment > -0.6) -> df # remove "D" from time column
str(df)  

# create physiodata object
test_df <- physiodata (d = df, # the name of your data object
                        t = "times", # the column with the scoring times
                        x = "treatment", # the column with the experimental treatment,
                        groups = "ID", # this dataset has X different species/populations
                        g = "germinated", # the column with the number of germinated seeds,
                        pg = "N_seeds") # the column with the total number of viable seeds
