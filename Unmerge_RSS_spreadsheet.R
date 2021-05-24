################################################################
#######     RSS - Manage RSS Database   ########################
################################################################

rm(list = ls())

library(dplyr)
library(tidyr)


# ---- Make each row of .csv file contain all relevant data (unmerge) -------- #

df <- read.csv('Activities by Priority.csv')

df <- df %>%
  select(-(starts_with("X"))) %>%
  rename("Region" = "ï..Region") 

df[df==""] <- NA

df <- df %>%
  fill(everything())
  


write.csv(df, file = "./Output/Activities_by_Priority.csv")
