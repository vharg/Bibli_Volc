# Script to bind all WoS downloads into one file
# WoS only allows 1000 articles to be downloaded at once with the data we're after
# This code will allow you to put all the downloads into one csv 
# GW
# Sep 2021

# load required libraries
library(tidyverse)
library(readxl)

# reading and writing the data ----
# start by making an empty csv file to put all your data into
## note that you will have to do this twice
## once for the first 75 downloads AND once for the last 75  
## because WoS can't store more than 100 consecutive searches and we needed 150 to capture all 150,000 articles
empty <- data.frame()
write_csv(empty,'raw_data/newest_all.csv', append = F)

# add headers to the empty csv
headers <- read_excel('raw_data/newest_WoS/1.xls', col_names = T) %>% 
  # delete all the data - leaving only the column names
  filter(row_number() == 0) %>% 
  # drop the final column, which is empty for some reason
  select(-last_col())

write_csv(headers,'raw_data/newest_all.csv', append = F)

# read each of the xls files you've downloaded and add their data one at a time into empty csv file
for (i in 1:75){
  
  # give path to file with article data in it
  path <- sprintf('raw_data/newest_WoS/%s.xls',i)
  print(path)
  
  # read the file
  rd <- read_excel(path = path)
  
  # check if there are 68 rows and remove the 68th row
  rd <- rd %>% select(-68)
  
  # write it to the csv file with append = TRUE
  write_csv(rd,'raw_data/newest_all.csv', append = TRUE)
}

# read in the new csv to check the loop worked properly

df <- read_csv('raw_data/newest_all.csv')


## repeat for oldest data ----
empty <- data.frame()
write_csv(empty,'raw_data/oldest_all.csv', append = F)

# add headers to the empty csv
headers <- read_excel('raw_data/oldest_WoS/1.xls', col_names = T) %>% 
  # delete all the data - leaving only the column names
  filter(row_number() == 0) %>% 
  # drop the final column, which is empty for some reason
  select(-last_col())

write_csv(headers,'raw_data/oldest_all.csv', append = F)

# read each of the xls files you've downloaded and add their data one at a time into empty csv file
for (i in 1:75){
  
  # give path to file with article data in it
  path <- sprintf('raw_data/oldest_WoS/%s.xls',i)
  print(path)
  
  # read the file
  rd <- read_excel(path = path)
  
  # check if there are 68 rows and remove the 68th row
  rd <- rd %>% select(-68)
  
  # write it to the csv file with append = TRUE
  write_csv(rd,'raw_data/oldest_all.csv', append = TRUE)
}

# read in the new csv to check the loop worked properly

df <- read_csv('raw_data/oldest_all.csv')

## bind oldest and newest csv files into one Rdata file ----

new <- read_csv('raw_data/newest_all.csv')
old <- read_csv('raw_data/oldest_all.csv')

all <- rbind(new,old)

# drop duplicate entries that result from overlapping oldest and newest downloads from WoS
dall <- all %>%
  distinct()

# save as Rdata for faster loading
saveRDS(dall,'raw_data/volcan_star.Rdata')
