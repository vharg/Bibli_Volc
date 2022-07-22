# Standardising Scopus data prior to analysis
# And make Scopus column titles the same as Web of Science
# GW
# Sept 2021

library(tidyverse)
library(tm)

# select, order and properly name required data ---------------

# columns from WoS for which we want the equivalent from Scopus
# Authors,`Article Title`, `Publication Year`, Abstract, 
# Addresses, DOI, `Times Cited, All Databases`,`Source Title`

jav <- read_csv('raw_data/scopus_JAV_stripped.csv') %>% 
  
  # first delete rows that have been formatted incorrectly from Scopus download
  mutate(Years = as.numeric(paste(Year))) %>% 
  filter(!is.na(Years)) %>% 
  select(-Years) %>% 
  
  # then select the columns that are needed + keywords for 'tak'
  select(Authors,Title, Year, Abstract, Affiliations,
         DOI, `Cited by`, `Source title`, 
         `Author Keywords`, `Index Keywords`) %>% 
  
  # rename columns to match WoS
  dplyr::rename('Article Title' = 2,
         'Publication Year' = 3,
         'Addresses' = 5,
         'Times Cited, All Databases' = 7,
         'Source Title' = 8)


# now repeat for Volcanica
volcanica <- read.csv('raw_data/scopus_volcanica_stripped.csv') %>%
  
  # first delete rows that have been downloaded incorrectly
  mutate(Years = as.numeric(paste(Year))) %>% 
  filter(!is.na(Years)) %>% 
  select(-Years) %>% 
  
  #then select the columns that are needed + keywords for 'tak'
  select(Authors,Title, Year, Abstract, Affiliations,
         DOI, `Cited.by`, `Source.title`, 
         `Author.Keywords`, `Index.Keywords`) %>% 
  
  # rename columns to match WoS
  dplyr::rename('Article Title' = 2,
         'Publication Year' = 3,
         'Addresses' = 5,
         'Times Cited, All Databases' = 7,
         'Source Title' = 8,
         'Author Keywords' = 9,
         'Index Keywords' = 10)


# create TAK field - title, abstract, keywords ----------------
df <- rbind(jav,volcanica) %>% 
  # drop duplicate Article titles
  distinct(`Article Title`, .keep_all = T) %>%
  mutate(tak = paste(" ", `Article Title`,
                     `Index Keywords`,`Author Keywords`, " "))
  
  
# change all the text to title case to avoid capitalisation issues
df$tak <- sapply(str_to_upper(df$tak),toString)

# keep dashes in place for now
df$tak <- sapply(removePunctuation(
  df$tak,preserve_intra_word_dashes = T),toString)

# instead, replace dashes with spaces e.g. Nevado-del-Ruiz --> Nevado del Ruiz
df$tak <- sapply(str_replace_all(string =df$tak, pattern = '-', replacement = ' '),toString)

df$`Article Title` <- sapply(str_to_upper(df$`Article Title`),toString)
df$`Article Title` <- sapply(removePunctuation(
  df$`Article Title`,preserve_intra_word_dashes = T),toString)

df$tak[1]

# delete keywords columns now that they are within tak
df <- df %>% 
  select(-`Index Keywords`,
         -`Author Keywords`)

saveRDS(df,'raw_data/scopus_tk.Rdata')
