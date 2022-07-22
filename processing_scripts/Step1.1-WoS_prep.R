# Standardising WoS volcan* data prior to analysis
# GW
# August 2021

library(tidyverse)
library(readxl)
library(tm)

# load data ----
rd <- readRDS('raw_data/volcan_star.Rdata')

# combine title and keywords into one string to expand search info
df <- rd %>%
  mutate(tak = paste(" ", `Article Title`,`Keywords Plus`,`Author Keywords`, " ")) %>% 
  mutate('Article Title' = paste(" ", `Article Title`, " ")) 

# delete duplicate articles based on title
df_dist <- df %>% 
  distinct(`Article Title`, .keep_all = T)

# change text to upper case to avoid capitalisation issues
df_dist$tak <- sapply(str_to_upper(df_dist$tak),toString)

# remove punctuation but keep intra word dashes in place
df_dist$tak <- sapply(removePunctuation(df_dist$tak,preserve_intra_word_dashes = T),toString)

# instead, replace dashes with spaces e.g. Nevado-del-Ruiz --> Nevado del Ruiz
df_dist$tak <- sapply(str_replace_all(string =df_dist$tak, pattern = '-', replacement = ' '),toString)

# Also capitalise titles on their own
df_dist$`Article Title` <- sapply(str_to_upper(df_dist$`Article Title`),toString)
df_dist$`Article Title` <- sapply(removePunctuation(df_dist$`Article Title`,preserve_intra_word_dashes = T),toString)

saveRDS(df_dist,file = 'raw_data/volcan_star_tk.Rdata')
