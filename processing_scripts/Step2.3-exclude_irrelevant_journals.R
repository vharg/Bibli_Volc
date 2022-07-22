# Calculate the percentage of articles from each journal/source 
# that contain a volcano name - use this as basis for exclusion of journals
# that are not relevant to volcanology
# March 2022

library(tidyverse)
library(readxl)

# load and prepare data ----
# read in all articles from original data set
rd_all <- readRDS('raw_data/all_articles.Rdata') %>% 
  
  # calculate number of articles per journal/source
  group_by(`Source Title`) %>% 
  tally() %>% 
  dplyr::rename(total_art = n)

# read and bind the data for volcanoes with duplicate and non-duplicate names
rd <- rbind(read_csv('data_processed/extracted_dup_volc_tk.csv'),
            read_csv('data_processed/extracted_volc_tk.csv')) 

# for each journal/source, calculate number of articles which name volcanoes
journals <- rd %>%
  # for articles that name mulitple volcanoes, drop the duplicate article titles
  # to avoid articles that name multiple volcanoes driving up the volc_name %
  distinct(`Article Title`, .keep_all = TRUE) %>%
  group_by(`Source Title`) %>% 
  tally() %>% 
  dplyr::rename(volc_art = n)

# Calculate percentage of articles that include a volcano name per journal
df <- left_join(journals,rd_all) %>% 
  mutate(volc_name_pt = volc_art/total_art) %>% 
  # lets not filter by number of articles - it excludes huge amounts of data
  #filter(volc_art < 10) %>% # drops 4000 articles out of 32,000
  filter(volc_name_pt > 0.05) #%>% # 10% = 1700 dropped; 5% = 362 dropped

# Check which articles come from relevant journals and drop the rest
df_drop <- left_join(rd,df) %>% 
  drop_na(volc_art) %>% 
  # delete the new columns that have been added so that the
  # structure of the data remains unchanged
  dplyr::select(-volc_art:-volc_name_pt)

# Calculate how many articles have been dropped
print(nrow(rd)-nrow(df_drop))

# Save newly binded and filtered data to csv for affiliation country extraction
saveRDS(df_drop, 'data_processed/extracted_volc_J_filt.Rdata')
  
