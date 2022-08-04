# Check how volcano countries and affiliation countries compare
# Oct 2021

library(tidyverse)
library(readxl)
#library(plotly)


# Inclusion analysis ------------------------------------------------------
rd <- readRDS('data_processed/countries_extracted_tk.Rdata')

# delete any duplicate articles that may exist in the data
# and drop Antarctic volcanoes
# and drop articles that have been published in 2022 for our analysis to be complete up until end 2021
df <- rd %>% 
  distinct() %>% 
  filter(!v_country == 'Antarctica') %>% 
  filter(is.na(`Publication Year`) | `Publication Year` < 2022) # keep the 67 rows with NA pub year

# for any volcano that borders multiple countries, replace commas with "|"
# so that affiliations based in either country will result in a match
df$v_country <- str_replace_all(string = df$v_country, pattern =', ',replacement = '|')

matching_pubs <- df %>%
  # delete any articles that have no affiliation info
  drop_na(affil) %>%  
  # extract v_country from pub_countries if it is there
  mutate(matching = str_extract_all(str_to_upper(pub_countries),str_to_upper(v_country))) %>%
  # delete non-matching articles
  drop_na(matching) %>%
  # assign a value of 1 to each mathcing article - so we can sum values later
  mutate(matching = 1) %>% 
  select(`Article Title`, v_name, matching)
  
volcano_pubs <- left_join(df,matching_pubs, by = c("Article Title","v_name")) %>%
  # delete any articles that have no affiliation info
  filter(!pub_countries == 'NA') %>%
  # delete duplications introduced by table joining
  distinct(`Article Title`,v_name, .keep_all = T)

# change NA into 0 for adding up countries that don't match
volcano_pubs$matching[is.na(volcano_pubs$matching)] <- 0
paste(sum(volcano_pubs$matching), 'instances of matching v_country and author countries')

## we know a number of articles name multiple volcanoes
## we can classify any article that has at least one researcher...
## from one or more of the named volcano countries as inclusive - generous classification
volcano_pubs_unique <- volcano_pubs %>% 
  # arrange data so inclusive articles (1s) are higher in table
  ## remove 'desc()' function to classify inclusivity with strict definition - (0s are higher in table)

  arrange(desc(`Article Title`, matching)) %>%  # use this line for Generous definition
  #arrange(`Article Title`, matching) %>% # use this line for Strict definition
  
  # for articles that name multiple volcanoes, 
  # distinct selects only the first instance of each article 
  distinct(`Article Title`, .keep_all = T)

# Inclusion summary ----
## What percentage of articles include at least one local researcher
paste(round(sum(volcano_pubs_unique$matching)/nrow(volcano_pubs_unique)*100), 
      'percent of articles are classified as inclusive.',
      sum(volcano_pubs_unique$matching),'out of',nrow(volcano_pubs_unique), 'articles')

## Calculating by instance of volcano name extraction instead of per individual article
## This is the main method we use in the paper
paste(round(sum(volcano_pubs$matching)/nrow(volcano_pubs)*100), 
      'percent of volcano name extractions have at least one local author.',
      sum(volcano_pubs$matching),'out of',nrow(volcano_pubs), 'mentions')


# What proportion of articles on each country that contains studied volcanoes...
# include authors from the country/countries the volcano is located in?
prevalence <- volcano_pubs %>% 
  group_by(v_country) %>%
  mutate(total_matching = sum(matching)) %>% 
  mutate(ones = 1) %>% 
  mutate(total_articles = sum(ones)) %>% 
  select(-ones) %>% 
  ungroup()

tidy_prevalence <- prevalence %>% 
  select(v_country,total_matching,total_articles) %>% 
  unique %>% 
  filter(total_articles >50) %>% 
  pivot_longer(cols = c('total_articles','total_matching'),
               names_to = 'metric',values_to = 'n_articles')

gprev <- ggplot(tidy_prevalence)+
  geom_col(aes(x=n_articles, y = reorder(v_country,n_articles), fill = metric), 
           position = 'dodge')+
  labs(x = 'Number of articles',
      y = '')+
  ggtitle('Countries with over 50 articles')+
  theme(legend.title = element_blank())

gprev
# #ggsave('figs/involved.png', height = 5, units = 'in')
# 
# p <- plotly::ggplotly(gprev)
# 
# htmlwidgets::saveWidget(p, "Inclusionover50.html")


# Lead author analysis ----------------------------------------------------
# which country is the lead author affiliated with?
prevalence$lead_country <-sapply(strsplit(prevalence$pub_countries, ","), "[", 1)

# does it match the volcano country?
# get articles where first affiliation country matches the volcano country
la_match <- prevalence %>%
  ungroup %>% 
  mutate(lead_match = str_extract_all(string = str_to_upper(lead_country),
                                      pattern = str_to_upper(v_country))) %>%
  filter(!lead_match == 'character(0)') %>% 
  mutate(lead_match = 1) %>% 
  select(`Article Title`,v_name,lead_match)

# label whether an articles is led by or includes local authors
all_pubs <- left_join(df,la_match, by = c("Article Title", "v_name")) %>% 
  left_join(.,matching_pubs, by = c("Article Title","v_name")) %>% 
  filter(!pub_countries == '') %>% 
  filter(!pub_countries == 'NA')

# filter for articles that contain a volcano name in the title
lead_pubs <- all_pubs %>% 
  filter(!v_name == '')

# change NA values to zeros for calculations
lead_pubs$lead_match[is.na(lead_pubs$lead_match)] <- 0
lead_pubs$matching[is.na(lead_pubs$matching)] <- 0

leading <- lead_pubs %>% 
  group_by(v_country) %>%
  mutate(total_leading = sum(lead_match)) %>%
  mutate(total_including = sum(matching)) %>% 
  mutate(ones = 1) %>% 
  mutate(total_articles = sum(ones)) %>%
  select(-ones)

# save the dataset
saveRDS(leading, 'data_processed/lead_and_include_tk.Rdata')
#write_csv(leading, 'data_processed/lead_and_include_tk.csv')


# leadership summary ----

## Calculating by instance of volcano name extraction instead of per individual article
## This is the main method we use in the paper
paste(round(sum(leading$lead_match)/nrow(leading)*100), 
      'percent of volcano name extractions are from an article led by a local author.',
      sum(leading$lead_match),'out of',nrow(leading), 'mentions')

