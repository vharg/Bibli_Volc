# Create plots showing inclusion and leadership per country
# updated to check how data looks for past 3-5 years since Volcanica started
# July 2022

library(tidyverse)
library(tools) # need for toTitleCase
library(ggsci)


# leadership and inlcusion prep ----------

rd <- readRDS('data_processed/lead_and_include_tk.Rdata') %>% 
  ungroup

# when did volcanica first start publishing? 2018
volcanica <- rd %>% 
  filter(`Source Title` == 'Volcanica')

df_int <- rd %>%
  rename(journal = `Source Title`) %>%
  group_by(journal) %>% 
  
  # calculate totals for each journal
  mutate(total_leading = sum(lead_match)) %>% 
  mutate(total_including = sum(matching)) %>% 
  mutate(total_articles = n()) %>% 
  
  # calculate percentages for each journal
  mutate(pt_lead = (total_leading/total_articles)*100) %>% 
  mutate(pt_inc = (total_including/total_articles)*100) %>% 
  select(journal, total_articles,last_col(1):last_col()) %>% 
  unique() %>% 
  ungroup() %>% 
  filter(total_articles >= 20)

df_int_18 <- rd %>%
  # keep only articles published since 2018 to compare Volcanica more fairly with other journals
  filter(`Publication Year` >= 2018) %>% 
  rename(journal = `Source Title`) %>%
  group_by(journal) %>% 
  
  # calculate totals for each journal
  mutate(total_leading = sum(lead_match)) %>% 
  mutate(total_including = sum(matching)) %>% 
  mutate(total_articles = n()) %>% 
  
  # calculate percentages for each journal
  mutate(pt_lead = (total_leading/total_articles)*100) %>% 
  mutate(pt_inc = (total_including/total_articles)*100) %>% 
  select(journal, total_articles,last_col(1):last_col()) %>% 
  unique() %>% 
  ungroup()

# rank the journals by number of articles, filter for top 15 + a few extras
df <- df_int %>% 
  mutate(j_rank = rank(desc(total_articles),ties.method = 'first')) %>% 
  filter(j_rank <= 15 |
           journal == 'Volcanica'| 
           journal == 'Journal of Applied Volcanology'|
           journal == 'NATURE'|
           journal == 'NATURE COMMUNICATIONS'|
           journal == 'EARTH-SCIENCE REVIEWS') %>% 
  # rank by inclusivity for plotting purposes
  mutate(rank_inc = rank(desc(pt_inc), ties.method = 'first')) %>% 
  # pivot longer for plotting
  pivot_longer(cols = c(pt_lead,pt_inc), names_to = 'metric', values_to = 'pt')

# rank the journals by number of articles, filter for top 15 + a few extras
df_18 <- df_int_18 %>% 
  mutate(j_rank = rank(desc(total_articles),ties.method = 'first')) %>% 
  # rank by inclusivity for plotting purposes
  mutate(rank_inc = rank(desc(pt_inc), ties.method = 'first')) %>% 
  # pivot longer for plotting
  pivot_longer(cols = c(pt_lead,pt_inc), names_to = 'metric', values_to = 'pt') %>% 
  select(1,2,5,6) %>% 
  rename(pt_18 = pt,
         total_articles_18 = total_articles)

# Comparing top journals to all journals ----

top_j_list <- df %>% 
  select(journal, j_rank) %>% 
  distinct()

articles_top_label <- rd %>% 
  rename(journal = `Source Title`) %>% 
  left_join(.,top_j_list)

top_articles <- articles_top_label %>% 
  drop_na(j_rank)

paste('20 key journals account for', nrow(top_articles)/nrow(rd), 'of articles')

# calculate journal inclusivity for these 20 as one group
ta_inc <- top_articles %>%
  mutate(total_articles = n(),
         total_including = sum(matching),
         total_leading = sum(lead_match)) %>% 
  mutate(pt_lead = total_leading/total_articles,
         pt_inc = total_including/total_articles) %>% 
  select(last_col(1):last_col()) %>% 
  distinct() %>% 
  view
  



# make journal titles nicer to read ----
df$journal= str_to_sentence(df$journal) # title case for all words
df$journal = toTitleCase(df$journal) # change Of to of

df_18$journal= str_to_sentence(df_18$journal) # title case for all words
df_18$journal = toTitleCase(df_18$journal) # change Of to of

df_plot <- df %>% 
  left_join(.,df_18, by = c('journal','metric'))

# the plot of all time ----------------------------------------------------------------

gg_all <- ggplot(df_plot)+
  geom_point(aes(x=pt,y=reorder(journal,rank_inc),color = metric, size = total_articles), alpha =0.9)+
  theme_minimal()+
  theme(legend.position = 'top',
        axis.title.y = element_blank(),
        legend.title = element_blank())+
  labs(x = '\nInclusivity percentage')+
  scale_colour_brewer(palette = 'Set1',
                      breaks = c('pt_lead',
                                 'pt_inc'),
                      labels = c('Led by LDA',
                                 'LDA(s) included'))+
  guides(size = FALSE)+
  geom_segment(aes(x=pt_18, xend = pt, 
                   y=journal, yend = journal,
                   color = metric), alpha =0.5)+
  geom_point(aes(x=pt_18, y = journal, color = metric, size = total_articles_18),pch = 4)

gg_all

ggsave('figs/Fig9_journal_inclusivity_both.png')


df_wide <- df %>% 
  pivot_wider(id_cols= c(1:4),
              names_from = metric, values_from = pt)

mean(df_wide$pt_inc)
mean(df_wide$pt_lead) 
# save csv for sharing
write_csv(df_wide,'data_processed/journal_inclusivity_2018.csv')


