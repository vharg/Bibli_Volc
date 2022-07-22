# How does inclusion change over time for various volcanoes
# Oct 2021

library(tidyverse)
library(readxl)
library(plotly)
library(zoo)
library(ggsci)
library(ghibli)

# load data
rd <- readRDS('data_processed/lead_and_include_tk.Rdata') 

df <- rd %>% 
  group_by(v_name) %>% 
  mutate(ones = 1) %>% 
  mutate(total_articles = sum(ones)) %>% 
  mutate(total_leading = sum(lead_match)) %>% 
  mutate(total_including = sum(matching))
  
ranks <- df %>% 
  select(total_articles, v_name) %>% 
  unique() %>% 
  ungroup() %>% 
  mutate(v_rank = rank(desc(total_articles), ties.method = 'first')) %>% 
  arrange(desc(total_articles))%>% 
  mutate(v_group = v_rank/6) %>% 
  mutate(v_group = ceiling(v_group)) %>% 
  filter(total_articles >= 50) %>% 
  rename(grand_total_articles = total_articles)

v_names_clean <- read_excel('raw_data/GVP.xlsx') %>% 
  select(`Volcano Name`, Original_name) %>% 
  rename(v_name = `Volcano Name`)

write_csv(ranks, 'figs/articles_per_volc.csv')

df$lead_country <- sapply(strsplit(df$pub_countries, ","), "[", 1)

df_sub <- df

# calculate solo country authorship on local volcanoes
df <- df_sub %>%
  mutate(ones = 1) %>%
  group_by(lead_country) %>%
  mutate(total_leading = sum(ones)) %>%
  # count number of countries extracted per article
  mutate(country_count = 1 + str_count(string = pub_countries,
                                       pattern = ",")) %>%
  # check if all countries are the same
  # i.e. check if all countries match the lead country
  mutate(same_count = str_count(string = pub_countries,
                                pattern = lead_country)) %>%
  #create column full of zeroes which will be populated with ones when local only is true
  mutate(only_local = 0) %>%
  ungroup()

# label articles where all authors are from the same country
# AND the lead country matches the volcano country
df$only_local[which(df$country_count == df$same_count &
                      df$lead_match == 1)]=1


# selected countries as percentage ----------------------------------------

pct_select <- df %>%

  filter(total_articles >= 50) %>% 
  filter(!is.na(`Publication Year`)) %>% 
  # calculate grand total of articles published per volcano over all time
  group_by(v_name) %>% 
  mutate(grand_total_articles = n()) %>% 
  # create label with v_country and grand total articles for plotting
  mutate(facet_title =  paste(v_name, "   (n =", grand_total_articles,")" ,sep = "")) %>% 
  
  group_by(`Publication Year`, v_name) %>% 
  mutate(total_articles = n()) %>% 
  mutate(total_leading = sum(lead_match)) %>% 
  mutate(total_including = sum(matching)) %>%
  mutate(total_solo = sum(only_local)) %>% 
  select(`Publication Year`, v_name,facet_title, grand_total_articles, total_articles,total_including,total_leading, total_solo) %>%
  unique() %>% 
    # calculate percentages
  mutate(lead_pt = ((total_leading/total_articles)*100),
         incl_nolead_pt = (((total_including-total_leading)/total_articles)*100),
         incl_pt = ((total_including/total_articles)*100),
         solo_pt = ((total_solo/total_articles)*100)) %>% 
  mutate(no_involvement_pt = 100 -(incl_pt)) %>% 
  arrange(v_name,`Publication Year`) %>% 
  ungroup() %>% 
    # now calculate rolling mean of inclusion metrics  
    # and cumulative percentage of grand total articles (gta) published over time
    group_by(facet_title) %>% 
    # cumulative sum of articles
    mutate(gta_pt = (cumsum(total_articles)/grand_total_articles)*100) %>% 
  mutate(rm_lead = rollmean(lead_pt,k=3,align = 'right', fill = 1),
         rm_incl_nolead = rollmean(incl_nolead_pt,k=3,align = 'right',fill = 1),
         rm_incl = rollmean(incl_pt,k=3,align = 'right', fill = 1),
         rm_no_in = rollmean(no_involvement_pt,k=3,align = 'right',fill = 1),
         rm_solo = rollmean(solo_pt,k=3,align = 'right',fill = 1)) %>%
  mutate(pub_year = `Publication Year`) %>%
  #add in years with no publications
  complete(pub_year = seq(min(pub_year),max(pub_year),by = 1)) %>% 
  #forward fill data for years with no publications
  fill(c(total_articles:last_col()), .direction = 'down') %>% 
  pivot_longer(cols=c('rm_lead', 'rm_incl','rm_incl_nolead','rm_solo'), names_to = 'metric', values_to = 'rolling') %>% 
  filter(pub_year >= 1990 & pub_year < 2021) %>% 
  # add in volcano ranks (by n article) to break up plots into groups
  left_join(.,ranks, by = c('v_name','grand_total_articles')) %>%
  # add in clean volcano names from GVP
  left_join(.,v_names_clean, by ='v_name') %>%
  # for v_names that were modified in R (and could not be joined with original GVP names)
  # use the modified name from R
  mutate(Original_name = coalesce(Original_name,v_name)) %>%
  # fill in missing missing rows for Original name - for years with no publications
  group_by(facet_title) %>% 
  #first fill down
  fill(c('v_name','Original_name','grand_total_articles'),.direction = 'down') %>% 
  # then fill up
  fill(c('v_name','Original_name','grand_total_articles'),.direction = 'up') %>% 
  # deselect then re-attach v_rank and v_group
  ungroup() %>%
  select(-v_rank,-v_group) %>% 
  left_join(.,ranks, by = c('v_name'))

# clean up the remaining names to keep only the name before the first "|" symbol
pct_select$Original_name <-  sapply(str_replace_all(pct_select$Original_name, "\\|",","), toString)
pct_select$Original_name <- sapply(strsplit(pct_select$Original_name, ","), "[", 1)

# recreate facet title with n articles and corrected v_names
pct_select <- pct_select %>%
  mutate(facet_title = paste(Original_name,'   n = ',grand_total_articles.y, sep = ""))

for (i in 1:max(pct_select$v_group)) {
  filename <- sprintf('figs/supplement_figs/volc_timeseries%s.pdf',i)
  print(filename)
  
  df_plot <- pct_select %>% 
    filter(v_group == i)

ggplot(df_plot)+
  geom_line(aes(x=pub_year, y = gta_pt), color = 'grey50', lty = 'dotted')+
  geom_line(aes(x=pub_year, y = rolling,color = metric))+
  facet_wrap(~facet_title,
             ncol = 2)+ #, scales = 'free_y'
  theme_light()+
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        strip.text = element_text(colour = 'white'),#'grey10'
        #strip.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(y = 'Percentage of articles',
       x = '\nPublication year')+
  xlim(c(1990,2021))+
  scale_color_manual(labels = c("Involved","Involved (not leading)",
                                "Leading", 'All LDAs'),
                     values = c("cornflowerblue","darkmagenta", "goldenrod","red2"))+
  # scale_color_ghibli_d(name = 'PonyoMedium',direction =1,
  #                      labels = c("Involved","Involved (not leading)",
  #                                 "Leading", 'All local authors'))
    
  
ggsave(filename = filename,height = 5.5, width = 5, units = 'in')

  }




