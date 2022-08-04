# How does inclusion change over time in various countries
# Oct 2021

library(tidyverse)
library(readxl)
library(plotly)
library(zoo)
library(ggsci)
library(ghibli)

# load data and prep data ----
rd <- readRDS('data_processed/lead_and_include_tk.Rdata') %>% 
  group_by(v_country) %>% 
  mutate(ones = 1) %>% 
  mutate(art_per_v_country = sum(ones)) %>% 
  filter(art_per_v_country >= 50) %>% 
  ungroup()

#check how many articles USA has - should be 7510 as per Figure 2
check_USA <- rd %>% 
  filter(v_country == 'USA')

# how many after dropping those with no pub year metadata? should be 7500
check_USA <- rd %>% 
  filter(v_country == 'USA') %>% 
  drop_na(`Publication Year`)
  
ranks <- rd %>% 
  select(art_per_v_country, v_country) %>% 
  unique() %>% 
  ungroup() %>% 
  mutate(c_rank = rank(desc(art_per_v_country))) 
  #arrange(desc(art_per_v_country))

rd$lead_country <- sapply(strsplit(rd$pub_countries, ","), "[", 1)

# calculate solo country authorship on local volcanoes
df <- rd %>% 
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
  #create column full of zeroes
  mutate(only_local = 0) %>% 
  ungroup()

# label articles where all authors are from the same country 
# AND the lead country matches the volcano country
df$only_local[which(df$country_count == df$same_count &
                          df$lead_match == 1)]=1

# selected countries as percentage ----------------------------------------

pct_select <- df %>%
  # select countries to appear in the plot
  filter(v_country == 'Indonesia'|
         v_country == 'China'|
         v_country == 'Ethiopia'|
         v_country == 'Colombia'|
         v_country == 'USA'|
         v_country == 'New Zealand') %>%
  drop_na(`Publication Year`) %>% 
  #filter(v_country == 'USA') %>% 
  # select(v_name) %>%
  # group_by(v_name) %>% 
  # tally()
  
  # calculate grand total of articles published per v_country over all time
  group_by(v_country) %>% 
  mutate(grand_total_articles = n()) %>% 
  # create label with v_country and grand total articles for plotting
  mutate(facet_title =  paste(v_country, "   (n =", grand_total_articles,")" ,sep = "")) %>% 
  # now calculate inclusion metrics
  group_by(`Publication Year`, facet_title) %>% 
  mutate(total_articles = n()) %>% 
  mutate(total_leading = sum(lead_match)) %>% 
  mutate(total_including = sum(matching)) %>%
  mutate(total_solo = sum(only_local)) %>% 
  select(`Publication Year`,v_country, facet_title, grand_total_articles, total_articles,total_including,total_leading, total_solo) %>%
  unique() %>% 
  # convert all to percentages
  mutate(lead_pt = ((total_leading/total_articles)*100)+0.2,
         incl_nolead_pt = (((total_including-total_leading)/total_articles)*100)+0.2,
         incl_pt = ((total_including/total_articles)*100)+0.2,
         solo_pt = ((total_solo/total_articles)*100)+0.2) %>% 
  mutate(no_involvement_pt = 100 -(incl_pt)) %>% 
  arrange(facet_title,`Publication Year`) %>% 
  ungroup() %>% 
  # now calculate rolling mean of inclusion metrics  
  # and cumulative percentage of grand total articles (gta) published over time
  group_by(facet_title) %>% 
  # cumulative sum of articles
  mutate(gta_pt = (cumsum(total_articles)/grand_total_articles)*100) %>% 
  # rolling means
  mutate(rm_lead = rollmean(lead_pt,k=5,align = 'right', fill = 1),
         rm_incl_nolead = rollmean(incl_nolead_pt,k=5,align = 'right',fill = 1),
         rm_incl = rollmean(incl_pt,k=5,align = 'right', fill = 1),
         rm_no_in = rollmean(no_involvement_pt,k=5,align = 'right',fill = 1),
         rm_solo = rollmean(solo_pt,k=5,align = 'right',fill = 1)) %>%
  mutate(pub_year = `Publication Year`) %>%
  # add in years with no publications to complete the timeseries
  complete(pub_year = seq(min(pub_year),max(pub_year),by = 1)) %>% 
  # forward fill data for years with no publications 
  # i.e. if no articles are published, percentages stay the same until the next year with articles
  fill(c(total_articles:last_col()), .direction = 'down') %>% 
  pivot_longer(cols=c('rm_lead', 'rm_incl','rm_incl_nolead', 'rm_solo'), names_to = 'metric', values_to = 'rolling') %>% 
  filter(pub_year >= 1990)

# set levels for plotting
pct_select$facet_title <- as.factor(pct_select$facet_title)
pct_select$facet_title <- factor(pct_select$facet_title,
                                 levels = rev(levels(pct_select$facet_title)))


ggplot(pct_select)+
  geom_line(aes(x=pub_year, y = gta_pt), color = 'grey50', lty = 'dotted')+
  geom_line(aes(x=pub_year, y = rolling,color = metric), alpha = 0.8)+
  #create sub plot for each country 
  facet_wrap(~facet_title,
             ncol = 2)+ #, scales = 'free_y'
  theme_light()+
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        strip.text = element_text(colour = 'white'),
        #strip.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(y = 'Percentage of articles',
       x = '\nPublication year')+
  xlim(c(1990,2022))+
  scale_color_manual(labels = c("Including","Including (not leading)",
                                "Leading", 'All LDAs'),
                     values = c("#9dae11","steelblue", "#4b5c09","orange2"))+ # "cornflowerblue","darkmagenta", "goldenrod","red2"
  
ggsave('figs/Fig4_timeseries.png',height = 5.5, width = 5, units = 'in')

