# How does the inclusivity analysis change when articles using 
# remote sensing methods are excluded or analysed on there own?

library(tidyverse)
library(readxl)
#library(ggrepel)
library(ggsci)

# data prep ---------------------------------------------------------------

rd <- readRDS('data_processed/lead_and_include_tk.Rdata') %>% 
  ungroup()

#### Note that for this analysis I have commented out the code 
#### that removes non-inclusive articles in instances
#### where an article names multiple volcanoes. This is to better capture
#### remote sensing article inclusivty where multiple volcanoes are more likely to be mentioned

# # Some articles name multiple volcanoes - need to filter down to 1 for analysis
# # If one volcano gives a lead match, filter out remaining non-matching volcano rows
# multiple <- rd %>%
#   mutate(ones = 1) %>%
#   group_by(`Article Title`) %>%
#   mutate(v_per_article = sum(ones)) %>%
#   # filter for articles naming more than 1 volcano
#   filter(v_per_article > 1) %>%
#   # remove all but one volcano per article,
#   # keeping the one that could make an article inclusive
#   arrange(desc(lead_match), .by_group = T) %>%
#   distinct(`Article Title`, .keep_all = T) %>%
#   ungroup()
# 
# # Keep articles that only name one volcano
# single <- rd %>%
#   mutate(ones = 1) %>%
#   group_by(`Article Title`) %>%
#   mutate(v_per_article = sum(ones)) %>%
#   filter(!v_per_article > 1) %>%
#   ungroup()
# 
# # Bind into one data frame
# rd <- rbind(single, multiple)

# Extract lead country from the lead author's affiliation
rd$lead_country <-sapply(strsplit(rd$pub_countries, ","), "[", 1)

# List of words likely associated with articles that use
# distant remote sensing methods

rs_list <- read_excel('raw_data/rs_list.xls')

# capitalise
rs_string <- str_c(rs_list$Keywords, collapse = '|') %>% 
  str_to_upper(.)

# Scan title, abstract and keywords for articles on remote sensing 
rd$taka = paste(rd$tak, str_to_upper(rd$Abstract))
rd$remote <- sapply(str_extract_all(rd$taka, rs_string), toString)


# Analyse effect on trends ------------------------------------------------
# Produce gap chart highlighting inclusivity differences 
# between remote sensing articles and all articles

df <- rd %>% 
  #filter(remote == '') %>% 
  filter(lead_match == 0) %>% 
  group_by(lead_country) %>%
  mutate(ones = 1) %>% 
  mutate(sum_match = sum(matching)) %>% 
  mutate(led_per_country = sum(ones)) %>% 
  mutate(inc_pt_all = (sum(matching)/sum(ones))*100) %>% 
  distinct(lead_country, .keep_all = T) %>% 
  ungroup() %>% 
  mutate(leadership_rank = rank(desc(led_per_country))) %>% 
  mutate(remote = 'non-remote') %>% 
  select(inc_pt_all, lead_country)

remote_df <- rd %>% 
  # filter out articles without words from rs_string
  filter(!remote == '') %>%
  filter(lead_match == 0) %>% 
  group_by(lead_country) %>%
  mutate(ones = 1) %>% 
  mutate(sum_match = sum(matching)) %>% 
  mutate(led_per_country = sum(ones)) %>% 
  mutate(inc_pt_remote = (sum(matching)/sum(ones))*100) %>% 
  distinct(lead_country, .keep_all = T) %>% 
  ungroup() %>% 
  mutate(leadership_rank = rank(desc(led_per_country))) %>% 
  mutate(remote = 'remote') %>% 
  filter(led_per_country >= 25) #25

# join in the data from all articles against the countries 
# that have at least 25 remote sensing articles published on other countries
df_both <- left_join(remote_df,df, by = "lead_country") %>% 
  select(lead_country,last_col(offset = 2):last_col()) 


# set levels for plotting
remote_df$lead_country <- factor(remote_df$lead_country, 
                               levels = remote_df$lead_country[
                                 (order(remote_df$inc_pt_remote,decreasing = T))])
  
# # pivot longer for plotting
df_long <- df_both %>%
  pivot_longer(cols = c(2,4),
               names_to = 'subset', values_to = 'inc_pt')

# # save remote sensing dataframe for use in this and other scripts
 write_csv(df_long,'data_processed/remote_sensing_df.csv')

all_articles <- read_csv('data_processed/parachute_rate.csv') %>%
  select(lead_country,inc_pt ) %>%
  rename(inc_pt_all = inc_pt)

df_both <- read_csv('data_processed/remote_sensing_df.csv') %>% 
  pivot_wider(names_from = 'subset', values_from = 'inc_pt') %>% 
  select(-inc_pt_all) %>% 
  left_join(.,all_articles, by = 'lead_country')

df_both$lead_country <- factor(df_both $lead_country, 
                               levels = df_both $lead_country[
                                 (order(df_both $inc_pt_all,decreasing = T))])
# plotting code -----------------------------------------------------------


# set color parameters for plot
crem <- '#a3c4de' # '#2ecddc' # '#a3c4de'
call <- '#1167b1' #'#2ecddc' #'#7597B4FF' # 'steelblue' # 'deepskyblue3' 

  
ggplot(df_both)+
  geom_vline(xintercept = mean(df_both$inc_pt_all),
             lty='dashed', color = call)+
  geom_vline(xintercept = mean(df_both$inc_pt_remote),
             lty='dashed', color = crem)+
  geom_segment(aes(x=inc_pt_remote, xend=inc_pt_all, y=lead_country, yend=lead_country), 
               color= crem, size =1.2)+
  geom_point(aes(x=inc_pt_remote, y= lead_country),
             size = 1.5, color = crem)+ # #a3c4de  #0e668b 'lightblue'
  geom_point(aes(x=inc_pt_all, y= lead_country),
             size = 1.5, color = call)+
  geom_label(aes(x=41, y = 'CHINA', label = 'Remote Sensing\narticles'), 
            color =crem, size = 3.5, vjust = .7, label.size = 0, alpha = 0.1)+
  geom_label(aes(x=23, y = 'CHINA', label = 'All articles'), 
            color =call, size = 3.5, vjust = 0.4, label.size = 0, alpha = 0.1)+
  theme_light()+
  xlim(c(0,50))+
  labs(y = 'Country of lead author\n',
       x = '\nPercentage of articles with a locally domiciled author') +#,
       #subtitle = 'Percentage of articles that include a local author \namongst articles where the lead author is non-local')+
  theme(legend.position = c(0.25,0.2),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid.minor.x  = element_blank(),
        panel.border = element_blank())+

ggsave('figs/Fig8_remote_sensing.png',  width = 6.58*0.8,  height = 4.67*0.8, units = 'in')

check <- df_both %>% 
  mutate(dif = inc_pt_all - inc_pt_remote) %>% 
  mutate(ab_dif = abs(dif))

mean(check$dif)

paste('all articles inclusivity = ', mean(check$inc_pt_all))
paste('remote sensing inclusivity = ', mean(check$inc_pt_remote))





# Other code may be useful----------------------------------------------------------------

# # Some articles name multiple volcanoes - need to filter down to 1 for analysis
# # If one volcano gives a lead match, filter out remaining non-matching volcano rows 
# multiple <- rd %>% 
#   mutate(ones = 1) %>%
#   group_by(`Article Title`) %>% 
#   mutate(v_per_article = sum(ones)) %>%
#   # filter for articles naming more than 1 volcano
#   filter(v_per_article > 1) %>%
#   # remove all but one volcano per article, 
#   # keeping the one that could make an article inclusive
#   arrange(desc(lead_match), .by_group = T) %>% 
#   distinct(`Article Title`, .keep_all = T) %>% 
#   ungroup()
# 
# # Keep articles that only name one volcano
# single <- rd %>% 
#   mutate(ones = 1) %>%
#   group_by(`Article Title`) %>% 
#   mutate(v_per_article = sum(ones)) %>%
#   filter(!v_per_article > 1) %>% 
#   ungroup()
# 
# # Bind into one data frame 
# df <- rbind(single, multiple) %>% 
#   # how many articles has each country led in total?
#   group_by(lead_country) %>% 
#   mutate(total_leading = sum(ones)) %>% 
#   
#   # count number of countries extracted per article
#   mutate(country_count = 1 + str_count(string = pub_countries, 
#                                  pattern = ",")) %>% 
#   
#   # check if all countries are the same 
#   # i.e. check if all countries match the lead country
#   mutate(same_count = str_count(string = pub_countries,
#                                 pattern = lead_country)) %>% 
#   mutate(single_country = 0)
#   
# df$single_country[which(df$country_count == df$same_count)]=1
# 
# # Which papers only have authors from one country
# df_summary <- df %>% 
#   group_by(lead_country) %>% 
#   mutate(lead_per_country = sum(ones)) %>% 
#   mutate(single_country_pt = (sum(single_country)/sum(ones))*100) %>% 
#   filter(lead_per_country > 25) %>% 
#   select(lead_per_country,last_col()) %>% 
#   distinct()
#   
#   
# 
# # Add countries to label
# c_lab <- c('NEW ZEALAND', 'SWITZERLAND', 'PORTUGAL', 'USA', 'SINGAPORE', 'TURKEY',
#            'TAIWAN', 'UNITED KINGDOM', 'ITALY', 'FRANCE') %>% 
#   as.data.frame() %>% 
#   rename(c_label = 1) %>% 
#   mutate(lead_country = c_label)
# 
# df_plot <- left_join(df_both,c_lab)
#                       
# 
# # set levels for plotting
# df_plot$lead_country <- factor(df_both$lead_country, 
#                              levels = df_both$lead_country[
#                                (order(df_both$inc_pt,decreasing = T))])
# 
# ggplot(df_plot)+
#   geom_text_repel(aes(x=inc_pt, y=lead_country, label = c_label),
#                   size = 2.6, hjust = 1.3, vjust = 1.1,
#                   segment.alpha = 0.4)+
#   geom_point(aes(x=inc_pt,y=lead_country,color=continent), alpha = 1)+
#   #geom_vline(xintercept = mean(df_both$inc_pt), linetype="dashed", color = 'grey30')+
#   geom_text(aes(x= inc_pt,y=lead_country, 
#                 label = label, hjust = -0.25),
#             size = 2.5)+
#   labs(x= 'Inclusion percentage',
#        y= '', # Country of lead author \n
#        subtitle = 'Percentage of articles that include a local author amongst \narticles where the lead author is non-local')+
#   xlim(c(0,50))+
#   scale_y_discrete(labels=NULL)+
#   scale_color_uchicago()+
#   theme_light()+
#   theme(legend.position = 'bottom',
#         legend.title = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.grid.major.y = element_blank(),
#         panel.border = element_blank())+
# 
# ggsave('figs/inclusivity_by_country.png', height = 5, units = 'in')
# 
# 
# # new scatterplot ---------------------------------------------------------
# 
# ggplot(df_plot)+
#   geom_point(aes(y=inc_pt, x=leadership_rank, color = continent), alpha = 0.9)+
#   #geom_vline(xintercept = mean(df_both$inc_pt), linetype="dashed", color = 'grey30')+
#   geom_text_repel(aes(y= inc_pt,x=leadership_rank, 
#                label = c_label),
#             hjust=0, vjust=1.0,
#             box.padding = 0.35, 
#             point.padding = 0.1,
#             segment.size = .5,
#             segment.color = 'grey50',
#             segment.alpha = 0.2,
#             min.segment.length = 0.2,
#             size = 2.5,
#             direction = 'x')+
#   labs(y= 'Inclusion percentage',
#        x= 'Leadership rank',
#        subtitle = '')+
#   scale_x_reverse()+
#   scale_color_uchicago()+
#   ylim(c(0,50))+
#   theme_light()+
#   coord_flip()+
#   theme(legend.position = 'none')+
#   
#   ggsave('figs/inclusivity_v_rank.png', height = 3.5, units = 'in')
# 
# 
# 
# # individual country checks -----------------------------------------------
# 
# check <- df %>% 
#   #first, filter out any locally led articles
#   filter(lead_match == 0)%>%
#   group_by(lead_country) %>% 
#   mutate(sum_match = sum(matching)) %>% 
#   mutate(led_per_country = sum(ones)) %>% 
#   mutate(inc_pt = (sum(matching)/sum(ones))*100) %>% 
#   filter(lead_country == 'CHINA')
#   
#   distinct(lead_country, .keep_all = T) %>%
#   # filter for countries that have led at least 20 articles
#   #filter(total_articles >= 100) %>% 
#   filter(led_per_country >= 25)
# 
# # trash -------------------------------------------------------------------
# 
# #pivot longer
# df_long <- df_both %>% 
#   pivot_longer(cols = c('inc_pt_all','inc_pt'),
#                names_to = 'Condition', values_to = 'Inclusivity')
# ggplot(df_both)+
#   geom_point(aes(x=Inclusivity,y=lead_country, colour = Condition), alpha = 0.5)+
#   theme(legend.position = 'bottom',
#         axis.title.y = element_blank())
