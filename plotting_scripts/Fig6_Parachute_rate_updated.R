# For articles led by someone from outside the volcano country
# what proportion do not inlcude any authors from that country?

library(tidyverse)
library(ggrepel)
#install.packages('countrycode')
library(countrycode)
library(ggsci)
library(tools) # need for toTitleCase and str_to_sentence


rd <- readRDS('data_processed/lead_and_include_tk.Rdata')

# Extract lead country from the lead author's affiliation
rd$lead_country <-sapply(strsplit(rd$pub_countries, ","), "[", 1)

# Some articles name multiple volcanoes - need to filter down to 1 for analysis
# If one volcano gives a lead match, filter out remaining non-matching volcano rows 
multiple <- rd %>% 
  mutate(ones = 1) %>%
  group_by(`Article Title`) %>% 
  mutate(v_per_article = sum(ones)) %>%
  # filter for articles naming more than 1 volcano
  filter(v_per_article > 1) %>%
  # remove all but one volcano per article, 
  # keeping the one that could make an article inclusive
  # remove desc() function to take the opposite - strict instead of generous measure of inclusivity
  arrange(desc(lead_match), .by_group = T) %>% 
  #distinct(`Article Title`, .keep_all = T) %>% # We are going to just use the instances method now
  ungroup()

# Add back in the articles that only name one volcano
single <- rd %>% 
  mutate(ones = 1) %>%
  group_by(`Article Title`) %>% 
  mutate(v_per_article = sum(ones)) %>%
  filter(!v_per_article > 1) %>% 
  ungroup()

# Bind into one data frame 
df <- rbind(single, multiple) %>% 
  # how many articles has each country lead in total?
  group_by(lead_country) %>% 
  mutate(total_leading = sum(ones)) %>% 
  mutate(total_local_lead = sum(lead_match))

# # Make country names not ALL CAPS apart from USA
df$lead_country = str_to_sentence(df$lead_country) # title case for all words
df$lead_country = toTitleCase(df$lead_country) # change Of to of
df['lead_country'][df$lead_country == 'Usa'] <- 'USA'

# Calculate proportion of non-inclusive papers per country
#  when only non-locally led (nll) articles are considered
df_nll <- df %>% 
  #first, filter out any locally led articles
  filter(lead_match == 0)%>%
  group_by(lead_country) %>% 
  mutate(sum_match = sum(matching)) %>% 
  mutate(led_per_country = sum(ones)) %>% 
  mutate(inc_pt = (sum(matching)/sum(ones))*100) %>% 
  distinct(lead_country, .keep_all = T) %>%
  # filter for countries that have led at least 20 articles
  #filter(total_articles >= 100) %>% 
  filter(led_per_country >= 25) %>% 
  mutate(label = paste(sum_match,'/',led_per_country)) %>% 
  mutate(non_local_lead_pt = led_per_country/total_leading) %>% 
  mutate(lead_country =  as.factor(lead_country))

class(df_nll$lead_country)


# set levels for plotting
df_nll$lead_country <- factor(df_nll$lead_country, 
                             levels = df_nll$lead_country[
                               (order(df_nll$inc_pt, decreasing = T))])


class(df_nll$lead_country)
levels(df_nll$lead_country)

# save this csv for use in the remote sensing plot
write_csv(df_nll,'data_processed/parachute_rate.csv')

ggplot(df_nll)+
  geom_point(aes(x=inc_pt,y= df_nll$lead_country),color = 'steelblue', alpha = 1)+
  geom_vline(xintercept = mean(df_nll$inc_pt), linetype="dashed", color = 'red3', alpha = 0.5)+
  geom_label(aes(x= inc_pt,y=lead_country, 
                label = label, hjust = +1.25),
            size = 2.5,label.size = 0, label.padding = unit(0.07, "lines"))+
  geom_label(aes(x= inc_pt,y=lead_country, 
                label = paste0(' ',total_local_lead), hjust = -0.4),
            size = 2.5, label.size = 0, label.padding = unit(0.07, "lines"))+
  labs(x= '\nPercentage of articles with a locally domiciled author',
       y= 'Country of lead author\n')+ # Country of lead author \n
       #subtitle = 'Percentage of articles that include a local author \namongst articles where the lead author is non-local')+
  xlim(c(0,50))+
  scale_color_uchicago()+
  theme_light()+
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor.x  = element_blank(),
        panel.border = element_blank())+
  geom_label(aes(x = mean(df_nll$inc_pt), y = 'China', label = paste('Mean inclusion percentage',
                                                                     # '\n',
                                                                     round(mean(df_nll$inc_pt),1),
                                                                     '%')), 
             color ='red3', size = 3.2, hjust = -0.005,vjust = 0.2, label.size = 0, alpha = 0.5)

ggsave('figs/Fig6_inclusivity_by_country.png', height = 5.65,width = 6, units = 'in')

