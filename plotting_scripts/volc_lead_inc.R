# Create plots showing inclusion and leadership per country
# May 2022

library(tidyverse)
library(countrycode)

# leadership and inlcusion prep ----------

rd <- readRDS('data_processed/lead_and_include_tk.Rdata') %>% 
  ungroup()

# extract lead country - first country extracted from the affiliations
rd$lead_country <- sapply(strsplit(rd$pub_countries, ","), "[", 1)


lead_percent <- rd %>%
  # tidy the data - one entry per country
  select(v_name,v_country,lead_match,matching,total_leading,total_including,total_articles) %>% 
  
  # recalculate totals by volcano rather than by v_country
  group_by(v_name) %>% 
  mutate(total_articles = n(),
         total_leading = sum(lead_match),
         total_including = sum(matching)) %>% 
  # reduce down to one row per volcano
  select(-lead_match,-matching) %>% 
  distinct()  %>% # 749 unique volcanoes
  
  # calculate percentages
  mutate(pt_lead = (total_leading/total_articles)*100) %>% 
  mutate(pt_include = (total_including/total_articles)*100) %>% 
  # create labels for plotting
  mutate(label_lead = total_leading) %>% #,'/',total_articles
  mutate(label_include = paste(total_including)) %>% 
  mutate(label_total = paste(total_articles)) %>% # (total_articles-total_including),"/",
  
  # rank volcanoes by total number of articles and filter for top 20
  ungroup() %>% 
  mutate(v_rank = rank(desc(total_articles), ties.method = 'random')) #%>% 
  filter(v_rank <= 20)


# calculate unweighted average leadership and inclusion for all countries in dataset to add to plot
li_global <- lead_percent %>%
  ungroup() %>% 
  mutate(total_leading = sum(total_leading)) %>%
  mutate(total_including = sum(total_including)) %>%
  mutate(total_articles = sum(total_articles)) %>%
  mutate(pt_lead = mean(pt_lead)) %>% 
  mutate(pt_include = mean(pt_include)) %>%
  mutate(pt_lead = total_leading/total_articles) %>%
  mutate(pt_include = total_including/total_articles) %>% 
  mutate(v_country = 'All Countries')

# rank the top twenty volcanoes for plotting purposes
lead_percent$rank_include = rank(lead_percent$pt_include)
lead_percent$rank_lead = rank(lead_percent$pt_lead)

# Change search names back to GVP names
v_names_clean <- read_excel('raw_data/GVP.xlsx') %>% 
  select(`Volcano Name`, Original_name) %>% 
  rename(v_name = `Volcano Name`)

# add in clean volcano names from GVP
top_lead <- lead_percent %>% 
  left_join(.,v_names_clean, by ='v_name') %>%
  # for v_names that were modified in R (and could not be joined with original GVP names)
  # use the modified name from R
  mutate(Original_name = coalesce(Original_name,v_name)) %>% 
  # replace v_name with Original_name %>% 
  select(-v_name) %>% rename(v_name = Original_name) %>%
  ungroup()  

# clean up border volcano countries to only have the first country for continent extraction
top_lead$v_country_clean = sub("\\|.*", "", top_lead$v_country)

# add in volcano coordinates for mapping
gvp <- read_excel('raw_data/GVP.xlsx') %>% 
  rename(v_name = Original_name)

#extract continent from country name
top_lead$continent <- countrycode(sourcevar = top_lead[["v_country_clean"]],
                            origin = "country.name",
                            destination = "continent")

# bring in additional list of continents that separates Americas into North and South
cc <- read_csv('raw_data/continents_countries.csv') %>% 
  rename(v_country_clean = country,
         full_continent = continent)

top_lead_c <- top_lead %>% 
  left_join(.,cc, by = 'v_country_clean')

unzen_fix <- top_lead_c
unzen_fix$v_name[unzen_fix$v_name == 'Unzendake|Unzen'] <- 'Unzendake'

map_data <- unzen_fix %>% 
  # calcualte each portion of the pie chart
  mutate(total_no_match = total_articles - total_including) %>% 
  mutate(total_including = total_including - total_leading) %>% 
  left_join(.,gvp) %>% 
  select(v_name,v_country,v_country_clean,full_continent, total_leading:total_articles,total_no_match, v_rank,
         Latitude,Longitude) %>% 
  distinct() %>% 
  # calculate volcano ranks by continent
  group_by(full_continent) %>% 
  mutate(c_v_rank = rank(desc(total_articles),ties.method = 'random')) %>% 
  filter(c_v_rank <= 3)

write_csv(map_data,'data_processed/volc_map_data.csv')


# manually rename Unzen
top_lead$v_name[top_lead$v_name == 'Unzendake|Unzen'] <- 'Mount Unzen'

# set levels for plotting
top_lead$v_name <- factor(top_lead$v_name, 
                             levels = top_lead$v_name[
                               (order(top_lead$rank_include, decreasing = T))])


# the plot ----------------------------------------------------------------

inc_mean <- mean(top_lead$pt_include)
lead_mean <- mean(top_lead$pt_lead)

ggplot(top_lead)+
  # add grey background
  geom_rect(aes(xmin=0,xmax=105,ymin=0,ymax=20),fill='white')+
  
  # add data bars
  geom_col(aes(x=100, y=v_name), fill = 'grey90')+
  geom_col(aes(x=pt_include,y=v_name),fill = '#9dae11')+
  geom_col(aes(x=pt_lead,y=v_name),fill = '#4b5c09') +
  
  
  # add in average line segments
  geom_segment(aes(x=mean(top_lead$pt_include)+0.6, xend= mean(top_lead$pt_include),
                   y= 0, yend= 16.75),
               lty='twodash', lwd =0.5 , color = 'darkorchid1')+
  geom_segment(aes(x=mean(top_lead$pt_lead), xend= mean(top_lead$pt_lead),
                   y= 0, yend= 19.25),
               lty='twodash', lwd =0.45 ,color = 'darkorchid1')+
  
  # add labels
  # leading
  geom_label(aes(x=rev(pt_lead),
                y=reorder(rev(v_name),pt_lead), 
                label = rev(label_lead),
                hjust = -0.19),
            size = 2.5, label.size = 0, label.padding = unit(0.1, "lines"),
            color = 'grey90', fill = '#9dae11') +
  # including
  geom_label(aes(x=rev(pt_include),
                y=reorder(rev(v_name),pt_include), 
                label = rev(label_include),
                hjust = -0.05),
            size = 2.5, label.size = 0, label.padding = unit(0.05, "lines"),
            color = '#9dae11', fill = 'grey90') +
  # total
  geom_label(aes(x=100,
                 y=reorder(rev(v_name),pt_include), 
                 label = rev(label_total),
                 hjust = -0.05),
             size = 2.5, label.size = 0, label.padding = unit(0.05, "lines"),
             color = 'grey20', fill = 'white') +
  
  labs(x = '\nPercentage of articles',
       y = element_blank())+
  theme_bw()+
  scale_x_continuous(expand = c(0,0),breaks = seq(0,100,20)) + #c(0,25,round(lead_mean,0),50,round(inc_mean,0),75,100))+
  scale_y_discrete(expand = c(0,0))+
  expand_limits(x=c(0,105))+
  theme(panel.border=element_blank(),
        panel.grid = element_blank())+
  ggplot2::annotate("text", x = mean(top_lead$pt_lead), y = 19.0, hjust = -0.02, size = 3.1, 
                    label = paste("First author is a local researcher: ",round(mean(top_lead$pt_lead),0),"%", sep = ""), color = "#4b5c09") + # x = 37
  ggplot2::annotate("text", x = mean(top_lead$pt_include), y = 17, hjust = -0.02, size = 3.1, 
                    label = paste("One or more local researchers involved: ",round(mean(top_lead$pt_include),0),"%", sep = ""), color = "#9dae11")+  # x = 41
  
ggsave('figs/volc_lead_inc.png',height = 5.5, width =6.58*1.12,units = 'in', scale = 1.1)



