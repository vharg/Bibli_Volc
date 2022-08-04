# Create plots showing inclusion and leadership per country
# May 2022

library(tidyverse)

# leadership and inlcusion prep ----------

rd <- readRDS('data_processed/lead_and_include_tk.Rdata') %>% 
  ungroup()

# extract lead country - first country extracted from the affiliations
rd$lead_country <- sapply(strsplit(rd$pub_countries, ","), "[", 1)


lead_percent <- rd %>%
  # tidy the data - one entry per country
  select(v_country,total_leading,total_including,total_articles) %>% 
  unique() %>% # 76 unique countries or country combinations
  # calculate percentages
  mutate(pt_lead = (total_leading/total_articles)*100) %>% 
  mutate(pt_include = (total_including/total_articles)*100) %>% 
  # create labels for plotting
  mutate(label_lead = total_leading) %>% #,'/',total_articles
  mutate(label_include = paste(total_including)) %>% 
  mutate(label_total = paste(total_articles)) %>% # (total_articles-total_including),"/",
  filter(total_articles >= 50)

# filter out countries with < 50 articles to limit plot size 
# and not calculate averages for a small sample size of articles
li_50 <- lead_percent 

# calculate unweighted average leadership and inclusion for all countries in dataset to add to plot
li_global <- lead_percent %>%
  ungroup() %>% 
  mutate(total_leading = sum(total_leading)) %>%
  mutate(total_including = sum(total_including)) %>%
  mutate(total_articles = sum(total_articles)) %>%
  mutate(pt_lead = mean(pt_lead)) %>% 
  mutate(pt_include = mean(pt_include)) %>%
  mutate(pt_lead = total_leading/total_articles) %>%
  mutate(pt_include = total_including/total_articles)
  
# rank the volcano countries
li_50$rank_include = rank(li_50$pt_include)
li_50$rank_lead = rank(li_50$pt_lead)

top_lead <- li_50

# set levels for plotting
top_lead$v_country <- factor(top_lead$v_country, 
                             levels = top_lead$v_country[
                               (order(top_lead$rank_include,decreasing = T))])

# the plot ----------------------------------------------------------------

inc_mean <- mean(top_lead$pt_include)
lead_mean <- mean(top_lead$pt_lead)

ggplot(top_lead)+
  # add grey background
  geom_rect(aes(xmin=0,xmax=105,ymin=0,ymax=40.5),fill='white')+
  
  # add data bars
  geom_col(aes(x=100, y=v_country), fill = 'grey90')+
  geom_col(aes(x=pt_include,y=v_country),fill = '#9dae11')+
  geom_col(aes(x=pt_lead,y=v_country),fill = '#4b5c09') +
  
  
  # add in average line segments
  geom_segment(aes(x=mean(top_lead$pt_include)+0.6, xend= mean(top_lead$pt_include),
                   y= 0, yend= 36.75),
               lty='twodash', lwd =0.5 , color = 'darkorchid1')+
  geom_segment(aes(x=mean(top_lead$pt_lead), xend= mean(top_lead$pt_lead),
                   y= 0, yend= 39.25),
               lty='twodash', lwd =0.45 ,color = 'darkorchid1')+
  
  # add labels
  # leading
  geom_label(aes(x=rev(pt_lead),
                y=reorder(rev(v_country),pt_lead), 
                label = rev(label_lead),
                hjust = -0.19),
            size = 2.5, label.size = 0, label.padding = unit(0.1, "lines"),
            color = 'grey90', fill = '#9dae11') +
  # including
  geom_label(aes(x=rev(pt_include),
                y=reorder(rev(v_country),pt_include), 
                label = rev(label_include),
                hjust = -0.05),
            size = 2.5, label.size = 0, label.padding = unit(0.05, "lines"),
            color = '#9dae11', fill = 'grey90') +
  # total
  geom_label(aes(x=100,
                 y=reorder(rev(v_country),pt_include), 
                 label = rev(label_total),
                 hjust = -0.05),
             size = 2.5, label.size = 0, label.padding = unit(0.05, "lines"),
             color = 'grey20', fill = 'white') +
  
  labs(x = 'Percentage of articles',
       y = element_blank())+
  theme_bw()+
  scale_x_continuous(expand = c(0,0),breaks = seq(0,100,20)) + #c(0,25,round(lead_mean,0),50,round(inc_mean,0),75,100))+
  scale_y_discrete(expand = c(0,0))+
  expand_limits(x=c(0,105))+
  theme(panel.border=element_blank(),
        panel.grid = element_blank())+
  ggplot2::annotate("text", x = mean(top_lead$pt_lead), y = 39.0, hjust = -0.02, size = 3.1, 
                    label = paste("First author is a local researcher: ",round(mean(top_lead$pt_lead),0),"%", sep = ""), color = "#4b5c09") + # x = 37
  ggplot2::annotate("text", x = mean(top_lead$pt_include), y = 37, hjust = -0.02, size = 3.1, 
                    label = paste("One or more local researchers involved: ",round(mean(top_lead$pt_include),0),"%", sep = ""), color = "#9dae11")+  # x = 41
  
ggsave('figs/fig2_inclusion_leadership.png',height = 5.5, width =6.58*1.12,units = 'in')


# # Supplemental material all countries
# df <- rd %>%
#   # tidy the data - one entry per country
#   select(v_country,total_leading,total_including,total_articles) %>% 
#   unique() %>% # 76 unique countries or country combinations
#   # calculate percentages so that they add to 100 - not the way they are in the plot (overlapping)
#   mutate(pt_lead = (total_leading/total_articles)*100) %>% 
#   mutate(pt_include = ((total_including-total_leading)/total_articles)*100) %>% 
#   rename(total_including_not_lead = total_including) %>% 
#   mutate(total_including_not_lead = total_including_not_lead - total_leading) %>% 
#   mutate(pt_no_inclusion = 100 -((total_including_not_lead + total_leading)/total_articles)*100) %>% 
#   arrange(desc(total_articles))
# 
# write_csv(df,'data_processed/S_V_country_inclusivity_full.csv')
