# Plotting all articles inclusivity over time
# Oct 2021

library(tidyverse)
library(readxl)
library(plotly)
#install.packages('RcppRoll')
library(RcppRoll)
library(zoo)
library(ggsci)
library(ghibli)
library(cowplot)


rd <- readRDS('data_processed/lead_and_include_tk.Rdata')
rd$lead_country <- sapply(strsplit(rd$pub_countries, ","), "[", 1)


# all countries as n_articles ---------------------------------------------
num_articles <- rd %>% 
  drop_na(`Publication Year`) %>% 
  group_by(`Publication Year`) %>%
  mutate(total_articles = n()) %>% 
  mutate(local_leading = sum(lead_match)) %>% 
  mutate(local_including = sum(matching)-local_leading) %>% 
  mutate(local_excluding = total_articles-(local_including+local_leading)) %>% 
  ungroup() %>%
  select(`Publication Year`,last_col(2):last_col()) %>%
  pivot_longer(cols = c(2:4),names_to = 'authorship',values_to = 'n_articles') %>% 
  filter(`Publication Year` >= 1990) %>% 
  unique() %>% 
  filter(!`Publication Year` > 2021)

number <- ggplot(num_articles)+
  geom_col(aes(x=`Publication Year`, y = n_articles,fill=authorship),alpha =0.85)+
  labs(y = 'Number of articles\n',
       x = '\nPublication year')+
  theme_light()+
  scale_y_continuous(expand = c(0.025,0.0)) +
  scale_x_continuous(expand = c(0.025,0.025))+
  scale_fill_manual(values = c("#b730c6ff",
                               "#9dae11",
                               "#4b5c09"),
                      labels = c('Local researchers not included',
                                 'Local researchers included',
                                 'Led by local researcher'))+
  # scale_fill_ghibli_d(name = 'LaputaMedium', direction = -1,
  #                     labels = c('Local researchers not included',
  #                                'Local researchers included',
  #                                'Led by local researcher'))+
  theme(legend.title = element_blank(),
        legend.position = c(0.39,0.85),
        #axis.title.x = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size=8))

number
ggsave('figs/authorship_timeseries.png')

gg
ggplotly(gg)

# all countries as percentage ---------------------------------------------
# first set a window for rolling mean calculation
window <- 3
alignment <- 'right'

all_pct <- rd %>% 
  drop_na(`Publication Year`) %>% 
  filter(!`Publication Year` > 2021) %>% 
  group_by(`Publication Year`) %>%
  mutate(total_articles = n()) %>% 
  mutate(local_leading = sum(lead_match)) %>% 
  mutate(local_including = sum(matching)-local_leading) %>% #including but not leading
  mutate(local_excluding = total_articles-(local_including+local_leading)) %>% 
  select(total_articles, last_col(2):last_col()) %>%
  unique() %>%
  
  # calculate percentages
  mutate(lead_pt = (local_leading/total_articles)*100) %>%
  mutate(incl_pt = (local_including/total_articles)*100) %>%
  mutate(excl_pt = (local_excluding/total_articles)*100) %>% 
  ungroup() %>% 
  arrange(`Publication Year`) %>%
  
  # calculate rolling mean of averages
  mutate(rm_lead = rollmean(lead_pt,k=window,align = alignment, fill = NA),
         rm_incl = rollmean(incl_pt,k=window,align = alignment, fill = NA)) %>% 
  mutate(rm_excl = rollmean(excl_pt,k=window,align = alignment, fill = NA)) %>% #
  select(`Publication Year`,last_col(5):last_col())

rolling <- all_pct %>% 
  pivot_longer(cols = last_col(2):last_col(),
               names_to = 'authorship',values_to = 'rolling') %>% 
  ungroup() %>% 
  filter(`Publication Year` >= 1990)

exact <- all_pct %>% 
  pivot_longer(cols = lead_pt:excl_pt,
               names_to = 'exact_authorship', values_to = 'exact_values') %>% 
  ungroup() %>% 
  filter(`Publication Year` >= 1990)

pct <- ggplot()+
  geom_col(data = exact,aes(x=`Publication Year`, y=exact_values, fill = exact_authorship),position = "fill",alpha = 0.85)+
  #geom_line(data =rolling,aes(x=`Publication Year`, y=rolling, color = authorship),position = "fill", size = 1)+
  
  theme_light()+
  scale_y_continuous(labels = scales::percent,expand = c(0.025,0.0))+
  scale_x_continuous(expand = c(0.025,0.025))+
  # scale_colour_ghibli_d(name = 'LaputaMedium', direction = -1,
  #                      labels = c('Local researchers not included',
  #                                 'Local researchers included',
  #                                 'Led by local researcher'))+
  # scale_fill_ghibli_d(name = 'LaputaMedium', direction = -1,
  #                       labels = c('Local researchers not included',
  #                                  'Local researchers included',
  #                                  'Led by local researcher'))+
  
  scale_fill_manual(values = c("#b730c6ff",
                               "#9dae11",
                               "#4b5c09"),
                    labels = c('Local researchers not included',
                               'Local researchers included',
                               'Led by local researcher'))+
  labs(x = '\nPublication year',
       y = 'Percentages')+
  theme(legend.position = "none")+

ggsave('figs/stacked_pct.png')
pct

ggplotly(pct)

# add the two plots into one - cowplot ----------------------------------

plots <- cowplot::plot_grid(number,pct,ncol  = 2, axis = "lr", align = "v", rel_heights = c(1,1))
plots

ggsave('figs/all_inclusion_roll.png', width = 6.5*1.1, height = 3*1.1, units = 'in', dpi = 400) #width = 4, height = 5.5 

gg 

ggplotly(gg)
