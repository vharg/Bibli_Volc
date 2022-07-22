# Extracting volcano names from WoS results
# Sept 2021

library(tidyverse)
library(readxl)

# load data ----
wos <- readRDS('raw_data/volcan_star_tk.Rdata')

# select publication data that is desired
wos_select <- wos %>% 
  select(Authors,`Article Title`, `Publication Year`, Abstract, Addresses, 
         DOI, `Times Cited, All Databases`,`Source Title`,tak)

#load scopus data and bind it with the WoS data
scopus <- readRDS('raw_data/scopus_tk.Rdata')

df <- rbind(wos_select,scopus)

# save this combined dataset for quick retrieval in future if needed
#saveRDS(df, 'raw_data/all_articles.Rdata')

# check range of years that articles were published
range(df$`Publication Year`,na.rm = T)

# list of volcano names and their country
rvolc <- read_excel('raw_data/GVP.xlsx') %>% 
  select(`Volcano Name`,Country) %>% 
  dplyr::rename(volc_name = `Volcano Name`, volc_country = Country) %>% 
  group_by(volc_name) %>% 
  # keep only unique volcano names - change to > 1 to get only duplicates
  filter(n() == 1) %>%
  ungroup() %>% 
  filter(!volc_country == 'Undersea Features')

# replace certain volcano names with strings that are more commonly used
# note that many names have already been changed within the GVP excel file
# so this is not a complete list of the changes
rvolc$volc_name <- str_replace_all(rvolc$volc_name, c("Akagisan"="Akagi|Akagisan", "Bulusan"="Bulu|Bulusan",
                    "Bandaisan"="Bandai|Bandaisan","Berutarubesan" = "Berutarube|Berutarubesan", 
                    "Chirippusan" = "Chirip|Chirippusan", "Chokaisan" = "Chokai|Chokaisan","Fujisan"= "Fuji|Fujisan",
                    "Hokkodasan" = "Hakkoda|Hokkodasan","Hakusan"="Haku|Hakusan","Harunasan"="Haruna|Harunasan","Iwakisan"="Iwaki|Iwakisan",
                    "Iwatesan"="Iwate|Iwatesan","Kujusan"="Kuju|Kujusan","Kusatsu-Shiranesan"="Kusatsu|Kusatsu-Shirane|Kusatsu-Shiranesan","Myokosan"="Myoko|Myokosan",
                    "Nantaisan"="Nantai|Nantaisan","Nikko-Shiranesan"="Nikko|Nikko-Shirane|Nikko-Shiranesan","Ontakesan"="Ontake|Ontakesan","Sanbesan"="Sanbe|Sanbesan",
                    "Akusekijima"="Asuseki|Akusekijima","Hachojijima"="Hachoji|Hachojijima","Kuchinoerabujima"="Kuchinoe|Kuchinoerabujima",
                    "Mikurajima"="Mikura|Mikurajima","Miyakejima"="Miyake|Miyakejima","Niijima"="Nii|Niijima", "Sumisujima"="Sumisu|Sumisujima", 
                    "Suwanosejima"="Suwanose|Suwanosejima", "Yokoatejima"="Yokoate|Yokoatejima", "Zaozan" = "Zaozan|Zaosan", "Tomariyama"="Tomari|Tomariyama",
                    "Sashiusudake"="Sashiusu|Sashiusudake","Ruruidake"="Rurui|Ruruidake","Raususan"="Rausu|Raususan","Rakkibetsudake"="Rakkibetsu|Rakkibetsudake","Odamoisan"="Odamoi|Odamoisan",
                    "Moyorodake"="Moyoro|Moyorodake", "Hitokappu Volcano Group"="Hitokappu|Hitokappu Volcano Group","Etorofu-Yakeyama"="Yakeyama|Etorofu-Yakeyama",
                    "Etorofu-Atosanupuri"="Atosanu|Atosanupuri|Etorofu-Atosanupuri","Chirippusan"="Chirip|Chirippusan", "Chachadake"="Chacha|Chachadake","Popocatepetl"="Pop.cat.petl","Gede-Pangrango"="Gede|Gede.Pangrango",
                    "Raoul Island"="Raoul", "Hakoneyama"="Hakone|Hakoneyama","Soufriere Guadeloupe"="La Soufriere|Soufriere Guadeloupe",
                    "Kick 'em Jenny"="Jenny","Tenduruk Dagi"="Tenduruk", "Tenerife"="Teide|Pico del Azucar|Piton de Pan de Azucar", 'Unzendake' = 'Unzendake|Unzen'))

#create a csv file containing only the column names before begining the analysis
empty <- tibble()
write_csv(empty,'data_processed/extracted_volc_tk.csv', append = F)

titles <- as.data.frame(t(c((colnames(df)),'v_name','v_country')))
write_csv(titles,'data_processed/extracted_volc_tk.csv', append = T)

# record all articles that name each volcano
# one volcano at a time
# this will take some time... about 10 mins
start_time <- timestamp()
for (i in 1:nrow(rvolc)){
  
  volc_name = rvolc$volc_name[i]
  
  print(volc_name)
  
  single <- df %>%
    # searching for a volcano name with a space either side of the name
    filter(str_detect(tak,paste("",str_to_upper(volc_name),""))) %>% #tak 
    mutate(v_name = volc_name) %>%
    mutate(v_country = rvolc$volc_country[i])
    
  
  write_csv(single,'data_processed/extracted_volc_tk.csv', append = T)
  
}
finish_time <- timestamp()

# analysing volcano extraction --------------------------------------------

v_data_raw <- read_csv('data_processed/extracted_volc_tk.csv')

vdf <- v_data_raw %>% 
  group_by(v_name) %>% 
  tally()
