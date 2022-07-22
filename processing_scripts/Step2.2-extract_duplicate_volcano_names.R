# Extracting duplicate volcano names from WoS results
# This code detects duplicate volcano names AND the country that the volcano is from
# Non-unique names can be made unique by scanning for the country name simultaneously
# Sept 2021

library(tidyverse)
library(readxl)

# load data ----
wos <- readRDS('raw_data/volcan_star_tk.Rdata')

# select publication data that is desired
wos_select <- wos %>% 
  select(Authors,`Article Title`, `Publication Year`, Abstract, Addresses, 
         DOI, `Times Cited, All Databases`,`Source Title`,tak)

#load scopus data
scopus <- readRDS('raw_data/scopus_tk.Rdata')

df <- rbind(wos_select,scopus)

# check range of years that articles were published
range(df$`Publication Year`,na.rm = T)

# list of duplicate volcano names and their country
rvolc <- read_excel('raw_data/GVP.xlsx') %>% 
  select(`Volcano Name`,Country) %>% 
  dplyr::rename(volc_name = `Volcano Name`, volc_country = Country) %>% 
  filter(!volc_country == 'Undersea Features') %>% 
  group_by(volc_name) %>% 
  # keep only non unique volcano names
  filter(n() > 1) %>% 
  ungroup()

#replace certain volcano names with multiple strings to more easily pick up
rvolc$volc_name <- str_replace_all(rvolc$volc_name, c("Akagisan"="Akagi|Akagisan", "Bulusan"="Bulu|Bulusan",
                    "Asosan"="Aso|Asosan","Bandaisan"="Bandai|Bandaisan","Berutarubesan" = "Berutarube|Berutarubesan", 
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
                    "Kick 'em Jenny"="Jenny","Tenduruk Dagi"="Tenduruk", "Tenerife"="Teide|Pico del Azucar|Piton de Pan de Azucar"))


#create a csv file containing only the column names before begining the analysis
empty <- tibble()
write_csv(empty,'data_processed/extracted_dup_volc_tk.csv', append = F)

titles <- as.data.frame(t(c((colnames(df)),'v_name','v_country')))
write_csv(titles,'data_processed/extracted_dup_volc_tk.csv', append = T)

# record all articles that name each volcano
for (i in 1:nrow(rvolc)){
  
  volc_name = rvolc$volc_name[i]
  volc_country = rvolc$volc_country[i]
  
  print(volc_name)
  print(volc_country)
  
  single <- df %>%
    # searching for a volcano name with a space either side of the name
    # and searching for the volcano's country name simultaneously
    dplyr::filter(str_detect(tak,paste("",str_to_upper(volc_name),""))
           &
             str_detect(tak,str_to_upper(volc_country))) %>% 
    mutate(v_name = volc_name) %>%
    mutate(v_country = rvolc$volc_country[i])
    
  
  write_csv(single,'data_processed/extracted_dup_volc_tk.csv', append = T)
  
}


# analysing volcano extraction --------------------------------------------

v_data_raw <- read_csv('data_processed/extracted_dup_volc_tk.csv')

vdf <- v_data_raw %>% 
  group_by(v_name,v_country) %>% 
  tally()
