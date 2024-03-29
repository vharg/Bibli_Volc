# Extracting affiliation country names from any article that contains a volcano name
# Sept 2021

library(tidyverse)
library(readxl)
library(maps) # load the maps library to get access to world.cities data
library(dplyr)

# load and prepare data ----
# read and bind the data for volcanoes with duplicate and non-duplicate names, filtered to remove irrelevant journals
rd <- readRDS('data_processed/extracted_volc_J_filt.Rdata')


## Standardise country names that are often written in different ways 
## within downloaded Wos and Scopus affiliation data

# First drop any rows with no affiliation addresses
rd <- rd %>% 
  drop_na(Addresses)
# Then capitalise all letters to help standardise them
rd$Addresses <- sapply(str_to_upper(rd$Addresses),toString) 
   

# Then insert replacements
rd$Addresses <-  str_replace_all(string = rd$Addresses, 
                                 pattern = c('INDIANA' = 'IN', 'CABO VERDE' = 'CAPE VERDE',
                                             'COMORES' = 'COMOROS', 'KOREA NORTH' = 'NORTH KOREA',
                                             'KOREA SOUTH' = 'SOUTH KOREA','U ARAB EMIRATES' = 'UNITED ARAB EMIRATES',
                                             'GAZA STRIP'= 'PALESTINE', 'ST KITTS AND NEVIS' = 'SAINT KITTS AND NEVIS', 
                                             'UNITED STATES' = 'USA', 'BOSNIA'= 'BOSNIA AND HERZEGOVINA', 
                                             'MADIERA' = 'MADEIRA', 'TRINIDAD TOBAGO' = 'TRINIDAD AND TOBAGO', 
                                             'IVORY COAST' = 'CÔTE D\'IVOIRE', 'BOSNIA' = 'BOSNIA AND HERZEGOVINA',
                                             'FALKLAND ISLAND' = 'FALKLAND ISLANDS', 'UKRAINE'='UCRAINE', 
                                             'PAPUA N GUINEA' = 'PAPUA NEW GUINEA', 'AUSTL' = 'AUSTRALIA', 'R.NION' = 'RÉUNION', 
                                             'ST KITTS' = 'SAINT KITTS AND NEVIS', 
                                             ' DROC | DRC |DEM REP CONGO|CONGO DEMOCRATIC REPUBLIC|DEMOCRATIC REPUBLIC CONGO|DEMOCRATIC REPUBLIC OF THE CONGO' = 'DR CONGO', 
                                             'UK|NORTHERN IRELAND|N. IRELAND|NORTH IRELAND|N IRELAND'= 'UNITED KINGDOM', 
                                             'GAZA' = 'PALESTINE', 'SAINT-BARTHELEMY|ST. BARTS|ST BARTHS' = 'SAINT BARTHÉLEMY'))


# Create a copy of rd for further processing
df <- rd %>% 
  mutate(affil = Addresses)

# create a list of countries as a single string that can be searched
wc_check <- maps::world.cities

world_countries <- wc_check %>% 
  dplyr::filter(!.$country.etc == 'Congo') %>% 
  mutate(c_name = case_when(country.etc == 'Guinea' ~ 'Guinea-Bissau')) %>% 
  mutate(c_name = coalesce(c_name,country.etc)) %>% 
  select(c_name) %>% 
  unique()

countries_string <- world_countries$c_name %>% 
  str_c(.,collapse = '|') %>%
  # edit some of the country names so that they match the way country names are written in GVP
  # e.g. UK --> United Kingdom
  # changing Ukraine to avoid 'UK' being extracted
  str_replace_all(string = ., 
                  pattern = c('Congo Democratic Republic'='DR Congo',
                              'Ukraine'='Ucraine', 'UK'= 'United Kingdom',
                              'R.union'='France')) %>% 
  # remove places that are not countries from the list
  str_replace_all(string = .,
                 pattern = 'Montserrat|Sicily|Easter Island|Mayotte|Pitcairn|Canary Islands|Guam|Wallis and Futuna|Faroe Islands|Azores|US Virgin Islands|Guadeloupe|Tokelau|Svalbard and Jan Mayen|Martinique|Cayman Islands|Netherlands Antilles|New Caledonia|Turks and Caicos|Madeira|Gibraltar|Saint-Barthelemy|Saint Barthélemy|Bermuda|Norfolk Island|Saint-Martin|Bermuda|Saint Pierre and Miquelon|Anguilla|Saint Helena|British Virgin Islands|American Samoa|French Polynesia',
                 replacement = "Place_Removed_Here")
countries_string

# Add in UK string
UK_string <- 'England|Scotland|Wales|Northern Ireland' %>%  
  str_to_upper(.)

# Add USA states - for affiliations that fail to include 'USA|United States'
US_cities <- read_excel('raw_data/USCities.xlsx')
US_states <- (str_c(unique(US_cities$state_name), collapse = "|")) %>% 
  str_to_upper(.)

# Add missing countries or alternative names - do not include Antarctica
other_countries_string <- 'North Korea|South Korea|Serbia|Montenegro|St Kitts and Nevis|Palestine|Bosnia and Herzegovina'

# concatenate all the country/state name strings together
loc_string <- str_c(c(countries_string,UK_string,other_countries_string,US_states), collapse = "|") %>% 
  # and capitalise this string to match the capitalised affiliation addresses
  str_to_upper(.)

# save the string as a text file for later use or manual checks
#writeLines(countries_string, "raw_data/c_string.txt")
#writeLines(loc_string, 'raw_data/locations_string.txt')

# process data ----

#remove author names from affiliations - i.e. delete all text within square brackets
df$affil <- str_replace_all(df$affil,"\\[[^\\]]*\\]", "")
df$affil <- str_replace_all(df$affil,"PERUGIA",'PERRUGIA')

# extract all countries from affiliation addresses 
# using your loc_string to search for matches - this will take some time ~30 seconds  
df$pub_countries <- sapply(str_extract_all(df$affil, loc_string), toString)

# change UK countries into 'UNITED KINGDOM' 
df$pub_countries <- sapply(str_replace_all(df$pub_countries, 
                                           pattern = UK_string, 
                                           replacement = 'UNITED KINGDOM'),toString)

# change USA states into 'USA'
df$pub_countries <- sapply(str_replace_all(df$pub_countries, 
                                           pattern = US_states,
                                           replacement = 'USA'),toString)

# select articles with no matching country - 320 articles all together for now
df_no_country <- df %>% 
  filter(pub_countries == '')

# from manual reading, nearly all of these articles have all authors from the USA, 
# most contain a 5 digit zip code which we can cross check against USA zip codes 
# search for and extract the final 5 digit number in all remaining addresses 
df_no_country$zip <- str_replace(df_no_country$affil,".*\\b(\\d{5})\\b.*", "\\1") %>% 
  as.integer(.)

# crosscheck the extracted 5 digit numbers against a list of US zipcodes 
# download source file, unzip and extract into table
ZipCodeSourceFile = "http://download.geonames.org/export/zip/US.zip"
temp <- tempfile()
download.file(ZipCodeSourceFile , temp)
ZipCodes <- read.table(unz(temp, "US.txt"), sep="\t")
unlink(temp)
names(ZipCodes) = c("CountryCode", "zip", "PlaceName", 
                    "State_name", "AdminCode1", "AdminName2", "AdminCode2", 
                    "AdminName3", "AdminCode3", "latitude", "longitude", "accuracy")

# change country code from 'US' to 'USA' to...
# match the rest of the USA articles
zip_codes <- ZipCodes %>% 
  mutate(CountryCode = 'USA')

# crosscheck zip codes against the list
states <- left_join(df_no_country,zip_codes[,c(2,1)], by='zip' ) %>% 
  mutate(pub_countries = CountryCode ) %>% 
  select(-zip,-CountryCode)

# select only the articles that had matching zip codes
states_matched <- states %>% 
  drop_na(pub_countries)

# for the few articles without a matching zip code, extract USA city names
leftovers <- subset(states, is.na(states$pub_countries))

US_cities_string <- toupper(str_c(unique(US_cities$city), collapse = "|"))

leftovers$pub_cities <- sapply(
  str_extract_all(string = leftovers$affil,
                  pattern = US_cities_string),
  toString)

leftovers_matched <- leftovers %>%
  filter(!pub_cities == '') %>% 
  mutate(pub_countries = 'USA') %>% 
  select(-last_col())

# combine the three groups together
# avoid double counting
df_has_country <- df %>% 
  filter(!pub_countries == '')

df <- rbind(df_has_country, states_matched, leftovers_matched)

saveRDS(df, 'data_processed/countries_extracted_tk.Rdata')

