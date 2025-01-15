#This file takes the project-level food aid data 
#and turns it into the country-year measures. It also 
#turns the UCDP data into our outcome variable.

######Set up: ######
#clear up
rm(list=ls())

#Read in the usual packages
library(tidyverse)
library(knitr)
library(scales)
library(kableExtra)
library(stargazer)
library(lmtest)
library(sandwich)
library(wesanderson)
library(lfe)
library(haven)
library(countrycode)

#Where the cleaned data is: 
data.dir <- "./03 Intermediate/cleaned data"
#Where I want the data to go: 
data.out <- "./03 Intermediate/constructs"
#Where the raw data is: 
data.raw <- "./01 Data"

set.seed(3759)

title1 <- read.csv("./raw_data/title_i.csv", 
                   stringsAsFactors = FALSE, 
                   encoding = "latin1")

title2 <- read.csv("./raw_data/title_ii.csv", 
                   stringsAsFactors = FALSE, 
                   encoding = "latin1")

##################################################################
## This section turns the program-level food aid data           ##
## into country-year observations                               ##
##################################################################

#First, we need to combine the title ii aid so that it's one observation per country-year

df <- title2 %>% 
  select(!X) %>% 
  mutate(Purpose = ifelse(is.na(Purpose), "Combined", Purpose)) %>% 
  group_by(Country, year, Purpose) %>% 
  mutate(commodities = paste(Commodities, sep = ",", collapse = ","), 
         partners    = paste(Sponsor, sep = ",", collapse = ","), 
         n = n()) %>% 
  mutate(commodities = str_to_lower(commodities)) %>% 
  summarise(Value      = sum(Value,      na.rm = TRUE), 
            Tonnage_mt = sum(Tonnage_mt, na.rm = TRUE), 
            Recipients = sum(Recipients, na.rm = TRUE), 
            commodities = first(commodities), 
            partners    = first(partners), 
            
  ) %>% 
  ungroup()

df <- pivot_wider(df, id_cols = c("Country", "year"), 
                  names_from = Purpose, 
                  values_from = c("Tonnage_mt", "Value", "commodities", "partners"))


#Next, we need to make sure we have a country-year observation for each combination: 
df <- complete(df, Country, year)

#Another thing we need to do is drop years for countries which didn't exist in that year
#(ie South Sudan)
df <- df %>% 
  filter(!(Country == "South Sudan" & year < 2011)) %>% 
  filter(!(Country == "Eritrea"     & year < 1993)) %>% 
  filter(!(Country == "Namibia"     & year < 1990)) %>% 
  filter(!(Country == "Montenegro"  & year < 2006)) %>% 
  filter(!(Country == "Slovenia"    & year < 1992)) %>% 
  filter(!(Country == "Macedonia"   & year < 1991)) %>% 
  filter(!(Country == "Timor-Leste" & year < 2002)) %>% 
  filter(!(Country == "Bosnia & Herzegovina" & year < 1992)) %>% 
  filter(!(Country == "Croatia" & year < 1992))
#Other recent countries are Czechia, Slovakia, and Kosovo, 
#but they don't appear in the data

#Replace the na's with zero, apart from the years before
#1994, when development/emergency aid wasn't broken out
title2 <- mutate(df, 
                 Tonnage_mt_development = ifelse(is.na(Tonnage_mt_development) & year > 1994, 0, Tonnage_mt_development),
                 Tonnage_mt_emergency   = ifelse(is.na(Tonnage_mt_emergency)   & year > 1994, 0, Tonnage_mt_emergency),
                 Value_development      = ifelse(is.na(Value_development)      & year > 1994, 0, Value_development),
                 Value_emergency        = ifelse(is.na(Value_emergency)        & year > 1994, 0, Value_emergency))

title2 <- title2 %>% 
  rowwise() %>% 
  mutate(Value = sum(Value_Combined, Value_emergency, Value_development, na.rm = TRUE), 
         Tonnage_mt = sum(Tonnage_mt_Combined, Tonnage_mt_emergency, Tonnage_mt_development, na.rm = TRUE)) %>% 
  ungroup()

#Now I build the shift-share for replicating Nunn and Qian: 
title2 <- mutate(title2, 
                 binary_emergency   = ifelse(Tonnage_mt_emergency   > 0 | Value_emergency   > 0, 1, 0)
  ) %>% 
  group_by(Country) %>% 
  mutate(n = n(), 
         frac_emergency   = sum(binary_emergency  , na.rm = TRUE)/n
  ) %>% 
  ungroup()

#Next the shares (note I am dividing tonnage by 1000 for interpretation): 
title2 <- title2 %>% 
  group_by(year) %>% 
  mutate(Tonnage_mt = Tonnage_mt/1000, 
         Tonnage_mt_development =  Tonnage_mt_development/1000, 
         Tonnage_mt_emergency   =  Tonnage_mt_emergency/1000) %>% 
  mutate(sum_tonnage_emergency   = sum(Tonnage_mt_emergency)) %>% 
  ungroup()

#Now the instrument itself (using tonnage, but we could switch to value)
#For easier interpretation, I shift the instrument by 1000 metric tons:
title2$instrument_emergency   <- title2$frac_emergency   * title2$sum_tonnage_emergency

#Now we can merge in the title 1 data because we already have country-year
#observations. We also multiply it by 1000 to be consistent:
title1 <- title1 %>%  
  select(!Sponsor) %>% 
  select(!Recipients_1000s) %>% 
  select(!funding) %>% 
  select(!X) %>% 
  rename(Value_t1 = Value, 
         Tonnage_mt_t1 = Tonnage_mt) %>% 
  mutate(Tonnage_mt_t1 = Tonnage_mt_t1/1000, 
         Commodities = str_to_lower(Commodities))%>% 
  rename(t1_commodities = Commodities)

title2 <- left_join(title2, title1, by = c("Country", "year")) %>% 
  group_by(Country) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(Tonnage_mt_t1 = ifelse(is.na(Tonnage_mt_t1), 0, Tonnage_mt_t1), 
         Value_t1      = ifelse(is.na(Value_t1)     , 0, Value_t1)) %>% 
  mutate(sum_tonnage_t1 = sum(Tonnage_mt_t1, na.rm = TRUE))

#Finally, build partner/commodity constructs
title2 <- title2 %>% 
  mutate(wheat_binary = paste(commodities_Combined, commodities_emergency, commodities_development, t1_commodities, 
                              sep = ","), 
         wheat_binary = ifelse(str_detect(wheat_binary, "wheat") & !str_detect(wheat_binary, "bulgur wheat"), 1, 0)) %>% 
  mutate(n_partners_emergency = str_count(partners_emergency, ",")+1, 
         n_partners_emergency = ifelse(is.na(n_partners_emergency), 0, n_partners_emergency), 
         n_partners_development = str_count(partners_development, ",")+1, 
         n_partners_development = ifelse(is.na(n_partners_development), 0, n_partners_development)) 

#And standardize the country names: 
title2 <- title2 %>% 
  mutate(Country = countrycode(Country, 
                               origin = "country.name", 
                               destination = "country.name")) %>% 
  filter(!is.na(Country))

##################################################################
## Adjust value data for inflation  and save                    ##
##################################################################



#Create an inflation adjustor: 
#Data from https://fred.stlouisfed.org/series/CPIAUCSL
# last accessed 9.30.2022

deflator <- read.csv(".././01 Data/not_included/us_cpi/CPIAUCSL.csv") %>% 
  mutate(year = substr(DATE, 1, 4) %>% as.numeric()) %>% 
  group_by(year) %>% 
  summarise(cpi = mean(CPIAUCSL, na.rm = TRUE)) %>% 
  mutate(index = ifelse(year == 2018, cpi, NA)) %>% 
  ungroup() %>% 
  mutate(index = mean(index, na.rm = TRUE)) %>% 
  mutate(inflation_adjustment = index / cpi) %>% 
  select(year, inflation_adjustment)

#And multiply all the value columns: 
df <- left_join(title2, deflator, by = "year") %>% 
  mutate(across(.cols = starts_with("Value"), 
                .fns  = ~ .x * inflation_adjustment))

write.csv(df, "./intermediate_data/foodaid.csv")

##################################################################
## This section accomplished a similar task with the UCDP data  ##
##################################################################
#Read in the raw conflict data: 
ucdp <- read.csv("./raw_data/ucdp.csv", 
                 stringsAsFactors = FALSE)

#UCDP conflict deaths data. These data can be accessed at: 
#https://ucdp.uu.se/downloads/index.html#battlerelated 
#(accessed 16 May, 2022)
ucdp_deaths <- read.csv(".././01 Data/not_included/ucdp_deaths/ucdp-brd-conf-201.csv") %>% 
  select(conflict_id, year, bd_best)

#Include only intrastate conflict
ucdp <- filter(ucdp, type_of_conflict != 2)

#We want to create a variable for whether or not a conflict exists: 
ucdp_clean <- ucdp %>% 
  left_join(., ucdp_deaths, by = c("conflict_id", "year")) %>% 
  group_by(location_n, year) %>% 
  mutate(conflict_exists = 1, 
         any_ended = max(ep_end, na.rm = TRUE), 
         intense_exists = ifelse(intensity_level == 2, 1, 0), 
         intense_exists = max(intense_exists, na.rm = TRUE),
         intense_ended  = ifelse(ep_end == 1 & intensity_level == 2, 1, 0), 
         intense_ended  = max(intense_ended, na.rm = TRUE), 
         peripheral     = ifelse(incompatibility == 1 , 1, 0),
         peripheral     = max(peripheral, na.rm = TRUE),
         center_seeking = ifelse(incompatibility == 2, 1, 0), 
         center_seeking = max(center_seeking, na.rm = TRUE), 
         bd_best = sum(bd_best))%>%
  select(location_n, year, conflict_exists, any_ended, intense_exists, intense_ended, 
         peripheral, center_seeking, start_date2, bd_best) %>% 
  distinct() %>% 
  #To avoid duplicates we'll use the older conflict as the start date: 
  mutate(col = nchar(start_date2), 
         start_date2 = substr(start_date2, col - 3, col) %>% as.numeric) %>% 
  select(!col) %>% 
  group_by(location_n, year) %>% 
  mutate(start_date2 = min(start_date2)) %>% 
  ungroup() %>% 
  distinct()

#Any duplicates? 
ucdp_clean %>% 
  group_by(location_n, year) %>% 
  mutate(new_n = n()) %>% 
  filter(new_n > 1)

#And save: 
write.csv(ucdp_clean, "./intermediate_data/ucdp.csv")

