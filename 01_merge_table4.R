#This package merges in the different data sources to make the dataframe I use for the 
#survival analyses in the paper. 

rm(list = ls())

#Read in the usual packages
library(tidyverse)
library(knitr)
library(scales)
library(kableExtra)
library(lmtest)
library(sandwich)
library(wesanderson)
library(lfe)
library(haven)
library(countrycode)
require(readxl)


set.seed(3759)

##################################################################
## This section builds the IV and DV to replicate Narang (2014) ## 
## First I Read in the UCDP data and turn into a conflict-year  ## 
## complete longitudinal dataset. Then I read in the food       ##
## aid data.                                                    ##
##                                                              ##
## This is enough to replicate table 1 and table 2 from         ##
## "Humanitarian Assistance and the Duration of Peace           ##
## after Civil War"                                             ##
##################################################################

#Read/simplify the ucdp conflict data: 
#https://ucdp.uu.se/downloads/index.html#armedconflict
#last accessed May 16, 2022. 
ucdp <- read.csv("../01 Data/not_included/ucdp_conflict/ucdp-prio-acd-201.csv") %>% 
  filter(type_of_conflict > 2)%>% 
  dplyr::select(!c(starts_with("side"), 
                   starts_with("gwno"))) %>% 
  mutate(conflict_active = 1) %>% 
  mutate(start_year = as.character(start_date2), 
         start_year = substr(start_year, nchar(start_year) - 3, nchar(start_year)), 
         start_year = as.numeric(start_year)) %>% 
  filter(year > 1994 & year < 2018) %>% 
  dplyr::select(conflict_id, year, conflict_active, start_year)

#Now expand into a country-year database: 
ucdp <- ucdp %>% 
  complete(year = min(year):max(year), conflict_id, 
           fill = list(conflict_active = 0))

#Termination data come from the UCDP conflict termination database, 
#available at: https://ucdp.uu.se/downloads/index.html#termination
#(last accessed 16 May, 2022)
term <- read_excel("../01 Data/narang/termination/ucdp-term-acd-3-2021.xlsx") %>% 
  filter(!is.na(outcome)) %>% 
  #Only post 1988, also civil wars: 
  filter(year > 1994 & year < 2018) %>% 
  filter(type_of_conflict > 2) %>% 
  dplyr::select(!c(starts_with("side"), 
                   starts_with("gwno"))) %>% 
  mutate(termination_year = year) %>% 
  dplyr::select(conflict_id, year, conflictep_id, termination_year, conflictepisode, outcome, location, 
                incompatibility)

#Now we add in the termination data: 
peace <- left_join(ucdp, term, by = c("conflict_id", "year")) %>% 
  #Make a binary indicator for peace: 
  mutate(peace = 1 - conflict_active) %>% 
  mutate(peace_ends = ifelse(lead(peace == 0), 1, 0)) %>% 
  #We're not looking at active conflict years: 
  filter(peace == 1 | !is.na(conflictepisode)) %>% 
  group_by(conflict_id) %>% 
  arrange(conflict_id, year) %>% 
  mutate(across(.cols = c("start_year", "conflictep_id", 
                          "termination_year", 
                          "conflictepisode", 
                          "outcome", "location", 
                          "incompatibility"), 
                .fns = ~ na.locf(.x, na.rm = FALSE))) %>% 
  ungroup() %>% 
  filter(!is.na(conflictep_id)) %>% 
  group_by(conflictep_id) %>% 
  mutate(peace_duration = cumsum(peace)) %>% 
  filter(!is.na(termination_year)) %>% 
  ungroup()

#And fix the country names: 
#one hand correction: this is 2015, it's definitely not north Yemen
peace$location[peace$location == "Yemen (North Yemen)"] <- "Yemen"

names <- peace %>% 
  dplyr::select(location) %>% 
  unique() %>% 
  mutate(country = countrycode(location, "country.name", "country.name", 
                               warn = FALSE)) 

peace <- left_join(peace, names, by = "location") %>% 
  filter(!is.na(country))

#Tidy up:   
rm(names, term, ucdp)

#Now start with the food aid variables:  
title2 <- read.csv("./intermediate_data/foodaid.csv") %>% 
  dplyr::select(!X) %>% 
  rename("country" = "Country") %>% 
  mutate(country = countrycode(country, "country.name", "country.name", 
                               warn = FALSE)) 

peace <- left_join(peace, title2, by = c("country", "year"))

#Now we check which countries have no food aid data: 
check <- peace %>% 
  group_by(country) %>% 
  summarise(mean_foodaid = mean(peace_duration, na.rm = TRUE))

#The US didn't give emergency food aid to any of these countries: 
peace$Tonnage_mt_emergency[is.na(peace$Tonnage_mt_emergency)] <- 0

rm(check)

##################################################################
## This section does the same thing for Narang (2015).           ## 
## First I Read in the UCDP data and turn into a conflict-year  ## 
## complete longitudinal dataset. Then I read in the food       ##
## aid data.                                                    ##
##################################################################

#Re-clean UCDP data 
ucdp <- read.csv("../01 Data/not_included/ucdp_conflict/ucdp-prio-acd-201.csv", 
                 stringsAsFactors = FALSE) %>% 
  mutate(location = ifelse(location == "Serbia (Yugoslavia)", "Serbia", location))

ucdp <- ucdp %>% 
  filter(year > 1990) %>% 
  filter(type_of_conflict != 2) %>% 
  mutate(
    peripheral     = ifelse(incompatibility == 1 , 1, 0),
    center_seeking = ifelse(incompatibility == 2, 1, 0)) %>% 
  mutate(col = nchar(start_date2), 
         start_date2 = substr(start_date2, col - 3, col) %>% as.numeric) %>% 
  dplyr::select(!col) %>% 
  mutate(duration = year - start_date2) %>% 
  mutate(country = countrycode(location, 
                               origin = "country.name", 
                               destination = "country.name"))%>% 
  group_by(conflict_id) %>% 
  mutate(duration2 = duration + 1) %>% 
  ungroup() %>% 
  dplyr::select(c("conflict_id", "year", "incompatibility", 
                  "intensity_level", "peripheral", 
                  "duration", "country", "duration2", 
                  "ep_end"))

#Read in the main data: 
peace2 <- left_join(ucdp, title2, by = c("country", "year"))
rm(title2, ucdp)



##################################################################
## Read in the controls used in Narang (2014)                   ##
##################################################################  

#Decisive Victory (already present in termination data:)
peace$decisive <- peace$outcome == 3 | peace$outcome == 4

#Lootable Resources; data available at: 
#https://journals.sagepub.com/doi/suppl/10.1177/0022343309350015
# Last accessed 16 May 2022
  loot <- read_dta("../01 Data/not_included/lootable/Lujala Spoils of Nature data ONSET.dta") %>% 
    mutate(lootable = allgemsP + drugs > 0) %>% 
    dplyr::select(country, lootable) %>% 
    unique() %>% 
    mutate(country = countrycode(country, "country.name", "country.name", 
                                 warn = FALSE)) %>% 
    group_by(country) %>% 
    summarise(lootable = max(lootable)) %>% 
    ungroup() %>% 
    filter(!is.na(country))
  
    peace <- left_join(peace, loot, by = c("country"))
    
    #This produces 7 NAs (for South Sudan). there are neither diamonds
    #nor drugs there, so I make this flase: 
    peace$lootable <- ifelse(is.na(peace$lootable), FALSE, peace$lootable)
    
    rm(loot)

#Treaty (already in database)
  peace$treaty <- peace$outcome == 1  

#Identity war (already in database)
  peace$identity <- peace$incompatibility == 1 | peace$incompatibility == 3

#War-related deaths from the UCDP battle deaths data
#https://ucdp.uu.se/downloads/index.html#termination
#; also Conflict duration (16 May 2022)
  deaths <- read.csv("../01 Data/not_included/deaths/ucdp-brd-dyadic-211.csv") %>% 
    dplyr::select(conflict_id, year, bd_best)

  #Now get the windows for each episode: 
  term <- read_excel("../01 Data/not_included/termination/ucdp-term-acd-3-2021.xlsx") %>%
    left_join(., deaths, by = c("conflict_id", "year")) %>% 
    mutate(conflict_duration = 1) %>% 
    group_by(conflictep_id) %>% 
    mutate(conflict_duration = sum(conflict_duration, na.rm = TRUE), 
           episode_deaths    = sum(bd_best, na.rm = TRUE)) %>% 
    dplyr::select(conflictep_id, conflict_duration, episode_deaths) %>% 
    unique()
  
  peace <- left_join(peace, term, by = "conflictep_id")
  
  rm(term, deaths)

#Factions (back to UCDP data)
  factions <- read.csv("../01 Data/not_included/ucdp_conflict/ucdp-prio-acd-201.csv") %>% 
    mutate(factions = str_count(side_b_id, ",") + 1) %>% 
    dplyr::select(conflict_id, factions) %>% 
    unique()
  
  peace <- left_join(peace, factions, by = "conflict_id")
  
  rm(factions)

#Democracy (measured via polity): 
#https://www.systemicpeace.org/polityproject.html
#last accessed May 16, 2022  
  polity <- read_excel("../01 Data/not_included/polity/p5v2018.xls") %>% 
    filter(year > 1994) %>% 
    dplyr::select(country, year, current_polity = polity) %>% 
    mutate(democracy = current_polity > 5) %>% 
    mutate(country = countrycode(country, "country.name", "country.name", 
                                 warn = FALSE))
  
  peace <- left_join(peace, polity, by = c("country", "year")) %>% 
    mutate(initial_polity = ifelse(conflict_active == 1, current_polity, NA)) %>% 
    group_by(conflictep_id)  %>% 
    mutate(initial_polity = mean(initial_polity, na.rm = TRUE), 
           initial_democracy = initial_polity > 5) %>% 
    ungroup()
  
  rm(polity)

#Infant Mortality Rate from the World Bank
#https://data.worldbank.org/indicator/SP.DYN.IMRT.IN
#Last accessed 16 May, 2022
  mortality <- read.csv("../01 Data/not_included/mortality/API_SP.DYN.IMRT.IN_DS2_en_csv_v2_3913342.csv", 
                        skip = 4) %>% 
    dplyr::select(!c("Country.Code", "Indicator.Name", 
                     "Indicator.Code")) %>% 
    pivot_longer(cols = starts_with("X"), 
                 names_to = "year", 
                 values_to = "mortality")%>% 
    mutate(year = as.numeric(substr(year, 2, 5))) %>% 
    rename("country" = "Country.Name") %>% 
    mutate(country = countrycode(country, "country.name", "country.name", 
                                 warn = FALSE))
  
  peace <- left_join(peace, mortality, by = c("country", "year")) %>% 
    mutate(initial_mortality = mortality * conflict_active) %>% 
    group_by(conflictep_id) %>% 
    mutate(initial_mortality = max(initial_mortality, na.rm = TRUE)) %>% 
    ungroup()
  
  rm(mortality)

#Past agreement comes from the UCDP peace agreement database: 
#https://ucdp.uu.se/downloads/index.html#peaceagreement
#last accessed 16 May, 2022
  pa <- read.csv("../01 Data/not_included/ceasefires/ucdp-peace-agreements-191.csv", 
                 stringsAsFactors = FALSE) %>% 
    dplyr::select(conflict_id, year)  %>% 
    mutate(peace_agreement = 1, 
           conflict_id = as.numeric(conflict_id)) %>% 
    filter(!is.na(conflict_id)) %>% 
    unique()
  
  peace <- left_join(peace, pa, by = c("conflict_id", "year")) %>% 
    group_by(conflictep_id) %>% 
    mutate(peace_agreement = ifelse(is.na(peace_agreement), 0, peace_agreement), 
           peace_agreement = max(peace_agreement, na.rm = TRUE))
  
  rm(pa)

#Government army size again from thw World Bank: 
#https://data.worldbank.org/indicator/MS.MIL.TOTL.P1
#accessed 16 May, 2022  
  army <- read.csv("../01 Data/not_included/armysize/API_MS.MIL.TOTL.P1_DS2_en_csv_v2_3931347.csv", 
                   skip = 4) %>% 
    dplyr::select(!c("Country.Code", "Indicator.Name", 
                     "Indicator.Code")) %>% 
    pivot_longer(cols = starts_with("X"), 
                 names_to = "year", 
                 values_to = "govt_military")%>% 
    mutate(year = as.numeric(substr(year, 2, 5))) %>% 
    rename("country" = "Country.Name") %>% 
    mutate(country = countrycode(country, "country.name", "country.name", 
                                 warn = FALSE))
  
  peace <- left_join(peace, army, by = c("country", "year")) %>% 
    group_by(conflictep_id) %>% 
    mutate(govt_military = ifelse(is.na(govt_military), 0, govt_military), 
           govt_military = max(govt_military, na.rm = TRUE)/1000)
  
  rm(army)

#Mountainous Terrain

  #Read in the geospatial packages: 
  require(sf)
  require(raster)
  require(exactextractr)
  #For this, first we need to read in the shapefiles: 
  #And I need country borders:
  borders <- st_read("../01 Data/not_included/climate_data/borders/world_countries_2020.shp",
                     stringsAsFactors  = FALSE)
  #Remove the countries not in my data set, to save calculation time:
  borders <- borders %>%
    mutate(CNTRY_NAME = countrycode(CNTRY_NAME,
                                    origin = "country.name",
                                    destination = "country.name",
                                    warn = FALSE))%>%
    filter(!is.na(CNTRY_NAME)) %>%
    filter(CNTRY_NAME %in% peace$country)
  
  #Ruggednes data from: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/WXUZBN
  #last accessed 16 May 2022
  terrain <- raster("../01 Data/not_included/Ruggedness_OneKilometerData/ruggedness1K.tif")

#exact_extract does the same thing as the standard extract function, but just faster. 
#So this is taking the mean of every cell in the terrain raster covered by country. 
  y <- exact_extract(terrain, borders, fun = "mean")
  
  y <- data.frame(
    rugged_terrain = y, 
    country = borders$CNTRY_NAME
  ) %>% 
    mutate(country = countrycode(country, 
                                 origin = "country.name",
                                 destination = "country.name",
                                 warn = FALSE))
  
  peace <- left_join(peace, y, by = "country")
  
  rm(y, terrain)

#Former P-5 colony (hand coded)
  colony <- read_excel("../01 Data/not_included/Colonizers.xlsx") %>% 
    mutate(country = countrycode(Country, 
                                 origin = "country.name",
                                 destination = "country.name",
                                 warn = FALSE)) %>% 
    mutate(p5_colony = Colonizer %in% c("United Kingdom", "France", "United States of America", 
                                        "China", "Russia"))
  
  y <- colony %>% 
    dplyr::select(country, p5_colony)
  
  peace <- left_join(peace, y, by = "country")

  #P-5 Contiguity  
  colony <- colony %>% 
    dplyr::select(country, P5)

  p5 <- left_join(borders, colony, by = c(CNTRY_NAME = "country")) %>% 
    dplyr::filter(P5 == 1)
  
  #Identify the intersections: 
  sf::sf_use_s2(FALSE)
  int <- st_intersects(borders, p5) 
  
  p5 <- data.frame(
    country = borders$CNTRY_NAME,
    p5_contiguous = map_int(1:nrow(borders), ~ sum(int[[.x]])) > 0
  )
  
  peace <- left_join(peace, p5, by = "country")
  
  rm(y, int, p5, borders)
  
  
  write.csv(peace, "./merged_data/table3a.csv")

  rm(peace, colony, names)

##################################################################
## Read in the controls used in Narang (2015)                   ##
##################################################################  

#Lagged battle deaths 
#https://ucdp.uu.se/downloads/index.html#termination
deaths <- read.csv("../01 Data/not_included/deaths/ucdp-brd-dyadic-211.csv") %>% 
  dplyr::select(conflict_id, year, bd_best) 

peace2 <- left_join(peace2, deaths, by = c("conflict_id", "year")) %>% 
  group_by(conflict_id) %>% 
  mutate(lagged_deaths = lag(bd_best)) %>% 
  ungroup() %>% 
  #We can do this because there should be zero pre-conflict deaths (or at least very few)
  mutate(lagged_deaths = ifelse(is.na(lagged_deaths), 0, lagged_deaths))
rm(deaths)

#Logged population data come from the world bank; these data can 
#be downloaded at https://data.worldbank.org/indicator/SP.POP.TOTL
#(accessed 16 May, 2022)
population <- read.csv("../01 Data/not_included/population/wb_pop.csv") %>% 
  dplyr::select(!c("Country.Code", "Indicator.Name", 
                   "Indicator.Code")) %>% 
  pivot_longer(cols = starts_with("X"), 
               names_to = "year", 
               values_to = "population")%>% 
  mutate(year = as.numeric(substr(year, 2, 5))) %>% 
  rename("country" = "Country.Name") %>% 
  mutate(country = countrycode(country, "country.name", "country.name", 
                               warn = FALSE))

peace2 <- left_join(peace2, population, by = c("country", "year")) %>% 
  mutate(population = log(population))

rm(population)

#GDP data come from the World Bank, available at: 
# https://data.worldbank.org/indicator/NY.GDP.MKTP.CD
# (last accessed 16 May, 2022) 
gdp <- read.csv("../01 Data/not_included/gdp/global_gdp.csv", 
                skip = 4)%>% 
  dplyr::select(!c("Country.Code", "Indicator.Name", 
                   "Indicator.Code")) %>% 
  pivot_longer(cols = starts_with("X"), 
               names_to = "year", 
               values_to = "gdp_pc")%>% 
  mutate(year = as.numeric(substr(year, 2, 5))) %>% 
  rename("country" = "Country.Name") %>% 
  mutate(country = countrycode(country, "country.name", "country.name", 
                               warn = FALSE))

#We need to deal with really high % of missing data for gdp_pc. I will 
#just carry forward the last non missing values. This is gonig to 
#add some error to this coefficient and also bias it downwards. 
gdp <- gdp %>% 
  group_by(country) %>% 
  arrange(country, year) %>% 
  mutate(gdp_pc = na.locf(gdp_pc, na.rm = FALSE)) %>% 
  ungroup()

peace2 <- left_join(peace2, gdp, by = c("country", "year"))
rm(gdp)


#Polity
#https://www.systemicpeace.org/polityproject.html
#last accessed May 16, 2022  
polity <- read_excel("../01 Data/not_included/polity/p5v2018.xls") %>% 
  filter(year > 1990) %>% 
  dplyr::select(country, year, current_polity = polity2) %>% 
  mutate(democracy = current_polity > 5) %>% 
  mutate(country = countrycode(country, "country.name", "country.name", 
                               warn = FALSE))%>% 
  group_by(country) %>% 
  arrange(country, year) %>% 
  mutate(current_polity = na.locf(current_polity, na.rm = FALSE)) %>% 
  ungroup()

  #Deal with two duplicates: 
  polity <- polity %>% 
    group_by(country, year) %>% 
    summarise(across(.cols = everything(), 
                     .fns = ~ first(.x)))

peace2 <- left_join(peace2, polity, by = c("country", "year"))
rm(polity)

#Diamonds/drugs data available at: 
#https://journals.sagepub.com/doi/suppl/10.1177/0022343309350015
# Last accessed 16 May 2022
loot <- read_dta("../01 Data/not_included/lootable/Lujala Spoils of Nature data ONSET.dta") %>% 
  mutate(lootable = allgemsP + drugs > 0) %>% 
  dplyr::select(country, lootable, 
                drugs, allgemsP) %>% 
  unique() %>% 
  rename("Diamonds" = "allgemsP") %>% 
  mutate(country = countrycode(country, "country.name", "country.name", 
                               warn = FALSE)) %>% 
  group_by(country) %>% 
  summarise(lootable = max(lootable), 
            Diamonds = max(Diamonds), 
            Drugs    = max(drugs)) %>% 
  ungroup() %>% 
  filter(!is.na(country))

  #NAs mean no diamonds or drugs:
  peace2 <- left_join(peace2, loot, by = c("country")) %>% 
    mutate(across(.cols = all_of(c("Diamonds", "Drugs", "lootable")), 
                  .fns  = ~ifelse(is.na(.x), 0, .x)))
  rm(loot)  

#Gurantees FROM ucdp UCDP peace agreement database: 
#https://ucdp.uu.se/downloads/index.html#peaceagreement
pa <- read.csv("../01 Data/not_included/ceasefires/ucdp-peace-agreements-191.csv", 
               stringsAsFactors = FALSE) %>% 
  mutate(guarantee = str_detect(pa_3rd, "guaran") | 
           str_detect(c_3rd , "guaran")) %>% 
  dplyr::select(conflict_id, year, guarantee) %>% 
  mutate(conflict_id = as.numeric(conflict_id)) %>% 
  filter(!is.na(conflict_id)) %>% 
  unique() 

#Need to figure out how to make this one for all years after the guarantee
peace2 <- left_join(peace2, pa, by = c("conflict_id", "year")) %>% 
  arrange(conflict_id, year) %>% 
  group_by(conflict_id) %>% 
  mutate(guarantee = na.locf(guarantee, na.rm = FALSE)) %>% 
  mutate(guarantee = ifelse(is.na(guarantee), FALSE, guarantee)) %>% 
  ungroup()

#mountains/forests  
borders <- st_read("../01 Data/not_included/climate_data/borders/world_countries_2020.shp",
                   stringsAsFactors  = FALSE)%>%
  mutate(CNTRY_NAME = countrycode(CNTRY_NAME,
                                  origin = "country.name",
                                  destination = "country.name",
                                  warn = FALSE))%>%
  filter(!is.na(CNTRY_NAME)) %>%
  filter(CNTRY_NAME %in% peace2$country)
#Ruggednes data from: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/WXUZBN
#last accessed 16 May 2022
  terrain <- raster("../01 Data/not_included/Ruggedness_OneKilometerData/ruggedness1K.tif")
  forest  <- raster("../01 Data/not_included/LandCover_OneKilometerData/LANDCOVER1K.tif")
  
  terrain <- exact_extract(terrain, borders, fun = "mean")
  forest  <- exact_extract(forest , borders, fun = "mean")
  
  
  y <- data.frame(
    rugged_terrain = terrain, 
    forest_cover   = forest,
    country = borders$CNTRY_NAME
  ) %>% 
    mutate(country = countrycode(country, 
                                 origin = "country.name",
                                 destination = "country.name",
                                 warn = FALSE)) %>% 
    group_by(country) %>% 
    summarise(across(.cols = everything(), 
                  .fns = ~ mean(.x, na.rm = TRUE))) %>% 
    ungroup()
  
  peace2 <- left_join(peace2, y, by = "country")
  
  rm(y, terrain)
  
#And output: 
write.csv(peace2, "./merged_data/table3b.csv")  
