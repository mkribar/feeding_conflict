#This file creates the data for: 
# - figure 1
# - table 2
# - table A1

######Set up: ######
#clear up
rm(list=ls())

#Read in the usual packages
library(tidyverse)
library(lfe)
library(haven)
library(countrycode)
require(readxl)
require(data.table)



set.seed(3759)

##################################################################
## This section reads in the food aid and UCDP conflict data    ##
##################################################################

#Read in the main data: 
title2 <- read.csv("./intermediate_data/foodaid.csv") %>% 
  dplyr::select(!X)

ucdp   <- read.csv("./intermediate_data/ucdp.csv", 
                   stringsAsFactors = FALSE) %>% 
  mutate(Country = countrycode(location_n, 
                               origin = "country.name", 
                               destination = "country.name")) %>% 
  mutate(Country = ifelse(Country == "Yemen Arab Republic", "Yemen", Country)) %>% 
  dplyr::select(!location_n) %>% 
  dplyr::select(!X)

#Build some more specific models: 
merged_data <- left_join(title2, ucdp, by = c("Country", "year")) %>% 
  mutate(conflict_exists = ifelse(is.na(conflict_exists), 0, conflict_exists), 
         intense_exists = ifelse(is.na(intense_exists), 0, intense_exists), 
         peripheral = ifelse(is.na(peripheral), 0, peripheral), 
         center_seeking = ifelse(is.na(center_seeking), 0, center_seeking)) %>% 
  mutate(conflict_start = ifelse(conflict_exists == 1 & lag(conflict_exists == 0), 1, 0), 
         intense_start = ifelse(intense_exists == 1 & lag(intense_exists == 0), 1, 0)) %>% 
  mutate(duration = year - start_date2) %>% 
  mutate(bd_best = ifelse(is.na(bd_best), 0, bd_best))

#Tidy up the workspace: 
rm(title2)
rm(ucdp)

##################################################################
## This section merges in the climate data that NQ use          ##
##################################################################
#Monthly recipient temperature and precipitation, from Willmott, C. J. and K. Matsuura 
#(2001) Terrestrial Air Temperature and Precipitation: Monthly and Annual Time Series 
#(1950 - 1999), http://climate.geog.udel.edu/~climate/html_pages/README.ghcn_ts2.html.

#This code takes about 20 minutes to run (on an i7 processor), 
#So I keep it commented out most times when I run the data: 

#This is a hassle because the data is raster, stored as a .nc:
# require(ncdf4)
# require(raster)
# require(sf)
# require(exactextractr)
# 
# #Open the .nc data file:
# nc_air    <- nc_open("../01 Data/climate_data/air.mon.mean.v501.nc")
# nc_precip <- nc_open("../01 Data/climate_data/precip.mon.total.v501.nc")
# 
# lon <- ncvar_get(nc_air, "lon")
# lat <- ncvar_get(nc_air, "lat", verbose = F)
# t <- ncvar_get(nc_air, "time")
# t <- as.POSIXct(("1900-01-01 00:00")) +
#   as.difftime(t, units = "hours") #Need to convert to R's usual date.
# 
#   #Open the actual data:
#   nc.air <- ncvar_get(nc_air, "air")
#   nc.precip <- ncvar_get(nc_precip, "precip")
# 
#   #And I need country borders:
#   borders <- st_read("../01 Data/climate_data/borders/world_countries_2020.shp",
#                      stringsAsFactors  = FALSE)
#     #Remove the countries not in my data set, to save calculation time:
#     borders <- borders %>%
#       mutate(CNTRY_NAME = countrycode(CNTRY_NAME,
#                                       origin = "country.name",
#                                       destination = "country.name", 
#                                       warn = FALSE)) %>%
#       filter(!is.na(CNTRY_NAME)) %>%
#       filter(CNTRY_NAME %in% merged_data$Country)
#     
#     borders <- st_shift_longitude(borders)
#     area    <- st_area(borders)
# 
#   #Now I use a loop to deal with this shit:
#     #filter for the t's we care about.
#     t <- t[as.numeric(substr(t, 1, 4)) >= 1990]
#     years <- 1990:2017
#     months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#     out_final <- list()
# 
# start_time <- Sys.time()
# for(i in 1:length(t)){
#   current_year <- years[i]
#   current_t    <- t[as.numeric(substr(t, 1, 4)) == current_year]
# 
#   data_yr <- list()
#   #We also have to loop over every month:
#   for(i2 in 1:12){
#     #These three lines just extract the right index:
#     t_i <- current_t[i2]
#     t_i <- t == t_i
#     t_i <- which(t_i)
#     #Calc average temp. per country.
#     air.slice <- nc.air[,,t_i]%>%
#       t() %>%
#       raster(., xmn=min(lon),
#              xmx=max(lon),
#              ymn=min(lat),
#              ymx=max(lat),
#              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
#              ) %>%
#       exact_extract(., borders, "mean")
#     
#     #Calc average precip per country:
#     precip.slice <- nc.precip[,,t_i]%>%
#       t() %>%
#       raster(., xmn=min(lon),
#              xmx=max(lon),
#              ymn=min(lat),
#              ymx=max(lat),
#              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0") )%>%
#       exact_extract(., borders, "mean")
# 
#     #put it into a data frame together
#     df <- data.frame(
#       col1 = air.slice,
#       col2 = precip.slice
#     )
#     #Rename for monthly variables: 
#     colnames(df) <- c(paste0("temp_", months[i2]), paste0("precip_", months[i2]))
#     data_yr[[i2]] <- df
#   }
#   #Combine all the data from one year: 
#   data_yr <- do.call("cbind", data_yr)
#   data_yr$year <- current_year
#   data_yr$Country <- borders$CNTRY_NAME
#   out_final[[i]] <- data_yr
# }
# 
# df <- do.call("rbind", out_final)
# end_time <- Sys.time()
# end_time - start_time
# 
# #Finally, we need to take weighted averages because of a few non-contigous states: 
# df <- df %>% 
#   #filter(Country %in% c("South Sudan", "Palestinian Territories")) %>% 
#   mutate(area = rep(area, length(t)/12)) %>% 
#   group_by(Country, year) %>% 
#   mutate(weights = area / sum(area)) %>% 
#   summarise(across(.cols = c(starts_with("temp"), 
#                              starts_with("precip")), 
#                    .fns = ~sum(.x * weights) %>% as.numeric))
# 
# #And now we save it so that we only have to run this code once: 
# climate_data <- write.csv(df, "../01 Data/not_included/climate_data/climate_clean.csv")

climate_data <- read.csv("../01 Data/not_included/climate_data/climate_clean.csv") %>% 
  select(!X)

merged_data <- left_join(merged_data, climate_data, by = c("Country", "year"))
rm(climate_data)

##################################################################
## This section merges in the other controls that NQ use        ##
##################################################################

#The region data are available from the World Bank here: 
# http://databank.worldbank.org/data/download/site-content/CLASS.xls
# (last accessed 16 May, 2022)
  wb_regions <- read_excel("../01 Data/not_included/wb_regions.xls", 
                           sheet = "List of economies", 
                           range = "C7:H223", 
                           col_names = c("Country", "Code", "Blank", "Region", "Income_Group", "Lending"))%>% 
    dplyr::select(Country, Region) %>% 
    mutate(Country = countrycode(Country, 
                                 origin = "country.name", 
                                 destination = "country.name", 
                                 warn = FALSE))
  merged_data <- left_join(merged_data, wb_regions, by = "Country")
  rm(wb_regions)

#US gdp per capita: 
#GDP data come from the World Bank, available at: 
# https://data.worldbank.org/indicator/NY.GDP.MKTP.CD
# (last accessed 16 May, 2022)  
  gdp <- read.csv("../01 Data/not_included/gdp/global_gdp.csv", skip = 4)  %>% 
  dplyr::select(c(Country.Name, starts_with("X"))) %>% 
  pivot_longer(cols = starts_with("X"), 
               names_to = "year", 
               values_to = "gdp_pc_usa") %>% 
  mutate(year = substr(year, 2, 5) %>% as.numeric) %>% 
  filter(Country.Name == "United States") %>% 
  dplyr::select(!Country.Name)

merged_data <- left_join(merged_data, gdp, by = "year")
rm(gdp)

#Binary for whether the US has a democratic president; 
#I hand-coded these. : 
  president <- read.csv("../01 Data/not_included/presidents.csv") %>% 
    dplyr::select(year, binary) %>% 
    rename("Democratic_president" = "binary")
  
  merged_data <- left_join(merged_data, president, by = "year")
  rm(president)

#Global oil price data (Cushing, OK crude oil (dollars per barrel))
#come from the US Dept. of Energy and are available here: 
# http://www.eia.gov/dnav/pet/pet_pri_spt_s1_d.htm
# (last accessed 16 May, 2022)
  oil <- read_excel("../01 Data/not_included/oil/PET_PRI_SPT_S1_D.xls", 
                    sheet = "Data 1", 
                    skip = 2) 
  colnames(oil) <- c("year", "crude_oil", "blank")
  oil <- oil %>% 
    dplyr::select(!blank) %>% 
    mutate(year = substr(as.character(year), 1, 4)) %>% 
    group_by(year) %>% 
    summarise(crude_oil = mean(crude_oil, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(year = as.double(year))

  merged_data <- left_join(merged_data, oil, by = "year")
  rm(oil)

#Average US military aid:
#Avg. US economic aid (net of food aid)
#These data come from https://foreignassistance.gov/
# (last accessed 16 May, 2022)

  #NQ interact year fixed effects with the average amount 
  #of per capita aid
  aid <- fread("../01 Data/not_included/us_foreign_aid_complete.csv")  %>% 
    filter(`Fiscal Year` > 1989) %>% 
    mutate(type = `Foreign Assistance Objective Name`, 
           type = ifelse(`International Sector Name` %in% 
                           c("Developmental Food Aid/Food Security Assistance", 
                             "Emergency Response", 
                             "Disaster Prevention and Preparedness"), 
                         "Humanitarian", type))

  #We need to do the country code early to get everything summarized 
  #properly, but this is a workaround to get it done faster (run country
  #code on unique values then merge  : 
  x <- data.frame(`Country Name` = unique(aid$`Country Name`)) %>% 
    mutate(Country = countrycode(Country.Name, 
                                 origin = "country.name", 
                                 destination = "country.name", 
                                 warn = FALSE))

  #reshape the foreign assistance data: 
  aid <- aid %>% 
    left_join(., x, by = c(`Country Name` = "Country.Name")) %>% 
    filter(!is.na(Country)) %>% 
    group_by(Country, `Fiscal Year`, type) %>% 
    rename("year" = `Fiscal Year`) %>% 
    summarise(amount = sum(`Constant Dollar Amount`)) %>% 
    mutate(year = as.numeric(year)) %>% 
    ungroup() %>% 
    pivot_wider(id_cols = c("Country", "year"), 
                names_from = "type", 
                values_from = "amount")

  #Population data come from the world bank; these data can 
  #be downloaded at https://data.worldbank.org/indicator/SP.POP.TOTL
  #(accessed 16 May, 2022)
  pop <- read.csv("../01 Data/not_included/population/wb_pop.csv")%>% 
    dplyr::select(`Country.Name`, starts_with("X")) %>% 
    rename(Country = `Country.Name`) %>% 
    pivot_longer(cols = starts_with("X"), 
                 values_to = "population", 
                 names_to  = "year") %>% 
    mutate(year = substr(year, 2, 5) %>% as.numeric) %>% 
    mutate(Country = countrycode(Country, 
                                 origin = "country.name", 
                                 destination = "country.name", 
                                 warn = FALSE)) %>% 
    filter(!is.na(Country))

  #Build the different categories: 
  aid <- left_join(aid, pop, by = c("Country", "year")) %>% 
    filter(!is.na(population)) %>% 
    mutate(Military = as.character(Economic), 
           Military = ifelse(is.na(Military), "0", Military), 
           Military = as.numeric(Military)) %>% 
    mutate(Economic = as.character(Economic), 
           Economic = ifelse(is.na(Economic), "0", Economic), 
           Economic = as.numeric(Economic)) %>% 
    mutate(military_aid_pc = Military / population, 
           military_aid_pc = ifelse(is.na(military_aid_pc), 0, military_aid_pc), 
           economic_aid_pc = Economic / population, 
           economic_aid_pc = ifelse(is.na(economic_aid_pc), 0, economic_aid_pc)) %>% 
    select(!Humanitarian)

  #Merge + tidy: 
  merged_data <- left_join(merged_data, aid, by = c("Country", "year"))
  rm(aid, pop)
  rm(x)

  #Check for duplicates
  merged_data %>% 
    group_by(Country, year) %>% 
    summarise(count = n()) %>% 
    filter(count > 1)

#Next I merge in data on cereal imports. These data are the FAOStat 
#detailed trade matrix, which can be downloaded here: 
# https://www.fao.org/faostat/en/#data/TM
# (accessed 16 May, 2022)
  
  #First define a list of cereals ('hand-coded' from the list of categories)
  cereals <- c("barley", "maize", "millet", "quinoa", "rice", "sorghum", "soybean", "teff", "wheat", "rye", "oat")
  
  #Note--this is a big dataset and it takes a while: 
  cereal_imports <- fread("../01 Data/not_included/faostat/Trade_DetailedTradeMatrix_E_All_Data.csv", 
                          encoding = "Latin-1")
  
  #This paragraph reads in the data and cleans it: 
  cereal_imports <- cereal_imports %>%   
    select(!ends_with("F")) %>% 
    filter(Element == "Import Quantity") %>% 
    #Filter only for grains
    mutate(Item = str_to_lower(Item),
           cereal_indicator = str_detect(Item, paste(cereals, collapse = "|"))) %>% 
    filter(cereal_indicator == 1) %>% 
    #lol: goat =/= oat
    filter(!str_detect(Item, "goat")) %>% 
    #create country year sums: 
    select(`Reporter Countries`, Item, starts_with("Y")) %>% 
    pivot_longer(cols = starts_with("Y"), 
                 names_to = "year", 
                 values_to = "tonnage") %>% 
    rename(Country = `Reporter Countries`) %>% 
    group_by(Country, year) %>% 
    summarise(cereal_imports = sum(tonnage, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(year = substr(year, 2, 5) %>% as.numeric) %>% 
    #rescale to matach the food aid data: 
    mutate(cereal_imports = cereal_imports / 1000)
  
  #This code cleans up the country name: 
  cereal_imports <- cereal_imports %>% 
    filter(Country != "French Guyanna")%>% 
    mutate(Country = countrycode(Country, 
                                 origin = "country.name", 
                                 destination = "country.name")) %>% 
    filter(!is.na(Country)) %>% 
    #This last step combines the USSR and Russia, plus the Ethiopia and Ethiopiua PDR data
    group_by(Country, year) %>% 
    summarise(cereal_imports = sum(cereal_imports, na.rm = TRUE)) %>% 
    ungroup()
  
  #Merge it in: 
  merged_data <- left_join(merged_data, cereal_imports, by = c("Country", "year"))
  
  #Divide this by population to get per capita numbers. 
  merged_data$cereal_imports_pc <- merged_data$cereal_imports/merged_data$population
  
  
  #Here I add vdem because it has much better coverage than polity. Vdem data
  #are available at https://www.v-dem.net/ (last accessed 16 May, 2022)
  vdem <- fread("../01 Data/not_included/vdem/V-Dem-CY-Core-v11.1.csv") %>% 
    filter(year > 1989) %>% 
    dplyr::select(country_name, year, v2x_regime) %>% 
    rename("Country" = "country_name", 
           "vdem" = "v2x_regime") %>% 
    mutate(Country = countrycode(Country, 
                                 origin = "country.name", 
                                 destination = "country.name", 
                                 warn = FALSE))

  merged_data <- left_join(merged_data, vdem, by = c("Country", "year"))
  rm(vdem)

##################################################################
## This section merges in more variables, the ones that Mary    ##
## and Mishra use.                                              ##
##################################################################

#Weighted average of per capita humanitarian food aid in 
#neighboring countries
require("sf")

#World boundaries are available from IPUMS: 
#https://international.ipums.org/international/gis.shtml
  #Last accessed 16 May, 2022
  
borders <- st_read("../01 Data/not_included/climate_data/borders/world_countries_2020.shp",
                   stringsAsFactors  = FALSE)
#Remove the countries not in my data set, to save calculation time:
borders <- borders %>%
  mutate(CNTRY_NAME = countrycode(CNTRY_NAME,
                                  origin = "country.name",
                                  destination = "country.name", 
                                  warn = FALSE)) %>%
  filter(!is.na(CNTRY_NAME)) %>%
  filter(CNTRY_NAME %in% merged_data$Country)

#This chunk of code finds the length of shared borders between 
#countries (i.e. the weights)
sf::sf_use_s2(FALSE)

  Touching_list <- st_touches(borders)
  perimeters <- st_length(borders)
  
  
  all.length.list <- lapply(1:length(Touching_list), function(from) {
    lines <- st_intersection(borders[from,], borders[Touching_list[[from]],])
    lines <- st_cast(lines) # In case of multiple geometries
    l_lines <- st_length(lines)
    if (length(Touching_list[[from]] > 0)){
      res <- data.frame(origin = from,
                        perimeter = as.vector(perimeters[from]),
                        touching = Touching_list[[from]],
                        t.length = as.vector(l_lines),
                        t.pc = as.vector(l_lines/perimeters[from]))
      res
    } else {
      res <- data.frame(origin = from,
                        perimeter = as.vector(perimeters[from]),
                        touching = NA,
                        t.length = 0,
                        t.pc = 0)         
    }
  })
  all.length.df <- do.call("rbind", all.length.list)

#Now create a crosswalk between "from" and the actual countries
cross_walk <- data.frame(
  Country = borders$CNTRY_NAME, 
  number = 1:nrow(borders)
)
#Merge this crosswalk with the data we care about
temp <- merged_data %>% 
  dplyr::select(year, Country, Tonnage_mt_emergency, conflict_exists) %>% 
  left_join(., cross_walk)

#And take the weighted average of all the touching countries. 
df <- all.length.df %>% 
  right_join(temp, by = c("touching" = "number")) %>% 
  group_by(Country, year) %>% 
  summarise(bordering_tonnage_emergency = sum(Tonnage_mt_emergency*t.pc, na.rm = TRUE),
            bordering_conflict_exists   = sum(conflict_exists*t.pc,      na.rm = TRUE))

merged_data <- left_join(merged_data, df, by = c("Country", "year"))
rm(df, all.length.df, all.length.list, cross_walk, Touching_list, 
   temp, perimeters, borders)

#GDP data come from the World Bank, available at: 
# https://data.worldbank.org/indicator/NY.GDP.MKTP.CD
# (last accessed 16 May, 2022)  
  gdp <- read.csv("../01 Data/not_included/gdp/global_gdp.csv", skip = 4) %>% 
    dplyr::select(Country.Name, starts_with("X")) %>% 
    pivot_longer(cols = starts_with("X"), 
                 names_to = "year", 
                 values_to = "gdp_pc") %>% 
    mutate(year = substr(year, 2, 5) %>% as.numeric) %>% 
    rename(Country = Country.Name) %>% 
    mutate(Country = countrycode(Country, 
                                 origin = "country.name", 
                                 destination = "country.name", 
                                 warn = FALSE)) %>% 
    filter(!is.na(Country))
  
  gdp %>% 
    group_by(Country, year) %>% 
    summarize(count = n()) %>% 
    filter(count > 1)
  
  merged_data <- left_join(merged_data, gdp, by = c("Country", "year"))
  rm(gdp)

#Inflation data come from the World Bank and are available at: 
#https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG
# (last accessd 16 MAy, 2022)
  inflation <- read.csv("../01 Data/not_included/inflation/API_FP.CPI.TOTL.ZG_DS2_en_csv_v2_3469540.csv",
                        skip = 4) %>% 
    dplyr::select(Country.Name, starts_with("X")) %>% 
    pivot_longer(cols = starts_with("X"), 
                 names_to = "year", 
                 values_to = "inflation") %>% 
    mutate(year = substr(year, 2, 5) %>% as.numeric) %>% 
    rename(Country = Country.Name) %>% 
    mutate(Country = countrycode(Country, 
                                 origin = "country.name", 
                                 destination = "country.name", 
                                 warn = FALSE)) 
  
  merged_data <- left_join(merged_data, inflation, by = c("Country", "year"))

##we seperately need US CPI to deflate the ITSH for figure 1:
#CPI is BLS's  Consumer Price Index for All Urban Consumers
#https://fred.stlouisfed.org/series/CPIAUCSL (16 May 2022)
  inflation <- read.csv("../01 Data/not_included/us_cpi/CPIAUCSL.csv", 
                        stringsAsFactors = FALSE) %>% 
    rename(year = DATE) %>% 
    mutate(year = format(as.POSIXct(year), "%Y"), 
           year = as.numeric(year)) %>% 
    group_by(year) %>% 
    summarise(us_cpi = mean(CPIAUCSL))
   
  merged_data <- left_join(merged_data, inflation, by = "year")
    
  rm(inflation)

#Polity Score
#Downloaded here: https://www.systemicpeace.org/inscrdata.html
#Last accessed 16 May, 2022
  polity <- read_excel("../01 Data/not_included/polity/p5v2018.xls") %>% 
    select(country, year, polity2) %>% 
    filter(year > 1989) %>% 
    mutate(country = countrycode(country, 
                                 origin = "country.name", 
                                 destination = "country.name", 
                                 warn = FALSE)) %>% 
    filter(!is.na(country)) %>% 
    #Now I do this to deal with some duplicates: 
    group_by(country, year) %>% 
    summarise(polity2 = mean(polity2)) %>% 
    ungroup()


  merged_data <- left_join(merged_data, polity, by = c("Country" = "country", "year"))
  
  merged_data %>% 
    group_by(Country, year) %>% 
    summarise(count = n()) %>% 
    filter(count > 1)

#The Ethnic Power Relations (EPR) data are available from: 
# https://icr.ethz.ch/data/epr/ (last accessed 16 May, 2022) 
  epr <- read.csv("../01 Data/not_included/epr/EPR-2019.csv", 
                  stringsAsFactors = FALSE) %>% 
    mutate(statename = countrycode(statename, 
                                   origin = "country.name", 
                                   destination = "country.name"))  

#Actual data manipulation
  epr <- epr %>% 
    mutate(pct_excluded = ifelse(status %in% c("DISCRIMINATED", "POWERLESS"), size, 0), 
           pct_ruling   = ifelse(status %in% c("DOMINANT", "MONOPOLY"), size, 0), 
           binary_autonomous = ifelse(reg_aut == "true", 1, 0), 
           pct_autonomous = ifelse(reg_aut == "true", size, 0)) %>% 
    group_by(statename, from) %>% 
    summarise(pct_excluded = sum(pct_excluded, na.rm = TRUE), 
              pct_ruling   = sum(pct_ruling, na.rm = TRUE), 
              binary_autonomous = max(binary_autonomous, na.rm = TRUE), 
              pct_autonomous = sum(pct_autonomous, na.rm = TRUE)) %>% 
    ungroup() %>% 
    complete(statename, from = 1946:2017) %>% 
    group_by(statename) %>% 
    fill(., pct_excluded, pct_ruling, binary_autonomous, pct_autonomous) %>% 
    ungroup() %>% 
    rename(year = from, 
           Country = statename) %>% 
    filter(!is.na(Country))
  
  merged_data <- left_join(merged_data, epr, by = c("Country", "year"))

#Avg recipient cereal production, also from FAOStat:
#https://www.fao.org/faostat/en/#data/QCL, 
#accessed 16 May, 2022  
  cereal_production <- fread("../01 Data/not_included/faostat/cereal_production.csv", 
                             encoding = "Latin-1") %>% 
    group_by(Area, Year) %>% 
    summarize(cereal_production = sum(Value, na.rm = TRUE)/1000) %>% 
    rename("Country" = "Area", 
           "year" = "Year") %>% 
    mutate(Country = countrycode(Country, 
                                 origin = "country.name", 
                                 destination = "country.name", 
                                 warn = FALSE))
  
  merged_data <- left_join(merged_data, cereal_production, by = c("Country", "year"))
  rm(cereal_production)
  rm(cereals)  
  
#This step merges in replication data from the Nunn and Qian (2014) paper; these
# data are available at: https://www.aeaweb.org/articles?id=10.1257/aer.104.6.1630
# (last accessed 16 May, 2016)  
  nq <- read_dta(file =  "../01 Data/not_included/nunn and qian/FAid_Final.dta") %>% 
    dplyr::select(Country = recipient_country, 
           nq_wheat_aid = wheat_aid, 
           year, 
           frac_nq = fadum_avg, 
           wb_region) %>% 
    group_by(year) %>% 
    mutate(instrument_nq = frac_nq * sum(nq_wheat_aid, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(Country = countrycode(Country, 
                                 origin = "country.name", 
                                 destination = "country.name"))
  
  df <- dplyr::select(nq, Country, wb_region) %>% 
    distinct()
  
  nq <- dplyr::select(nq, !wb_region)
  
  
  merged_data <- left_join(merged_data, nq, by = c("Country", "year"))
  merged_data <- left_join(merged_data, df, by = "Country")
  
  
################################################################
## Here I sift through the bulk downloaded DAC data from the  ##
## CRS. This takes a lot of time, but the pre-cleaned         ##
## humanitarian aid time series are only available back to    ##
## 2015. This is for figure 2.                                ##      
################################################################
  #Pre-populate: 
  years <- 2006:2019%>% as.character()
  years <- c(years, "2002-03", "2004-05", "2000-01", "1995-99", "1973-94")
  
  dac_out <- list()
  
  #Now loop over each year of DAC data which is stored seperately: 
  for(i in 1:length(years)){
    filename <- paste0("CRS ", years[i], " data.txt")
    
    #Read in the file and extract only the data I need (US humanitarian
    #aid disbursements). Data are from the OECD creditor reporting system
    # https://stats.oecd.org/DownloadFiles.aspx?DatasetCode=CRS1
    #accessed 16 May, 2022
    
      #NB: I had this saved somewhere else because it's a comically large dataset. 
      #3.14 gigs altogether.
    x <- fread(paste0("C:/Users/mkrib/Documents/007 Data/OECD CRS/", filename), 
               sep = "|") %>% 
      filter(DonorName == "United States")
    
    colnames(x)[colnames(x) %in% c("Year", "Years")] <- "year"
    
    #We have to do this seperately for the single-year and multiple-year
    #datasets. 
    x <- x %>% 
      filter(SectorCode == 720 & DonorCode == 302) %>% 
      mutate(Country = countrycode(RecipientName, 
                                   origin = "country.name", 
                                   destination = "country.name")) %>% 
      filter(!is.na(Country)) %>% 
      group_by(Country, year) %>% 
      summarise(oecd_disbursement = sum(USD_Disbursement_Defl, na.rm = TRUE)) %>% 
      ungroup()
    
    
    #And output: 
    dac_out[[i]] <- x
  }
  
  dac <- do.call("rbind", dac_out)
  
  merged_data <- left_join(merged_data, dac, by = c("Country", "year")) %>% 
    mutate(oecd_disbursement = ifelse(is.na(oecd_disbursement), 0, oecd_disbursement))
  
    

##################################################################
## Merge in WFP tonnage (for figure 2)                          ##
##################################################################  

  #This is a little more complicated, since all these data are in differenct csv files
  #for each year:  
  datalist <- list()
  
  for(i in 1991:2012){
    
    wfp <- read.csv(paste0("../01 Data/wfp/Tonnage/", 
                           i, 
                           " Tonnage & IRMAt.csv"))
    
    df <- wfp %>% 
      group_by(Recipient, Food.Aid.Type) %>% 
      summarise(Tonnage = sum(Actual.Tons.Delivered, na.rm = TRUE)/1000) %>% 
      pivot_wider(id_cols = "Recipient", 
                  names_from = "Food.Aid.Type", 
                  values_from = "Tonnage", 
                  values_fill = 0) %>% 
      mutate(year = i)
    
    datalist[[i]] <- df
  }
  
  wfp <- do.call(rbind, datalist)
  rm(df, datalist)
  
  
  #Standardize the names: 
  wfp <- wfp %>% 
    rename("Country" = "Recipient") %>% 
    mutate(Country = countrycode(Country, 
                                 origin = "country.name", 
                                 destination = "country.name")) %>% 
    filter(!is.na(Country)) %>% 
    rename("wfp_tonnage_emergency" = "Emergency", 
           "wfp_tonnage_project"   = "Project", 
           "wfp_tonnage_program"   = "Programme")
  
  
  merged_data <- left_join(merged_data, wfp, by = c("Country", "year"))    
  
  
##################################################################
## Add area (for table 1)                                       ##
##################################################################  
  
  #Read in the geospatial packages: 
  require(sf)
  borders <- st_read("../01 Data/not_included/climate_data/borders/world_countries_2020.shp",
                     stringsAsFactors  = FALSE)
  #Remove the countries not in my data set, to save calculation time:
  borders <- borders %>%
    mutate(CNTRY_NAME = countrycode(CNTRY_NAME,
                                    origin = "country.name",
                                    destination = "country.name",
                                    warn = FALSE))%>%
    filter(!is.na(CNTRY_NAME)) %>%
    filter(CNTRY_NAME %in% merged_data$Country)
  
  sf_use_s2(FALSE)
  z <- st_area(borders)/1000000
  
  y <- data.frame(
    area           = z %>% as.numeric(),
    Country = borders$CNTRY_NAME
  ) %>% 
    #Now just take the sum of the noncontiguous countries: 
    group_by(Country) %>% 
    summarize(area           = sum(area)) %>% 
    ungroup() %>% 
    mutate(area           = area           > median(area))
  
  #and merge
  merged_data <- left_join(merged_data, y, by = "Country")
  

  
##################################################################
## Add ruggedness (for table 1)                                       ##
##################################################################  
  require(exactextractr)
  
  rugged <- raster::raster("../01 Data/not_included/Ruggedness_OneKilometerData/ruggedness1K.tif")
  rugged <- exact_extract(
    rugged, 
    borders, 
    fun = "mean", 
    append_cols = "CNTRY_NAME"
  )
  rugged$area <- unclass(z) #unclass here gets rid of units so we can take the weighted mean
  
  
  #Combine non-contiguous areas: 
  rugged <- rugged %>% 
    group_by(CNTRY_NAME) %>% 
    summarise(rugged = weighted.mean(mean, area)) %>% 
    mutate(rugged = rugged > median(rugged))
  
  merged_data <- left_join(merged_data, rugged, by = c("Country" = "CNTRY_NAME"))

##################################################################
## Output the data                                              ##
##################################################################
  
write.csv(merged_data, "./merged_data/table2_data.csv")  
