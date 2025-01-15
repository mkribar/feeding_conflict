#This code recreates Table 1 in the paper. 


######Set up: ######
#clear up
rm(list=ls())

#Read in the usual packages
library(tidyverse)
library(ggthemes)
library(knitr)
library(foreign)
library(scales)
library(kableExtra)
library(viridis)
library(stargazer)
library(lmtest)
library(sandwich)
library(wesanderson)
library(lfe)
library(haven)
library(data.table)
library(countrycode)

set.seed(3759)

merged_data <- read.csv("./merged_data/table2_data.csv", 
                        encoding = "latin1") %>% 
  dplyr::select(year, us_cpi) %>% 
  distinct() %>% 
  rename(cpi = us_cpi)

#Here we're gonna look at ITSH as a percentage of value, but also deflate for inflation. 

  #Read in the raw food aid data: 
  title2 <- read.csv("./raw_data/title_ii.csv", 
                     stringsAsFactors = FALSE, 
                     encoding = "latin1") %>% 
    mutate(Country = countrycode(Country, 
                                 origin = "country.name", 
                                 destination = "country.name")) %>% 
    left_join(., merged_data, by = "year")

#Account for inflation:   
cpi_2018 <- merged_data$cpi[merged_data$year == 2018] 

data <- title2 %>% 
  mutate(itsh_inf = ITSH * (cpi_2018/cpi)) %>% 
  filter(!is.na(ITSH)) %>% 
  filter(Tonnage_mt > 0) %>% 
  mutate(cost_per_ton = itsh_inf / Tonnage_mt) %>% 
  mutate(big_program = ifelse(Tonnage_mt > median(Tonnage_mt), 1, 0))


#####Merge the conflict indicator ####

#conflict: 
merged_data <- read.csv("./merged_data/table2_data.csv") %>% 
  dplyr::select(Country, year, conflict_exists, rugged, area) %>% 
  distinct()

data <- left_join(data, merged_data, by = c("Country", "year"))

#####Make the table#####

#Mean for countries 

data <- data %>% 
  mutate(size_tercile = ntile(Tonnage_mt, 3))

table1 <- data.frame(Type = c("Peace", 
                              "Conflict", 
                              "Lower Tercile", 
                              "Middle Tercile", 
                              "Upper Tercile", 
                              "Development", 
                              "Emergency", 
                              "Above median area", 
                              "Below median area", 
                              "Above median ruggedness", 
                              "Below median ruggedness"), 
                     Mean = c(mean(data$cost_per_ton[which(data$conflict_exists == 1)], na.rm = TRUE), 
                              mean(data$cost_per_ton[which(data$conflict_exists == 0)], na.rm = TRUE), 
                              mean(data$cost_per_ton[which(data$size_tercile    == 1)], na.rm = TRUE), 
                              mean(data$cost_per_ton[which(data$size_tercile    == 2)], na.rm = TRUE), 
                              mean(data$cost_per_ton[which(data$size_tercile    == 3)], na.rm = TRUE),
                              mean(data$cost_per_ton[which(data$Purpose == "emergency")]  , na.rm = TRUE),
                              mean(data$cost_per_ton[which(data$Purpose == "development")], na.rm = TRUE),
                              mean(data$cost_per_ton[which(data$area    == TRUE)], na.rm = TRUE), 
                              mean(data$cost_per_ton[which(data$area    == FALSE)], na.rm = TRUE), 
                              mean(data$cost_per_ton[which(data$rugged    == TRUE)], na.rm = TRUE), 
                              mean(data$cost_per_ton[which(data$rugged    == FALSE)], na.rm = TRUE)
                     )
                     )

kable(table1,
      booktabs = T, 
      format = "latex",
      caption = "ITSH Cost Per Ton of Food Aid \\label{table_itsh}", 
      digits = 2, 
      col.names = c("", "Mean Dollars per Ton")) %>% 
  pack_rows("Presence of conflict", 1, 2) %>% 
  pack_rows("Tonnage of food grant", 3, 5) %>% 
  pack_rows("Type of food grant", 6, 7) %>% 
  pack_rows("Area of recipient country", 8, 9) %>%
  pack_rows("Terrain ruggedness of recipient country", 10, 11) %>%
  footnote(general = "Internal Shipping and Handling (ITSH) figures come from the 2014-2019 IFAR reports. Before 2014, these costs were not broken out. Costs are calculated at the program level with constant 2018 dollars.", 
           footnote_as_chunk = TRUE, 
           threeparttable = TRUE) %>% 
  kable_styling(font_size = 10) %>% 
  save_kable(., file = "./output/table_itsh.tex")


#############Some stats for later: #####
data$cost_per_ton[data$Country == "Pakistan"] %>% mean(., na.rm = TRUE)
data$cost_per_ton[data$Country == "Kenya"] %>% mean(., na.rm = TRUE)

data$cost_per_ton[data$Country == "Somalia"] %>% mean(., na.rm = TRUE)

