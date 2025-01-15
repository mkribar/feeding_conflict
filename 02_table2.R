#This code creates three tables: 
# - Table 2 in the article (replicates three specifications)
#   of humanitarian aid -> conflict) 
# - Table A.1 (fully replicates table 2 from Nunn and Qian 2014)
# - Table A.5 which uses the same specifications as Table 2 
#   (from my article) but using value instead of tonnage. 


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

set.seed(3759)

merged_data <- read.csv("./merged_data/table2_data.csv", 
                        encoding = "latin1")


#Extract this to deflate values: 
cpi_2018 <- merged_data$us_cpi[merged_data$year == 2018] %>% 
  unique()

#First I create a dataframe called data where the data from 
#above are turned into the version NQ use. 
data <- merged_data %>% 
  mutate(Region = as.factor(Region), 
         year   = as.factor(year)) %>% 
  #US real per capital GDP * avg, prob, of any US food aid
  mutate(per_capita_control = gdp_pc_usa * frac_emergency) %>% 
  #US democratic president * avg. prob. of any US food aid
  mutate(per_capita_control = gdp_pc_usa * frac_emergency) %>% 
  #Monthly weather * avg. prob. of receiving US food aid. 
  mutate(across(c(starts_with("temp"), 
                  starts_with("precip")), 
                .fns = ~ .x * frac_emergency, 
                .names = paste0("{.col}", "_avg"))) %>% 
  #Avg. amount of US military aid: 
  group_by(Country) %>% 
  mutate(avg_military = mean(military_aid_pc, na.rm = TRUE)) %>% 
  #Avg. amount of US economic aid: 
  mutate(avg_economic = mean(economic_aid_pc, na.rm = TRUE)) %>% 
  #Avg. recipient cereal imports: 
  mutate(avg_cereal_imports = mean(cereal_imports, na.rm = TRUE)) %>% 
  #Avg. recipient cereal production: 
  mutate(avg_cereal_production = mean(cereal_production, na.rm = TRUE)) %>% 
  #And make value into 1000s of USD, for table A.5
  mutate(Value_emergency = Value_emergency * (cpi_2018/us_cpi), 
         Value_emergency = Value_emergency/1000000)


##################################################################
## This section fully replicates Table 2 of Nunn and Qian       ##
## 2014 (on page 1644) (Table A.1 in my article)                ##
##################################################################

#Column 1: 
  panel_A <- felm(conflict_exists ~ Tonnage_mt_emergency | Country + Region:year| 0 | Country, 
     data = data) 

  panel_B <- felm(conflict_exists ~ instrument_emergency | Country + Region:year| 0 | Country, 
     data = data) 
  
  panel_C <- felm(conflict_exists ~ 0 | Country + Region:year| 
       (Tonnage_mt_emergency ~ instrument_emergency) | Country, 
     data = data) 

  column_1 <- c(
    summary(panel_A)$coefficients[1,1],
    summary(panel_A)$coefficients[1,2],
    summary(panel_A)$r.squared,
    summary(panel_B)$coefficients[1,1],
    summary(panel_B)$coefficients[1,2],
    summary(panel_B)$r.squared,
    summary(panel_C)$coefficients[1,1],
    summary(panel_C)$coefficients[1,2],
    summary(panel_C)$r.squared,
    summary(panel_C$stage1)$coefficients[1,1], 
    summary(panel_C$stage1)$coefficients[1,2], 
    panel_C$stage1$iv1fstat[[1]][5], 
    panel_C$N
  )

#Column 2: 
  panel_A <- felm(conflict_exists ~ Tonnage_mt_emergency + 
                    per_capita_control:frac_emergency + 
                    Democratic_president:frac_emergency | Country + Region:year| 0 | Country, 
                  data = data) 
  
  panel_B <- felm(conflict_exists ~ instrument_emergency + 
                    per_capita_control:frac_emergency + 
                    Democratic_president:frac_emergency | Country + Region:year| 0 | Country, 
                  data = data) 
  
  panel_C <- felm(conflict_exists ~  
                    per_capita_control:frac_emergency + 
                    Democratic_president:frac_emergency | Country + Region:year | 
                    (Tonnage_mt_emergency ~ instrument_emergency) | Country, 
                  data = data) 
  
  column_2 <- c(
    summary(panel_A)$coefficients[1,1],
    summary(panel_A)$coefficients[1,2],
    summary(panel_A)$r.squared,
    summary(panel_B)$coefficients[1,1],
    summary(panel_B)$coefficients[1,2],
    summary(panel_B)$r.squared,
    summary(panel_C)$coefficients[1,1],
    summary(panel_C)$coefficients[1,2],
    summary(panel_C)$r.squared,
    summary(panel_C$stage1)$coefficients[nrow(summary(panel_C$stage1)$coefficients),1], 
    summary(panel_C$stage1)$coefficients[nrow(summary(panel_C$stage1)$coefficients),2], 
    panel_C$stage1$iv1fstat[[1]][5], 
    panel_C$N
  )
  
#Column 3: 
  panel_A <- felm(conflict_exists ~ Tonnage_mt_emergency + 
                    per_capita_control:frac_emergency + 
                    Democratic_president:frac_emergency + 
                    crude_oil:frac_emergency + 
                    precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                    precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                    precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                    precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                    precip_Nov_avg + precip_Dec_avg + 
                    temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                    temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                    temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                    temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                    temp_Nov_avg + temp_Dec_avg
                    | Country + Region:year| 0 | Country, 
                  data = data)   
  
  panel_B <- felm(conflict_exists ~ instrument_emergency + 
                    per_capita_control:frac_emergency + 
                    Democratic_president:frac_emergency + 
                    precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                    precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                    precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                    precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                    precip_Nov_avg + precip_Dec_avg + 
                    temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                    temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                    temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                    temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                    temp_Nov_avg + temp_Dec_avg
                  | Country + Region:year| 0 | Country, 
                  data = data) 
  
  panel_C <- felm(conflict_exists ~  
                    per_capita_control:frac_emergency + 
                    Democratic_president:frac_emergency + 
                    precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                    precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                    precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                    precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                    precip_Nov_avg + precip_Dec_avg + 
                    temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                    temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                    temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                    temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                    temp_Nov_avg + temp_Dec_avg
                  | Country + Region:year | 
                    (Tonnage_mt_emergency ~ instrument_emergency) | Country, 
                  data = data)  
  

  column_3 <- c(
    summary(panel_A)$coefficients[1,1],
    summary(panel_A)$coefficients[1,2],
    summary(panel_A)$r.squared,
    summary(panel_B)$coefficients[1,1],
    summary(panel_B)$coefficients[1,2],
    summary(panel_B)$r.squared,
    summary(panel_C)$coefficients[1,1],
    summary(panel_C)$coefficients[1,2],
    summary(panel_C)$r.squared,
    summary(panel_C$stage1)$coefficients[nrow(summary(panel_C$stage1)$coefficients),1], 
    summary(panel_C$stage1)$coefficients[nrow(summary(panel_C$stage1)$coefficients),2], 
    panel_C$stage1$iv1fstat[[1]][5], 
    panel_C$N
  )

#Column 4: 
  panel_A <- felm(conflict_exists ~ Tonnage_mt_emergency + 
                    per_capita_control:frac_emergency + 
                    Democratic_president:frac_emergency + 
                    crude_oil:frac_emergency + 
                    precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                    precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                    precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                    precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                    precip_Nov_avg + precip_Dec_avg + 
                    temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                    temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                    temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                    temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                    temp_Nov_avg + temp_Dec_avg + 
                    avg_military:year +  
                    avg_economic:year
                  | Country + Region:year| 0 | Country, 
                  data = data)   
  
  panel_B <- felm(conflict_exists ~ instrument_emergency + 
                    per_capita_control:frac_emergency + 
                    Democratic_president:frac_emergency + 
                    precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                    precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                    precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                    precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                    precip_Nov_avg + precip_Dec_avg + 
                    temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                    temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                    temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                    temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                    temp_Nov_avg + temp_Dec_avg + 
                    avg_military:year +  
                    avg_economic:year
                  | Country + Region:year| 0 | Country, 
                  data = data) 
  
  panel_C <- felm(conflict_exists ~  
                    per_capita_control:frac_emergency + 
                    Democratic_president:frac_emergency + 
                    precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                    precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                    precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                    precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                    precip_Nov_avg + precip_Dec_avg + 
                    temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                    temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                    temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                    temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                    temp_Nov_avg + temp_Dec_avg + 
                    avg_military:year +  
                    avg_economic:year
                  | Country + Region:year | 
                    (Tonnage_mt_emergency ~ instrument_emergency) | Country, 
                  data = data)  
  
  
  column_4 <- c(
    summary(panel_A)$coefficients[1,1],
    summary(panel_A)$coefficients[1,2],
    summary(panel_A)$r.squared,
    summary(panel_B)$coefficients[1,1],
    summary(panel_B)$coefficients[1,2],
    summary(panel_B)$r.squared,
    summary(panel_C)$coefficients[1,1],
    summary(panel_C)$coefficients[1,2],
    summary(panel_C)$r.squared,
    summary(panel_C$stage1)$coefficients[nrow(summary(panel_C$stage1)$coefficients),1], 
    summary(panel_C$stage1)$coefficients[nrow(summary(panel_C$stage1)$coefficients),2], 
    panel_C$stage1$iv1fstat[[1]][5], 
    panel_C$N
  )
  
  
  
    
#Column 5:   
  panel_A <- felm(conflict_exists ~ Tonnage_mt_emergency + 
                    per_capita_control:frac_emergency + 
                    Democratic_president:frac_emergency + 
                    crude_oil:frac_emergency + 
                    precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                    precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                    precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                    precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                    precip_Nov_avg + precip_Dec_avg + 
                    temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                    temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                    temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                    temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                    temp_Nov_avg + temp_Dec_avg + 
                    avg_military:year +  
                    avg_economic:year + 
                    avg_cereal_imports:year + 
                    avg_cereal_production:year
                  | Country + Region:year| 0 | Country, 
                  data = data)   
  
  panel_B <- felm(conflict_exists ~ instrument_emergency + 
                    per_capita_control:frac_emergency + 
                    Democratic_president:frac_emergency + 
                    precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                    precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                    precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                    precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                    precip_Nov_avg + precip_Dec_avg + 
                    temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                    temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                    temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                    temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                    temp_Nov_avg + temp_Dec_avg + 
                    avg_military:year +  
                    avg_economic:year + 
                    avg_cereal_imports:year + 
                    avg_cereal_production:year
                  | Country + Region:year| 0 | Country, 
                  data = data) 
  
  panel_C <- felm(conflict_exists ~  
                    per_capita_control:frac_emergency + 
                    Democratic_president:frac_emergency + 
                    precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                    precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                    precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                    precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                    precip_Nov_avg + precip_Dec_avg + 
                    temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                    temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                    temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                    temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                    temp_Nov_avg + temp_Dec_avg + 
                    avg_military:year +  
                    avg_economic:year + 
                    avg_cereal_imports:year + 
                    avg_cereal_production:year
                  | Country + Region:year | 
                    (Tonnage_mt_emergency ~ instrument_emergency) | Country, 
                  data = data)  
  
  
  column_5 <- c(
    summary(panel_A)$coefficients[1,1],
    summary(panel_A)$coefficients[1,2],
    summary(panel_A)$r.squared,
    summary(panel_B)$coefficients[1,1],
    summary(panel_B)$coefficients[1,2],
    summary(panel_B)$r.squared,
    summary(panel_C)$coefficients[1,1],
    summary(panel_C)$coefficients[1,2],
    summary(panel_C)$r.squared,
    summary(panel_C$stage1)$coefficients[nrow(summary(panel_C$stage1)$coefficients),1], 
    summary(panel_C$stage1)$coefficients[nrow(summary(panel_C$stage1)$coefficients),2], 
    panel_C$stage1$iv1fstat[[1]][5], 
    panel_C$N
  )
  
  table1a1<- data.frame(
    model_1 <- column_1, 
    model_2 <- column_2, 
    model_3 <- column_3, 
    model_4 <- column_4, 
    model_5 <- column_5
  )
  
##################################################################
## This section produces table 2 in the article                 ##
##################################################################  
#We'll show the coefficients from the NQ version, the Christian and Barret article, 
#And the Mary and Mishra article: 

#First the NQ variant, which is column 4 from above: 
  panel_A <- felm(conflict_exists ~ Tonnage_mt_emergency + 
                    per_capita_control:frac_emergency + 
                    Democratic_president:frac_emergency + 
                    crude_oil:frac_emergency + 
                    precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                    precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                    precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                    precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                    precip_Nov_avg + precip_Dec_avg + 
                    temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                    temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                    temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                    temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                    temp_Nov_avg + temp_Dec_avg + 
                    avg_military:year +  
                    avg_economic:year + 
                    avg_cereal_imports:year + 
                    avg_cereal_production:year
                  | Country + Region:year| 0 | Country, 
                  data = data)   
  
  panel_B <- felm(conflict_exists ~ instrument_emergency + 
                    per_capita_control:frac_emergency + 
                    Democratic_president:frac_emergency + 
                    precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                    precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                    precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                    precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                    precip_Nov_avg + precip_Dec_avg + 
                    temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                    temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                    temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                    temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                    temp_Nov_avg + temp_Dec_avg + 
                    avg_military:year +  
                    avg_economic:year + 
                    avg_cereal_imports:year + 
                    avg_cereal_production:year
                  | Country + Region:year| 0 | Country, 
                  data = data) 
  
  panel_C <- felm(conflict_exists ~  
                    per_capita_control:frac_emergency + 
                    Democratic_president:frac_emergency + 
                    precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                    precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                    precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                    precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                    precip_Nov_avg + precip_Dec_avg + 
                    temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                    temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                    temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                    temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                    temp_Nov_avg + temp_Dec_avg + 
                    avg_military:year +  
                    avg_economic:year + 
                    avg_cereal_imports:year + 
                    avg_cereal_production:year
                  | Country + Region:year | 
                    (Tonnage_mt_emergency ~ instrument_emergency) | Country, 
                  data = data)  
  
  
  column_1 <- c(
    summary(panel_A)$coefficients[1,1],
    summary(panel_A)$coefficients[1,2],
    summary(panel_A)$r.squared,
    summary(panel_B)$coefficients[1,1],
    summary(panel_B)$coefficients[1,2],
    summary(panel_B)$r.squared,
    summary(panel_C)$coefficients[1,1],
    summary(panel_C)$coefficients[1,2],
    summary(panel_C)$r.squared,
    summary(panel_C$stage1)$coefficients[nrow(summary(panel_C$stage1)$coefficients),1], 
    summary(panel_C$stage1)$coefficients[nrow(summary(panel_C$stage1)$coefficients),2], 
    panel_C$stage1$iv1fstat[[1]][5], 
    panel_C$N
  )
  
  #Then the Christian and Barret one, which uses time trends: 
  data$year_2 <- as.numeric(data$year)^2
  data$year_3 <- as.numeric(data$year)^3
  
  panel_A <- felm(conflict_exists ~ Tonnage_mt_emergency + 
                    per_capita_control:frac_emergency + 
                    Democratic_president:frac_emergency + 
                    crude_oil:frac_emergency + 
                    precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                    precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                    precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                    precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                    precip_Nov_avg + precip_Dec_avg + 
                    temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                    temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                    temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                    temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                    temp_Nov_avg + temp_Dec_avg + 
                    avg_military:year +  
                    avg_economic:year + 
                    avg_cereal_imports:year + 
                    avg_cereal_production:year + 
                    Country:year_2 + Country:year_3
                  | Country + year| 0 | Country, 
                  data = data)  
  
  panel_B <- felm(conflict_exists ~ instrument_emergency + 
                    per_capita_control:frac_emergency + 
                    Democratic_president:frac_emergency + 
                    precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                    precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                    precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                    precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                    precip_Nov_avg + precip_Dec_avg + 
                    temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                    temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                    temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                    temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                    temp_Nov_avg + temp_Dec_avg + 
                    avg_military:year +  
                    avg_economic:year + 
                    avg_cereal_imports:year + 
                    avg_cereal_production:year + 
                    Country:year_2 + Country:year_3
                  | Country + Region:year| 0 | Country, 
                  data = data) 
  
  #Why doesn't this one work? 
  panel_C <- felm(conflict_exists ~  
                    per_capita_control:frac_emergency + 
                    Democratic_president:frac_emergency + 
                    precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                    precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                    precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                    precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                    precip_Nov_avg + precip_Dec_avg + 
                    temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                    temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                    temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                    temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                    temp_Nov_avg + temp_Dec_avg + 
                    avg_military:year +  
                    avg_economic:year + 
                    avg_cereal_imports:year + 
                    avg_cereal_production:year + 
                    Country:year_2 + Country:year_3
                  | Country + Region:year| (Tonnage_mt_emergency ~ instrument_emergency) | Country, 
                  data = data) 
  
  column_2 <- c(
    summary(panel_A)$coefficients[1,1],
    summary(panel_A)$coefficients[1,2],
    summary(panel_A)$r.squared,
    summary(panel_B)$coefficients[1,1],
    summary(panel_B)$coefficients[1,2],
    summary(panel_B)$r.squared,
    summary(panel_C)$coefficients[1,1],
    summary(panel_C)$coefficients[1,2],
    summary(panel_C)$r.squared,
    summary(panel_C$stage1)$coefficients[nrow(summary(panel_C$stage1)$coefficients),1], 
    summary(panel_C$stage1)$coefficients[nrow(summary(panel_C$stage1)$coefficients),2], 
    panel_C$stage1$iv1fstat[[1]][5], 
    panel_C$N
  )
  
  #Now the Mary and Mishra one:
  #Their instrument is the share of humanitarian food aid otu of 
  #total aid averaged across all sampled countries other than i. 
  data <- data %>% 
    mutate(pct_emergency = Military + Economic + Value_emergency + Value_development + Value_t1, 
           pct_emergency = Value_emergency / pct_emergency) %>% 
    group_by(year) %>% 
    mutate(instrument_mm = ifelse(is.nan(pct_emergency), 
                                  (sum(pct_emergency, na.rm = TRUE) - pct_emergency)/n(), 
                                  (sum(pct_emergency, na.rm = TRUE)                )/n())) %>% 
    mutate(instrument_mm = ifelse(is.na(instrument_mm), 0, instrument_mm))
  
  
  panel_A <- felm(conflict_exists ~ Tonnage_mt_emergency + 
                    precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                    precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                    temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                    temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                    Country:year_2 + Country:year_3 + 
                    bordering_conflict_exists + 
                    bordering_tonnage_emergency + 
                    gdp_pc + 
                    inflation + 
                    pct_excluded + 
                    vdem
                  | Country + Region:year| 0 | Country, 
                  data = data)  
  
  panel_B <- felm(conflict_exists ~ instrument_mm + 
                    precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                    precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                    temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                    temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                    Country:year_2 + Country:year_3 + 
                    bordering_conflict_exists + 
                    bordering_tonnage_emergency + 
                    gdp_pc + 
                    inflation + 
                    pct_excluded + 
                    vdem
                  | Country + Region:year| 0 | Country, 
                  data = data) 
  
  
  
  panel_C <- felm(conflict_exists ~  
                    precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                    precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                    temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                    temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                    Country:year_2 + Country:year_3 + 
                    bordering_conflict_exists + 
                    bordering_tonnage_emergency + 
                    gdp_pc + 
                    inflation + 
                    pct_excluded + 
                    vdem
                  | Country + Region:year | 
                    (Tonnage_mt_emergency ~ instrument_mm) | Country, 
                  data = data)
  
  column_3 <- c(
    summary(panel_A)$coefficients[1,1],
    summary(panel_A)$coefficients[1,2],
    summary(panel_A)$r.squared,
    summary(panel_B)$coefficients[1,1],
    summary(panel_B)$coefficients[1,2],
    summary(panel_B)$r.squared,
    summary(panel_C)$coefficients[1,1],
    summary(panel_C)$coefficients[1,2],
    summary(panel_C)$r.squared,
    summary(panel_C$stage1)$coefficients[nrow(summary(panel_C$stage1)$coefficients),1], 
    summary(panel_C$stage1)$coefficients[nrow(summary(panel_C$stage1)$coefficients),2], 
    panel_C$stage1$iv1fstat[[1]][5], 
    panel_C$N
  )
  

table2<- data.frame(
  model_1 <- column_1, 
  model_2 <- column_2, 
  model_3 <- column_3
)

##################################################################
## This section produces table A.5 in the appendix              ##
##################################################################  
#We'll show the coefficients from the NQ version, the Christian and Barret article, 
#And the Mary and Mishra article. This table uses value, not tonnage: 

data <- data %>% 
  group_by(year) %>% 
  mutate(sum_value_emergency = sum(Value_emergency, na.rm = TRUE), 
         instrument_emergency_value = frac_emergency * sum_value_emergency) %>% 
  ungroup()

#First the NQ variant, which is column 4 from above: 
panel_A <- felm(conflict_exists ~ Value_emergency + 
                  per_capita_control:frac_emergency + 
                  Democratic_president:frac_emergency + 
                  crude_oil:frac_emergency + 
                  precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                  precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                  precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                  precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                  precip_Nov_avg + precip_Dec_avg + 
                  temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                  temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                  temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                  temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                  temp_Nov_avg + temp_Dec_avg + 
                  avg_military:year +  
                  avg_economic:year + 
                  avg_cereal_imports:year + 
                  avg_cereal_production:year
                | Country + Region:year| 0 | Country, 
                data = data)   

panel_B <- felm(conflict_exists ~ instrument_emergency_value + 
                  per_capita_control:frac_emergency + 
                  Democratic_president:frac_emergency + 
                  precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                  precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                  precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                  precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                  precip_Nov_avg + precip_Dec_avg + 
                  temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                  temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                  temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                  temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                  temp_Nov_avg + temp_Dec_avg + 
                  avg_military:year +  
                  avg_economic:year + 
                  avg_cereal_imports:year + 
                  avg_cereal_production:year
                | Country + Region:year| 0 | Country, 
                data = data) 

panel_C <- felm(conflict_exists ~  
                  per_capita_control:frac_emergency + 
                  Democratic_president:frac_emergency + 
                  precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                  precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                  precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                  precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                  precip_Nov_avg + precip_Dec_avg + 
                  temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                  temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                  temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                  temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                  temp_Nov_avg + temp_Dec_avg + 
                  avg_military:year +  
                  avg_economic:year + 
                  avg_cereal_imports:year + 
                  avg_cereal_production:year
                | Country + Region:year | 
                  (Value_emergency ~ instrument_emergency_value) | Country, 
                data = data)  


column_1 <- c(
  summary(panel_A)$coefficients[1,1],
  summary(panel_A)$coefficients[1,2],
  summary(panel_A)$r.squared,
  summary(panel_B)$coefficients[1,1],
  summary(panel_B)$coefficients[1,2],
  summary(panel_B)$r.squared,
  summary(panel_C)$coefficients[1,1],
  summary(panel_C)$coefficients[1,2],
  summary(panel_C)$r.squared,
  summary(panel_C$stage1)$coefficients[nrow(summary(panel_C$stage1)$coefficients),1], 
  summary(panel_C$stage1)$coefficients[nrow(summary(panel_C$stage1)$coefficients),2], 
  panel_C$stage1$iv1fstat[[1]][5], 
  panel_C$N
)

#Then the Christian and Barret one, which uses time trends: 
data$year_2 <- as.numeric(data$year)^2
data$year_3 <- as.numeric(data$year)^3

panel_A <- felm(conflict_exists ~ Value_emergency + 
                  per_capita_control:frac_emergency + 
                  Democratic_president:frac_emergency + 
                  crude_oil:frac_emergency + 
                  precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                  precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                  precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                  precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                  precip_Nov_avg + precip_Dec_avg + 
                  temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                  temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                  temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                  temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                  temp_Nov_avg + temp_Dec_avg + 
                  avg_military:year +  
                  avg_economic:year + 
                  avg_cereal_imports:year + 
                  avg_cereal_production:year + 
                  Country:year_2 + Country:year_3
                | Country + year| 0 | Country, 
                data = data)  

panel_B <- felm(conflict_exists ~ instrument_emergency_value + 
                  per_capita_control:frac_emergency + 
                  Democratic_president:frac_emergency + 
                  precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                  precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                  precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                  precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                  precip_Nov_avg + precip_Dec_avg + 
                  temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                  temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                  temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                  temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                  temp_Nov_avg + temp_Dec_avg + 
                  avg_military:year +  
                  avg_economic:year + 
                  avg_cereal_imports:year + 
                  avg_cereal_production:year + 
                  Country:year_2 + Country:year_3
                | Country + Region:year| 0 | Country, 
                data = data) 

panel_C <- felm(conflict_exists ~  
                  per_capita_control:frac_emergency + 
                  Democratic_president:frac_emergency + 
                  precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                  precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                  precip_Jan_avg + precip_Feb_avg + precip_Mar_avg + precip_Apr_avg + precip_May_avg + 
                  precip_Jun_avg + precip_Jul_avg + precip_Aug_avg + precip_Sep_avg + precip_Oct_avg + 
                  precip_Nov_avg + precip_Dec_avg + 
                  temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                  temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                  temp_Jan_avg + temp_Feb_avg + temp_Mar_avg + temp_Apr_avg + temp_May_avg + 
                  temp_Jun_avg + temp_Jul_avg + temp_Aug_avg + temp_Sep_avg + temp_Oct_avg +
                  temp_Nov_avg + temp_Dec_avg + 
                  avg_military:year +  
                  avg_economic:year + 
                  avg_cereal_imports:year + 
                  avg_cereal_production:year + 
                  Country:year_2 + Country:year_3
                | Country + Region:year | 
                  (Value_emergency ~ instrument_emergency_value) | Country, 
                data = data)  

column_2 <- c(
  summary(panel_A)$coefficients[1,1],
  summary(panel_A)$coefficients[1,2],
  summary(panel_A)$r.squared,
  summary(panel_B)$coefficients[1,1],
  summary(panel_B)$coefficients[1,2],
  summary(panel_B)$r.squared,
  summary(panel_C)$coefficients[1,1],
  summary(panel_C)$coefficients[1,2],
  summary(panel_C)$r.squared,
  summary(panel_C$stage1)$coefficients[nrow(summary(panel_C$stage1)$coefficients),1], 
  summary(panel_C$stage1)$coefficients[nrow(summary(panel_C$stage1)$coefficients),2], 
  panel_C$stage1$iv1fstat[[1]][5], 
  panel_C$N
)

panel_A <- felm(conflict_exists ~ Value_emergency + 
                  precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                  precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                  temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                  temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                  Country:year_2 + Country:year_3 + 
                  bordering_conflict_exists + 
                  bordering_tonnage_emergency + 
                  gdp_pc + 
                  inflation + 
                  pct_excluded + 
                  vdem
                | Country + year| 0 | Country, 
                data = data)  

panel_B <- felm(conflict_exists ~ instrument_mm + 
                  precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                  precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                  temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                  temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                  Country:year_2 + Country:year_3 + 
                  bordering_conflict_exists + 
                  bordering_tonnage_emergency + 
                  gdp_pc + 
                  inflation + 
                  pct_excluded + 
                  vdem
                | Country + Region:year| 0 | Country, 
                data = data) 



panel_C <- felm(conflict_exists ~  
                  precip_Jan + precip_Feb + precip_Mar + precip_Apr + precip_May + precip_Jun + 
                  precip_Jul + precip_Aug + precip_Sep + precip_Oct + precip_Nov + precip_Dec + 
                  temp_Jan + temp_Feb + temp_Mar + temp_Apr + temp_May + temp_Jun + 
                  temp_Jul + temp_Aug + temp_Sep + temp_Oct + temp_Nov + temp_Dec + 
                  Country:year_2 + Country:year_3 + 
                  bordering_conflict_exists + 
                  bordering_tonnage_emergency + 
                  gdp_pc + 
                  inflation + 
                  pct_excluded + 
                  vdem
                | Country + Region:year | 
                  (Value_emergency ~ instrument_mm) | Country, 
                data = data)

column_3 <- c(
  summary(panel_A)$coefficients[1,1],
  summary(panel_A)$coefficients[1,2],
  summary(panel_A)$r.squared,
  summary(panel_B)$coefficients[1,1],
  summary(panel_B)$coefficients[1,2],
  summary(panel_B)$r.squared,
  summary(panel_C)$coefficients[1,1],
  summary(panel_C)$coefficients[1,2],
  summary(panel_C)$r.squared,
  summary(panel_C$stage1)$coefficients[nrow(summary(panel_C$stage1)$coefficients),1], 
  summary(panel_C$stage1)$coefficients[nrow(summary(panel_C$stage1)$coefficients),2], 
  panel_C$stage1$iv1fstat[[1]][5], 
  panel_C$N
)


tablea5<- data.frame(
  model_1 <- column_1, 
  model_2 <- column_2, 
  model_3 <- column_3
)
 
##################################################################
## This section makes the tables legible and outputs them       ##
##################################################################  
#Stop scientific notation
options(scipen = 999)

#The table in the main article: 
  #Fix colnames: 
  colnames(table2) <- c("Model 1", "Model 2", "Model 3")
  
  #Round: 
  table2 <- apply(table2, 2, function(x) {
    as.character(format(round(x, digits = 4), nsmall = 4))
    })
  
  #Add the parentheses for std. dev. 
  table2[2,] <-  paste0("(", table2[2,], ")")
  table2[5,] <-  paste0("(", table2[5,], ")")
  table2[8,] <-  paste0("(", table2[8,], ")")
  table2[11,] <- paste0("(", table2[11,], ")")
  
  #Add column with the labels: 
  table2 <- data.frame(table2, stringsAsFactors = FALSE) %>% 
    mutate(labels = c("Humanitarian food aid (1000MTs)", 
                      "", 
                      "R^2", 
                      "Instrument", 
                      "", 
                      "R^2", 
                      "Instrumented food aid (1000MTs)", 
                      "", 
                      "R^2", 
                      "Instrument", 
                      "", 
                      "Kleibergen-Paap F-statistic", 
                      "Number of observations"))
  
  #Re-order 
  table2<- table2[c(4, 1:3)]
  
  table2 <- rbind(table2, 
                  c("Nunn and Qian Controls", "X", "X", " "), 
                  c("Unit-specific cubic time trends", " ", "X", "X"), 
                  c("Mary and Mishra Controls", " ", " ", "X"))
  
  #Fix the N of observations: 
  
  table2[13,2:4] <- as.numeric(table2[13,2:4]) %>% 
    round(0) %>% 
    as.character()
  
  note <- "This table replicates the main findings from Nunn and Qian (\\citeyear{NunnandQian2014}), Christian and Barret (\\citeyear{ChristianandBarret2017}), and Mary and Mishra (\\citeyear{MaryandMishra2020}). Estimates are from a linear probability model. The unit of observation is the country-year. The binary outcome variable is whether an intrastate war exists in a given country-year. Standard errors are clustered at the country level. Tables A.5 and A.6 list data sources and control variables. "  
  kable(table2, 
        booktabs = TRUE, 
        format = "latex", 
        caption = "Humanitarian food aid does not increase conflict \\label{table2}", 
        col.names = c("", 
                      "Nunn and Qian", 
                      "Christian and Barrett", 
                      "Mary and Mishra")) %>% 
    pack_rows("Panel A: OLS Estimates", 1, 3) %>% 
    pack_rows("Panel B: Reduced Form Estimates", 4, 6) %>% 
    pack_rows("Panel C: 2SLS Estimates", 7, 9) %>% 
    pack_rows("Panel D: First-Stage Estimates", 10, 12) %>% 
    add_header_above(c(" " = 1, "Intrastate conflict" = 3) ) %>% 
    kable_styling(font_size = 8) %>% 
    footnote(general = note, threeparttable = TRUE, 
             footnote_as_chunk = T) %>% 
    row_spec(12, hline_after = TRUE) %>% 
    save_kable(., file = "./output/table2.tex")


#Now do the same for the appendix table that fully replicates NQ: 
  table <- table1a1 
  
  colnames(table) <- c("Model 1", "Model 2", "Model 3", 
                       "Model 4", "Model 5")
  
  #Round: 
  table <- apply(table, 2, function(x) {
    as.character(format(round(x, digits = 4), nsmall = 4))
  })
  
  #Add the parentheses for std. dev. 
  table[2,] <-  paste0("(", table[2,], ")")
  table[5,] <-  paste0("(", table[5,], ")")
  table[8,] <-  paste0("(", table[8,], ")")
  table[11,] <- paste0("(", table[11,], ")")
  
  #Add column with the labels: 
  table <- data.frame(table, stringsAsFactors = FALSE) %>% 
    mutate(labels = c("Humanitarian food aid (1000MTs)", 
                      "", 
                      "R^2", 
                      "Instrument", 
                      "", 
                      "R^2", 
                      "Instrumented food aid (1000MTs)", 
                      "", 
                      "R^2", 
                      "Instrument", 
                      "", 
                      "Kleibergen-Paap F-statistic", 
                      "Number of observations"))


  note <- "This table replicates columns 1-5 from Table 2 in Nunn and Qian (\\citeyear{NunnandQian2014}). Estimates are from a linear probability model. The unit of observation is the country-year. The binary outcome variable is whether an intrastate war exists in a given country-year. Standard errors are clustered at the country level. Data sources and control variables are listed in Table A.5 Column 5 is the specification used in table \\ref{table2}."  
  #Re-order 
  table<- table[c(6, 1:5)]
  
  table <- rbind(table, 
                  c("Country FEs"                      , "X", "X", "X", "X", "X"), 
                  c("Region-year FEs"                   , "X", "X", "X", "X", "X"), 
                  c("US GDP per capita * avg. prob."   , " ", "X", "X", "X", "X"), 
                  c("US democratic pres. * avg. prob." , " ", "X", "X", "X", "X"), 
                  c("Oil price * avg. prob. "          , " ", "X", "X", "X", "X"), 
                  c("Monthly recipient temp. and rainfall", " ", " ", "X", "X", "X"), 
                  c("Monthly weather * avg. prob."     , " ", " ", "X", "X", "X"), 
                  c("Avg. US military aid * avg. prob.", " ", " ", " ", "X", "X"), 
                  c("Avg. US economic aid * avg. prob.", " ", " ", " ", "X", "X"), 
                  c("Avg. cereal imports * year FEs"   , " ", " ", " ", " ", "X"), 
                  c("Avg. cereal production * year FEs", " ", " ", " ", " ", "X"))
  
  #Fix the N of observations: 
  
  table[13,2:6] <- as.numeric(table[13,2:6]) %>% 
    round(0) %>% 
    as.character()
  
  kable(table, 
        booktabs = TRUE, 
        format = "latex", 
        caption = "Humanitarian Food Aid and Conflict Incidence \\label{table_nq_full}", 
        col.names = c(" ", 
                      "Model 1", 
                      "Model 2", 
                      "Model 3", 
                      "Model 4", 
                      "Model 5")) %>% 
    pack_rows("Panel A: OLS Estimates", 1, 3) %>% 
    pack_rows("Panel A: Reduced Form Estimates", 4, 6) %>% 
    pack_rows("Panel A: 2SLS Estimates", 7, 9) %>% 
    pack_rows("Panel A: First-Stage Estimates", 10, 12) %>% 
    add_header_above(c(" " = 1, "Intrastate conflict" = 5) ) %>% 
    kable_styling(font_size = 8) %>% 
    footnote(general = note, threeparttable = TRUE, 
             footnote_as_chunk = T) %>% 
    row_spec(12, hline_after = TRUE) %>% 
    save_kable(., file = "./output/table_nq_full.tex")


#This produces table a.5: table 2 but with values not tonnage. 
  #Fix colnames: 
  colnames(tablea5) <- c("Model 1", "Model 2", "Model 3")
  
  #Round: 
  tablea5 <- apply(tablea5, 2, function(x) {
    as.character(format(round(x, digits = 4), nsmall = 4))
  })
  
  #Add the parentheses for std. dev. 
  tablea5[2,] <-  paste0("(", tablea5[2,], ")")
  tablea5[5,] <-  paste0("(", tablea5[5,], ")")
  tablea5[8,] <-  paste0("(", tablea5[8,], ")")
  tablea5[11,] <- paste0("(", tablea5[11,], ")")
  
  #Add column with the labels: 
  tablea5 <- data.frame(tablea5, stringsAsFactors = FALSE) %>% 
    mutate(labels = c("Humanitarian food aid (mil. of USD)", 
                      "", 
                      "R^2", 
                      "Instrument", 
                      "", 
                      "R^2", 
                      "Instrumented food aid (mil. of USD)", 
                      "", 
                      "R^2", 
                      "Instrument", 
                      "", 
                      "Kleibergen-Paap F-statistic", 
                      "Number of observations"))
  
  #Re-order 
  tablea5<- tablea5[c(4, 1:3)]
  
  tablea5 <- rbind(tablea5, 
                  c("Nunn and Qian Controls", "X", "X", " "), 
                  c("Unit-specific cubic time trends", " ", "X", "X"), 
                  c("Mary and Mishra Controls", " ", " ", "X"))
  
  #Fix the N of observations: 
  tablea5[13,2:4] <- as.numeric(tablea5[13,2:4]) %>% 
    round(0) %>% 
    as.character()
  
  note <- "This table replicates table 2 in the main paper, but uses the value of humanitarian food aid (derived from the IFAR reports) rather than tonnage. Data are in millions of constant 2018 dollars. Estimates are from a linear probability model. The unit of observation is the country-year. The binary outcome variable is whether an intrastate war exists in a given country-year. Standard errors are clustered at the country level. Data sources are listed in Tables A.5 and A.6"
  
  kable(tablea5, 
        booktabs = TRUE, 
        format = "latex", 
        caption = "Value of Humanitarian Food Aid and Conflict Incidence \\label{tablea5}", 
        col.names = c("", 
                      "Nunn and Qian", 
                      "Christian and Barrett", 
                      "Mary and Mishra")) %>% 
    pack_rows("Panel A: OLS Estimates", 1, 3) %>% 
    pack_rows("Panel B: Reduced Form Estimates", 4, 6) %>% 
    pack_rows("Panel C: 2SLS Estimates", 7, 9) %>% 
    pack_rows("Panel D: First-Stage Estimates", 10, 12) %>% 
    add_header_above(c(" " = 1, "Intrastate conflict" = 3) ) %>% 
    kable_styling(font_size = 8) %>% 
    footnote(general = note, threeparttable = TRUE, 
             footnote_as_chunk = T) %>% 
    row_spec(12, hline_after = TRUE) %>% 
    save_kable(., file = "./output/tablea5.tex")
  
  
  