
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
require(marginaleffects)
require(modelsummary)
require(fixest)

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
  #And make value into millions of USD, for table A.5
  mutate(Value_emergency = Value_emergency * (cpi_2018/us_cpi), 
         Value_emergency = Value_emergency/1000000) %>% 
  mutate(Tonnage_mt_emergency = Tonnage_mt_emergency / 1000)

#Add the Mary + Mishra instrument ;
data <- data %>% 
  mutate(pct_emergency = Military + Economic + Value_emergency + Value_development + Value_t1, 
         pct_emergency = Value_emergency / pct_emergency) %>% 
  group_by(year) %>% 
  mutate(instrument_mm = ifelse(is.nan(pct_emergency), 
                                (sum(pct_emergency, na.rm = TRUE) - pct_emergency)/n(), 
                                (sum(pct_emergency, na.rm = TRUE)                )/n())) %>% 
  mutate(instrument_mm = ifelse(is.na(instrument_mm), 0, instrument_mm))

data$year_2 <- as.numeric(data$year)^2
data$year_3 <- as.numeric(data$year)^3

#########################################################
##### Set the control variables                      ----
#########################################################

setFixest_fml(..nq_controls = ~ Value_emergency * Tonnage_mt_emergency + 
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
                temp_Nov_avg + temp_Dec_avg)

setFixest_fml(..mm_controls = ~ Country:year_2 + Country:year_3 + 
                bordering_conflict_exists + 
                bordering_tonnage_emergency + 
                gdp_pc + 
                inflation + 
                pct_excluded + 
                vdem)

#########################################################
##### Run the main regressions                      ----
#########################################################

m1 <- feols(conflict_exists ~ Tonnage_mt_emergency + ..nq_controls + ..mm_controls | Country + Region^year, 
            data = data, 
            vcov = ~ Country)

m2 <- feols(conflict_exists ~ Value_emergency + ..nq_controls + ..mm_controls| Country + Region^year, 
            data = data, 
            vcov = ~ Country)

m3 <- feols(conflict_exists ~ Tonnage_mt_emergency*Value_emergency + ..nq_controls + ..mm_controls| Country + Region^year, 
            data = data, 
            vcov = ~ Country)

m4 <- feols(conflict_exists ~ Tonnage_mt_emergency * n_partners_emergency + ..nq_controls + ..mm_controls| Country + Region^year, 
            data = data, 
            vcov = ~ Country)

m5 <- feols(conflict_exists ~ Value_emergency * n_partners_emergency + ..nq_controls + ..mm_controls| Country + Region^year, 
            data = data, 
            vcov = ~ Country)

models <- list(m1, m2, m3, m4, m5)

cm <- c('Tonnage_mt_emergency'                      = 'Tonnage (1000MTs)',
        'Value_emergency'                           = 'Value (millions of USD)',
        'Value_emergency:Tonnage_mt_emergency'      = 'Tonnage * Value',
        'Tonnage_mt_emergency:n_partners_emergency' = 'Tonnage * N. of partners',
        'Value_emergency:n_partners_emergency'      = 'Value * N. of partners',
        'n_partners_emergency'                      = 'N. of partners')
        
marginaleffects::avg_comparisons(m2, 
                                 variables = list("Value_emergency" = c(0,2.55)))

sd(data$Value_emergency, na.rm = TRUE)
mean(data$conflict_exists)

#fix the notes: 
note <- "The outcome variable for this table is the incidence of intrastate conflict. Explanatory variables are the tonnage of humanitarian food aid provided by USAID (in 1000MTs), the value of humanitarian food aid provided by USAID (in millions of 2018 USD), and the number of implementing partners through which the aid was delivered. Estimates are from a linear probability model where the unit of observation is the country-year. Standard errors are clustered at the country level. Tables A7 and A8 list data sources and control variables; humanitarian food aid data are from the IFARs. * p < 0.05, ** p < 0.01, *** p < 0.001"
#Add the rows describing controls: 
rows <- tribble(~term, ~model1, ~model2, ~model3, ~model4, ~model5,  
                "Nunn + Qian controls"   , "X", "X", "X", "X", "X",
                "Mary + Mishra controls" , "X", "X", "X", "X", "X")
attr(rows, 'position') <- c(13:14)

out <- modelsummary(models,  
                    output = 'latex',
                    gof_map = c("nobs", "r.squared"), 
                    coef_map = cm, 
                    stars = TRUE, 
                    title = "Humanitarian food aid does not increase the incidence of conflict \\label{direct_value_tonnage}", 
                    escape = FALSE, 
                    add_rows = rows, 
                    estimate = "{estimate}{stars}") %>% 
  kable_styling(font_size = 8) %>% 
  footnote(general = note, 
           threeparttable = TRUE, 
           footnote_as_chunk = T) %>% 
  add_header_above(., c(" " = 1, 
                        "Conflict Incidence" = 5))

save_kable(out, "./output/direct_value_tonnage.tex" )

