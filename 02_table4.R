######Set up: ######
#clear up
rm(list=ls())

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
require(skimr)


set.seed(3759)


peace    <- read.csv("./merged_data/table3a.csv")
duration <- read.csv("./merged_data/table3b.csv")

skim(peace$Value_emergency)
skim(peace$Tonnage_mt_emergency)




##################################################################
## Replicate Narang 2014 (table 1)                              ##
##################################################################  
require(survival)
require(modelsummary)
  
peace$status <- peace$year == 2017
peace$peace_duration2 <- peace$peace_duration + 1
  
fit1 <- coxph(Surv(peace_duration, peace_duration2, status) ~ Tonnage_mt_emergency + decisive 
              + lootable + treaty + identity + episode_deaths + factions + 
                initial_democracy + initial_mortality +  peace_agreement +  
                govt_military + rugged_terrain + 
                + p5_contiguous + p5_colony + conflict_duration, 
              data = peace)    

fit2 <- coxph(Surv(peace_duration, peace_duration2, status) ~ Tonnage_mt_emergency + decisive 
              + lootable + treaty + identity + episode_deaths + factions + 
                initial_democracy + initial_mortality +  peace_agreement +  
                govt_military + rugged_terrain + 
                + conflict_duration, 
              data = peace) 

fit3 <- coxph(Surv(peace_duration, peace_duration2, status) ~ Tonnage_mt_emergency + decisive 
              + lootable + treaty + identity + episode_deaths + factions + 
                initial_mortality +  peace_agreement +  
                govt_military + rugged_terrain + 
                + conflict_duration, 
              data = peace)

fit4 <- coxph(Surv(peace_duration, peace_duration2, status) ~ Tonnage_mt_emergency + decisive 
              + lootable + treaty + identity + episode_deaths + factions + 
                peace_agreement +  
                govt_military + rugged_terrain + 
                + conflict_duration, 
              data = peace) 

fit5 <- coxph(Surv(peace_duration, peace_duration2, status) ~ Tonnage_mt_emergency + decisive 
              + lootable + treaty + identity + episode_deaths + factions + 
                peace_agreement +  
                govt_military +  
                + conflict_duration, 
              data = peace)

fit6 <- coxph(Surv(peace_duration, peace_duration2, status) ~ Tonnage_mt_emergency + decisive 
              + lootable + treaty + identity + episode_deaths + factions + 
                peace_agreement +  
                + conflict_duration, 
              data = peace) 

fit7 <- coxph(Surv(peace_duration, peace_duration2, status) ~ Tonnage_mt_emergency + decisive 
              + lootable + treaty + identity + episode_deaths + factions + 
                + conflict_duration, 
              data = peace) 


models <- list(fit1, fit2, fit3, fit4, fit5, fit6, fit7)

cm <- c('Tonnage_mt_emergency'    = 'Humanitarian food aid',
        'decisiveTRUE' = 'Decisive victory',
        'lootable' = 'Lootable resources', 
        'treatyTRUE' = 'Treaty', 
        'identityTRUE' = 'Identity war', 
        'episode_deaths'  = 'War-related deaths', 
        'factions' = 'Factions', 
        'initial_democracyTRUE' = 'Democracy', 
        'initial_mortality' = 'Infant mortality rate', 
        'peace_agreement' = 'Past agreement', 
        'govt_military' = 'Government army size', 
        'rugged_terrain' = 'Mountainous terrain', 
        'p5_contiguousTRUE' = 'P-5 contiguity', 
        'p5_colonyTRUE' = 'Former P-5 colony', 
        'conflict_duration' = 'Duration of war')
      
note <- "This table replicates table 1 in Narang (2014). Estimates are from a Cox Proportional Hazards model. Data sources are listed in Table A7. * p < 0.05, ** p < 0.01, *** p < 0.001" 


out <- modelsummary(models, 
             output = 'latex',
             gof_map = c("nobs", "r.squared"), 
             coef_map = cm, 
             stars = TRUE, 
             title = "Effect of Increasing Humanitarian Aid on the Risk of Peace Failing after All Civil Wars \\label{a_peace_table1}", 
             estimate = "{estimate}{stars}",
             gof_omit = ".*"
             
             ) %>% 
  kable_styling(font_size = 8) %>% 
  footnote(general = note, 
           threeparttable = TRUE, 
           footnote_as_chunk = T)

save_kable(out, "./output/a_narang2014_table1.tex" )

##################################################################
## Replicate Narang 2014 (table 2)                              ##
##################################################################  

fit1 <- coxph(Surv(peace_duration, peace_duration2, status) ~ Tonnage_mt_emergency * decisive 
              + lootable + treaty + identity + episode_deaths + factions + 
                initial_democracy + initial_mortality +  peace_agreement +  
                govt_military + rugged_terrain + 
                + p5_contiguous + p5_colony + conflict_duration, 
              data = peace)    

fit2 <- coxph(Surv(peace_duration, peace_duration2, status) ~ Tonnage_mt_emergency * decisive 
              + lootable + treaty + identity + episode_deaths + factions + 
                initial_democracy + initial_mortality +  peace_agreement +  
                govt_military + rugged_terrain + 
                + conflict_duration, 
              data = peace) 

fit3 <- coxph(Surv(peace_duration, peace_duration2, status) ~ Tonnage_mt_emergency * decisive 
              + lootable + treaty + identity + episode_deaths + factions + 
                initial_mortality +  peace_agreement +  
                govt_military + rugged_terrain + 
                + conflict_duration, 
              data = peace)

fit4 <- coxph(Surv(peace_duration, peace_duration2, status) ~ Tonnage_mt_emergency * decisive 
              + lootable + treaty + identity + episode_deaths + factions + 
                peace_agreement +  
                govt_military + rugged_terrain + 
                + conflict_duration, 
              data = peace) 

fit5 <- coxph(Surv(peace_duration, peace_duration2, status) ~ Tonnage_mt_emergency * decisive 
              + lootable + treaty + identity + episode_deaths + factions + 
                peace_agreement +  
                govt_military +  
                + conflict_duration, 
              data = peace)

fit6 <- coxph(Surv(peace_duration, peace_duration2, status) ~ Tonnage_mt_emergency * decisive 
              + lootable + treaty + identity + episode_deaths + factions + 
                peace_agreement +  
                + conflict_duration, 
              data = peace) 

fit7 <- coxph(Surv(peace_duration, peace_duration2, status) ~ Tonnage_mt_emergency * decisive 
              + lootable + treaty + identity + episode_deaths + factions + 
                + conflict_duration, 
              data = peace) 


models <- list(fit1, fit2, fit3, fit4, fit5, fit6, fit7)

cm <- c('Tonnage_mt_emergency'    = 'Humanitarian food aid',
        'Tonnage_mt_emergency:decisiveTRUE' = "Food aid * Decisive victory",
        'decisiveTRUE' = 'Decisive victory',
        'lootable' = 'Lootable resources', 
        'treatyTRUE' = 'Treaty', 
        'identityTRUE' = 'Identity war', 
        'episode_deaths'  = 'War-related deaths', 
        'factions' = 'Factions', 
        'initial_democracyTRUE' = 'Democracy', 
        'initial_mortality' = 'Infant mortality rate', 
        'peace_agreement' = 'Past agreement', 
        'govt_military' = 'Government army size', 
        'rugged_terrain' = 'Mountainous terrain', 
        'p5_contiguousTRUE' = 'P-5 contiguity', 
        'p5_colonyTRUE' = 'Former P-5 colony', 
        'conflict_duration' = 'Duration of war')

note <- "This table replicates table 2 in Narang (2014). Estimates are from a Cox Proportional Hazards model. Data sources are listed in Table A7. * p < 0.05, ** p < 0.01, *** p < 0.001"

out <- modelsummary(models, 
                    output = 'latex',
                    gof_map = c("n", "r.squared"), 
                    coef_map = cm, 
                    stars = TRUE, 
                    title = "Effect of Humanitarian Aid on the Risk of Peace Failing after Decisive/Nondecisive Victories \\label{a_peace_table2}", 
                    estimate = "{estimate}{stars}",
  ) %>% 
  kable_styling(font_size = 8) %>% 
  footnote(general = note, 
           threeparttable = TRUE, 
           footnote_as_chunk = T)


save_kable(out, "./output/a_narang2014_table2.tex" )

##################################################################
## Replicate Narang 2015 (table 1)                              ##
################################################################## 

fit1 <- coxph(Surv(duration, duration2, ep_end) ~ Tonnage_mt_emergency + 
                lagged_deaths + population + gdp_pc + current_polity + 
                Diamonds + Drugs + guarantee + rugged_terrain + 
                forest_cover, 
              data = duration)

fit2 <- coxph(Surv(duration, duration2, ep_end) ~ Tonnage_mt_emergency * peripheral + 
                lagged_deaths + population + gdp_pc + current_polity + 
                Diamonds + Drugs + guarantee + rugged_terrain + 
                forest_cover, 
              data = duration)

models <- list(fit1, fit2)

cm <- c('Tonnage_mt_emergency'    = 'Humanitarian food aid',
        'peripheral' = 'Peripheral conflict',
        'Tonnage_mt_emergency:peripheral' = "Food aid * Peripheral",
        'lagged_deaths' = "Deaths (lagged)", 
        'population' = "Population (logged)", 
        'gdp_pc' = "GDP per capita", 
        'current_polity' = "Polity2 Score", 
        'Diamonds' = "Diamonds", 
        'Drugs' = "Drugs", 
        'guaranteeTRUE' = "Guarantee", 
        'rugged_terrain' = " Rugged terrain", 
        'forest_cover' = "Forest cover")

note <- "This table replicates column 2 of Tables 1 and 2 from Narang (2015). Estimates are from a Cox Proportional Hazards model. Data sources are listed in Table A8. * p < 0.05, ** p < 0.01, *** p < 0.001"

out <- modelsummary(models, 
                    output = 'latex',
                    gof_map = c("nobs", "r.squared"), 
                    coef_map = cm, 
                    stars = TRUE, 
                    title = "Humanitarian food aid and the risk of civil war termination \\label{a_duration}", 
                    estimate = "{estimate}{stars}",
                    gof_omit = ".*"
  ) %>% 
  kable_styling(font_size = 8) %>% 
  footnote(general = note, threeparttable = TRUE, 
           footnote_as_chunk = T)

save_kable(out, "./output/a_narang2015.tex" )



##################################################################
## Paper for the main table                                    ##
##################################################################  


fit1 <- coxph(Surv(peace_duration, peace_duration2, status) ~ Value_emergency + decisive 
              + lootable + treaty + identity + episode_deaths + factions + 
                initial_democracy + initial_mortality +  peace_agreement +  
                govt_military + rugged_terrain + 
                + p5_contiguous + p5_colony + conflict_duration, 
              data = peace)    

fit2 <- coxph(Surv(peace_duration, peace_duration2, status) ~ Value_emergency * decisive 
              + lootable + treaty + identity + episode_deaths + factions + 
                initial_democracy + initial_mortality +  peace_agreement +  
                govt_military + rugged_terrain + 
                + p5_contiguous + p5_colony + conflict_duration,
              data = peace)  

fit3 <- coxph(Surv(peace_duration, peace_duration2, status) ~ Tonnage_mt_emergency + decisive 
              + lootable + treaty + identity + episode_deaths + factions + 
                initial_democracy + initial_mortality +  peace_agreement +  
                govt_military + rugged_terrain + 
                + p5_contiguous + p5_colony + conflict_duration, 
              data = peace)    

fit4 <- coxph(Surv(peace_duration, peace_duration2, status) ~ Tonnage_mt_emergency * decisive 
              + lootable + treaty + identity + episode_deaths + factions + 
                initial_democracy + initial_mortality +  peace_agreement +  
                govt_military + rugged_terrain + 
                + p5_contiguous + p5_colony + conflict_duration,
              data = peace)  

fit5 <- coxph(Surv(duration, duration2, ep_end) ~ Value_emergency + peripheral + 
                lagged_deaths + population + gdp_pc + current_polity + 
                Diamonds + Drugs + guarantee + rugged_terrain + 
                forest_cover, 
              data = duration)

fit6 <- coxph(Surv(duration, duration2, ep_end) ~ Value_emergency * peripheral + 
                lagged_deaths + population + gdp_pc + current_polity + 
                Diamonds + Drugs + guarantee + rugged_terrain + 
                forest_cover, 
              data = duration)

fit7 <- coxph(Surv(duration, duration2, ep_end) ~ Tonnage_mt_emergency + peripheral + 
                lagged_deaths + population + gdp_pc + current_polity + 
                Diamonds + Drugs + guarantee + rugged_terrain + 
                forest_cover, 
              data = duration)

fit8 <- coxph(Surv(duration, duration2, ep_end) ~ Tonnage_mt_emergency * peripheral + 
                lagged_deaths + population + gdp_pc + current_polity + 
                Diamonds + Drugs + guarantee + rugged_terrain + 
                forest_cover, 
              data = duration)


models <- list(fit1, fit2, fit3, fit4, 
               fit5, fit6, fit7, fit8)

cm <- c('Tonnage_mt_emergency'              = 'Tonnage (1000MTs)',
        'Tonnage_mt_emergency:decisiveTRUE' = 'Tonnage * Decisive victory',
        'Tonnage_mt_emergency:peripheral'   = 'Tonnage * Peripheral conflict',
        'Value_emergency'                   = 'Value (millions of USD)',
        'Value_emergency:decisiveTRUE'      = 'Value * Decisive victory',
        'Value_emergency:peripheral'        = 'Value * Peripheral conflict',
        'decisiveTRUE'                      = 'Decisive victory', 
        'peripheral'                        = 'Peripheral conflict')
        
#fix the notes: 
note <- "This table re-estimates models from Narang (\\cite*{ Narang2014}) and Narang (\\cite*{ Narang2015}) using the IFAR data. The outcome variable in columns 1-4 is whether a peace deal fails (i.e. conflict re-emerges). The outcome variable in columns 5-8 is whether a war terminates (i.e. a peace deal is signed). Explanatory variables are the tonnage of humanitarian food aid provided by USAID (in 1000MTs), the value of humanitarian food aid provided by USAID (in millions of 2018 USD), whether the victory in the previous conflict was decisive, and whether a conflict is peripheral. Standard errors are clustered at the country level. Estimates are from a Cox Proportional Hazards model, and coefficients are displayed as odds ratios. The unit of observation is the country-year. Data sources and control variables are listed in Tables A9 and A10. * p < 0.05, ** p < 0.01, *** p < 0.001"
#Add the rows describing controls: 
rows <- tribble(~term, ~model1, ~model2, ~model3, ~model4, ~model1, ~model2, ~model3, ~model4, 
                "Narang (2014) controls" , "X", "X", "X", "X", " ", " ", " ", " ",
                "Narang (2015) controls" , " ", " ", " ", " ", "X", "X", "X", "X")
attr(rows, 'position') <- c(17:18)

out <- modelsummary(models,  
                    output = 'latex',
                    gof_map = c("nobs", "rmse"), 
                    coef_map = cm, 
                    stars = TRUE, 
                    title = "Humanitarian food aid, the duration of peace, and risk of war termination \\label{survival_value_tonnage}", 
                    escape = FALSE, 
                    add_rows = rows, 
                    estimate = "{estimate}{stars}", 
                    exponentiate = TRUE
) %>% 
  kable_styling(font_size = 8) %>% 
  footnote(general = note, 
           threeparttable = TRUE, 
           footnote_as_chunk = T, 
           escape = FALSE) %>% 
  add_header_above(., c(" " = 1, 
                        "Risk of peace failing" = 4, 
                        "Risk of war termination" = 4))

save_kable(out, "./output/survival_value_tonnage.tex" )
