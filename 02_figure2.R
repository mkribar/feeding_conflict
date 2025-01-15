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



##################################################################
## This section generates figure 3                              ##
##################################################################

df <- read.csv("./merged_data/table2_data.csv", 
               encoding = "latin1")

#Turn them into country averages: 
df <- df %>% 
  ungroup() %>% 
  group_by(Country) %>% 
  summarise(Tonnage_mt_emergency   = log(mean(Tonnage_mt_emergency, na.rm = TRUE) + 0.01), 
            Tonnage_mt_development = log(mean(Tonnage_mt_development, na.rm = TRUE) + 0.01),
            nq_wheat_aid           = log(mean(nq_wheat_aid, na.rm = TRUE) + 0.01),
            oecd_disbursement      = log(mean(oecd_disbursement, na.rm = TRUE) + 0.01), 
            wfp_tonnage_emergency  = log(mean(wfp_tonnage_emergency, na.rm = TRUE) + 0.01)) %>% 
  ungroup()

df <- df %>% 
  arrange(Tonnage_mt_emergency, wfp_tonnage_emergency)


#And turn into somethigng I can use: 
df <- df %>% 
  dplyr::select(c(Country, Tonnage_mt_emergency, nq_wheat_aid, oecd_disbursement, Tonnage_mt_development, wfp_tonnage_emergency)) %>% 
  pivot_longer(cols = c(oecd_disbursement, nq_wheat_aid, Tonnage_mt_development, wfp_tonnage_emergency), 
               names_to = "series", 
               values_to = "value") %>% 
  mutate(series = case_when(
    series == "nq_wheat_aid"           ~ "Nunn and Qian's US Wheat Aid (1000s of tons)", 
    series == "oecd_disbursement"      ~ "OECD Humanitarian Disburse- ments (Millions of USD)", 
    series == "Tonnage_mt_development" ~ "USAID Title II Development Food Aid (1000s of tons)", 
    series == "wfp_tonnage_emergency"  ~ "WFP Emergency Food Aid (1000s of tons)"
  )) %>% 
  mutate(ISO = countrycode(Country, "country.name", "iso3c"))
  
#Now I actually generate the side-by-side scatter plots: 
ggplot(data = df) + 
  geom_point(aes(x = value, y = Tonnage_mt_emergency, color = series), 
             size = 2, alpha = 0.8) + 
  facet_wrap(vars(series), nrow = 2, ncol = 2,
             scales = "fixed", 
             labeller = label_wrap_gen(width=32)) + 
  scale_color_manual(values = wes_palette("Zissou1")[c(1,2,4,5)]) + 
  theme(text=element_text(family = "serif"), 
        legend.position = "none", 
        axis.title.y = element_text(size = 10), 
        axis.title.x = element_text(size = 10)) + 
  scale_y_continuous(name = "Tonnage of USAID Humanitarian Food Aid (1000s)") + 
  scale_x_continuous(name = "Alternative Measures of Aid")
  
ggsave("figure2.png", 
       path = "./output/", 
       width = 6, height = 5)

##################################################
# Correlation coefficient of USAID/WFP tonnage   #
##################################################

#Read in my food aid data: 
title2 <- read.csv("./merged_data/table2_data.csv") %>% 
  dplyr::select(!X) 

cor(title2$wfp_tonnage_emergency, title2$Tonnage_mt_emergency, use = "pairwise.complete.obs")


