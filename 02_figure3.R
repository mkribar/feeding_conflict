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
require(data.table)
require(skimr)
require(stringr)

#Multilateral donor codes come from the DAC list of donor codes: 
#https://www.oecd.org/dac/financing-sustainable-development/development-finance-standards/dacandcrscodelists.htm
# (accessed 26 September, 2022)  
multilateral <- c(104, 	807, 	811, 	812, 	901, 	902, 	903, 	905, 	906, 	907, 	909, 	913, 	914, 	915, 	921, 	923, 	926, 	928, 	932, 	940, 	944, 	948, 	951, 	952, 	953, 	954, 	956, 	958, 	959, 	960, 	963, 	964, 	966, 	967, 	971, 	974, 	976, 	978, 	979, 	980, 	981, 	982, 	983, 	988, 	990, 	992, 	997, 	1011, 	1012, 	1013, 	1014, 	1015, 	1016, 	1017, 	1018, 	1019, 	1020, 	1023, 	1024, 	1025, 	1037, 	1038, 	1311, 	1312, 	1313)


######################################
# Read in OECD CRS                   #
######################################

#Different years of DAC data are in different .txt files, 
#so I loop over them to build my own dataset. 

#extract the filenames: 
files <- list.files("C:/Users/mkrib/Documents/007 Data/OECD CRS/")
  #drop the READ ME
  files <- files[!str_detect(files, "READ")]
  
#also easiest to set the wd to where these files live: 
  setwd("C:/Users/mkrib/Documents/007 Data/OECD CRS/")
  
#make a list to put the data: 
  dac <- list()
  
  bias <- list()
  
#Now loop over every one of these files:   
for(i in 1:length(files)){
  #read the data
  df <- fread(files[i], 
            stringsAsFactors = FALSE)

  #Select only humanitarian aid: 
  df <- df %>% 
    filter(str_detect(SectorName, "VIII"))
  
  #Extract the data for the country biases figure
  figure2 <- df %>% 
    mutate(DonorName = ifelse(DonorName == "United States", "USA", "Other")) %>% 
    group_by(DonorName, RecipientName, Year) %>% 
    summarise(total   = sum(USD_Disbursement, na.rm = TRUE)) %>% 
    ungroup() %>% 
    pivot_wider(names_from  = "DonorName", 
                values_from = "total") %>% 
    group_by(Year) %>% 
    mutate(year_sum = sum(Other, na.rm = TRUE), 
           USA_sum  = sum(USA, na.rm = TRUE)) %>% 
    mutate(across(.cols = everything(), 
                  .fns  = ~ replace_na(.x, 0)))
  
  bias[[i]] <- figure2
  
  #Make into dyads: 
  df1 <- df %>% 
    mutate(DonorName = ifelse(DonorName == "United States", "USA", "Other")) %>% 
    group_by(DonorName, Year) %>% 
    summarise(disbursements = sum(USD_Disbursement, na.rm = TRUE)) %>% 
    mutate(type = "emergency")
  
  out <- df1
  
  #Subset only to food assistance 
  df1 <- df %>% 
    filter(PurposeName == "Emergency food assistance") %>% 
    mutate(DonorName = ifelse(DonorName == "United States", "USA", "Other")) %>% 
    group_by(DonorName, Year) %>% 
    summarise(disbursements = sum(USD_Disbursement, na.rm = TRUE)) %>% 
    mutate(type = "food_only")
  
  out <- rbind(out, df1)
  
  #Now we drop multilateral institutions to avoid double counting. 
  dfm <- df %>% 
    mutate(multi = DonorCode %in% multilateral) %>% 
    filter(!multi) %>% 
    mutate(DonorName = ifelse(DonorName == "United States", "USA", "Other")) %>% 
    group_by(DonorName, Year) %>% 
    summarise(disbursements = sum(USD_Disbursement, na.rm = TRUE)) %>% 
    mutate(type = "emergency_no_multilateral")
  
  out <- rbind(out, dfm)
  
  #Subset only to food assistance (no multilaterals)
  dfm <- df %>% 
    mutate(multi = DonorCode %in% multilateral) %>% 
    filter(!multi) %>% 
    filter(PurposeName == "Emergency food assistance") %>% 
    mutate(DonorName = ifelse(DonorName == "United States", "USA", "Other")) %>% 
    group_by(DonorName, Year) %>% 
    summarise(disbursements = sum(USD_Disbursement, na.rm = TRUE)) %>% 
    mutate(type = "food_no_multilateral")
  
  out <- rbind(out, dfm)
  
  #and store in the list: 
  dac[[i]] <- out
}

#And concatenate: 
df <- do.call("rbind", dac)  

######################################
# Build constructs                   #
######################################

df <- pivot_wider(df, 
                  names_from = c("DonorName", "type"), 
                  values_from = "disbursements")

#verything is zero before 1983: 
df <- df[df$Year > 2001,]

table <- data.frame(
  Year = df$Year, 
  emergency                 = df$USA_emergency                 / (df$Other_emergency + df$USA_emergency), 
  food_only                 = df$USA_food_only                 / (df$Other_food_only + df$USA_food_only), 
  emergency_no_multilateral = df$USA_emergency_no_multilateral / (df$Other_emergency_no_multilateral + df$USA_emergency_no_multilateral), 
  food_no_multilateral      = df$USA_food_no_multilateral      / (df$Other_food_no_multilateral + df$USA_food_no_multilateral)
)

##################################################
# Make figure : frac of aid which is from the US #
##################################################
figure <- table %>% 
 pivot_longer(cols = emergency:food_no_multilateral, 
              names_to = "type")

#For now, don't include multilaterals: 
figure <- figure %>% 
  filter(type != "emergency") %>% 
  filter(type != "food_only") %>% 
  mutate(type = ifelse(type == "emergency_no_multilateral", "All emergency aid", "Emergency food aid"))

ggplot(data = figure) + 
  geom_line(aes(x = Year, y = value, color = type), 
            size = 1.2) + 
  scale_color_manual(values = wes_palette("Zissou1")[c(1,5)]) + 
  scale_y_continuous(name = "Fraction of global humanitarian aid", 
                     limits = c(0.2,0.85)) + 
  theme(text=element_text(family = "serif"), 
        legend.position = "bottom", 
        axis.title.y = element_text(size = 8), 
        axis.title.x = element_blank(), 
        legend.title = element_blank())
  
ggsave("figure3.png", 
       path = "./output/", 
       width = 6, height = 2.5)














