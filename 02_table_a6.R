######Set up: ######
#clear up
rm(list=ls())

#Read in the usual packages
library(tidyverse)
require(kableExtra)


#Reshape the dataset
test <- read.csv("./merged_data/table2_data.csv", 
                 encoding = "latin1") %>% 
  group_by(Country) %>% 
  mutate(binary_combined = Tonnage_mt_Combined > 0, 
         binary_development = Tonnage_mt_development > 0, 
         binary_any = Tonnage_mt_emergency > 0 | Tonnage_mt_Combined > 0 | Tonnage_mt_development > 0) %>% 
  summarise(binary_emergency   = sum(binary_emergency, na.rm = TRUE), 
            binary_development = sum(binary_development, na.rm = TRUE), 
            binary_combined    = sum(binary_combined, na.rm = TRUE), 
            binary_any         = sum(binary_any, na.rm = TRUE)) 


#Make table A.6. 
kable(test,
      booktabs = T, 
      longtable = TRUE,
      format = "latex",
      caption = "Countries represented in the IFAR data", 
      digits = 0, 
      col.names = c("Country", "Emergency", "Development", "Combined", "Any"), 
      linesep = "") %>% 
  add_header_above(c(" " = 1, "Years receiving aid" = 4)) %>% 
  # footnote(general = "Data from the IFAR reports. 'Combined' aid measures aid delivered from 1991 to 1994; 1995 is the first year that data are broken into emergency and development food assistance.", 
  #          footnote_as_chunk = TRUE, 
  #          threeparttable = TRUE) %>% 
  kable_styling(font_size = 10) %>% 
  save_kable(., file = "./output/table_all_countries.tex")
