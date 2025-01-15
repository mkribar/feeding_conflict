
######Set up: ---- 
#clear up
rm(list=ls())

require(readxl)
require(tidyverse)
require(knitr)
require(kableExtra)

##### Data sources ---- 

df1 <- read_excel("./raw_data/Data sources.xlsx", 
                  sheet = "Nunn and Qian 2014")

df2 <- read_excel("./raw_data/Data sources.xlsx", 
                  sheet = "Mary and Mishra 2021")

df3 <- read_excel("./raw_data/Data sources.xlsx", 
                  sheet = "Narang 2014")

df4 <- read_excel("./raw_data/Data sources.xlsx", 
                  sheet = "Narang 2015")

kable(df1[,1:3], 
      booktabs = TRUE, 
      format = "latex", 
      caption = "Control variables from Nunn and Qian (2014)", 
      col.names = c("Variable", 
                    "Operationalization", 
                    "Source"), 
      linesep = "\\addlinespace") %>% 
  kable_styling(font_size = 8, 
                latex_options = "hold_position") %>% 
  column_spec(1, width = "10em") %>% 
  column_spec(2, width = "20em") %>% 
  column_spec(3, width = "20em") %>% 
  save_kable(., file = "./output/a_data_1.tex")

kable(df2[,1:3], 
      booktabs = TRUE, 
      format = "latex", 
      caption = "Control variables from Mary and Mishra (2020)", 
      col.names = c("Variable", 
                    "Operationalization", 
                    "Source"), 
      linesep = "\\addlinespace") %>% 
  kable_styling(font_size = 8) %>% 
  column_spec(1, width = "10em") %>% 
  column_spec(2, width = "20em") %>% 
  column_spec(3, width = "20em") %>% 
  save_kable(., file = "./output/a_data_2.tex")

kable(df3[,1:3], 
      booktabs = TRUE, 
      format = "latex", 
      caption = "Control variables from Narang (2014)", 
      col.names = c("Variable", 
                    "Operationalization", 
                    "Source"), 
      linesep = "\\addlinespace") %>% 
  kable_styling(font_size = 8) %>% 
  column_spec(1, width = "10em") %>% 
  column_spec(2, width = "20em") %>% 
  column_spec(3, width = "20em") %>% 
  save_kable(., file = "./output/a_data_3.tex")

kable(df4[,1:3], 
      booktabs = TRUE, 
      format = "latex", 
      caption = "Control variables from Narang (2015)", 
      col.names = c("Variable", 
                    "Operationalization", 
                    "Source"), 
      linesep = "\\addlinespace") %>% 
  kable_styling(font_size = 8) %>% 
  column_spec(1, width = "10em") %>% 
  column_spec(2, width = "20em") %>% 
  column_spec(3, width = "20em") %>% 
  save_kable(., file = "./output/a_data_4.tex")

      
      
        
