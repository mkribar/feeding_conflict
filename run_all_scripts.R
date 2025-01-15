########################################################
##### This script replicates the figures and tables ----
##### for "Feeding Conflict? New data on the impact ----
##### of humanitarian food aid on civil conflict    ----
########################################################



#This file uses R projects to resolve the relative filepath problem. 
#Please ensure there is a file titled "foreign aid and conflict.Rproj"
#In the folder where you have saved this replication package. 



#######################################################
##### Build the data set                          -----
#######################################################

#I do not actually include the raw, unprocessed from external sources
#UCDP, etc. in this replication file. I include these R script to 
#show how I build my constructs. 


#source("00_build_constructs.R")
#source("01_merge_table2.R")
#source("01_merge_table4.R")
#source("02_data_sources.R")

#######################################################
##### Make the three figures                      -----
#######################################################

source("02_figure2.R")
source("02_figure1.R")

#This figure again uses data downloaded directly from CRS, which 
#I do not include here for size reasons. 
#source("02_figure3.R")

#######################################################
##### Make the tables                      -----
#######################################################

source("02_table_a6.R")
source("02_table1.R")
source("02_table2.R")
source("02_table3.R")
source("02_table4.R")


