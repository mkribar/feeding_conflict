# feeding_conflict
Replication file for "Feeding conflict? New data on the impact of humanitarian food aid on civil conflict"

There are three subdirectories in this replication set. 

•	raw_data contains the project-level food aid data. If you want to incorporate tonnage values for humanitarian food aid into a different project, this is probably the folder that you want. See the paper for a discussion of title 1 versus title 2 humanitarian food aid. 

•	intermediate_data: this folder is empty, but is necessary to contain the intermediate files generated by the script. 

•	merged_data: this file contains the datasets with all variables moved in. These data are provided fully merged, so that the user only has to run the code recreating a specific table, rather than merging all the data.  

This replication file was run on R version 4.2.2 using an intel i7-9750H CPU with 32 gigs of installed RAM. 

##00_build_constructs

This file takes the program level data on USAID humanitarian food aid and turns it into country-year measures. It also turns the raw UCDP conflict data into country-year observations. I do not include the raw UCDP conflict data in this replication database, but it can be easily downloaded, and the links are provided as comments in the replication file whenever I merge in outside data. 

##01_merge_table2 

This is the code which creates the merged data I later use for my analysis. Note: the raw control variables are not included here, both for storage space and to respect copyrights. The ‘merged_data’ folder contains the output of this script. I have commented the links to the source of these data, if the user would like to recreate the merged data. 

##01_merge_table3 

This code merges the data used for table 2, as well as all of the subsequent tables which use that format of data. As above, I have not included the raw control variables. 

##02_data_sources 

This code creates tables A7 to A10 in the article, which detail the data sources for the replications in the project. 

##02_figure1

This file contains the code used to create figure 1 in the paper, as well as a companion map which displays the number of years a country received food aid rather than tonnage. 

##02_figure2

This file contains the code used to make figure 2 in the figure, which shows the correlation between different sources of data on humanitarian food aid. 

##02_figure3

This file contains the code used to make figure 4 in the text, which shows the fraction of global food aid provided by USAID. Note: I do not include the raw data used to create this figure. These data are available as bulk downloads from the OECD DAC.  

##02_table1

This file contains the code used to generate table 1, which shows a breakdown of internal shipping and handling (ITSH) costs. 

##02_table2

This file contains the code to generate a surprising number of tables: table 2, table A1, table A5. The common thread is that the files all use same source of data (made in 01_merge_table2). 

##02_table_3

This file contains the code to replicate table 3. 

##02_table_4

This file contains the code to replicate table 4 and tables A2 to A4. 

##02_table_a6

The file contains the code to replicate table A6 in the appendix. 

