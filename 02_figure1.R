######Set up: ######
#clear up
rm(list=ls())

#Read in the usual packages
library(tidyverse)
library(wesanderson)
library(lfe)
library(countrycode)
require(readxl)
require(data.table)
require(skimr)
require(stringr)

#geospatial
require(sf)

#Read in the data: 
df <- read.csv("./merged_data/table2_data.csv", 
               encoding = "latin1")



#Read in the shapefiles: 
shp <- read_sf("./raw_data/World_Countries__Generalized_.shp", 
               stringsAsFactors = FALSE, ) %>% 
  mutate(COUNTRY = ifelse(COUNTRY == "Turkiye", "Turkey", COUNTRY)) %>% 
  mutate(COUNTRY = countrycode(COUNTRY, "country.name", "country.name", 
                               warn = FALSE)) %>% 
  filter(!is.na(COUNTRY))


######Set up: ######
#Build the two constructs: 
df <- df %>% 
  group_by(Country) %>% 
  summarise(mean_food_aid = mean(Tonnage_mt_emergency, na.rm = TRUE), 
            mean_years    = sum(binary_emergency, na.rm = TRUE)) %>% 
  ungroup()

shp <- left_join(shp, df, by = c("COUNTRY" = "Country")) 

#I'm going to drop Antarctica to make the graph cleaner: 
shp <- shp %>% 
  filter(COUNTRY != "Antarctica")

#This should be true: 
nrow(shp) - sum(is.na(shp$mean_years)) == 103

#This should be zero: 
left_join(df, shp, by = c("Country" = "COUNTRY")) %>% 
  filter(is.na(ISO)) %>% 
  nrow()

#how many countries received 0 years of humanitarian food aid: 
table({df$mean_years == 0})


###### Make the map ######

#Rescale or else everything looks silly. 
shp$mean_food_aid <- log(shp$mean_food_aid + 0.01)

ggplot(data = shp) + 
  geom_sf(aes(fill = mean_food_aid)) + 
  theme_bw()+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        text=element_text(family = "serif"),
        legend.position = "bottom",
        plot.caption = element_text(size = 8, family = "serif", 
                                    hjust = 0.5)) + 
  scale_fill_gradientn(colors = wes_palette("Zissou1", 21, type = "continuous"))+ 
  labs(fill = "Logged Tonnage")

ggsave("figure1.png", 
       path = "./output", 
       width = 6, height = 3.5)

###### Alternate: number of years ######
ggplot(data = shp) + 
  geom_sf(aes(fill = mean_years)) + 
  theme_bw()+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        text=element_text(family = "serif"),
        legend.position = "bottom",
        #legend.title = element_blank(),
        plot.caption = element_text(size = 8, family = "serif", 
                                    hjust = 0.5)) + 
  scale_fill_gradientn(colors = wes_palette("Zissou1", 25, type = "continuous"))+ 
  labs(fill = "Mean years")

ggsave("map_years.png", 
       path = "./output", 
       width = 6, height = 3.5)





  
