library(readr)        
library(dplyr)        
library(ggplot2)      
library(sf)          
library(terra)        
library(lubridate)

#Task 2

wildschwein_BE <- read_delim("data/wildschwein_BE_2056.txt", ",") %>%
  st_as_sf(coords = c("E", "N"), crs = 2056, remove= FALSE) %>%
  group_by(TierID) %>%
  
  mutate(steplength = sqrt((E - lead(E,1))^2 + (N - lead(N,1))^2))%>%
  mutate(timediff = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC,1)))

#Task 3