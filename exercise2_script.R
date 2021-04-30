library(readr)        
library(dplyr)        
library(ggplot2)      
library(sf)          
library(terra)        
library(lubridate)
library(zoo)

#Task 2 (Task 1 follows)
wildschwein_BE <- read_delim("data/wildschwein_BE_2056.txt", ",")%>%
  st_as_sf(coords = c("E", "N"), crs = 2056, remove= FALSE) %>%
  group_by(TierID) %>%
  
  mutate(steplength = as.numeric(sqrt((E - lead(E,1))^2 + (N - lead(N,1))^2)))%>%
  mutate(timediff = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC,1)))%>%
  mutate(speed = steplength/timediff)
  
#Task 1 getting an overview
ggplot(wildschwein_BE)+
  geom_point( aes(x = DatetimeUTC, y =timediff/3600, color = TierID))
  

summarise(wildschwein_BE, 
          mintime = min(DatetimeUTC), 
          maxtime = max(DatetimeUTC), 
          timeinterval = difftime(maxtime, mintime),
          meantime = mean(timediff, na.rm = T))
    
#3 Different Boars were tracked
#They were tracked between 261 and 338 days
#The Plot shows that they were tracked all at the same time (=concurrently).
#Typically the sampling intervall is between 15 minutes and 3 hours it is usually similar in all the locations


#Task 3

#Reading in the Caro dataset
caro <- read_delim("data/caro60.txt", ",")%>%
  st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE)

#Preserving every n-th value

caro_1 <- caro %>%
  mutate(steplength = as.numeric(sqrt((E - lead(E,1))^2 + (N - lead(N,1))^2)))%>%
  mutate(timediff = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")))%>%
  mutate(speed = steplength/timediff)

caro_3 <- slice(caro, seq(1, nrow(caro), 3))%>%
  mutate(steplength = as.numeric(sqrt((E - lead(E,1))^2 + (N - lead(N,1))^2)))%>%
  mutate(timediff = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")))%>%
  mutate(speed = steplength/timediff)

caro_6 <- slice(caro, seq(1, nrow(caro), 6))%>%
  mutate(steplength = as.numeric(sqrt((E - lead(E,1))^2 + (N - lead(N,1))^2)))%>%
  mutate(timediff = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")))%>%
  mutate(speed = steplength/timediff)

caro_9 <- slice(caro, seq(1, nrow(caro), 9))%>%
  mutate(steplength = as.numeric(sqrt((E - lead(E,1))^2 + (N - lead(N,1))^2)))%>%
  mutate(timediff = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")))%>%
  mutate(speed = steplength/timediff)

#Creating line geometries
caro_3_line <- caro_3 %>%
  group_by() %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

caro_6_line <- caro_6 %>%
  group_by() %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

caro_9_line <- caro_9 %>%
  group_by() %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")


caro_line <- caro %>%
  group_by() %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")


#Plotting
ggplot() +
  ggtitle("Trajectories of Boar Caro")+
  ylab("Northings [m]")+
  xlab("Eastings [m]")+
  geom_sf(data = caro, aes(color = "1 minute"), alpha = 0.5) +
  geom_sf(data = caro_line, aes(color = "1 minute"), alpha = 0.5) +
  geom_sf(data = caro_6, aes(color = "6 minutes"), alpha = 0.5)+
  geom_sf(data = caro_6_line, aes(color = "6 minutes"), alpha = 0.5)+
  coord_sf(datum = 2056)+
  scale_color_manual(name = "Granularity of the trajectory", breaks = c("1 minute", "6 minutes"), values = c("1 minute" = "blue", "6 minutes" = "red"))
  

ggplot() +
  ggtitle("Trajectories of Boar Caro")+
  ylab("Northings [m]")+
  xlab("Eastings [m]")+
  geom_sf(data = caro, aes(color = "1 minute"), alpha = 0.5) +
  geom_sf(data = caro_line, aes(color = "1 minute"), alpha = 0.5) +
  geom_sf(data = caro_9, aes(color = "9 minutes"), alpha = 0.5)+
  geom_sf(data = caro_9_line, aes(color = "9 minutes"), alpha = 0.5)+
  coord_sf(datum = 2056)+
  scale_color_manual(name = "Granularity of the trajectory", breaks = c("1 minute", "9 minutes"), values = c("1 minute" = "blue", "9 minutes" = "red"))



ggplot() +
  ggtitle("Trajectories of Boar Caro")+
  ylab("Northings [m]")+
  xlab("Eastings [m]")+
  geom_sf(data = caro, aes(color = "1 minute"), alpha = 0.5) +
  geom_sf(data = caro_line, aes(color = "1 minute"), alpha = 0.5) +
  geom_sf(data = caro_3, aes(color = "3 minutes"), alpha = 0.5)+
  geom_sf(data = caro_3_line, aes(color = "3 minutes"), alpha = 0.5)+
  coord_sf(datum = 2056)+
  scale_color_manual(name = "Granularity of the trajectory", breaks = c("1 minute", "3 minutes"), values = c("1 minute" = "blue", "3 minutes" = "red"))

#Time-Speed Plot
ggplot()+
  ggtitle("Time - Speed Plot")+
  ylab("Speed [m/s]")+
  xlab("Time")+
  geom_line(data = caro_1, aes(x = DatetimeUTC, y = speed, color ="1 Minute"))+
  geom_line(data = caro_3, aes(x = DatetimeUTC, y = speed, color ="3 Minutes"))+
  geom_line(data = caro_6, aes(x = DatetimeUTC, y = speed, color ="6 Minutes"))+
  geom_line(data = caro_9, aes(x = DatetimeUTC, y = speed, color ="9 Minutes"))


#Task 4
caro2 <- caro_1 %>%
  mutate(rm_k3 = rollmean(speed, k= 3, fill = NA, align = "left"),
          rm_k6 = rollmean(speed, k= 6, fill = NA, align = "left"),
          rm_k10 = rollmean(speed, k= 10, fill = NA, align = "left"),
          rm_k25 = rollmean(speed, k= 25, fill = NA, align = "left"))

#Plotting the data
ggplot(data = caro2)+
  ggtitle("Rolling mean comparison")+
  ylab("Speed [m/s]")+
  xlab("Time")+
  geom_line(aes(x = DatetimeUTC, y = speed, color ="speed"))+
  geom_line(aes(x = DatetimeUTC, y = rm_k3, color ="k = 3"))+
  geom_line(aes(x = DatetimeUTC, y = rm_k6, color ="k = 6"))+
  geom_line(aes(x = DatetimeUTC, y = rm_k10, color ="k = 10"))+
  geom_line(aes(x = DatetimeUTC, y = rm_k25, color ="k = 25"))

#Observation: The higher the k-value the smoother the plotted graph




