# Regeneration 3.0 Temperature Data 
library(tidyverse)
library(cowplot)

#check working directory and set working directory to folder with raw temperature data 
getwd()
setwd("/Users/ninahmunk/Documents/Projects/Regeneration/Temperature/Data")

#read in raw temperature csv file and change column names for date/time and temperature
hotsump<-read.csv('21512790_hot_sump_20230703.csv')
colnames(hotsump)[2] = "date_time"
colnames(hotsump)[3] = "temp_c"

#separate data and time into two columns 
hotsump_date <- tidyr::separate(hotsump, 'date_time',
                                    into = c('longdate', 'time'),
                                    sep= ' ') 

#visualize variation in daily temperatures across each day of the experiment in the hot sump
hotsump_graph <- ggplot(data=hotsump_date, 
                        aes(x=as.Date(longdate, format = "%m / %d / %Y"), 
                            y=temp_c)) +
  geom_point(size=1, color = 'red')+ theme_bw()+ 
  theme(axis.text.x = element_text(angle=45, margin = margin(t=20, r=100)))+
  labs(title="Raw temperature data", y="Temperature (°C)", x="Date")
hotsump_graph

#new data frame to separate data/time column into month, day, and year 
hotsumpfull <- hotsump_date %>%
  tidyr::separate('longdate',
                  into = c('month', 'day', 'year'),
                  sep= '/',
                  remove = FALSE)
head(hotsumpfull)

hotsumpfull_mean <- hotsumpfull %>%
  group_by(year, month, day, longdate)%>%
  summarise(meantemp = mean(temp_c))

head(hotsumpfull_mean)




ambientsump<-read.csv('21512796_ambient_sump_20230703.csv')
colnames(ambientsump)[2] = "date_time"
colnames(ambientsump)[3] = "temp_c"
ambientsump_date <- tidyr::separate(ambientsump, 'date_time',
                                into = c('longdate', 'time'),
                                sep= ' ') 

ambientsump_graph <- ggplot(data=ambientsump_date, 
                        aes(x=as.Date(longdate, format = "%m / %d / %Y"), 
                            y=temp_c)) +
  geom_point(size=1, color = 'blue')+ theme_bw()+ 
  theme(axis.text.x = element_text(angle=45, margin = margin(t=20, r=100)))+
  labs(title="Raw temperature data", y="Temperature (°C)", x="Date")
ambientsump_graph