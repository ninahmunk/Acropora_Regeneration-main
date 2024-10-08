# Regeneration 3.0 Temperature Data 

#load packages
library(tidyverse)
library(cowplot)
library(lme4)
library(lmerTest)
library(here)
library(rstatix)
library(Rmisc)
library(janitor)
library(ggthemes)
library(pbkrtest)
library(sjPlot)
library(knitr)
library(kableExtra)

#check working directory and set working directory to folder with raw temperature data 
getwd()

#set working directory to where data exists on computer
setwd("/Users/ninahmunk/Desktop/Projects/Acropora_Regeneration-main/Temperature/Data")

################### HOT SUMP ################################################### #####
# read in raw temperature csv file and change column names for date/time and temperature
hotsump<-read.csv('21512790_hot_sump_20230703.csv')%>%slice(-2, -3, -3032, -3031)
colnames(hotsump)[2] = "date_time"
colnames(hotsump)[3] = "temp_c"

# separate data and time into two columns 
hotsump_date <- separate(hotsump, 'date_time',
                                    into = c('longdate', 'time'),
                                    sep= ' ') 

# visualize variation in daily temperatures across each day of the experiment in the hot sump
 ggplot(data=hotsump_date, 
                        aes(x=as.Date(longdate, format = "%m / %d / %Y"), 
                            y=temp_c)) +
  geom_point(size=1, color = 'red')+ theme_bw()+ 
  theme(axis.text.x = element_text(angle=45, margin = margin(t=20, r=100)))+
  labs(title="Raw temperature data", y="Temperature (°C)", x="Date")


# new data frame to separate data/time column into month, day, and year 
hotsumpfull <- hotsump_date %>%separate('longdate',
                  into = c('month', 'day', 'year'),
                  sep= '/',
                  remove = FALSE)

# group data by year, month, day, and longdate. Within these groups, calculate the group mean
hotsumpfull_mean <- hotsumpfull %>%
  group_by(year, month, day, longdate)%>%
  summarise(meantemp = mean(temp_c))

# look at the means
head(hotsumpfull_mean)

# get the mean temperature of the hot sump over the entire experimental period
hotmean <- hotsumpfull %>%
  summarise(meantemp = mean(temp_c))
print(hotmean) # mean = 29.492

# get the standard error of mean temperature from the hot sump over the entire experimental period
hotstderr<- hotsumpfull%>%aggregate(temp_c ~ temp_c, FUN = function(x) sd(x)/sqrt(length(x))) # SE = 0.006429682
print(hotstderr)

# visualize variation in daily mean temperature across each day of the experiment in the hot sump
 ggplot(data=hotsumpfull_mean, 
                        aes(x=day, 
                            y=meantemp)) +
  geom_point(size=1, color = 'red')+ theme_bw()+ 
  theme(axis.text.x = element_text(angle=45, margin = margin(t=20, r=100)))+
  labs(title="Mean Temperature data - Elevated", y="Temperature (°C)", x="Date")

 ################## HOT TANK #1 ################################################ #####
 # read in raw temperature csv file and change column names for date/time and temperature
 hottank<-read.csv('21538154_tank1_H_20230703.csv')%>%slice(-3030, -3029, -3028)
 colnames(hottank)[2] = "date_time"
 colnames(hottank)[3] = "temp_c"
 
 #separate data and time into two columns 
 hottank_date <- separate(hottank, 'date_time',
                                 into = c('longdate', 'time'),
                                 sep= ' ') 
 
 # visualize variation in daily temperatures across each day of the experiment in the hot tank
 ggplot(data=hottank_date, 
        aes(x=as.Date(longdate, format = "%m / %d / %Y"), 
            y=temp_c)) +
   geom_point(size=1, color = 'red')+ theme_bw()+ 
   theme(axis.text.x = element_text(angle=45, margin = margin(t=20, r=100)))+
   labs(title="Raw temperature data", y="Temperature (°C)", x="Date")
 
 
 # new data frame to separate data/time column into month, day, and year 
 hottankfull <- hottank_date %>%
   separate('longdate',
                   into = c('month', 'day', 'year'),
                   sep= '/',
                   remove = FALSE)
 
 # group data by year, month, day, and longdate. Within these groups, calculate the group mean
 hottankfull_mean <- hottankfull %>%
   group_by(year, month, day, longdate)%>%
   summarise(meantemp = mean(temp_c))
 
# look at the means
 head(hottankfull_mean)
 
 # get the standard error of mean temperature from the hot tank over the entire experimental period
 hot.tank.stderr<- hottankfull%>%aggregate(temp_c ~ temp_c, FUN = function(x) sd(x)/sqrt(length(x))) 
 print(hot.tank.stderr) #SE = 0.0062963
 
 
 # visualize daily mean temperature across each day of the experiment in the hot tank
 ggplot(data=hottankfull_mean, 
        aes(x=day, 
            y=meantemp)) +
   geom_point(size=1, color = 'red')+ theme_bw()+ 
   theme(axis.text.x = element_text(angle=45, margin = margin(t=20, r=100)))+
   labs(title="Mean Temperature data - Elevated", y="Temperature (°C)", x="Date")
################### COMBINE HOT ################################################ #####
# clean data frames to prepare to combine 
 hotsumpfull%>%select(longdate, month, day, year, temp_c)-> hotsumpfull
 hottankfull%>%select(longdate, month, day, year, temp_c)-> hottankfull
 
 # combine data frames from hot tank and hot sump
 rbind(hottankfull, hotsumpfull)-> hot.only
 
 # group by year, month, day, longdate and calculate means within these groups
 hotfull_mean <- hot.only %>%
   group_by(year, month, day, longdate)%>%
   summarise(meantemp = mean(temp_c))
 # look at means
 print(hotfull_mean)
 
 # get overall mean of hot treatment
 hot.only.mean <- hot.only %>%
   summarise(meantemp = mean(temp_c)) 
 print(hot.only.mean) # mean = 29.51483
 
 # get standard error for overall mean 
 hotstderr<- hot.only%>%aggregate(temp_c ~ temp_c, FUN = function(x) sd(x)/sqrt(length(x))) 
 print(hotstderr)# SE = 0.00450876
 
################### AMBIENT #################################################### #####
 
# complete all of the same steps as done above for cleaning ambient temperature data, I only have ambient temp data from the ambient sump. 
ambientsump<-read.csv('21512796_ambient_sump_20230703.csv')%>%slice(-3024, -3023, -3022)
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


#new data frame to separate data/time column into month, day, and year 
ambientsumpfull <- ambientsump_date %>%
separate('longdate',
                  into = c('month', 'day', 'year'),
                  sep= '/',
                  remove = FALSE)

ambientsumpfull_mean <- ambientsumpfull %>%
  group_by(year, month, day, longdate)%>%
  summarise(meantemp = mean(temp_c))

ambient.only.mean <- ambientsumpfull %>%
  summarise(meantemp = mean(temp_c)) # mean = 27.86313

ambientstderr<- ambientsumpfull%>%aggregate(temp_c ~ temp_c, FUN = function(x) sd(x)/sqrt(length(x))) # SE = 0.01361969


#visualize variation in daily temperatures across each day of the experiment in the hot sump
ggplot(data=ambientsumpfull_mean, 
       aes(x=day, 
           y=meantemp)) +
  geom_point(size=1, color = 'blue')+ theme_bw()+ 
  theme(axis.text.x = element_text(angle=45, margin = margin(t=20, r=100)))+
  labs(title="Mean Temperature data - Elevated", y="Temperature (°C)", x="Date")


############## COMBINE ALL TEMP DATA ########################################### #####
ambientsumpfull_mean%>%mutate(treatment = "Ambient") ->ambientsumpfull_mean
hotfull_mean%>%mutate(treatment = "Elevated") ->hotfull_mean
#hottankfull_mean%>%mutate(treatment = "Elevated") ->hottankfull_mean


rbind(hotfull_mean, ambientsumpfull_mean)-> all.sump.temp

summary(model<- lmer(meantemp ~ treatment + (1|longdate), data = all.sump.temp))

ggplot(data=all.sump.temp, 
       aes((x=as.Date(longdate, format = "%m / %d / %Y")), 
           y=meantemp, color = treatment)) +
  geom_point(size=3)+ theme_bw()+ 
  theme(axis.text.x = element_text(angle=45, margin = margin(t=20, r=100)))+
  labs(title=" ", y="Temperature (°C)", x="Day")+
  scale_color_manual(values = c("Elevated" = "#CC79A7", "Ambient" = "#D55E00"))

######### Means and Standard Errors from raw data to make final figure ######### #####
ambient <- ambientsumpfull%>%group_by(longdate)%>%summarise(meantemp = mean(temp_c))%>%mutate(treatment = "Ambient")
std_err<- ambientsumpfull%>%aggregate(temp_c ~ longdate, FUN = function(x) sd(x)/sqrt(length(x)))
ambient%>%left_join(std_err, by = "longdate") -> ambient

summarySE(ambientsumpfull, measurevar = "temp_c",
          na.rm = FALSE, conf.interval = 0.95, .drop = TRUE) # average 27.86313 +/- 0.7485876 SD

hot<- hot.only%>%group_by(longdate)%>%summarise(meantemp = mean(temp_c))%>%mutate(treatment = "Elevated") 
std_err2<- hot.only%>%aggregate(temp_c ~ longdate, FUN = function(x) sd(x)/sqrt(length(x))) 
hot%>%left_join(std_err2, by = "longdate") -> hot

summarySE(hot.only, measurevar = "temp_c",
          na.rm = FALSE, conf.interval = 0.95, .drop = TRUE) # average 29.51483 +/- 0.3508441 SD

rbind(ambient, hot)%>%rename(std_err = temp_c) -> temperature 

ggplot(data=temperature, 
       aes((x=as.Date(longdate, format = "%m / %d / %Y")), 
           y=meantemp, color = treatment, shape = treatment)) +
  geom_point(size=3)+ theme_bw()+ 
  geom_errorbar(aes(ymin=meantemp-std_err, ymax=meantemp+std_err), width=.9)+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, margin = margin(t=20, r=100)), text=element_text(size=20))+
  labs(title=" ", y="Temperature (°C)", x="Day")+
  scale_color_manual(values = c("Elevated" = "#CC79A7", "Ambient" = "#D55E00")) -> Figure.1

#this one
ggplot(data=temperature, aes((x=as.Date(longdate, format = "%m / %d / %Y")), y=meantemp, color = treatment, shape = treatment)) +
  geom_point(size=3)+ 
  geom_errorbar(aes(ymin=meantemp-std_err, ymax=meantemp+std_err), width=.9)+
  theme(axis.text.x = element_text(angle=45, margin = margin(t=20, r=100)), text=element_text(size=20))+
  labs(y="Temperature (°C)", x="Time (Day)")+
  scale_color_manual(values = c("Elevated" = "red", "Ambient" = "blue"))+
  scale_shape_manual(values = c(1, 19), labels = c("Ambient", "Elevated"))+
  guides(col = guide_legend("Temperature"), shape = guide_legend("Temperature"))+
  theme_classic()+
  theme_few(base_size = 20) -> temp



ggsave("temperature.jpg", plot = temp, path = here(),
       width = 12,
       height = 8,
       units = "in")

#temp ranges, elevated = 29.69927 =/- xx to 28.73736 +/- xx; ambient 28.02507 +/- xx to 27.88757 =/- xx
############## t-test ########################################################## #####
all.sump.temp%>%pivot_wider(names_from = treatment, values_from = meantemp)-> test.wide

mod <- t.test(x = test.wide$Elevated,
                  y = test.wide$Ambient,
                  alternative = "two.sided",
                  mu = 0, paired = FALSE, var.equal = FALSE,
                  conf.level = 0.95)
print(mod)

############## temp differences between TT ##################################### #####
test.wide%>%mutate(diff = Elevated - Ambient)-> test.wide.diff
#  difference of mean temperature between treatments ranged from 2.4964236 to 0.8497917. 

# 19 day average difference in C between both temperatures 
summary(test.wide.diff) # across 19 days the mean difference in temperatures between TT was 1.6475 C
summarySE(test.wide.diff, measurevar = "diff",
          na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)

