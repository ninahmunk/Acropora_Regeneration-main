#bouyant weight to skeletal mass calculations for acropora pulchra frags

library(tidyverse)
library(readxl)
library(janitor)

#set working directory
getwd()
setwd("/Users/ninahmunk/Documents/Projects/Regeneration/Growth/Data")
################### Initial Skeletal Mass ########################################## ##### 
#load data 
weight_initial<- read_xlsx("bouyantweight_initial.xlsx", sheet= "raw_data")%>%clean_names()%>% 
  
#add column to calculate density of the glass stopper, 0.9965 is the density of freshwater
  mutate(density_stopper= (air_weight_g * 0.9965)/(air_weight_g - fresh_weight_g))%>%
  
#add column to calculate density of seawater
  mutate(density_sw= (air_weight_g - salt_weight_g)/ (air_weight_g / density_stopper))%>%
  
#add column to calculate volume of the coral (to use later in respo code)
  mutate(vol_coral_cm3= bouyantweight_g / (density_aragonite - density_sw))%>%
  
#add column to calulate dry mass of the coral 
  mutate(dry_mass_coral_g= vol_coral_cm3 * density_aragonite)%>%
  
#select columns I want to use in data visualization
  select(c(coral_id, dry_mass_coral_g))%>%
  
#rename column of coral mass to 'initial'
  rename("initial" = "dry_mass_coral_g")
  
#option to skip directly to this equation below after calculating density of seawater if you donʻt need to extract coral volume
  #mutate(dry_mass_coral_2= bouyantweight_g / (1 - (density_sw/density_aragonite)))

#download new dataframe as a csv file to wherever you want on computer
write_csv(weight_initial, path = "/Users/ninahmunk/Documents/Projects/Regeneration/Growth/Output/dry_mass_initial.csv")
################### Initial Chamber Volumes ########################################## ##### 
#load data 
chamber_vols_initial<- read_xlsx("bouyantweight_initial.xlsx", sheet= "raw_data")%>%clean_names()%>% 
  
  #add column to calculate density of the glass stopper, 0.9965 is the density of freshwater
  mutate(density_stopper= (air_weight_g * 0.9965)/(air_weight_g - fresh_weight_g))%>%
  
  #add column to calculate density of seawater
  mutate(density_sw= (air_weight_g - salt_weight_g)/ (air_weight_g / density_stopper))%>%
  
  #add column to calculate volume of the coral (to use later in respo code)
  mutate(vol_coral_cm3= bouyantweight_g / (density_aragonite - density_sw))%>%
  
  #add column to calculate volume of chamber
  mutate(chamber_vol= 650 - vol_coral_cm3)%>%
  
  #select columns I need
  select(c(date, coral_id, chamber_vol))

write_csv(chamber_vols_initial, path = "/Users/ninahmunk/Documents/Projects/Regeneration/Respiration/Data/chamber_vol_initial.csv")

################### Skeletal Mass for 24, Day 10, and Final Timepoints ############### #####


weight_24hr<- read_xlsx("bouyantweight_24hr.xlsx", sheet= "raw_data")%>%clean_names()%>% 
  mutate(density_stopper= (air_weight_g * 0.9965)/(air_weight_g - fresh_weight_g))%>%
  mutate(density_sw= (air_weight_g - salt_weight_g)/ (air_weight_g / density_stopper))%>%
  mutate(vol_coral_cm3= bouyantweight_g / (density_aragonite - density_sw))%>%
  mutate(dry_mass_coral_g= vol_coral_cm3 * density_aragonite)%>%
  select(c(coral_id, dry_mass_coral_g))%>%
  rename("hr24" = "dry_mass_coral_g")
write_csv(weight_24hr, path = "/Users/ninahmunk/Documents/Projects/Regeneration/Growth/Output/dry_mass_24.csv")

weight_day10<- read_xlsx("bouyantweight_day10.xlsx", sheet= "raw_data")%>%clean_names()%>% 
  mutate(density_stopper= (air_weight_g * 0.9965)/(air_weight_g - fresh_weight_g))%>%
  mutate(density_sw= (air_weight_g - salt_weight_g)/ (air_weight_g / density_stopper))%>%
  mutate(vol_coral_cm3= bouyantweight_g / (density_aragonite - density_sw))%>%
  mutate(dry_mass_coral_g= vol_coral_cm3 * density_aragonite)%>%
  select(c(coral_id, dry_mass_coral_g))%>%
  rename("day10" = "dry_mass_coral_g")
write_csv(weight_day10, path = "/Users/ninahmunk/Documents/Projects/Regeneration/Growth/Output/dry_mass_day10.csv")

weight_final<- read_xlsx("bouyantweight_final.xlsx", sheet= "raw_data")%>%clean_names()%>% 
  mutate(density_stopper= (air_weight_g * 0.9965)/(air_weight_g - fresh_weight_g))%>%
  mutate(density_sw= (air_weight_g - salt_weight_g)/ (air_weight_g / density_stopper))%>%
  mutate(vol_coral_cm3= bouyantweight_g / (density_aragonite - density_sw))%>%
  mutate(dry_mass_coral_g= vol_coral_cm3 * density_aragonite)%>%
  select(c(coral_id, dry_mass_coral_g))%>%
  rename("final" = "dry_mass_coral_g")
write_csv(weight_final, path = "/Users/ninahmunk/Documents/Projects/Regeneration/Growth/Output/dry_mass_final.csv")

#load master datasheet with treatment information
master<- read_xlsx("regen_3_coral_mastersheet.xlsx", sheet ='mastersheet_extended')

#load final surface areas  
setwd("/Users/ninahmunk/Documents/Projects/Regeneration/Surface_Area/Output")
surface_area<- read.csv("final_surface_areas.csv")%>%
  select(c(coral_id,CSA_cm2))

#combine all these dataframes together and calculate for growth + standardize by surface area and days
list_df = list(weight_initial, weight_24hr, weight_day10, weight_final, master, surface_area)
full_df<-list_df%>%reduce(inner_join, by='coral_id')%>%
  mutate(growth_mg= ((hr24 - initial)*1000))%>%
  mutate(growth_mg_cm2_day = (growth_mg/CSA_cm2)/19)%>%
  mutate(wound = as_factor(wound))

ggplot(full_df)+ 
  geom_boxplot(aes(genotype, growth_mg_cm2_day, group = genotype))+ 
  geom_point(aes(genotype, growth_mg_cm2_day, color= wound))

ggplot(full_df)+
  geom_boxplot(aes(wound, growth_mg_cm2_day, fill = temp))

ggplot(full_df)+
  geom_boxplot(aes(temp, growth_mg_cm2_day, fill = wound))

ggplot(full_df)+
  geom_boxplot(aes(temp, growth_mg_cm2_day))



