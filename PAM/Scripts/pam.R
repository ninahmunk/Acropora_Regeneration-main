#Pulse Amplitude Modulation (PAM) measurement analysis for photosynthetic efficiency (fv/fm)
#load packages
library(tidyverse)
library(readxl)
library(janitor)

#look at working directory and then set it to where datasheets are 
getwd()
setwd("/Users/ninahmunk/Documents/Projects/Regeneration_3/PAM/data")

#load initial PAM datasheet 
initial<- read_xlsx("20230601_initial.xlsx", sheet= "20230601")%>%clean_names()%>%
  select(c(date,genotype, id, f0,fm,fv_fm))%>%
  rename("coral_id" = "id")
head(initial)

#use aggregate function to find the mean of values in other column(s) (f0, fm, fv/fm) based on factor levels in coral_id column 
initial_mean<-aggregate(.~coral_id,data=initial,mean)

#load day 10 PAM datasheets - now separated by groups/date bc of how data was collected
day10_g1<- read_xlsx("day10_PAM.xlsx", sheet= "20230622")%>%clean_names()%>%
  select(c(date, coral_id, f0,fm,fv_fm))
head(day10_g1)

day10_g2<- read_xlsx("day10_PAM.xlsx", sheet= "20230623")%>%clean_names()%>%
  select(c(date, coral_id, f0,fm,fv_fm))
head(day10_g2)

day10_g3<- read_xlsx("day10_PAM.xlsx", sheet= "20230624")%>%clean_names()%>%
  select(c(date, coral_id, f0,fm,fv_fm))%>%
  fill(coral_id) #Fills missing values in selected columns using the next or previous entry. This is useful in the common output format where values are not repeated, and are only recorded when they change
head(day10_g3)

#bind three dataframes with data from day 10 timepoint together - append 'add' rows
day10_bind <- bind_rows(day10_g1, day10_g2, day10_g3)
#get mean values of (f0, fm, fv/fm) for each coral id 
day10_mean<-aggregate(.~coral_id,data=day10_bind,mean)
