#bouyant weight to skeletal mass calculations for acropora pulchra frags

install.packages("dplyr")
library("tidyverse")
library("readxl")
library("janitor")
library("ggplot2")
library('plyr')
library('readxl')
  library("tidyr")
#install.packages("tidyr")

#set working directory
getwd()
setwd("/Users/ninahmunk/Desktop/Projects/Acropora_Regeneration-main")
################### Initial Skeletal Mass ###################################### ##### 
#load data 
weight_initial<- read_xlsx("Growth/Data/bouyantweight_initial.xlsx", sheet= "raw_data")%>%clean_names()%>% 
  
#add column to calculate density of the glass stopper, 0.9965 is the density of freshwater
  mutate(density_stopper= (air_weight_g * 0.9965)/(air_weight_g - fresh_weight_g))%>%
  
#add column to calculate density of seawater
  mutate(density_sw= (air_weight_g - salt_weight_g)/ (air_weight_g / density_stopper))%>%
  
#add column to calculate volume of the coral (to use later in respo code)
  mutate(vol_coral_cm3= bouyantweight_g / (density_aragonite - density_sw))%>%
  
#add column to calulate dry mass of the coral 
  mutate(dry_mass_coral_g= vol_coral_cm3 * density_aragonite)%>%
  
#select columns I want to use in data visualization
  select(c(coral_id, dry_mass_coral_g))
  
#rename column of coral mass to 'initial'
names(weight_initial)[names(weight_initial) == "dry_mass_coral_g"] <- "initial"

weight_initial<- weight_initial%>%rename(initial = dry_mass_coral_g)
#option to skip directly to this equation below after calculating density of seawater if you donʻt need to extract coral volume
  #mutate(dry_mass_coral_2= bouyantweight_g / (1 - (density_sw/density_aragonite)))

#standardize by SA specific to this timepoint 
initial_SA<-read.csv("Surface_Area/Output/initial_surface_areas.csv")
initial_weight_SA<- left_join(weight_initial, initial_SA, by= 'coral_id')
initial_weight_norm<-initial_weight_SA%>%mutate(initial_g_cm2 = initial / SA)%>%select(coral_id, initial_g_cm2)

write_csv(initial_weight_norm, path = "Growth/Output/normalized_weight/initial.csv")



#download new dataframe as a csv file to wherever you want on computer
write_csv(weight_initial, path = "/Users/ninahmunk/Documents/Projects/Regeneration/Growth/Output/dry_mass_initial.csv")
################### Initial Chamber Volumes #################################### ##### 
#load data 
chamber_vols_initial<- read_xlsx("Growth/Data/bouyantweight_initial.xlsx", sheet= "raw_data")%>%clean_names()%>% 
  
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

########## Weight + Chamber Volumes for 24hr, Day 10, and Final time points ######### #####

# 24 hour weight
weight_24hr<- read_xlsx("Growth/Data/bouyantweight_24hr.xlsx", sheet= "raw_data")%>%clean_names()%>% 
  mutate(density_stopper= (air_weight_g * 0.9965)/(air_weight_g - fresh_weight_g))%>%
  mutate(density_sw= (air_weight_g - salt_weight_g)/ (air_weight_g / density_stopper))%>%
  mutate(vol_coral_cm3= bouyantweight_g / (density_aragonite - density_sw))%>%
  mutate(dry_mass_coral_g= vol_coral_cm3 * density_aragonite)%>%
  select(coral_id, hr24 = dry_mass_coral_g)

# 24 hour weight standardized by post wound geometric SA
SA_post_wound<-read.csv("Surface_Area/Output/post_wound_surface_areas.csv")
post24_weight_SA<- left_join(weight_24hr, SA_post_wound, by= 'coral_id')
post24_weight_norm<-post24_weight_SA%>%mutate(hr24_g_cm2 = hr24 / SA_post_wound)%>%select(coral_id, hr24_g_cm2)

# 24 hours weight standardized by final SA
SA_final<-read.csv("Surface_Area/Output/final_surface_areas.csv")
hr24_weight_SA<- left_join(weight_24hr, SA_final, by= 'coral_id')
hr24_weight_norm<-hr24_weight_SA%>%mutate(hr24_g_cm2 = hr24 / CSA_cm2)%>%select(coral_id, hr24_g_cm2)

write_csv(hr24_weight_norm, path = "Growth/Output/normalized_weight/24hr.csv")

#write_csv(weight_24hr, path = "/Users/ninahmunk/Documents/Projects/Regeneration/Growth/Output/dry_mass_24.csv")

# 24 hour respo chamber volumes
chamber_vols_24hr<- read_xlsx("Growth/Data/bouyantweight_24hr.xlsx", sheet= "raw_data")%>%clean_names()%>% 
  mutate(density_stopper= (air_weight_g * 0.9965)/(air_weight_g - fresh_weight_g))%>%
  mutate(density_sw= (air_weight_g - salt_weight_g)/ (air_weight_g / density_stopper))%>%
  mutate(vol_coral_cm3= bouyantweight_g / (density_aragonite - density_sw))%>%
  mutate(chamber_vol= 650 - vol_coral_cm3)%>%
  select(c(date, coral_id, chamber_vol))
#write_csv(chamber_vols_24hr, path = "/Users/ninahmunk/Desktop/Projects/Acropora_Regeneration-main/Respiration/Data/24hours/chamber_vol_24hrs.csv")

# day 10 weight 
weight_day10<- read_xlsx("Growth/Data/bouyantweight_day10.xlsx", sheet= "raw_data")%>%clean_names()%>% 
  mutate(density_stopper= (air_weight_g * 0.9965)/(air_weight_g - fresh_weight_g))%>%
  mutate(density_sw= (air_weight_g - salt_weight_g)/ (air_weight_g / density_stopper))%>%
  mutate(vol_coral_cm3= bouyantweight_g / (density_aragonite - density_sw))%>%
  mutate(dry_mass_coral_g= vol_coral_cm3 * density_aragonite)%>%
  select(c(coral_id, dry_mass_coral_g))%>%
  rename("day10" = "dry_mass_coral_g")

# day 10 weight standardized by final SA  
SA_final<-read.csv("Surface_Area/Output/final_surface_areas.csv")
day10_weight_SA<- left_join(weight_day10, SA_final, by= 'coral_id')
day10_weight_norm<-day10_weight_SA%>%mutate(day10_g_cm2 = day10 / CSA_cm2)%>%select(coral_id, day10_g_cm2)

write_csv(day10_weight_norm, path = "Growth/Output/normalized_weight/day10.csv")

#write_csv(weight_day10, path = "/Users/ninahmunk/Documents/Projects/Regeneration/Growth/Output/dry_mass_day10.csv")

# day 10 respo chamber volumes
chamber_vols_day10<- read_xlsx("Growth/Data/bouyantweight_day10.xlsx", sheet= "raw_data")%>%clean_names()%>% 
  mutate(density_stopper= (air_weight_g * 0.9965)/(air_weight_g - fresh_weight_g))%>%
  mutate(density_sw= (air_weight_g - salt_weight_g)/ (air_weight_g / density_stopper))%>%
  mutate(vol_coral_cm3= bouyantweight_g / (density_aragonite - density_sw))%>%
  mutate(chamber_vol= 650 - vol_coral_cm3)%>%
  select(c(date, coral_id, chamber_vol))

write.csv(chamber_vols_day10, "/Users/ninahmunk/Desktop/Projects/Acropora_Regeneration-main/Respiration/Data/day10/chamber_vol_day10.csv")

# final weight 
weight_final<- read_xlsx("Growth/Data/bouyantweight_final.xlsx", sheet= "raw_data")%>%clean_names()%>% 
  mutate(density_stopper= (air_weight_g * 0.9965)/(air_weight_g - fresh_weight_g))%>%
  mutate(density_sw= (air_weight_g - salt_weight_g)/ (air_weight_g / density_stopper))%>%
  mutate(vol_coral_cm3= bouyantweight_g / (density_aragonite - density_sw))%>%
  mutate(dry_mass_coral_g= vol_coral_cm3 * density_aragonite)%>%
  select(c(coral_id, dry_mass_coral_g))%>%
  rename("final" = "dry_mass_coral_g")

# final weight standardized by final SA
final_weight_SA<- left_join(SA_final, weight_final, by= 'coral_id')
final_weight_norm<-final_weight_SA%>%mutate(final_g_cm2 = final / CSA_cm2)%>%select(coral_id, final_g_cm2)
#write_csv(weight_final, path = "/Users/ninahmunk/Documents/Projects/Regeneration/Growth/Output/dry_mass_final.csv")

write_csv(final_weight_norm, path = "Growth/Output/normalized_weight/final.csv")

#final respo chamber volumes
chamber_vols_final<- read_xlsx("Growth/Data/bouyantweight_final.xlsx", sheet= "raw_data")%>%clean_names()%>% 
  mutate(density_stopper= (air_weight_g * 0.9965)/(air_weight_g - fresh_weight_g))%>%
  mutate(density_sw= (air_weight_g - salt_weight_g)/ (air_weight_g / density_stopper))%>%
  mutate(vol_coral_cm3= bouyantweight_g / (density_aragonite - density_sw))%>%
  mutate(chamber_vol= 650 - vol_coral_cm3)%>%
  select(c(date, coral_id, chamber_vol))

write.csv(chamber_vols_final, "Respiration/Data/final/chamber_vol_final.csv")

########################## GROWTH PLOTS ################################# ##### 
#load master datasheet with treatment information
master<- read_xlsx("Growth/Data/regen_3_coral_mastersheet.xlsx", sheet ='mastersheet_extended')

#load normalized weights 
initial<- read.csv("Growth/Output/normalized_weight/initial.csv")
hr24<- read.csv("Growth/Output/normalized_weight/24hr.csv")
day10<- read.csv("Growth/Output/normalized_weight/day10.csv")
final<- read.csv("Growth/Output/normalized_weight/final.csv")

#checking relationship between initial size and final size 
data<- left_join(initial, final, by = "coral_id")
ggplot(data, aes(x = initial_g_cm2, y = final_g_cm2)) +
  geom_point()

#calculate the curve coefficients for slope and intercept to apply as the standard
stnd.curve <- lm(initial_g_cm2~final_g_cm2, data=data)
plot(initial_g_cm2~final_g_cm2, data=data)
stnd.curve$coefficients
summary(stnd.curve)$r.squared

list_df = list(initial, hr24, day10, final, master)
all_weights<-list_df%>%reduce(inner_join, by='coral_id')

# calculating growth (final - 24 hour) 
growth<-all_weights%>%mutate(growth_g = final_g_cm2 - hr24_g_cm2)%>%
  mutate(growth_mg= ((growth_g)*1000))%>%
  mutate(mg_cm2_day = (growth_mg/19))%>%
  mutate(wound = as_factor(wound))

#creating a column to group wounded vs un wounded corals regardless of wound type
growth<-growth%>%
  mutate(wound_status = case_when(
    wound == 0 ~ "no wound",
    wound %in% c(1, 2) ~ "wounded"
  ))

range(growth$mg_cm2_day)

quartz()
ggplot(growth)+ 
  geom_boxplot(aes(genotype, mg_cm2_day, group = genotype))+ 
  geom_point(aes(genotype, mg_cm2_day, color= wound))

ggplot(growth)+
  geom_boxplot(aes(wound_status, mg_cm2_day, fill = temp))+
  facet_wrap(~ genotype, scales = "free")

ggplot(growth)+
  geom_boxplot(aes(wound, mg_cm2_day, fill = temp))+
  facet_wrap(~ genotype, scales = "free")

ggplot(growth)+
  geom_boxplot(aes(wound_status, mg_cm2_day, fill=temp))+
  facet_wrap(~ num_tips, scales = "free")

ggplot(growth)+
  geom_boxplot(aes(wound, mg_cm2_day, fill=temp))

#Plotting weight through time
# Combine the weight columns into a long format 
long_growth <- tidyr::gather(growth, key = "time_point", value = "weight", hr24_g_cm2, day10_g_cm2, final_g_cm2)
long_growth$time_point <- factor(long_growth$time_point, levels = c("hr24_g_cm2", "day10_g_cm2", "final_g_cm2"))
long_growth <- mutate(long_growth,
                     Time_Post_Wounding = case_when(
                        time_point == "hr24_g_cm2" ~ "24 hours",
                        time_point == "day10_g_cm2" ~ "Day 10",
                        time_point == "final_g_cm2" ~ "Day 19"
                      ))

view(long_growth)

# Create a line plot
quartz()
ggplot(long_growth, aes(x = time_point, y = weight, color = genotype, shape = wound)) +
  geom_point() +
  geom_line(aes(group = coral_id), linetype = "dashed") +
  labs(title = "Coral Weight Over Time",
       x = "Time Point",
       y = "Weight (g/cm2)",
       color = "Genotype",
       shape = "Wound Type") +
  theme_minimal() +
  theme(legend.position = "top")  # Adjust legend position

ggplot(long_growth)+
  geom_boxplot(aes(wound, weight, fill=Time_Post_Wounding))+
  labs(x = "Wound Type",
       y = "Weight (g/cm2)") +
 # theme_minimal() +
  facet_wrap(~ temp, scales = "free")


########################## DRY MASS X SA ################################ #####

weight<- read.csv("Growth/Output/dry_mass_final.csv")%>%select(coral_id, dry_mass_coral_g)
view(weight)
SA<- read.csv("Surface_Area/Output/final_surface_areas.csv")%>%select(coral_id, CSA_cm2)%>%rename(SA = CSA_cm2)
view(SA)
data<- left_join(weight, SA, by = "coral_id")
view(data)
plot(SA, weight, data = data)
plot(x=data$dry_mass_coral_g, y=data$SA)
