#initial surface area calculations
library(tidyverse)
library(readxl)
library(janitor)
getwd()
setwd("/Users/ninahmunk/Desktop/Projects/Acropora_Regeneration-main/Surface_Area")

################### FINAL SA (WAX DIPPING) ##################################### ##### 
#wax dipping calibration: calculate surface area of dowels 
calibration<- read.csv('Data/20230712_wax_calibration.csv')%>%clean_names()%>%
  mutate(wax_weight_g = postwax_weight_g - prewax_weight_g)%>%
  mutate(cal_radius_mm = diameter_mm / 2)%>%
  mutate(cal_radius_cm = cal_radius_mm /10)%>%
  mutate(height_cm = height_mm / 10)%>%
  mutate(CSA_cm2= ((2*3.14*cal_radius_cm*height_cm) + 3.14*(cal_radius_cm)^2)) #curved surface area (CSA) = 2piRH + piR^2 (one area of circle for top of coral)

#calculate the curve coefficients for slope and intercept to apply as the standard
stnd.curve <- lm(CSA_cm2~wax_weight_g, data=calibration)
plot(CSA_cm2~wax_weight_g, data=calibration)
stnd.curve$coefficients
summary(stnd.curve)$r.squared

#bring in the datasheet with coral samples 
smpls<- read.csv("Data/20230712_wax_weights.csv")%>%clean_names()%>%
#subtract postwax weight from prewax weight
  mutate(wax_weight_g = postwax_weight_g - prewax_weight_g)
#Calculate surface area using the standard curve
smpls$CSA_cm2 <- stnd.curve$coefficients[2] * smpls$wax_weight_g + stnd.curve$coefficients[1]

#check the range to make sure your samples fall within the range of the standards
range(smpls$CSA_cm2)
range(calibration$CSA_cm2) # 6.944456 - 83.406250

#save the output
write_csv(smpls, path = "/Users/ninahmunk/Documents/Projects/Regeneration_3/surface_area/output/final_surface_areas.csv")

################### INITIAL SA (GEOMETRIC) ##################################### ##### 
#need to figure out how to add branches together (aka rows of CSA_cm2 together to get total coral SA)
#curved surface area (CSA) = 2piRH + piR^2 (one area of circle for top of coral)

data<- read.csv("Data/geometric_SA_initial.csv")%>%clean_names()%>%
  mutate(branch_height_cm = branch_height_mm / 10) %>%
  mutate(avg_diameter_mm= (diameter_base_mm + diameter_tip_mm) / 2)%>%
  mutate(radius_cm = (avg_diameter_mm/2)/10)%>%
  mutate(radius_tip_cm = (diameter_tip_mm/2)/10)%>%
  mutate(CSA_cm2= (2*3.14*(radius_cm*branch_height_cm) + 3.14*(radius_tip_cm)^2))

#add surface areas of branches together 
  summarized_initial_data <- data %>%
  group_by(coral_id) %>%
  summarize(SA = sum(CSA_cm2))

#looking at the range of the SA
range(summarized_initial_data$SA) #7.445882 - 38.165366

#visualizing data
ggplot(summarized_initial_data)+
  geom_point(aes(coral_id,SA))

write_csv(summarized_data, path = "Output/initial_surface_areas.csv")

#calculating mean tip diameter to use for removing SA of wound type 2 from initial surface areas
wound_2_avg_tip_diameter<- mean(data$diameter_tip_mm)

################### ADJUSTING FOR SA REMOVAL BC OF WOUNDING TREATMENTS ######### ##### 
#importing data sheet which has height, tip, base measurement for wound type 1
wound_amount<-read.csv("Surface_Area/Data/wound_type_1_measurements.csv")%>%clean_names()
#filling in height for wound type 2
wound_amount$branch_h_mm <- ifelse(wound_amount$wound == 2 & wound_amount$coral_id %in% unique(wound_amount$coral_id[wound_amount$wound == 2]), 10, wound_amount$branch_h_mm)
#filling in tip diameter for wound type 2 based on average diameter of wound tips from initial geometric measurements (see above)
wound_amount$tip_d_mm <- ifelse(wound_amount$wound == 2 & wound_amount$coral_id %in% unique(wound_amount$coral_id[wound_amount$wound == 2]), wound_2_avg_tip_diameter, wound_amount$tip_d_mm)
# make all other NAs in the data frame zero 
wound_amount[is.na(wound_amount)] <- 0

data2<- wound_amount%>%
  mutate(branch_height_cm = branch_h_mm / 10) %>%
  mutate(avg_diameter_mm= (base_d_mm + tip_d_mm) / 2)%>%
  mutate(radius_cm = (avg_diameter_mm/2)/10)%>%
  mutate(radius_tip_cm = (tip_d_mm/2)/10)%>%
  mutate(CSA_cm2= (2*3.14*(radius_cm*branch_height_cm) + 3.14*(radius_tip_cm)^2))

SA_to_subtract<-data2%>%select(coral_id, CSA_cm2)

initialSA_woundSA<- left_join(summarized_initial_data, SA_to_subtract, by= 'coral_id')

SA_after_wounding<-initialSA_woundSA%>%mutate(SA_post_wound = SA - CSA_cm2)

Post_Wound_Surface_Areas<- SA_after_wounding%>%select(coral_id, SA_post_wound)
range(Post_Wound_Surface_Areas$SA_post_wound)
write_csv(Post_Wound_Surface_Areas, path = "Surface_Area/Output/post_wound_surface_areas.csv")

################### WAX SA VS GEOMETRIC SA ###################################### #####
# I want to know if I can translate geometric SA into wax derived SA  
# I can plot geometric SA and wax SA for the control corals
# under the assumption that these control corals grew at the same rate (i.e., grew the same amount after 19 days) 
# here I build a standard curve to predict wax derived SA from geometric SA

#import wax derived SA data
wax<- read.csv("output/final_surface_areas.csv")%>%select(coral_id, wax_weight_g, CSA_cm2)%>%rename(SA_wax = CSA_cm2)
head(wax)
#import geometrically derived SA data
geo<- read.csv("output/initial_surface_areas.csv")%>%rename(SA_geo=SA)
head(geo)
#combine data sets by coral id
data1<- left_join(wax, geo, by = "coral_id")
head(data1)

#read in sample info
setwd("/Users/ninahmunk/Desktop/Projects/Acropora_Regeneration-main/Respiration/Data")
sample.info<- read.csv("samp_info.csv")

#combine SA data with sample info by coral id 
data2<- left_join(data1, sample.info, by = "coral_id")%>%mutate(wound = as.factor(wound))

#filter data to include only control corals, also looking at the 
controls<- data2%>%filter(wound == 0)%>%filter(temp == "A")

#plot SA from both methods against each other 
plot(controls$SA_wax, controls$SA_geo) #col = controls$coral_id

#calculate the curve coefficients for slope and intercept to apply as the standard
stnd.curve <- lm(SA_geo~SA_wax, data=controls)
plot(SA_geo~SA_wax, data=controls)
stnd.curve$coefficients
summary(stnd.curve)$r.squared

#bring in the SA data from corals in treatments
treatment_corals<- data2%>%filter(!wound == 0)
#Calculate wax derived SA using the standard curve
treatment_corals$derived_wax_SA <- stnd.curve$coefficients[2] * treatment_corals$SA_geo + stnd.curve$coefficients[1]

range(controls$SA_geo)
range(treatment_corals$SA_geo)


controls_long <- controls %>%
  gather(key = "measurement", value = "value", SA_wax, SA_geo)



