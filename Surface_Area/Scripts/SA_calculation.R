#initial surface area calculations
library(tidyverse)
library(readxl)
library(janitor)
getwd()
setwd("/Users/ninahmunk/Documents/Projects/Regeneration_3/surface_area/raw_data")

####################### Final Surface Areas (wax dipping) ###################### ##### 
#wax dipping calibration: calculate surface area of dowels 
calibration<- read.csv('Data/20230712_wax_calibration.csv')%>%clean_names()%>%
  mutate(wax_weight_g = postwax_weight_g - prewax_weight_g)%>%
  mutate(cal_radius_mm = diameter_mm / 2)%>%
  mutate(cal_radius_cm = cal_radius_mm /10)%>%
  mutate(height_cm = height_mm / 10)%>%
  mutate(CSA_cm2= ((2*3.14*cal_radius_cm*height_cm) + 3.14*(cal_radius_cm)^2)) #curved surface area (CSA) = 2piRH + piR^2 (one area of circle for top of coral)

# calculate the curve coefficients for slope and intercept to apply as the standard
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
range(calibration$CSA_cm2)

#save the output
write_csv(smpls, path = "/Users/ninahmunk/Documents/Projects/Regeneration_3/surface_area/output/final_surface_areas.csv")

####################### Initial Surface Area (geometric) ####################### ##### 
#need to figure out how to add branches together (aka rows of CSA_cm2 together to get total coral SA)
#curved surface area (CSA) = 2piRH + piR^2 (one area of circle for top of coral)

data<- read.csv("Data/geometric_SA_initial.csv")%>%clean_names()%>%
  mutate(branch_height_cm = branch_height_mm / 10) %>%
  mutate(avg_diameter_mm= (diameter_base_mm + diameter_tip_mm) / 2)%>%
  mutate(radius_cm = (avg_diameter_mm/2)/10)%>%
  mutate(radius_tip_cm = (diameter_tip_mm/2)/10)%>%
  mutate(CSA_cm2= (2*3.14*(radius_cm*branch_height_cm) + 3.14*(radius_tip_cm)^2))

#add surface areas of branches together 
  summarized_data <- data %>%
  group_by(coral_id) %>%
  summarize(SA = sum(CSA_cm2))
  
range(summarized_data$SA)

ggplot(summarized_data)+
  geom_point(aes(coral_id,SA))

write_csv(summarized_data, path = "Output/initial_surface_areas.csv")

