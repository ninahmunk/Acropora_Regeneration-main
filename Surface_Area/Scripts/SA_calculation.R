#initial surface area calculations
library(tidyverse)
library(readxl)
library(janitor)
getwd()
setwd("/Users/ninahmunk/Documents/Projects/Regeneration_3/surface_area/raw_data")

#curved surface area (CSA) = 2piRH + piR^2 (one area of circle for top of coral)

data<- read_xlsx("geometric_SA_initial.xlsx", sheet = "initial")%>%clean_names()%>%
  mutate(branch_height_cm = branch_height_mm / 10) %>%
  mutate(avg_diameter_mm= (diameter_base_mm + diameter_tip_mm) / 2)%>%
  mutate(radius_cm = (avg_diameter_mm/2)/10)%>%
  mutate(radius_tip_cm = (diameter_tip_mm/2)/10)%>%
  mutate(CSA_cm2= (2*3.14*(radius_cm*branch_height_cm) + 3.14*(radius_tip_cm)^2))

ggplot(data)+
  geom_point(aes(coral_id,CSA_cm2))

#wax dipping calibration 
calibration<- read_xlsx("20230712_wax_dipping.xlsx", sheet = "calibration")%>%clean_names()%>%
  mutate(wax_weight_g = postwax_weight_g - prewax_weight_g)%>%
  mutate(cal_radius_mm = diameter_mm / 2)%>%
  mutate(cal_radius_cm = cal_radius_mm /10)%>%
  mutate(height_cm = height_mm / 10)%>%
  mutate(CSA_cm2= ((2*3.14*cal_radius_cm*height_cm) + 3.14*(cal_radius_cm)^2))

# calculate the curve coefficients for slope and intercept to apply as the standard
stnd.curve <- lm(CSA_cm2~wax_weight_g, data=calibration)
plot(CSA_cm2~wax_weight_g, data=calibration)
stnd.curve$coefficients
summary(stnd.curve)$r.squared

#bring in the datasheet with coral samples 
smpls<- read_xlsx("20230712_wax_dipping.xlsx", sheet = "data")%>%clean_names()%>%
#subtract postwax weight from prewax weight
  mutate(wax_weight_g = postwax_weight_g - prewax_weight_g)
#Calculate surface area using the standard curve
smpls$CSA_cm2 <- stnd.curve$coefficients[2] * smpls$wax_weight_g + stnd.curve$coefficients[1]

#check the range to make sure your samples fall within the range of the standards
range(smpls$CSA_cm2)
range(calibration$CSA_cm2)

write_csv(smpls, path = "/Users/ninahmunk/Documents/Projects/Regeneration_3/surface_area/output/final_surface_areas.csv")

#need to figure out how to add branches together (aka rows of CSA_cm2 together to get total coral SA)
#save the output
