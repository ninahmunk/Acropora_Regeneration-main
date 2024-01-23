# photosynthesis and respiration code for A. pulchra experiments June-July 2023
remotes::install_github('colin-olito/LoLinR')
library("ggplot2")
library("segmented")
library("plotrix")
library("gridExtra")
library("LoLinR")
library("lubridate")
library("chron")
library('plyr')
library('dplyr')
library('tidyverse')
library('stringr')
library('Rmisc')
library('janitor')
library('readxl')
library("tidyr")

#install.packages("tidyverse")
#install.packages("tidyr")


#Set working directory and the path of all respo data files
setwd("/Users/ninahmunk/Desktop/Projects/Acropora_Regeneration-main")
getwd()
path.p<-"/Users/ninahmunk/Desktop/Projects/Acropora_Regeneration-main/Respiration/Data/final/runs"

#make a list of the respo file names inside intial timepoint folder, n=120
file.names <- list.files(path = path.p, pattern = "csv$")
file.names.full <- tools::file_path_sans_ext(file.names) #removes .csv 
print(file.names.full)

#create respiration data frame, 4 columns and # rows corresponding to respo files (n=120)
Respiration<- data.frame(matrix(NA, nrow=length(file.names.full), ncol=4))
colnames(Respiration) <- c("coral_id","Intercept", "umol.L.sec","Temp.C")
Respiration$coral_id <- file.names.full # Insert file names into "coral_id" column 
View(Respiration)


#create photosynthesis data frame
Photosynthesis<- data.frame(matrix(NA, nrow=length(file.names.full), ncol=4))
colnames(Photosynthesis) <- c("coral_id","Intercept", "umol.L.sec","Temp.C")
Photosynthesis$coral_id <- file.names.full
View(Photosynthesis)

############### INITIAL SAMPLE INFO ############################################ ##### 

#load in sample information files
Treatments<- read.csv("Respiration/Data/initial/samp_info.csv") #genotype, wound type, temp
Volume<- read.csv("Respiration/Data/initial/chamber_vol.csv") #vol of water in each chamber 
SA<- read.csv("Surface_Area/Output/initial_surface_areas.csv")%>% #initial SA of each coral
  mutate(coral_id = as.character(coral_id)) 
list_df = list(Treatments, Volume, SA) 
Sample.Info<-list_df%>%purrr::reduce(left_join, by= 'coral_id') #combine all info by coral id 
# Add '_g1', '_g2', '_g3' based on values in the 'date' column
Sample.Info$file.names.full <- ifelse(Sample.Info$date == '20230605', paste(Sample.Info$date, 'g1', sep = '_'),
                                 ifelse(Sample.Info$date == '20230606', paste(Sample.Info$date, 'g2', sep = '_'),
                                        ifelse(Sample.Info$date == '20230607', paste(Sample.Info$date, 'g3', sep = '_'),
                                               as.character(Sample.Info$coral_id))))  # Convert coral_id to character for NA in 'date'
rows_to_modify <- 109:120
Sample.Info$coral_id[rows_to_modify] <- substr(Sample.Info$coral_id[rows_to_modify], 10, nchar(Sample.Info$coral_id[rows_to_modify])) # This removes the first 9 characters from rows 109-118 in the new_column
Sample.Info$file.names.full <- paste(Sample.Info$file.names.full, Sample.Info$coral_id, sep = "_") #creating new column to combine new column and coral_id separated by _
Sample.Info$file.names.full <- paste(Sample.Info$file.names.full, 'O2', sep = '_') # Add '_O2' to the end of each value in the new column
  

# View the updated data frame
View(Sample.Info)


############### FORLOOP FOR RESPIRATION RATES ################################## #####
# for every file in list calculate O2 uptake or release rate and add the data to the Respiration dataframe
for(i in 1:length(file.names)) { # for every file in list calculate O2 uptake or release rate and add the data to the Photo.R dataframe
  
  #find the lines in sample info that have the same file name that is being brought in
  
  #FRow<-which(Sample.Info$file.names.full==strsplit(file.names[i],'.csv'))
  
  # read in the O2 data one by one
  Photo.Data1 <-read.csv(file.path(path.p,file.names[i]), skip = 1, header=T) # skips the first line
  Photo.Data1  <- Photo.Data1[,c("delta_t","Value","Temp")] #subset columns of interest
  #Photo.Data1$Time <- as.POSIXct(Photo.Data1$Time,format="%H:%M:%S", tz = "") #convert time from character to time
  Photo.Data1 <- na.omit(Photo.Data1) #omit NA from data frame
  
  # clean up some of the data
  n<-dim(Photo.Data1)[1] # length of full data
  Photo.Data1 <- Photo.Data1 %>% mutate(delta_t=as.numeric(delta_t))%>%filter(delta_t > 25) #start at beginning of dark phase data point (25 minutes in) 
  n<-dim(Photo.Data1)[1] #list length of trimmed data
  Photo.Data1$sec <- seq(1, by = 3, length.out = n) #set seconds by three from start to finish of run in a new column
  
  
  #Save plot prior to and after data thinning to make sure thinning is not too extreme
  rename <- sub(".csv","", file.names[i]) # remove all the extra stuff in the file name
  
  pdf(paste0("Respiration/Output/Respiration/Final/",rename,"thinning.pdf")) # open the graphics device
  
  par(omi=rep(0.3, 4)) #set size of the outer margins in inches
  par(mfrow=c(1,2)) #set number of rows and columns in multi plot graphic
  plot(Value ~ sec, data=Photo.Data1 , xlab='Time (seconds)', ylab=expression(paste(' O'[2],' (',mu,'mol/L)')), type='n', axes=FALSE) #plot (empty plot to fill) data as a function of time
  usr  <-  par('usr') # extract the size of the figure margins
  rect(usr[1], usr[3], usr[2], usr[4], col='grey90', border=NA) # put a grey background on the plot
  whiteGrid() # make a grid
  box() # add a box around the plot
  points(Photo.Data1 $Value ~ Photo.Data1 $sec, pch=16, col=transparentColor('dodgerblue2', 0.6), cex=1.1)
  axis(1) # add the x axis
  axis(2, las=1) # add the y-axis
  
  # Thin the data to make the code run faster
  Photo.Data.orig <-Photo.Data1 #save original unthinned data
  Photo.Data1 <-  thinData(Photo.Data1 ,by=5)$newData1 #thin data by every 20 points for all the O2 values
  Photo.Data1$sec <- as.numeric(rownames(Photo.Data1 )) #maintain numeric values for time
  Photo.Data1$Temp<-NA # add a new column to fill with the thinned data
  Photo.Data1$Temp <-  thinData(Photo.Data.orig,xy = c(1,3),by=5)$newData1[,2] #thin data by every 20 points for the temp values
  
  # plot the thinned data
  plot(Value ~ sec, data=Photo.Data1 , xlab='Time (seconds)', ylab=expression(paste(' O'[2],' (',mu,'mol/L)')), type='n', axes=FALSE) #plot thinned data
  usr  <-  par('usr')
  rect(usr[1], usr[3], usr[2], usr[4], col='grey90', border=NA)
  whiteGrid()
  box()
  points(Photo.Data1 $Value ~ Photo.Data1 $sec, pch=16, col=transparentColor('dodgerblue2', 0.6), cex=1.1)
  axis(1)
  axis(2, las=1)
  ##Olito et al. 2017: It is running a bootstrapping technique and calculating the rate based on density
  #option to add multiple outputs method= c("z", "eq", "pc")
  Regs  <-  rankLocReg(xall=Photo.Data.orig$sec, yall=Photo.Data.orig$Value, alpha=0.5, method="pc", verbose=TRUE)  
  
  # add the regression data
  plot(Regs)
  dev.off()
  
  
  # fill in all the O2 consumption and rate data
  Respiration[i,2:3] <- Regs$allRegs[1,c(4,5)] #inserts slope and intercept in the dataframe
  Respiration[i,1] <- rename #stores the file name in the Date column
  Respiration[i,4] <- mean(Photo.Data1$Temp, na.rm=T)  #stores the Temperature in the Temp.C column
#Photo.R[i,5] <- PR[j] #stores whether it is photosynthesis or respiration
  
  
  # rewrite the file everytime... I know this is slow, but it will save the data that is already run
}
write.csv(Respiration, 'Respiration/Output/Respiration/Final/Respiration.csv')  

############### INITIAL RESPIRATION DATA ANALYSIS ############################## #####
setwd("/Users/ninahmunk/Desktop/Projects/Acropora_Regeneration-main/Respiration")

#renaming and reordering columns in sample info and then combining it with respiration df based on coral id
names(Sample.Info)[2] <- "coral_num"
names(Sample.Info)[8] <- "coral_id"
Sample.Info <- Sample.Info[, c(8,7,6,5,4,3,1,2)]

#read in Photo.R file so dont need to run entire for loop again
Respiration <- read.csv('Output/Respiration/Initial/Respiration.csv')%>% select(-X)
Respiration$coral_id[9]='20230605_g1_25_O2'
#list_df = list(Respiration, Sample.Info) 
#Respiration<-list_df%>%purrr::reduce(left_join, by= 'coral_id')
Respiration<-left_join(Respiration, Sample.Info, by='coral_id')%>%
  rename(surf.area.cm2=SA)
#Convert sample volume to L
Respiration$chamber_vol <- Respiration$chamber_vol/1000 #calculate volume
#Account for chamber volume to convert from umol L-1 s-1 to umol s-1. This standardizes across water volumes (different because of coral size) and removes per Liter
Respiration$umol.sec <- Respiration$umol.L.sec*Respiration$chamber_vol

# Extract rows with blank data from respiration data frame
blankrows <- c(37, 77, 117, 38, 78, 118, 39, 79, 119, 40, 80, 120)
blank_rates <- Respiration[blankrows, ]%>% 
  rename(blank_id = coral_id)%>%
  select(blank_id, umol.sec)

Respiration<- drop_na(Respiration)%>%mutate(coral_num = as.numeric(coral_num))
View(Respiration)

blanks<-read.csv('Data/initial/blanks.csv')%>%
  select(-'X')%>%
  rename(blank_id = coral_id)
View(blanks)

merged_data <- merge(blanks, blank_rates, by = "blank_id", all.x = TRUE)
merged_data <- merged_data[, c(2,1,3)]%>%rename(blank.umol.sec=umol.sec)
View(merged_data)
Respiration<- left_join(Respiration, merged_data, by= 'coral_num')
Respiration$umol.sec.corr<-Respiration$umol.sec-Respiration$blank.umol.sec
View(Respiration)

#### Normalize to SA (surface area)

#Calculate net R
Respiration$umol.cm2.hr <- (Respiration$umol.sec.corr*3600)/Respiration$surf.area.cm2 #mmol cm-2 hr-1
# Take the absolute (positive) value of the 'umol.cm2.hr' column
Respiration<- Respiration%>%
  mutate(umol.cm2.hr = abs(umol.cm2.hr))%>%
  mutate(genotype = as.factor(genotype))

# log the rates
Respiration$Rate.ln<-log(Respiration$umol.cm2.hr+0.1)

#visualize respo rates by genotype 
Respiration<- read.csv('Output/Respiration/Initial/norm_resp_initial.csv')%>% mutate(genotype = as.factor(genotype))
quartz()
ggplot(Respiration, aes(x=genotype, y=umol.cm2.hr))+
  geom_boxplot()+
  ylab('Respiration (umol.cm2.hr)')

#export normalized rates
write.csv(Respiration, 'Output/Respiration/Initial/norm_resp_initial.csv')

############### FORLOOP FOR PHOTOSYNTHESIS RATES ############################### #####
# for every file in list calculate O2 uptake or release rate and add the data to the Photo.R dataframe
for(i in 1:length(file.names)) { # for every file in list calculate O2 uptake or release rate and add the data to the Photo.R dataframe
  
  #find the lines in sample info that have the same file name that is being brought in
  
  # Exclude the specific file
  #if (file.names[i] == "20230605_g1_blank4_O2.csv") {
   # next  # Skip the rest of the loop and move to the next iteration
 # }
  
  #FRow<-which(Sample.Info$file.names.full==strsplit(file.names[i],'.csv'))
  
  # read in the O2 data one by one
  Photo.Data1 <-read.csv(file.path(path.p,file.names[i]), skip = 1, header=T) # skips the first line
  Photo.Data1  <- Photo.Data1[,c("delta_t","Value","Temp")] #subset columns of interest
  #Photo.Data1$Time <- as.POSIXct(Photo.Data1$Time,format="%H:%M:%S", tz = "") #convert time from character to time
  Photo.Data1 <- na.omit(Photo.Data1) #omit NA from data frame
  
  # clean up some of the data
  n<-dim(Photo.Data1)[1] # length of full data
  Photo.Data1 <- Photo.Data1 %>% mutate(delta_t=as.numeric(delta_t))%>%filter(delta_t > 10 & delta_t < 25) #start at beginning of light phase (10 minutes in) and stop at 25 min (start of dark phase)
  n<-dim(Photo.Data1)[1] #list length of trimmed data
  Photo.Data1$sec <- seq(1, by = 3, length.out = n) #set seconds by three from start to finish of run in a new column
  
  
  #Save plot prior to and after data thinning to make sure thinning is not too extreme
  rename <- sub(".csv","", file.names[i]) # remove all the extra stuff in the file name
  
  pdf(paste0("Respiration/Output/Photosynthesis/Final/",rename,"thinning.pdf")) # open the graphics device
  
  par(omi=rep(0.3, 4)) #set size of the outer margins in inches
  par(mfrow=c(1,2)) #set number of rows and columns in multi plot graphic
  plot(Value ~ sec, data=Photo.Data1 , xlab='Time (seconds)', ylab=expression(paste(' O'[2],' (',mu,'mol/L)')), type='n', axes=FALSE) #plot (empty plot to fill) data as a function of time
  usr  <-  par('usr') # extract the size of the figure margins
  rect(usr[1], usr[3], usr[2], usr[4], col='grey90', border=NA) # put a grey background on the plot
  whiteGrid() # make a grid
  box() # add a box around the plot
  points(Photo.Data1 $Value ~ Photo.Data1 $sec, pch=16, col=transparentColor('dodgerblue2', 0.6), cex=1.1)
  axis(1) # add the x axis
  axis(2, las=1) # add the y-axis
  
  # Thin the data to make the code run faster
  Photo.Data.orig <-Photo.Data1 #save original unthinned data
  Photo.Data1 <-  thinData(Photo.Data1 ,by= 5)$newData1 #thin data by every 5 points for all the O2 values
  Photo.Data1$sec <- as.numeric(rownames(Photo.Data1 )) #maintain numeric values for time
  Photo.Data1$Temp<-NA # add a new column to fill with the thinned data
  Photo.Data1$Temp <-  thinData(Photo.Data.orig,xy = c(1,3),by=5)$newData1[,2] #thin data by every 5 points for the temp values
  
  # plot the thinned data
  plot(Value ~ sec, data=Photo.Data1 , xlab='Time (seconds)', ylab=expression(paste(' O'[2],' (',mu,'mol/L)')), type='n', axes=FALSE) #plot thinned data
  usr  <-  par('usr')
  rect(usr[1], usr[3], usr[2], usr[4], col='grey90', border=NA)
  whiteGrid()
  box()
  points(Photo.Data1 $Value ~ Photo.Data1 $sec, pch=16, col=transparentColor('dodgerblue2', 0.6), cex=1.1)
  axis(1)
  axis(2, las=1)
  ##Olito et al. 2017: It is running a bootstrapping technique and calculating the rate based on density
  #option to add multiple outputs method= c("z", "eq", "pc")
  Regs  <-  rankLocReg(xall=Photo.Data.orig$sec, yall=Photo.Data.orig$Value, alpha=0.5, method="pc", verbose=TRUE)  
  
  # add the regression data
  plot(Regs)
  dev.off()
  
  
  # fill in all the O2 consumption and rate data
  Photosynthesis[i,2:3] <- Regs$allRegs[1,c(4,5)] #inserts slope and intercept in the dataframe
  Photosynthesis[i,1] <- rename #stores the file name in the Date column
  Photosynthesis[i,4] <- mean(Photo.Data1$Temp, na.rm=T)  #stores the Temperature in the Temp.C column
  #Photo.R[i,5] <- PR[j] #stores whether it is photosynthesis or respiration
  
  
  # rewrite the file everytime... I know this is slow, but it will save the data that is already run
}
write.csv(Photosynthesis, 'Respiration/Output/Photosynthesis/Final/Photosynthesis.csv')  

############### INITIAL PHOTOSYNTHESIS DATA ANALYSIS ########################### #####

setwd("/Users/ninahmunk/Desktop/Projects/Acropora_Regeneration-main/Respiration")

#renaming and reordering columns in sample info and then combining it with respiration df based on coral id
names(Sample.Info)[2] <- "coral_num"
names(Sample.Info)[8] <- "coral_id"
Sample.Info <- Sample.Info[, c(8,7,6,5,4,3,1,2)]

#read in Photosynthesis file so dont need to run entire for loop again
Photosynthesis <- read.csv('Output/Photosynthesis/Initial/Photosynthesis.csv')%>%select(-X)
Photosynthesis$coral_id[9]='20230605_g1_25_O2'
#list_df = list(Photosynthesis, Sample.Info) 
#Photosynthesis<-list_df%>%purrr::reduce(left_join, by= 'coral_id')
Photosynthesis<-left_join(Photosynthesis, Sample.Info, by='coral_id')%>%
  #mutate(coral_num = as.numeric(coral_num))%>%
  rename(surf.area.cm2=SA)
#Convert sample volume to L
Photosynthesis$chamber_vol <- Photosynthesis$chamber_vol/1000 #calculate volume
#Account for chamber volume to convert from umol L-1 s-1 to umol s-1. This standardizes across water volumes (different because of coral size) and removes per Liter
Photosynthesis$umol.sec <- Photosynthesis$umol.L.sec*Photosynthesis$chamber_vol

# Extract rows with blank data from respiration data frame
blankrows <- c(117, 118, 119, 120, 77, 78, 79, 80, 40, 39, 38, 37)
blank_rates <- Photosynthesis[blankrows, ]%>% 
  rename(blank_id = coral_id)%>%
  select(blank_id, umol.sec)

Photosynthesis<- drop_na(Photosynthesis)%>%mutate(coral_num = as.numeric(coral_num))
View(Photosynthesis)

merged_data <- merge(blanks, blank_rates, by = "blank_id", all.x = TRUE) #need to input 20230605_g1_blank4 rate here
merged_data <- merged_data[, c(2,1,3)]%>%rename(blank.umol.sec=umol.sec)
View(merged_data)
Photosynthesis<- left_join(Photosynthesis, merged_data, by= 'coral_num')
Photosynthesis$umol.sec.corr<-Photosynthesis$umol.sec-Photosynthesis$blank.umol.sec
View(Photosynthesis)

#Normalize to SA (surface area)

#Calculate net P 
Photosynthesis$umol.cm2.hr <- (Photosynthesis$umol.sec.corr*3600)/Photosynthesis$surf.area.cm2 #mmol cm-2 hr-1
# Take the absolute (positive) value of the 'umol.cm2.hr' column
Photosynthesis<- Photosynthesis%>%
  mutate(umol.cm2.hr = abs(umol.cm2.hr))%>%
  mutate(genotype = as.factor(genotype))

# log the rates
Photosynthesis$Rate.ln<-log(Photosynthesis$umol.cm2.hr+0.1)

#visualize respo rates by genotype 
quartz()
ggplot(Photosynthesis, aes(x=genotype, y=umol.cm2.hr))+
  geom_boxplot()+
  ylab('Photosynthesis (umol.cm2.hr)')

#export normalized rates
write.csv(Photosynthesis, 'Output/Photosynthesis/Initial/norm_photo_initial.csv')


############### INITIAL GROSS PHOTOSYNTHESIS DATA ANALYSIS ##################### #####

#calculating Gross Photosynthesis: Pgross = Pnet + R (when using absolute value of R)

Resp.1<- read.csv('Respiration/Output/Respiration/Initial/norm_resp_initial.csv')%>%
  select(coral_num, umol.cm2.hr)%>%
  add_column(Rate = "Respiration")
view(Resp.1)

Photo.1<- read.csv('Respiration/Output/Photosynthesis/Initial/norm_photo_initial.csv')%>%
  select(coral_num, umol.cm2.hr)%>%
  add_column(Rate = "Net Photosythesis")
view(Photo.1)

P.gross<- left_join(Resp.1, Photo.1, by= 'coral_num')%>%
  select(coral_num, umol.cm2.hr.x, umol.cm2.hr.y)%>%
  mutate(umol.cm2.hr = umol.cm2.hr.x + umol.cm2.hr.y)%>%
  select(coral_num, umol.cm2.hr)%>%
  add_column(Rate = "Gross Photosynthesis")

rates<- rbind(Resp.1, Photo.1, P.gross)
view(rates)
rates$coral_num <- as.character(rates$coral_num)

Sample.Info<-Sample.Info%>%rename(coral_num = coral_id)

rates_full<- left_join(rates, Sample.Info, by= 'coral_num')
rates_full$wound <- as.character(rates_full$wound)
rates_full<-rates_full%>%
  mutate(wound_status = case_when(
    wound == 0 ~ "no wound",
    wound %in% c(1, 2) ~ "wounded"
  ))

write.csv(rates_full, 'Respiration/Output/All_Rates/rates_initial.csv')

# only "Gross Photosynthesis" and "Respiration"
filtered_data <- rates_full %>%
  filter(Rate %in% c("Gross Photosynthesis", "Respiration"))

# Create the plot with the filtered data
quartz()

ggplot(filtered_data, aes(x = Rate, y = umol.cm2.hr, color=Rate)) +
  geom_boxplot() +
  ylab('O2 umol.cm2.hr') +
  xlab('Rate')

ggplot(filtered_data, aes(x = Rate, y = umol.cm2.hr, color=Rate)) +
  geom_boxplot() +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Rate') +
  facet_wrap(~ genotype, scales = "free")

ggplot(filtered_data, aes(x = wound, y = umol.cm2.hr, color=Rate)) +
  geom_boxplot() +
  ylab('Rate (umol.cm2.hr)') +
  xlab('')+
  facet_wrap(~ temp, scales = "free")

quartz()
ggplot(filtered_data, aes(x=wound_status, y= umol.cm2.hr, fill = temp))+
  geom_boxplot()+
  facet_wrap(~ Rate, scales = "free")

ggplot(filtered_data, aes(x = wound, y = umol.cm2.hr, color=temp)) +
  geom_boxplot() +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Wound Type')+
  facet_wrap(~ Rate, scales = "free")


############### 24 HOUR SAMPLE INFO ############################################ #####
#load in sample information files
Treatments<- read.csv("Respiration/Data/samp_info.csv")%>% #genotype, wound type, temp
  mutate(coral_id = as.character(coral_id)) 
Treatments$temp[Treatments$coral_id == 17] <- "H"
Treatments$temp[Treatments$coral_id == 5] <- "A"
Volume<- read.csv("Respiration/Data/24hours/chamber_vol_24hrs.csv")%>% #vol of water in each chamber 
  mutate(coral_id = as.character(coral_id)) 
SA<- read.csv("Surface_Area/Output/final_surface_areas.csv")%>% #initial SA of each coral
  mutate(coral_id = as.character(coral_id))%>%
  select(coral_id, CSA_cm2)%>%
  rename(SA = CSA_cm2)
list_df = list(Treatments, Volume, SA) 
Sample.Info<-list_df%>%purrr::reduce(left_join, by= 'coral_id')%>% #combine data frames by coral_id
  mutate(file.names.full = paste0(date, "_", temp, "_", coral_id)) #create file.names.full column that matches data file names
Sample.Info$file.names.full <- paste0(substring(Sample.Info$file.names.full, 1, 8), "_24", substring(Sample.Info$file.names.full, 9)) #add _24 after the 8th character 
blank_files<-read.csv("Respiration/Data/24hours/blanks_only.csv")
blank_files<-blank_files%>%mutate(blank_files, file.names.full = coral_id)
Sample.Info <- rbind(Sample.Info, blank_files)
Sample.Info$file.names.full <- paste(Sample.Info$file.names.full, 'O2', sep = '_') # Add '_O2' to the end of each value in the new column
# View the updated data frame
View(Sample.Info)
############### 24 hour RESPIRATION ############################################ #####
for(i in 1:length(file.names)) { # for every file in list calculate O2 uptake or release rate and add the data to the Respiration dataframe
  
  #find the lines in sample info that have the same file name that is being brought in
  
  #FRow<-which(Sample.Info$file.names.full==strsplit(file.names[i],'.csv'))
  
  # read in the O2 data one by one
  Photo.Data1 <-read.csv(file.path(path.p,file.names[i]), skip = 1, header=T) # skips the first line
  Photo.Data1  <- Photo.Data1[,c("delta_t","Value","Temp")] #subset columns of interest
  #Photo.Data1$Time <- as.POSIXct(Photo.Data1$Time,format="%H:%M:%S", tz = "") #convert time from character to time
  Photo.Data1 <- na.omit(Photo.Data1) #omit NA from data frame
  
  # clean up some of the data
  n<-dim(Photo.Data1)[1] # length of full data
  Photo.Data1 <- Photo.Data1 %>% mutate(delta_t=as.numeric(delta_t))%>%filter(delta_t > 25) #Respiration: start at beginning of dark phase data point (25 mins in) Photosynthesis: start after light acclimation (10 mins in) stop at start of dark phase (25 mins in) 
  n<-dim(Photo.Data1)[1] #list length of trimmed data
  Photo.Data1$sec <- seq(1, by = 3, length.out = n) #set seconds by three from start to finish of run in a new column
  
  
  #Save plot prior to and after data thinning to make sure thinning is not too extreme
  rename <- sub(".csv","", file.names[i]) # remove all the extra stuff in the file name
  
  pdf(paste0("Respiration/Output/Respiration/day10/",rename,"thinning.pdf")) # open the graphics device, edit this to where you want figures to save based on phase//timepoint
  
  par(omi=rep(0.3, 4)) #set size of the outer margins in inches
  par(mfrow=c(1,2)) #set number of rows and columns in multi plot graphic
  plot(Value ~ sec, data=Photo.Data1 , xlab='Time (seconds)', ylab=expression(paste(' O'[2],' (',mu,'mol/L)')), type='n', axes=FALSE) #plot (empty plot to fill) data as a function of time
  usr  <-  par('usr') # extract the size of the figure margins
  rect(usr[1], usr[3], usr[2], usr[4], col='grey90', border=NA) # put a grey background on the plot
  whiteGrid() # make a grid
  box() # add a box around the plot
  points(Photo.Data1 $Value ~ Photo.Data1 $sec, pch=16, col=transparentColor('dodgerblue2', 0.6), cex=1.1)
  axis(1) # add the x axis
  axis(2, las=1) # add the y-axis
  
  # Thin the data to make the code run faster
  Photo.Data.orig <-Photo.Data1 #save original unthinned data
  Photo.Data1 <-  thinData(Photo.Data1 ,by=5)$newData1 #thin data by every 20 points for all the O2 values
  Photo.Data1$sec <- as.numeric(rownames(Photo.Data1 )) #maintain numeric values for time
  Photo.Data1$Temp<-NA # add a new column to fill with the thinned data
  Photo.Data1$Temp <-  thinData(Photo.Data.orig,xy = c(1,3),by=5)$newData1[,2] #thin data by every 20 points for the temp values
  
  # plot the thinned data
  plot(Value ~ sec, data=Photo.Data1 , xlab='Time (seconds)', ylab=expression(paste(' O'[2],' (',mu,'mol/L)')), type='n', axes=FALSE) #plot thinned data
  usr  <-  par('usr')
  rect(usr[1], usr[3], usr[2], usr[4], col='grey90', border=NA)
  whiteGrid()
  box()
  points(Photo.Data1 $Value ~ Photo.Data1 $sec, pch=16, col=transparentColor('dodgerblue2', 0.6), cex=1.1)
  axis(1)
  axis(2, las=1)
  ##Olito et al. 2017: It is running a bootstrapping technique and calculating the rate based on density
  #option to add multiple outputs method= c("z", "eq", "pc")
  Regs  <-  rankLocReg(xall=Photo.Data.orig$sec, yall=Photo.Data.orig$Value, alpha=0.5, method="pc", verbose=TRUE)  
  
  # add the regression data
  plot(Regs)
  dev.off()
  
  
  # fill in all the O2 consumption and rate data
  Respiration[i,2:3] <- Regs$allRegs[1,c(4,5)] #inserts slope and intercept in the dataframe
  Respiration[i,1] <- rename #stores the file name in the Date column
  Respiration[i,4] <- mean(Photo.Data1$Temp, na.rm=T)  #stores the Temperature in the Temp.C column
  #Photo.R[i,5] <- PR[j] #stores whether it is photosynthesis or respiration
  
  
  # rewrite the file everytime... I know this is slow, but it will save the data that is already run
}

write.csv(Respiration, 'Respiration/Output/Respiration/day10/Respiration.csv')  #edit this to where you want rates to save based on phase//timepoint
############### 24 HOUR RESPIRATION DATA ANALYSIS  ############################# #####
setwd("/Users/ninahmunk/Desktop/Projects/Acropora_Regeneration-main/Respiration")

#renaming and reordering columns in sample info and then combining it with respiration df based on coral id
names(Sample.Info)[2] <- "coral_num"
names(Sample.Info)[8] <- "coral_id"
Sample.Info <- Sample.Info[, c(8,7,6,5,4,3,1,2)]

#read in Photo.R file so dont need to run entire for loop again
Respiration <- read.csv('Output/Respiration/24hours/Respiration.csv')%>% select(-X)
Respiration$coral_id <- gsub(" ", "", Respiration$coral_id)
#list_df = list(Respiration, Sample.Info) 
#Respiration<-list_df%>%purrr::reduce(left_join, by= 'coral_id')
Respiration<-left_join(Respiration, Sample.Info, by='coral_id')%>%
  rename(surf.area.cm2=SA)
#Convert sample volume to L
Respiration$chamber_vol <- Respiration$chamber_vol/1000 #calculate volume
#Account for chamber volume to convert from umol L-1 s-1 to umol s-1. This standardizes across water volumes (different because of coral size) and removes per Liter
Respiration$umol.sec <- Respiration$umol.L.sec*Respiration$chamber_vol
View(Respiration)

# Extract rows with blank data from respiration data frame
blankrows <- c(99, 100, 119, 120, 79, 80, 60, 50, 40, 39, 19, 20)
blank_rates <- Respiration[blankrows, ]%>% 
  rename(blank_id = coral_id)%>%
  select(blank_id, umol.sec)

Respiration<- drop_na(Respiration)%>%mutate(coral_num = as.numeric(coral_num))
View(Respiration)

blanks<-read.csv('Data/24hours/blanks.csv')%>%
  rename("blank_id" = "coral_id")
View(blanks)

merged_data <- merge(blanks, blank_rates, by = "blank_id", all.x = TRUE)
merged_data <- merged_data[, c(2,1,3)]%>%rename(blank.umol.sec=umol.sec)
View(merged_data)
Respiration<- left_join(Respiration, merged_data, by= 'coral_num')
Respiration$umol.sec.corr<-Respiration$umol.sec-Respiration$blank.umol.sec
View(Respiration)

#### Normalize to SA (surface area)

#Calculate net R
Respiration$umol.cm2.hr <- (Respiration$umol.sec.corr*3600)/Respiration$surf.area.cm2 #mmol cm-2 hr-1
# Take the absolute (positive) value of the 'umol.cm2.hr' column
Respiration<- Respiration%>%
  mutate(umol.cm2.hr = abs(umol.cm2.hr))%>%
  mutate(genotype = as.factor(genotype))

# log the rates
Respiration$Rate.ln<-log(Respiration$umol.cm2.hr+0.1)

#export normalized rates
write.csv(Respiration, 'Output/Respiration/24hours/norm_resp_24.csv')

#visualize respo rates by genotype 
Respiration<- read.csv('Output/Respiration/24hours/norm_resp_24.csv')%>% mutate(genotype = as.factor(genotype))%>% mutate(wound= as.factor(wound))
quartz()
ggplot(Respiration, aes(x=wound, y=umol.cm2.hr, color= temp))+
  geom_boxplot()+
  ylab('Respiration (umol.cm2.hr)')+
  xlab('Wound Type')+
  scale_x_discrete(labels = c("0" = "No Wound", "1" = "Abrasion", "2" = "Fragment")) +
  #labs(fill = "Temperature")+
  #scale_fill_manual(values = c("lightblue", "lightpink"))+
  scale_fill_discrete(name = "Temperature", labels = c("Ambient", "Hot"))

quartz()
ggplot(Respiration, aes(x=temp, y=umol.cm2.hr))+
  geom_boxplot()+
  ylab('Respiration (umol.cm2.hr)')

#export normalized rates
write.csv(Respiration, 'Output/Respiration/24hours/norm_resp_24.csv')
############### 24 HOUR PHOTOSYNTHESIS DATA ANALYSIS ########################### #####
setwd("/Users/ninahmunk/Desktop/Projects/Acropora_Regeneration-main/Respiration")

#renaming and reordering columns in sample info and then combining it with respiration df based on coral id
names(Sample.Info)[2] <- "coral_num"
names(Sample.Info)[8] <- "coral_id"
Sample.Info <- Sample.Info[, c(8,7,6,5,4,3,1,2)]

#read in Photosynthesis file so dont need to run entire for loop again
Photosynthesis <- read.csv('Output/Photosynthesis/24hours/Photosynthesis.csv')%>%select(-X)
Photosynthesis$coral_id <- gsub(" ", "", Photosynthesis$coral_id)
Photosynthesis<-left_join(Photosynthesis, Sample.Info, by='coral_id')%>%
  rename(surf.area.cm2=SA)
#Convert sample volume to L
Photosynthesis$chamber_vol <- Photosynthesis$chamber_vol/1000 #calculate volume
#Account for chamber volume to convert from umol L-1 s-1 to umol s-1. This standardizes across water volumes (different because of coral size) and removes per Liter
Photosynthesis$umol.sec <- Photosynthesis$umol.L.sec*Photosynthesis$chamber_vol

# Extract rows with blank data from respiration data frame
blankrows <- c(99, 100, 119, 120, 79, 80, 60, 50, 40, 39, 19, 20)
blank_rates <- Photosynthesis[blankrows, ]%>% 
  rename(blank_id = coral_id)%>%
  select(blank_id, umol.sec)

Photosynthesis<- drop_na(Photosynthesis)%>%mutate(coral_num = as.numeric(coral_num))
View(Photosynthesis)

blanks<-read.csv('Data/24hours/blanks.csv')%>%
  rename("blank_id" = "coral_id")
View(blanks)

merged_data <- merge(blanks, blank_rates, by = "blank_id", all.x = TRUE) #need to input 20230605_g1_blank4 rate here
merged_data <- merged_data[, c(2,1,3)]%>%rename(blank.umol.sec=umol.sec)
View(merged_data)
Photosynthesis<- left_join(Photosynthesis, merged_data, by= 'coral_num')
Photosynthesis$umol.sec.corr<-Photosynthesis$umol.sec-Photosynthesis$blank.umol.sec
View(Photosynthesis)

#Normalize to SA (surface area)

#Calculate net P 
Photosynthesis$umol.cm2.hr <- (Photosynthesis$umol.sec.corr*3600)/Photosynthesis$surf.area.cm2 #mmol cm-2 hr-1
# Take the absolute (positive) value of the 'umol.cm2.hr' column
Photosynthesis<- Photosynthesis%>%
  mutate(umol.cm2.hr = abs(umol.cm2.hr))%>%
  mutate(genotype = as.factor(genotype))

# log the rates
Photosynthesis$Rate.ln<-log(Photosynthesis$umol.cm2.hr+0.1)

#visualize respo rates by genotype 
quartz()
ggplot(Photosynthesis, aes(x=temp, y=umol.cm2.hr))+
  geom_boxplot()+
  ylab('Photosynthesis (umol.cm2.hr)')

#export normalized rates
write.csv(Photosynthesis, 'Output/Photosynthesis/24hours/norm_photo_24hr.csv')

############### 24 HOUR GROSS PHOTOSYNTHESIS DATA ANALYSIS ##################### ##### 

Respiration<-read.csv("Respiration/Output/Respiration/24hours/norm_resp_24.csv") 
Photosynthesis<-read.csv("Respiration/Output/Photosynthesis/24hours/norm_photo_24hr.csv")

Resp.1<- Respiration%>%
  select(coral_num, umol.cm2.hr)%>%
  add_column(Rate = "Respiration")
view(Resp.1)

Photo.1<- Photosynthesis%>%
  select(coral_num, umol.cm2.hr)%>%
  add_column(Rate = "Net Photosythesis")
view(Photo.1)

P.gross<- left_join(Resp.1, Photo.1, by= 'coral_num')%>%
  select(coral_num, umol.cm2.hr.x, umol.cm2.hr.y)%>%
  mutate(umol.cm2.hr = umol.cm2.hr.x + umol.cm2.hr.y)%>%
  select(coral_num, umol.cm2.hr)%>%
  add_column(Rate = "Gross Photosynthesis")

rates<- rbind(Resp.1, Photo.1, P.gross)
view(rates)
rates$coral_num <- as.character(rates$coral_num)

Sample.Info<-Sample.Info%>%rename(coral_num = coral_id)

rates_full<- left_join(rates, Sample.Info, by= 'coral_num')
rates_full$wound <- as.character(rates_full$wound)
rates_full<-rates_full%>%
  mutate(wound_status = case_when(
    wound == 0 ~ "no wound",
    wound %in% c(1, 2) ~ "wounded"
  ))

write.csv(rates_full, 'Respiration/Output/All_Rates/rates_24hr.csv')

quartz()
ggplot(rates_full, aes(x=wound_status, y= umol.cm2.hr, fill = Rate))+
  geom_boxplot()+
  facet_wrap(~ temp, scales = "free")

quartz()
ggplot(rates_full, aes(x=Rate, y=umol.cm2.hr, color=wound))+
  geom_boxplot()+
  ylab('Rate (umol.cm2.hr)')+
  xlab("")+
  facet_wrap(~temp)

quartz()
ggplot(rates_full, aes(x=wound, y=umol.cm2.hr, color=temp))+
  geom_boxplot()+
  ylab('Rate (umol.cm2.hr)')+
  xlab("")+
  facet_wrap(~Rate)

# only "Gross Photosynthesis" and "Respiration"
filtered_data <- rates_full %>%
  filter(Rate %in% c("Gross Photosynthesis", "Respiration"))

# Create the plot with the filtered data
ggplot(filtered_data, aes(x = Rate, y = umol.cm2.hr, fill= temp)) +
  geom_boxplot() +
  ylab('Rate (umol.cm2.hr)') +
  xlab('') +
  facet_wrap(~ wound_status, scales = "free")


############### DAY 10 SAMPLE INFO ############################################# #####
#load in sample information files
Treatments<- read.csv("Respiration/Data/samp_info.csv")%>% #genotype, wound type, temp
  mutate(coral_id = as.character(coral_id)) 
Treatments$temp[Treatments$coral_id == 17] <- "H"
Treatments$temp[Treatments$coral_id == 5] <- "A"
Volume<- read.csv("Respiration/Data/day10/chamber_vol_day10.csv")%>% #vol of water in each chamber 
  mutate(coral_id = as.character(coral_id)) 
SA<- read.csv("Surface_Area/Output/final_surface_areas.csv")%>% #initial SA of each coral
  mutate(coral_id = as.character(coral_id))%>%
  select(coral_id, CSA_cm2)%>%
  rename(SA = CSA_cm2)
list_df = list(Treatments, Volume, SA) 
Sample.Info<-list_df%>%purrr::reduce(left_join, by= 'coral_id')%>%select(-X)%>% #combine data frames by coral_id
  mutate(file.names.full = paste0(date, "_", temp, "_", coral_id)) #create file.names.full column that matches data file names
Sample.Info$file.names.full <- paste0(substring(Sample.Info$file.names.full, 1, 8), "_10", substring(Sample.Info$file.names.full, 9)) #add _10 after the 8th character 
blank_files<-read.csv("Respiration/Data/day10/blanks_only.csv")
blank_files<-blank_files%>%mutate(blank_files, file.names.full = coral_id)
Sample.Info <- rbind(Sample.Info, blank_files)
Sample.Info$file.names.full <- paste(Sample.Info$file.names.full, 'O2', sep = '_') # Add '_O2' to the end of each value in the new column
# View the updated data frame
View(Sample.Info)
############### FINAL SAMPLE INFO ############################################## ##### 
#load in sample information files
Treatments<- read.csv("Respiration/Data/samp_info.csv")%>% #genotype, wound type, temp
  mutate(coral_id = as.character(coral_id)) 
Treatments$temp[Treatments$coral_id == 17] <- "H"
Treatments$temp[Treatments$coral_id == 5] <- "A"
Volume<- read.csv("Respiration/Data/final/chamber_vol_final.csv")%>% #vol of water in each chamber 
  mutate(coral_id = as.character(coral_id)) 
SA<- read.csv("Surface_Area/Output/final_surface_areas.csv")%>% #initial SA of each coral
  mutate(coral_id = as.character(coral_id))%>%
  select(coral_id, CSA_cm2)%>%
  rename(SA = CSA_cm2)
list_df = list(Treatments, Volume, SA) 
Sample.Info<-list_df%>%purrr::reduce(left_join, by= 'coral_id')%>%select(-X)%>% #combine data frames by coral_id
  mutate(file.names.full = paste0(date, "_", temp, "_", coral_id)) #create file.names.full column that matches data file names
Sample.Info$file.names.full <- paste0(substring(Sample.Info$file.names.full, 1, 8), "_F", substring(Sample.Info$file.names.full, 9)) #add _10 after the 8th character 
blank_files<-read.csv("Respiration/Data/final/blanks_only.csv")
blank_files<-blank_files%>%mutate(blank_files, file.names.full = coral_id)
Sample.Info <- rbind(Sample.Info, blank_files)
Sample.Info$file.names.full <- paste(Sample.Info$file.names.full, 'O2', sep = '_') # Add '_O2' to the end of each value in the new column
# View the updated data frame
View(Sample.Info)
#######MAKE THIS ONE PRETTY##################

ggplot(filtered_data, aes(x = wound, y = umol.cm2.hr, color=temp)) +
  geom_boxplot() +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Wound Type')+
  facet_wrap(~ Rate, scales = "free")

quartz()
theme_custom <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#212121"),  # Updated background color
    panel.background = element_rect(fill = "#212121"),  # Updated panel background color
    text = element_text(color = "white", size = 16),  # Adjusted font size
    axis.title.y = element_text(size = 16, color = "white"),  # Adjusted Y-axis title font size and color
    axis.text = element_text(size = 14, color = "white"),  # Adjusted axis text font size and color
    panel.grid.major.x = element_blank(),  # Removed vertical grid lines
    panel.border = element_blank(),  # Removed panel borders
    strip.background = element_rect(fill = "#17202A", color = "#17202A")  # Set strip background color
  )

# Update the plot with the custom theme
ggplot(filtered_data, aes(x = wound_status, y = umol.cm2.hr, fill = temp)) +
  geom_boxplot(lwd = 1.5, color = "white") +  # Made boxplot lines thicker and white
  ylab('Rate (umol.cm2.hr)') +
  xlab('') +
  facet_wrap(~Rate, scales = "free") +
  
  # Change labels of wound_status
  scale_x_discrete(labels = c("Ambient" = "A", "Heated" = "H")) +
  
  # Adjust fill colors for boxplots
  scale_fill_manual(values = c("A" = "#7ab8d6", "H" = "#e0474c")) +  # Updated color hex codes
  
  # Apply the custom theme
  theme_custom



###stats

library(stargazer)

resp<- filtered_data %>% 
  filter(Rate=="Respiration")


#resp model

#note temp is significant only when we also consider genotype also significant

full.model <- lm(umol.cm2.hr ~ temp*wound_status + genotype, data = resp)
summary(full.model)
stargazer(full_model, title = "Linear Regression Results", type = "text")

full_model <- lm(umol.cm2.hr ~ temp * wound_status + genotype, data = resp)

# Display the summary using stargazer
stargazer(full_model, title = "Linear Regression Results", type = "text")

# Using a multiple linear regression model, I explored how umol.cm2.hr is influenced by temperature (temp), wound status (wound_status), and genotype (genotype). I also considered the potential interaction between temperature and wound status (temp * wound_status)




quartz()
ggplot(filtered_data, aes(x=temp, y= umol.cm2.hr, color= Rate))+
  geom_boxplot()+
  facet_wrap(~ wound_status, scales = "free")

################ initial & 24 ####################

initial_rates<-read.csv("Respiration/Output/All_Rates/rates_initial.csv") 
initial_rates$timepoint <- "initial"

hr24_rates<-read.csv("Respiration/Output/All_Rates/rates_24hr.csv") 
hr24_rates$timepoint <- "24 hours"

initial_24_rates<- rbind(initial_rates, hr24_rates)

filtered_GP <- initial_24_rates %>%
  filter(Rate %in% c("Gross Photosynthesis"))
filtered_GP$wound <- as.factor(filtered_GP$wound)

filtered_R <- initial_24_rates %>%
  filter(Rate %in% c( "Respiration"))
filtered_R$wound <- as.factor(filtered_R$wound)

quartz()
ggplot(initial_24_rates, aes(x = wound, y = umol.cm2.hr, color = temp)) +
  geom_boxplot(aes(group = timepoint)) +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Wound Type')+
  facet_wrap(~ Rate, scales = "free")

ggplot(initial_24_rates, aes(x = wound, y = umol.cm2.hr, color = temp)) +
  geom_point() +
  geom_line(aes(group = timepoint), linetype = "dashed") +
  labs(x = "Wound Type",
       y = "Rate (umol.cm2.hr)") +
  facet_wrap(~ timepoint, scales = "free")

ggplot(filtered_GP)+
  geom_boxplot(aes(wound, umol.cm2.hr, fill=timepoint))+
  labs(x = "Wound Type",
       y = "Rate (umol.cm2.hr)", 
       title = "Gross Photosynthesis") +
  # theme_minimal() +
  facet_wrap(~ temp, scales = "free")


ggplot(filtered_R)+
  geom_boxplot(aes(wound, umol.cm2.hr, fill=timepoint))+
  labs(x = "Wound Type",
       y = "Rate (umol.cm2.hr)",
       title = "Respiration") +
  # theme_minimal() +
  facet_wrap(~ temp, scales = "free")
