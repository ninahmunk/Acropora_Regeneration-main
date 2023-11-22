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


#Set working directory and the path of all respo data files
setwd("/Users/ninahmunk/Desktop/Projects/Acropora_Regeneration-main")
getwd()
path.p<-"/Users/ninahmunk/Desktop/Projects/Acropora_Regeneration-main/Respiration/Data/24hours/runs"

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

############################## Initial Sample Info ######################################### ##### 

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


############### Initial RESPIRATION ############################ #####
# for every file in list calculate O2 uptake or release rate and add the data to the Respiration dataframe
for(i in 1:length(file.names)) { # for every file in list calculate O2 uptake or release rate and add the data to the Photo.R dataframe
  
  #find the lines in sample info that have the same file name that is being brought in
  
  FRow<-which(Sample.Info$file.names.full==strsplit(file.names[i],'.csv'))
  
  # read in the O2 data one by one
  Photo.Data1 <-read.csv(file.path(path.p,file.names[i]), skip = 1, header=T) # skips the first line
  Photo.Data1  <- Photo.Data1[,c("delta_t","Value","Temp")] #subset columns of interest
  #Photo.Data1$Time <- as.POSIXct(Photo.Data1$Time,format="%H:%M:%S", tz = "") #convert time from character to time
  Photo.Data1 <- na.omit(Photo.Data1) #omit NA from data frame
  
  # clean up some of the data
  n<-dim(Photo.Data1)[1] # length of full data
  Photo.Data1 <- Photo.Data1 %>% mutate(delta_t=as.numeric(delta_t))%>%filter(delta_t > 129) #start at beginning of dark phase data point (25 minutes in) 
  n<-dim(Photo.Data1)[1] #list length of trimmed data
  Photo.Data1$sec <- seq(1, by = 3, length.out = n) #set seconds by three from start to finish of run in a new column
  
  
  #Save plot prior to and after data thinning to make sure thinning is not too extreme
  rename <- sub(".csv","", file.names[i]) # remove all the extra stuff in the file name
  
  pdf(paste0("Output/Respiration/",rename,"thinning.pdf")) # open the graphics device
  
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
write.csv(Respiration, 'Output/Respiration/Respiration.csv')  

#renaming and reordering columns in sample info and then combining it with respiration df based on coral id
names(Sample.Info)[2] <- "coral_num"
names(Sample.Info)[8] <- "coral_id"
Sample.Info <- Sample.Info[, c(8,7,6,5,4,3,1,2)]

setwd("/Users/ninahmunk/Desktop/Projects/Acropora_Regeneration-main/Respiration")
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

############### Initial PHOTOSYNTHESIS ############################ #####
# for every file in list calculate O2 uptake or release rate and add the data to the Photo.R dataframe
for(i in 1:length(file.names)) { # for every file in list calculate O2 uptake or release rate and add the data to the Photo.R dataframe
  
  #find the lines in sample info that have the same file name that is being brought in
  
  # Exclude the specific file
  #if (file.names[i] == "20230605_g1_blank4_O2.csv") {
   # next  # Skip the rest of the loop and move to the next iteration
 # }
  FRow<-which(Sample.Info$file.names.full==strsplit(file.names[i],'.csv'))
  
  # read in the O2 data one by one
  Photo.Data1 <-read.csv(file.path(path.p,file.names[i]), skip = 1, header=T) # skips the first line
  Photo.Data1  <- Photo.Data1[,c("delta_t","Value","Temp")] #subset columns of interest
  #Photo.Data1$Time <- as.POSIXct(Photo.Data1$Time,format="%H:%M:%S", tz = "") #convert time from character to time
  Photo.Data1 <- na.omit(Photo.Data1) #omit NA from data frame
  
  # clean up some of the data
  n<-dim(Photo.Data1)[1] # length of full data
  Photo.Data1 <- Photo.Data1 %>% mutate(delta_t=as.numeric(delta_t))%>%filter(delta_t > 114 & delta_t < 129) #start at beginning of light phase (10 minutes in) and stop at 25 min (start of dark phase)
  n<-dim(Photo.Data1)[1] #list length of trimmed data
  Photo.Data1$sec <- seq(1, by = 3, length.out = n) #set seconds by three from start to finish of run in a new column
  
  
  #Save plot prior to and after data thinning to make sure thinning is not too extreme
  rename <- sub(".csv","", file.names[i]) # remove all the extra stuff in the file name
  
  pdf(paste0("Output/Photosynthesis/",rename,"thinning.pdf")) # open the graphics device
  
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
write.csv(Photosynthesis, 'Output/Photosynthesis/Photosynthesis.csv')  

# Calculate P and R rate
#Respiration$fragment.ID.full<-Respiration$fragment.ID
#Respiration$fragment.ID<-NULL
#View(Respiration)

#renaming and reordering columns in sample info and then combining it with respiration df based on coral id
names(Sample.Info)[2] <- "coral_num"
names(Sample.Info)[8] <- "coral_id"
Sample.Info <- Sample.Info[, c(8,7,6,5,4,3,1,2)]

setwd("/Users/ninahmunk/Desktop/Projects/Acropora_Regeneration-main/Respiration")
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


#################### Initial Gross Photosynthesis ############################ #####

#calculating Gross Photosynthesis: Pgross = Pnet + R (when using absolute value of R)

Resp.1<- Respiration%>%
  select(coral_num, umol.cm2.hr, genotype)%>%
  add_column(Rate = "Respiration")
view(Resp.1)

Photo.1<- Photosynthesis%>%
  select(coral_num, umol.cm2.hr, genotype)%>%
  add_column(Rate = "Net Photosythesis")
view(Photo.1)

P.gross<- left_join(Resp.1, Photo.1, by= 'coral_num')%>%
  select(coral_num, umol.cm2.hr.x, umol.cm2.hr.y, genotype.y)%>%
  mutate(umol.cm2.hr = umol.cm2.hr.x + umol.cm2.hr.y)%>%
  select(coral_num, umol.cm2.hr, genotype.y)%>%
  rename(genotype= genotype.y)%>%
  add_column(Rate = "Gross Photosynthesis")

rates<- rbind(Resp.1, Photo.1, P.gross)
view(rates)

quartz()
ggplot(rates, aes(x=genotype, y= umol.cm2.hr, fill = Rate))+
  geom_boxplot()

quartz()
ggplot(rates, aes(x=Rate, y=umol.cm2.hr, fill=Rate))+
  geom_boxplot()+
  ylab('Rate (umol.cm2.hr)')+
  xlab("")




#check all rate types (GP, NP and R) curves
ggplot(Photo.T, aes(x=Temp.C, y=umol.cm2.hr, group = individual.ID, col = individual.ID))+
  geom_line(size=2)+
  geom_point()+  
  theme_bw ()+
  #ylim(0,1.5)+  
  facet_wrap(~ treatment*rate.type, labeller = labeller(.multi_line = FALSE))+ 
  ggsave(filename = "../Respirometry/Output/initialcurves.pdf", device = "pdf", width = 10, height = 10)

#check three various rate types (GP, NP and R) individaul curves
Photo.T.GP <- Photo.T %>%
  filter(rate.type =="GP")

ggplot(Photo.T.GP, aes(x=Temp.C, y=umol.cm2.hr, group = individual.ID, col = individual.ID))+
  geom_line(size=2)+
  geom_point()+  
  theme_bw ()+
  #ylim(0,1.5)+  
  facet_wrap(~ treatment*rate.type*fragment.ID, labeller = labeller(.multi_line = FALSE))+ 
  ggsave(filename = "../Respirometry/Output/GPcurves.pdf", device = "pdf", width = 10, height = 10)

Photo.T.NP <- Photo.T %>%
  filter(rate.type =="NP")

ggplot(Photo.T.NP, aes(x=Temp.C, y=umol.cm2.hr, group = individual.ID, col = individual.ID))+
  geom_line(size=2)+
  geom_point()+  
  theme_bw ()+
  #ylim(0,1.5)+  
  facet_wrap(~ treatment*rate.type*fragment.ID, labeller = labeller(.multi_line = FALSE))+ 
  ggsave(filename = "../Respirometry/Output/NPcurves.pdf", device = "pdf", width = 10, height = 10)

Photo.T.R <- Photo.T %>%
  filter(rate.type =="R")

ggplot(Photo.T.R, aes(x=Temp.C, y=umol.cm2.hr, group = individual.ID, col = individual.ID))+
  geom_line(size=2)+
  geom_point()+  
  theme_bw ()+
  #ylim(0,1.5)+  
  facet_wrap(~ treatment*rate.type*fragment.ID, labeller = labeller(.multi_line = FALSE))+ 
  ggsave(filename = "../Respirometry/Output/Rcurves.pdf", device = "pdf", width = 10, height = 10)





#################### 24 hr Sample Info ############################ #####
#load in sample information files
Treatments<- read.csv("Respiration/Data/24hours/samp_info_24.csv") #genotype, wound type, temp
Volume<- read.csv("Respiration/Data/24hours/chamber_vol_24hrs.csv") #vol of water in each chamber 
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
################ 24 hour RESPIRATION ########################## #####
for(i in 1:length(file.names)) { # for every file in list calculate O2 uptake or release rate and add the data to the Respiration dataframe
  
  #find the lines in sample info that have the same file name that is being brought in
  
  FRow<-which(Sample.Info$file.names.full==strsplit(file.names[i],'.csv'))
  
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
  
  pdf(paste0("Output/Respiration/",rename,"thinning.pdf")) # open the graphics device
  
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
write.csv(Respiration, 'Output/Respiration/Respiration.csv')  
