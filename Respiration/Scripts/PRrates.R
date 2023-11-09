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
setwd("/Users/ninahmunk/Desktop/Projects/Acropora_Regeneration-main/Respiration/Data/initial")
getwd()
path.p<-"/Users/ninahmunk/Desktop/Projects/Acropora_Regeneration-main/Respiration/Data/initial/runs"

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

#load in sample information files
Treatments<- read.csv(file= "samp_info.csv") #genotype, wound type, temp
Volume<- read.csv(file= "chamber_vol.csv") #vol of water in each chamber 
SA<- read.csv(file= "final_surface_areas.csv")%>% #final SA of each coral
  select(coral_id, CSA_cm2)%>% 
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

#load in times as a list the same length as the number of files, im not sure I need this 
starttimes<- read.csv(file= "starttimes.csv")
rtime<-starttimes$rtime #list respiration start times. For respiration measurements, filter as > than this time
ptime<-starttimes$ptime #for photosynthesis, filter as < than this time

# #create path to single respo file 
# path.p1 <- file.path(path.p, "20230605_g1_2_O2.csv")
# path.p1 <- path.p1[,nrow(path.p1)-2]
# # Read the respo CSV file using the created path
# 
# R <- read.csv(path.p1, skip = 1, header=T)
# R <- R[1:(nrow(R)-1),]
# 
# 
# R <- R%>%
#   select(delta_t, Value, Temp)%>% # select columns 'delta_t', 'Value', and 'Temp'
#   mutate(delta_t=as.numeric(delta_t))%>%
#   filter(delta_t > 25.0)
# #fit all possible linear regressions 
# Rmodel <- rankLocReg(xall=R$delta_t, yall=R$Value, alpha=0.4, method= "pc", verbose= TRUE) 
# plot(Rmodel)
# summary()
# #trying to extract intercept and slope and put it into R.rates dataframe corresponding to file name in coral_id column 
# row_index <- which(R.rates$coral_id == file.names.clean)
# R.rates[row_index, c("Intercept", "umol.L.sec")] <- c(Rmodel$allRegs, c(4,5))
# 
# P<-  read.csv(path.p1, skip = 1, header=T)%>%
#   select(delta_t, Value, Temp)%>% # select columns 'delta_t', 'Value', and 'Temp'
#   mutate(delta_t=as.numeric(delta_t))%>%
#   filter(delta_t > 10 & delta_t < 25)
# 
# Pmodel <- rankLocReg(xall=P$delta_t, yall=P$Value, alpha=0.4, method= "pc", verbose= TRUE) # 0.4 means it has to encompass at least 40% of the data points
# plot(Pmodel)

# for every file in list calculate O2 uptake or release rate and add the data to the Photo.R dataframe
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
  Photo.Data1 <- Photo.Data1 %>% mutate(delta_t=as.numeric(delta_t))%>%filter(delta_t > 25) #start at beginning of dark phase data point (25 minutes in) 
  n<-dim(Photo.Data1)[1] #list length of trimmed data
  Photo.Data1$sec <- seq(1, by = 3, length.out = n) #set seconds by three from start to finish of run in a new column
  
  
  #Save plot prior to and after data thinning to make sure thinning is not too extreme
  rename <- sub(".csv","", file.names[i]) # remove all the extra stuff in the file name
  
  pdf(paste0("Respiration/Output/",rename,"thinning.pdf")) # open the graphics device
  
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
write.csv(Respiration, 'Respiration/Output/Respiration.csv')  

setwd("/Users/ninahmunk/Desktop/Projects/Acropora_Regeneration-main/Respiration/Output")
#read in Photo.R file so dont need to run entire for loop again
Respiration <- read.csv('Respiration.csv')
Respiration$coral_id[9]='20230605_g1_25_O2'

# Calculate P and R rate
#Respiration$fragment.ID.full<-Respiration$fragment.ID
#Respiration$fragment.ID<-NULL
#View(Respiration)

names(Sample.Info)[2] <- "coral_num"
names(Sample.Info)[8] <- "coral_id"
Sample.Info <- Sample.Info[, c(8,7,6,5,4,3,1,2)]
list_df = list(Respiration, Sample.Info) 
Respiration<-list_df%>%purrr::reduce(left_join, by= 'coral_id')
Respiration<-left_join(Respiration, Sample.Info)
View(Respiration)

#Convert sample volume to L
Respiration$chamber_vol <- Respiration$chamber_vol/1000 #calculate volume

#Account for chamber volume to convert from umol L-1 s-1 to umol s-1. This standardizes across water volumes (different because of coral size) and removes per Liter
Respiration$umol.sec <- Respiration$umol.L.sec*Respiration$chamber_vol

# extract the blank rates for each run and put them into a new column in main respo dataframe corresponding to the correct coral numbers
blank_rate <- Respiration %>%
  filter(coral_id == "20230605_g1_blank_O2") %>%
  select(umol.sec) %>%
  pull()

Respiration_corr <- Respiration %>%
  mutate(umol.sec.corr = ifelse(coral_num %in% c(105, 76, 18, 25, 41, 64, 38, 21, 7), blank_rate, umol.sec))

       
# Extract blank rates
blanks <- c(37, 77, 117, 38, 78, 118, 39, 79, 119, 40, 80, 120)

# Extract rows from the data frame
blank_rates <- Respiration[blanks, ]

# Print the extracted rows
print(blank_rates)



# Find the rate for 20230605_g1_blank_O2
blank_rate <- Respiration %>%
  filter(coral_id == "20230605_g1_blank_O2") %>%
  select(umol.sec) %>%
  pull()

# List of coral numbers you want to calculate differences for
coral_numbers <- c(105, 76, 18, 25, 41, 64, 38, 21, 7)

# Calculate differences for the specified coral numbers
differences <- Respiration %>%
  filter(coral_num %in% coral_numbers) %>%
  mutate(difference = umol.sec - blank_rate) %>%
  select(coral_num, difference)

print(differences)

#Account for blank rate by temperature
#convert character columns to factors
Respiration <- Respiration %>%
  mutate_if(sapply(., is.character), as.factor)
View(Respiration)

#make the blank column a factor
Respiration$BLANK<-ifelse(Respiration$treatment=='BLANK', 1,0)
Respiration$BLANK<-as.factor(Respiration$BLANK)
View(Respiration)

#aggregate certain columns together for easier viewing
photo.blnk <- aggregate(umol.sec ~ species*temp.Cat*light_dark*BLANK, data=Respiration, mean)

#pull out only the blanks
#photo.blnk<-photo.blnk[photo.blnk$Species=='BK',]
photo.blnk<-photo.blnk[photo.blnk$BLANK==1,]

# remove the blank column
photo.blnk$BLANK<-NULL

# rename the blank rate
colnames(photo.blnk)[4]<-'blank.rate'  

# join the blank data with the rest of the data
Respiration<-left_join(Respiration, photo.blnk)
View(Respiration)

# subtract the blanks
Respiration$umol.sec.corr<-Respiration$umol.sec-Respiration$blank.rate

View(Respiration)

#### Normalize to SA (surface area)#####

#Calculate net P and R
Respiration$umol.cm2.hr <- (Respiration$umol.sec.corr*3600)/Respiration$surf.area.cm2 #mmol cm-2 hr-1

# remove NAs and blanks
Respiration<-Respiration[Respiration$BLANK==0,]

#make respiration positive
Respiration$umol.cm2.hr<- Respiration$umol.cm2.hr

# log the rates
Respiration$Rate.ln<-log(Respiration$umol.cm2.hr+0.1)

#remove empty rows
Respiration<-Respiration[-which(is.na(Respiration$fragment.ID.full)),]

#making the respiration values positive (pull out data for dark treaments)
Respiration$umol.cm2.hr[Respiration$light_dark=="dark"]<-Respiration$umol.cm2.hr[Respiration$light_dark=="dark"]*-1 
lessthan <- which(Respiration$light_dark=="dark" & Respiration$umol.cm2.hr < 0)
Respiration$umol.cm2.hr[lessthan] <- 0
View(Respiration)

ggplot(Respiration, aes(x=Temp.C, y=umol.cm2.hr,group = fragment.ID, col = fragment.ID))+
  geom_line(size=2)+
  geom_point()+  
  #ylim(0,1.5)+  
  facet_wrap(~ treatment*light_dark, labeller = labeller(.multi_line = FALSE))+
  ggsave(filename = "Output/lowvshigh_curves.pdf", device = "pdf", width = 10, height = 10)

write.csv(Respiration, 'Output/TT_Rates.csv') # export all the uptake rates

View(Respiration)


###calculating gross photosynthesis from data frame###

#make ifelse statements to assign light treatments as NP and dark treatments as resp
#light will be assigned NP for net photosynthesis 
Photo.R$rate.type <-ifelse(Photo.R$light_dark=='light', "NP", "R")
Photo.R$rate.type<-as.factor(Photo.R$rate.type)

View(Photo.R)

#rename fragment ID 
Photo.R$individual.ID <- str_split(Photo.R$fragment.ID, "_", n = Inf, simplify = TRUE)[,1]
Photo.R$individual.ID <- as.factor(Photo.R$individual.ID)

#rename dataframe to work from Photo.R2 to preserve Photo.R
#make column for GP and group by fragment ID and temp to keep R and NP together
Photo.R2 <- Photo.R %>% 
  filter(temp.Cat <= 37) %>%
  group_by(individual.ID, temp.Cat, treatment) %>% 
  summarize(rates = sum(umol.cm2.hr), Temp.C=mean(Temp.C)) %>%
  mutate(rate.type="GP", light_dark="L") %>%
  rename(umol.cm2.hr = rates) %>%
  mutate(fragment.ID=paste0(individual.ID, "_", light_dark))

#rename light darks
Photo.R2$light_dark <- ifelse(Photo.R2$light_dark=="L", "light", "dark")

#make the negative gphoto values zeros
Photo.R2$umol.cm2.hr[Photo.R2$umol.cm2.hr<0]<-0

Photo.R <- Photo.R[,c("individual.ID", "temp.Cat","treatment", "Temp.C", "umol.cm2.hr", "rate.type", "light_dark", "fragment.ID")]
view(Photo.R)

#left join Photo.R and Photo.R2
Photo.T <- rbind(Photo.R, as.data.frame(Photo.R2))
View(Photo.T)


write.csv(Photo.T, '../Thermal_Performance/Data/Photo.T.csv') # export all the uptake rates

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





################## loop shenanigans ##### 



for(i in 1:length(file.names)) {
  Resp.Data <-read.table(file.path(path.p,file.names[i]), skip = 1, header=T, sep=",", na.string="NA", fill = TRUE, as.is=TRUE, fileEncoding="latin1")
}



# LOOP example from Arianna 

# Run loop to extract slopes from respiration data
for(i in 1:length(file.names)) { # for every file in list start at the first and run this following function
  Resp.Data <-read.table(file.path(path.p,file.names[i]), skip = 203, header=T, sep=",", na.string="NA", fill = TRUE, as.is=TRUE, fileEncoding="latin1") #reads in the data files
  Resp.Data$Temp <- Resp.Data[,16] #assigns temp column
  Resp.Data$Time.Min <- seq.int(from = 0, to = ((nrow(Resp.Data) * 0.05) - 0.05), by = 0.05) #set time in min
  Resp.Data <- Resp.Data %>% #filters data by phase (respiration only)
    filter(Time.Min > rtime[i])
  Resp.Data.N <- Resp.Data[,9] #subset desired columns
  
  for(j in 1:(ncol(Resp.Data.N))){
    model <- rankLocReg(
      xall=Resp.Data$Time.Min, yall=as.numeric(Resp.Data.N[, j]), 
      alpha=0.4, method="pc", verbose=TRUE) #extract slopes, percentile rank method with minimum window size of 0.4. This means that in order to fit a slope, it has to encompass at least 40% of available datapoints. 
    
    pdf(paste0("../Regeneration_3/respiration/output/",date[i], "_",run[i],"_",rename[j],"_regression_trunc.pdf")) #generate output file names
    plot(model)
    dev.off()
    
    Resp.Rb[j,1] <- as.character(date[i]) #stores the date
    Resp.Rb[j,2] <- as.character(run[i]) #stores the run number
    Resp.Rb[j,3] <- as.character(samp[j+(i-1)*ncol(Resp.Data.N)]) #stores the sample ID
    Resp.Rb[j,4] <- as.character(rename[j]) #stores the chamber ID
    Resp.Rb[j,5:6] <- model$allRegs[i,c(4,5)] #inserts slope and intercept in the dataframe
    
  }
  Resp.R <- rbind(Resp.R, Resp.Rb) #bind final data frame
}

processFile <- function(file.path, rtime, date, run, rename) {

  Resp.Data <- read.table(file.path(path.p, "20230606_g2_89_O2.csv"), skip = 201, sep = ",", na.string = "NA", fill = TRUE, as.is = TRUE, fileEncoding = "latin1")
  #Resp.Data <- read.table(file.path, skip = 203, header = T, sep = ",", na.string = "NA", fill = TRUE, as.is = TRUE, fileEncoding = "latin1")
  Resp.Data$Temp <- Resp.Data[,16]
  Resp.Data$Time.Min <- seq.int(from = 0, to = ((nrow(Resp.Data) * 0.05) - 0.05), by = 0.05)
  Resp.Data <- Resp.Data %>% filter(Time.Min > rtime)
  Resp.Data.N <- Resp.Data[, 9]
  
  Resp.Rb <- data.frame(Date = character(0), Run = character(0), Sample.ID = character(0),
                        Chamber.ID = character(0), Slope = numeric(0), Intercept = numeric(0))
  
  for (j in 1:(ncol(Resp.Data.N))) {
    model <- rankLocReg(xall = Resp.Data$Time.Min, yall = as.numeric(Resp.Data.N[, j]),
                        alpha = 0.4, method = "pc", verbose = TRUE)
    
    pdf(paste0("../Regeneration_3/respiration/output/", date, "_", run, "_", rename[j], "_regression_trunc.pdf"))
    plot(model)
    dev.off()
    
    Resp.Rb[j, 1] <- as.character(date)
    Resp.Rb[j, 2] <- as.character(run)
    Resp.Rb[j, 3] <- as.character(samp[j + (i - 1) * ncol(Resp.Data.N)])
    Resp.Rb[j, 4] <- as.character(rename[j])
    Resp.Rb[j, 5:6] <- model$allRegs[i, c(4, 5)]
  }
  
  return(Resp.Rb)
}
