#### Respiration Data for Functional Trait Diversity ####

#### Source code from: https://github.com/daniellembecker/Nutrient_sediment_loading_affect_coral_functionality/blob/master/Respirometry/Scripts/rates.R

rm(list=ls())

### Install packages
if ("devtools" %in% rownames(installed.packages()) == 'FALSE') install.packages('devtools') 
library(devtools)
if ("segmented" %in% rownames(installed.packages()) == 'FALSE') install.packages('segmented') 
if ("plotrix" %in% rownames(installed.packages()) == 'FALSE') install.packages('plotrix') 
if ("gridExtra" %in% rownames(installed.packages()) == 'FALSE') install.packages('gridExtra') 
if ("LoLinR" %in% rownames(installed.packages()) == 'FALSE') install_github('colin-olito/LoLinR') 
if ("lubridate" %in% rownames(installed.packages()) == 'FALSE') install.packages('lubridate') 
if ("chron" %in% rownames(installed.packages()) == 'FALSE') install.packages('chron') 
if ("plyr" %in% rownames(installed.packages()) == 'FALSE') install.packages('plyr') 
if ("tidyverse" %in% rownames(installed.packages()) == 'FALSE') install.packages('tidyverse') 


### Read in required libraries

# Include Versions of libraries
#install_github('colin-olito/LoLinR')
library(segmented)
library(plotrix)
library(gridExtra)
library(LoLinR)
library(lubridate)
library(chron)
library(plyr)
library(tidyverse)
library(here)


### Specify file path

path.p <- here("Data","Rosie_Trial_Data","Respiration")


### Bring in respiration files

# list all csv file names in the folder and subfolders
# basename removes the subdirectory name from the file
file.names <- basename(list.files(path = path.p, pattern = "csv$", recursive = TRUE)) 

file.names.full<-list.files(path = path.p, pattern = "csv$", recursive = TRUE) 


### Generate a 3 column dataframe with specific column names and column types
Respo.R <- tibble("assemb.ID" = as.character(),
                 "Intercept" = as.numeric(),
                 "umol.L.sec" = as.numeric(),
                 "Temp.C" = as.numeric())

############################
### DATA MANAGEMENT ###
############################

### MEASUREMENT, VOLUME, AND WEIGHT DATA ###

### Load Organism measurement file, Volume Chamber file, and weights file
measure<-read_csv(here("Data","Rosie_Trial_Data","Org_Measurements.csv"))
volume<-read_csv(here("Data","Rosie_Trial_Data","Respo_RunTime_and_Volume.csv"))
afdw<-read_csv(here("Data","Rosie_Trial_Data","AFDW.csv"))

# # Calculate Dry weights and AFDW and remove outliers
# afdw<-afdw %>% 
#   mutate(dry = Dry.weight.wPan.g - Pan.weight.g, # calculate wihtout pan weight
#          ashfree = afdw.wPan.g - Pan.weight.g) %>% 
#   filter(Species.ID != 'G7_MytilusB' & # remove outliers; will back-calculate later
#            Species.ID != 'G10_Tegula' &
#            Species.ID != 'G8_Tegula' &
#            Species.ID != 'G4_Tegula' & # removal bumps r2 to 0.7999 with 4 points
#            Species.ID != 'G9_Tegula' & # removal bumps r2 to 0.9763 with 3 points
#            Species.ID != 'G5_Acanthina') # 7 points remaining
# 
# # View remaining raw data ashfree ~ dry per species
# afdw %>% 
#   drop_na() %>% 
#   ggplot(aes(x = dry, y = ashfree)) +
#   geom_point() +
#   facet_wrap(~Species, scales = "free")
# 
# ### Calculate Ashfree Dry weight
# 
# # Mytilus
# afdw.M <- afdw %>% 
#   filter(Species == 'Mytilus') %>% 
#   drop_na()
# afdw.M %>% 
#   ggplot(aes(x = dry, y = ashfree)) +
#   geom_point() +
#   geom_smooth(method = "lm")
# M.model <- lm(afdw.M$ashfree ~ afdw.M$dry)
# summary(M.model)$r.squared # Rsquared = 0.9625
# afdw.M<-afdw %>% # calculate ash free dry weights
#   filter(Species == "Mytilus") %>% 
#   mutate(b = M.model$coefficients[1],
#          m = M.model$coefficients[2])
# 
# 
# # Acanthina
# afdw.A <- afdw %>% 
#   filter(Species == "Acanthina") %>% 
#   drop_na()
# afdw.A %>% 
#   ggplot(aes(x = dry, y = ashfree)) +
#   geom_point() +
#   geom_smooth(method = "lm", formula = y ~ log(x))
# A.model <- lm(afdw.A$dry ~ log(afdw.A$ashfree))
# summary(A.model)$r.squared # one outlier removed; Rsquared = 0.7699
# afdw.A<-afdw %>% # calculate ash free dry weights
#   filter(Species == "Acanthina") %>% 
#   mutate(b = A.model$coefficients[1],
#          m = A.model$coefficients[2])
# 
# 
# # Tegula
# afdw.T <- afdw %>% 
#   filter(Species == "Tegula") %>% 
#   drop_na()
# afdw.T %>% 
#   ggplot(aes(x = dry, y = ashfree)) +
#   geom_point() +
#   geom_smooth(method = "lm")
# T.model <- lm(afdw.T$dry ~ afdw.T$ashfree)
# summary(T.model)$r.squared # four outliers removed; Rsquared = 0.9763
# afdw.T<-afdw %>% # calculate ash free dry weights
#   filter(Species == "Tegula") %>% 
#   mutate(b = T.model$coefficients[1],
#          m = T.model$coefficients[2])
# 
# 
# afdw <- rbind(afdw.M,afdw.A) %>% 
#   rbind(afdw.T) %>% 
#   mutate(ashfree.calc = m*dry + b)
# View(afdw)
# 
# 
# afdw %>% 
#   drop_na() %>% 
#   ggplot(aes(x = dry, y = ashfree.calc)) +
#   geom_point() +
#   facet_wrap(~Species, scales = "free")

## need to back-calculate dry weights and get remaining afdw's


######################################
###### SURFACE AREA and WEIGHTS ######
######################################

### Join dataframes by Species.ID
measure <- measure %>% 
  left_join(afdw) %>% # join dry weights with organism measurements
  mutate(dry.weight.g = Dry.weight.wPan.g - Pan.weight.g, # calculate dry weight
         afdw.g = afdw.wPan.g - Pan.weight.g) %>%  # calculate ash free dry weight (as able)
  separate(col = 'Species.ID', into = c('Group.ID',NA), sep = "_", remove = F)


### Calculate surface areas

# isolate mytilus, tegula, and acanthinucella rows for distinct SA calculations
# calculate SA for each species (estimates for now)
# join dataframes into one

# mussels
mussel <- measure %>% 
  filter(Species == 'Mytilus') %>% 
  mutate(SA.cm2 = (2*(Length.mm*Width.mm + Length.mm*Height.mm + Width.mm*Height.mm)) / 100) %>% 
  group_by(Group.ID) %>% 
  mutate(wet.weight.total = sum(Weight.g),
         SA.cm2.total = sum(SA.cm2),
         dry.total.g = sum(dry.weight.g),
         Assemblage = 'Mussels',
         Assemblage.ID = 'A5') %>%
  ungroup() %>% 
  select(Species.ID, Group.ID, Assemblage.ID, Assemblage, Species, 
         dry.total.g, SA.cm2.total)


# tegula - herbivorous snail
tegula <- measure %>% 
  filter(Species == 'Tegula') %>% 
  mutate(wet.weight.total = Weight.g,
         radius = (Length.mm + Width.mm)/2,
         SA.cm2.total = ((pi*radius*Height.mm + pi*(radius^2)) / 100),
         dry.total.g = dry.weight.g,
         Assemblage = 'Herbivory Snail',
         Assemblage.ID = 'A7') %>% 
  select(Species.ID, Group.ID, Assemblage.ID, Assemblage, Species, 
         dry.total.g, SA.cm2.total)


# acanthina - predatory snail
acanthina <- measure %>% 
  filter(Species == 'Acanthina') %>% 
  mutate(wet.weight.total = Weight.g,
         radius = (Height.mm + Width.mm)/2,
         SA.cm2.total = (pi*radius*Length.mm + pi*(radius^2)) / 100,
         dry.total.g = dry.weight.g,
         Assemblage = 'Predatory Snail',
         Assemblage.ID = 'A6') %>% 
  select(Species.ID, Group.ID, Assemblage.ID, Assemblage, Species, 
         dry.total.g, SA.cm2.total)


# join dataframes
measure <- mussel %>% 
  full_join(tegula) %>% 
  full_join(acanthina) %>% 
  arrange(Group.ID)


### Sum Measurements for each assemblage

# Combine organism measurements into assemblage totals
A4.data<-measure %>% 
  filter(Assemblage == 'Predatory Snail' | 
           Assemblage == 'Herbivory Snail') %>% 
  group_by(Group.ID) %>% 
  mutate(dry.total.g = sum(dry.total.g),
         SA.cm2.total = sum(SA.cm2.total),
         Assemblage = 'Predatory Snail and Herbivory Snail',
         Assemblage.ID = 'A4') %>% 
  select(-c(Species,Species.ID)) %>% 
  distinct()

A3.data<-measure %>% 
  filter(Assemblage == 'Mussels' |
           Assemblage == 'Herbivory Snail') %>% 
  group_by(Group.ID) %>% 
  mutate(dry.total.g = sum(dry.total.g),
         SA.cm2.total = sum(SA.cm2.total),
         Assemblage = 'Mussels and Herbivory Snail',
         Assemblage.ID = 'A3') %>% 
  select(-c(Species,Species.ID)) %>% 
  distinct()

A2.data<-measure %>% 
  filter(Assemblage == 'Mussels' |
           Assemblage == 'Predatory Snail') %>% 
  group_by(Group.ID) %>% 
  mutate(dry.total.g = sum(dry.total.g),
         SA.cm2.total = sum(SA.cm2.total),
         Assemblage = 'Mussels and Predatory Snail',
         Assemblage.ID = 'A2') %>% 
  select(-c(Species,Species.ID)) %>% 
  distinct()

A1.data<-measure %>% 
  filter(Assemblage == 'Mussels' |
           Assemblage == 'Predatory Snail' |
           Assemblage == 'Herbivory Snail') %>% 
  group_by(Group.ID) %>% 
  mutate(dry.total.g = sum(dry.total.g),
         SA.cm2.total = sum(SA.cm2.total),
         Assemblage = 'Mussels, Predatory Snail, Herbivory Snail',
         Assemblage.ID = 'A1') %>% 
  select(-c(Species,Species.ID)) %>% 
  distinct()

### Join dataframes into one 

measure <- measure %>% 
  select(-c(Species,Species.ID)) %>% 
  full_join(A4.data) %>% 
  full_join(A3.data) %>% 
  full_join(A2.data) %>% 
  full_join(A1.data) %>% 
  arrange(Assemblage.ID)



####################
###### VOLUME ######
####################

# Blank chamber volume = 640mL
# Calculate the volume of water
volume <- volume %>% 
  mutate(run = "Run") %>% # create intermediate character column
  unite(col = Run.ID, run, Run, sep = "", remove = F) %>% 
  select(-run) %>% 
  unite(col = Run.ID, Run.ID, Chamber, sep = "_", remove = F) %>%
  unite(col = Run.ID, Run.ID, ID, sep = "_", remove = F) %>% 
  separate(col = ID, into = c('Group.ID', 'Assemblage.ID'), sep = "_", remove = F) %>% 
  separate(col = Assemblage, into = c('Assemblage',NA), sep="\\(", remove = T) %>% 
  separate(col = Chamber_Volume, into = c("Chamber_Volume",NA), sep = " ", remove = T) %>% 
  mutate(Assemblage = str_trim(Assemblage),
         Chamber_Volume = as.numeric(Chamber_Volume))



# join measurement and volume into single dataframe
full.measures <- full_join(volume, measure)


### RESPIRATION DATA ###

# Create dataframe for respo data to go into
respo.df <- tibble(Date = as.character())

# Read in all datafiles and join together into one super dataframe
for(i in 1:length(file.names)) {
  Run.ID<-file.names[[i]]
  
  file.names.list<-list.files(path.p, pattern = c(Run.ID, "csv$"), recursive = TRUE) #list all csv file names in the folder and subfolders
  
  respo.data <- file.names.list %>%
    purrr::map_dfr(~ readr::read_csv(file.path(path.p, .), skip=1, col_names=T)) # read all csv files at the file path, skipping 1 line of metadata
  
  respo.data<-respo.data %>%
    dplyr::select(Date,Time,Channel,delta_t,Time_Unit,Value,O2_Unit,Temp,Temp_Unit,
           Pressure,Pressure_Unit,Salinity,Salinity_Unit,Error) %>%
    dplyr::mutate(Run.ID=Run.ID) %>%  # add column for file ID
    tidyr::separate(col = 'Run.ID', into = c('Run.ID',NA), sep = "_O2.csv", remove = T) %>% # remove the '.csv'
    tidyr::drop_na()
  
  respo.data <- respo.data %>%
    unite(col = Time, Date, Time, sep = " ")
  respo.data$Time <- as.POSIXct(respo.data$Time, format = "%m/%d/%Y %H:%M:%S", tz = "") + 3*60*60 # add three hours to correct for wrong time
  respo.data <- respo.data %>% 
    separate(col = 'Time', into = c('Date', 'Time'), sep = " ") %>% 
    dplyr::mutate(Date = lubridate::ymd(Date), # convert date from character to date
                  Time = readr::parse_time(Time,format="%H:%M:%S", trim_ws = T)) # convert time from character to time
  
  respo.data$Date <- as.character(respo.data$Date)
  respo.data$Time <- as.character(respo.data$Time)
  respo.data$Channel <- as.numeric(respo.data$Channel)
  respo.data$delta_t <- as.numeric(respo.data$delta_t)
  
  # Add each subsequent file to the dataframe
  respo.df <- respo.df %>%
    dplyr::full_join(respo.data) # save your dataframes into a larger df

}


# Clean up full dataframe
full.respo <- respo.df %>% 
  dplyr::full_join(full.measures, by = 'Run.ID') # join full respiration dataframe with volume dataframe


### WRITE CSV

write.csv(full.respo, here("Output","Rosie_Trial_Output","Full_Respo.csv"))

full.respo <- readr::read_csv(here("Output","Rosie_Trial_Output","Full_Respo.csv")) %>% 
  select(-X1)


##############################################################################################################
##############################################################################################################

############################
### DATA ANALYSIS ###
############################

### PROCESSING RESPIRATION DATA ###

respo.times <- tibble(Time = as.character())

# for every file in list calculate O2 uptake or release rate and add the data to the Photo.R dataframe
for(i in 1:length(file.names.full)) { # for every file in list calculate O2 uptake or release rate and add the data to the Photo.R dataframe
  
  #find the lines in sample info that have the same file name that is being brought it
  FRow<-which(full.respo$Run.ID==strsplit(file.names[i],'_O2.csv'))
  
  # read in the O2 data one by one
#  Respo.Data1 <- purrr::map_dfr(~ readr::read_csv(file.path(path.p, .), skip=1, col_names=T))
  Respo.Data1 <- read_csv(file.path(path.p,file.names.full[i]), skip = 1, col_names =T) # skips the first line
  measure.data1 <- full.respo %>% 
    filter(Run.ID == strsplit(file.names[i],'_O2.csv')) %>% # remove end of file name
    select(Time, Start_Time, End_Time)  # only keep time columns; hms format

  Start_Time <- measure.data1$Start_Time[1]
  End_Time <- measure.data1$End_Time[1]

  Respo.Data1 <- Respo.Data1 %>% 
    unite(col = Time, Date, Time, sep = " ") %>%
    select(Time, Value, Temp)
  Respo.Data1$Time <- as.POSIXct(Respo.Data1$Time, format = "%m/%d/%Y %H:%M:%S", tz = "")
  Respo.Data1 <- na.omit(Respo.Data1)
  Respo.Data1$Time <- Respo.Data1$Time + 3*60*60 #add time to match start and end times 
  Respo.Data1 <- Respo.Data1 %>% 
    separate(col = 'Time', into = c('Date', 'Time'), sep = " ") %>% 
    mutate(Time = readr::parse_time(Time,format="%H:%M:%S", trim_ws = T)) %>%  # convert time from character to time
    select(-Date)
  
  
  # clean up data
  Respo.Data1 <- Respo.Data1 %>% 
    filter(between(Time, Start_Time, End_Time)) %>%   # filter between start and end times
    mutate(Time = as.character(Time))
    
  
  n<-dim(Respo.Data1)[1] # length of full data
  Respo.Data1 <-Respo.Data1[300:n-3,] #start at data point 5 minute in to avoid excess noise from start of run and only keep final 10 minutes
  n<-dim(Respo.Data1)[1] #list length of trimmed data
  Respo.Data1$sec <- (1:n) #set seconds by one from start to finish of run in a new column
  
  respo.times <- respo.times %>% 
    full_join(Respo.Data1) %>% 
    select(-sec)
  
  #Save plot prior to and after data thinning to make sure thinning is not too extreme
  rename <- sub(".csv","", file.names[i]) # remove all the extra stuff in the file name
  
  pdf(here("Output","Rosie_Trial_Output",paste0(rename,"_thinning.pdf"))) # open the graphics device
  
  par(omi=rep(0.3, 4)) #set size of the outer margins in inches
  par(mfrow=c(1,2)) #set number of rows and columns in multi plot graphic
  plot(Value ~ sec, data=Respo.Data1 , xlab='Time (seconds)', ylab=expression(paste(' O'[2],' (',mu,'mol/L)')), type='n', axes=FALSE) #plot (empty plot to fill) data as a function of time
  usr  <-  par('usr') # extract the size of the figure margins
  rect(usr[1], usr[3], usr[2], usr[4], col='grey90', border=NA) # put a grey background on the plot
  whiteGrid() # make a grid
  box() # add a box around the plot
  points(Respo.Data1$Value ~ Respo.Data1$sec, pch=16, col=transparentColor('dodgerblue2', 0.6), cex=1.1)
  axis(1) # add the x axis
  axis(2, las=1) # add the y-axis
  
  # Thin the data to make the code run faster
  Respo.Data.orig <- Respo.Data1 # save original unthinned data
  Respo.Data1 <- thinData(Respo.Data1, by=20)$newData1 # thin data by every 20 points for all the O2 values
  Respo.Data1$sec <- as.numeric(rownames(Respo.Data1 )) # maintain numeric values for time
  Respo.Data1$Temp <- NA # add a new column to fill with the thinned data
  Respo.Data1$Temp <- thinData(Respo.Data.orig,xy = c(1,3), by=20)$newData1[,2] # thin data by every 20 points for the temp values
  
  # plot the thinned data
  plot(Value ~ sec, data=Respo.Data1 , xlab='Time (seconds)', ylab=expression(paste(' O'[2],' (',mu,'mol/L)')), type='n', axes=FALSE) #plot thinned data
  usr  <-  par('usr')
  rect(usr[1], usr[3], usr[2], usr[4], col='grey90', border=NA)
  whiteGrid()
  box()
  points(Respo.Data1$Value ~ Respo.Data1$sec, pch=16, col=transparentColor('dodgerblue2', 0.6), cex=1.1)
  axis(1)
  axis(2, las=1)
  ##Olito et al. 2017: It is running a bootstrapping technique and calculating the rate based on density
  #option to add multiple outputs method= c("z", "eq", "pc")
  Regs  <-  rankLocReg(xall=Respo.Data1$sec, yall=Respo.Data1$Value, alpha=0.5, method="pc", verbose=TRUE)  
  
  # add the regression data
  plot(Regs)
  dev.off()
  
  
  # fill in all the O2 consumption and rate data
  Respo.R[i,2:3] <- Regs$allRegs[1,c(4,5)] #inserts slope and intercept in the dataframe
  Respo.R[i,1] <- rename #stores the file name in the Date column
  Respo.R[i,4] <- mean(Respo.Data1$Temp, na.rm=T)  #stores the Temperature in the Temp.C column
  #Photo.R[i,5] <- PR[j] #stores whether it is photosynthesis or respiration
  
  
  # rewrite the file every time... I know this is slow, but it will save the data that is already run
}

write.csv(respo.times, here("Output","Rosie_Trial_Output","Respo.times.csv"))
write.csv(Respo.R, here("Output","Rosie_Trial_Output","Respo.R.csv"))


respo.times <-readr::read_csv(here("Output","Rosie_Trial_Output","Respo.times.csv"))  %>% 
  select(-X1)
Respo.R <- readr::read_csv(here("Output","Rosie_Trial_Output","Respo.R.csv")) %>% 
  select(-X1)


# Calculate R rate
full.respo <- full.respo %>% # only keep run times
  right_join(respo.times)

Respo.R <- Respo.R %>% 
  rename(Run.ID = 'assemb.ID') %>% # rename column for easier join
  separate(Run.ID, c('Run.ID',NA),sep = "_O2") %>% 
  left_join(full.respo)



# Add blank volumes and standardize by volume
Respo.R <- Respo.R %>% 
  # Add Blank chamber volume of 640mL
  mutate(Chamber_Volume = ifelse(Group.ID == 'Blank', c(Chamber_Volume = 640), Chamber_Volume),
         # Convert sample volume to mL
         Chamber_Volume = Chamber_Volume/1000, # calculate volume
         # Account for chamber volume to convert from umol L-1 s-1 to umol s-1. 
         # This standardizes across water volumes (different because of organism sizes) and removes per Liter
         umol.sec = umol.L.sec * Chamber_Volume)


#Account for blank rate by temperature
#convert character columns to factors
Respo.R <- Respo.R %>%
  mutate_if(sapply(., is.character), as.factor)

#make the blank column a factor
Respo.R <- Respo.R %>% 
  mutate(BLANK = ifelse(Group.ID=='Blank', 1,0),
         BLANK = as.factor(BLANK))


# Aggregate: Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form
respo.blnk <- aggregate(umol.sec ~ Run*Temp.C*BLANK, data=Respo.R, mean)
  
# pull out only the blanks
respo.blnk<-respo.blnk %>% 
  filter(BLANK == 1) %>% 
  group_by(Run) %>% # get average of run 4's two blanks
  mutate(Temp.C = mean(Temp.C),
         umol.sec = mean(umol.sec)) %>%
  distinct() # remove duplicate run 4 data


# remove the blank column
respo.blnk$BLANK<-NULL


respo.blnk<- respo.blnk %>% 
  mutate(blank.rate = umol.sec) %>%   # rename the blank rate 
  select(-c(Temp.C,umol.sec))
  
Respo.R <- Respo.R %>%
  left_join(respo.blnk)   # join the blank data with the rest of the data
  

# subtract the blanks 
Respo.R <- Respo.R %>% 
  mutate(umol.sec.corr = umol.sec - blank.rate)



#####################################
###### NORMALIZE TO DRY WEIGHT ######
#####################################


#Calculate net P and R
Respo.R <- Respo.R %>% 
  mutate(umol.g.hr = (umol.sec.corr*3600)/dry.total.g) #mmol g hr-1

#Respo.R<-Respo.R[complete.cases(Respo.R),] # remove NAs and blanks
Respo.R <- Respo.R %>% 
  filter(Group.ID != 'Blank') # remove Blanks

#make respiration positive
#Respo.R$umol.cm2.hr[Respo.R$PR=='Respiration']<-abs(Respo.R$umol.cm2.hr[Respo.R$PR=='Respiration'])
Respo.R <- Respo.R %>% 
  mutate(umol.g.hr = -umol.g.hr) # multiply column by -1 to make values positive
lessthan <- which(Respo.R$umol.g.hr < 0) # if any values are negative,
Respo.R$umol.g.hr[lessthan] <- 0 # change value to 0

# log the rates
Respo.R <- Respo.R %>% 
  mutate(Rate.ln = log(Respo.R$umol.g.hr+0.1))


ggplot(Respo.R, aes(x=Temp.C, y=umol.g.hr,group = Group.ID, col = Group.ID)) +
  geom_line(size=2) +
  geom_point() +  
  #ylim(0,1.5) +  
  facet_wrap(~ Assemblage.ID, labeller = labeller(.multi_line = FALSE)) +
  ggsave(filename = here("Output","Rosie_Trial_Output","Respo_curves.png"), device = "png", width = 10, height = 10)

write.csv(Respo.R, here("Output","Rosie_Trial_Output","TT_Rates.csv")) # export all the uptake rates
View(Respo.R)







