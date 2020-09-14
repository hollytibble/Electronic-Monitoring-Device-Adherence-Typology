########################################################################################
#      Preamble
########################################################################################

rm(list=ls()) # remove all variables from workspace
library(readxl)
library(plyr)
library(dplyr)
library(foreign)
library(data.table)
library(reshape2)
library(tidyr)
library(ggplot2)
library(corrplot)
library(stringr)
library(fpc)
library(rpart)
library(lubridate)
library(openxlsx)
library(varhandle)
Sys.setenv(TZ = "Pacific/Auckland")
set.seed(12345)

########################################################################################
#      Importing Data
########################################################################################

# Patients withdrawn from the trial
Patients_withdrawn_from_trial <- read_excel("atients withdrawn from trial.xls")
Patients_withdrawn_from_trial <- as.numeric(Patients_withdrawn_from_trial[[1]])
Patients_withdrawn_from_trial <- Patients_withdrawn_from_trial[!is.na(Patients_withdrawn_from_trial)]

# Appending all patient datasets
data <- read_excel("Preventer results_001.xlsx")[,1:4]
data<-data[!(is.na(data$StudyID)),]
for(i in 2:9) {
  if(i %in% Patients_withdrawn_from_trial) next
  temp <- read_excel(paste("Preventer results_00",i,".xlsx", sep=""))[,1:4]
  temp<-temp[!(is.na(temp$StudyID)),]
  data <- rbind(data, temp)
}
for(i in 10:99) {
  if(i %in% Patients_withdrawn_from_trial) next
  temp <- read_excel(paste("Preventer results_0",i,".xlsx", sep=""))[,1:4]
  temp<-temp[!(is.na(temp$StudyID)),]
  if(i==14) temp<-temp[!(is.na(temp$`Smartinhaler ID`)),]
  data <- rbind(data, temp)
}
for(i in 100:220) {
  if(i %in% Patients_withdrawn_from_trial) next
  temp <- read_excel(paste("Preventer results_",i,".xlsx", sep=""))[,1:4]
  temp<-temp[!(is.na(temp$StudyID)),]
  data <- rbind(data, temp)
}
rm(temp,i)

# Rename the variables, and take out the records of events not needed in this analysis
names(data)[names(data)=="Event Date & Time (dd/MM/yyyy hh:mm:ss)"] <- "event_time"
data <- data[!(data$Event=="Power Up" | 
                 data$Event=="Battery Replaced" | 
                 data$Event=="Battery Removed" | 
                 data$Event=="Inhaler Out" |
                 data$Event=="Logging Event 48" |
                 data$Event=="PMDI(Dose Counter) Installed" |  
                 data$Event=="PMDI(Non Dose Counter) Installed" |  
                 data$Event=="Last day of visit"),]
data$Event<-ifelse(data$Event=="Medication","Medication","Tone")
data$`Smartinhaler ID`<-NULL

# check times of doses
round(table(substr(unlist(data[which(data$Event=="Tone"),"event_time"]), 12, 13))*100/sum(data$Event=="Tone"),2)

# process time
data$event_time <- lubridate::as_datetime(data$event_time, format = "%d/%m/%Y %H:%M:%S")
data$event_time <-as.POSIXct(data$event_time,tz="Pacific/Auckland")


# Adding in baseline visit and final visit dates
dates<-read_excel("HollySummary reliever results_TOTALcapped_summary.xls")[,c(1,3:4)]
dates<- dates[-which(as.numeric(dates$StudyID) %in% c(Patients_withdrawn_from_trial)),]
# start dates
dates$first_day <-convertToDateTime(as.numeric(dates$`Baseline visit date`),tz="Pacific/Auckland")
dates$first_day[is.na(dates$first_day)]<-lubridate::as_datetime(dates$`Baseline visit date`,format = "%d/%m/%Y",tz="Pacific/Auckland")[is.na(dates$first_day)]
# end dates 
dates$last_day<-convertToDateTime(as.numeric(dates$`Final visit date`),tz="Pacific/Auckland")
dates$last_day[is.na(dates$last_day)]<-lubridate::as_datetime(dates$`Final visit date`,format = "%d/%m/%Y",tz="Pacific/Auckland")[is.na(dates$last_day)]
# drop unformatted date variables
dates$`Baseline visit date` <- dates$`Final visit date` <- NULL
# merge into the main dataset
data<-left_join(data,dates)
rm(dates)

### Adding in the trial arm information
data <- merge(data, read_excel("Trial Arms.xlsx"), sort = TRUE)
names(data)[names(data)=="Randomised group"] <- "group"

### creating a variable indicating patient-specific study day
data$studyday<-as.numeric(ceiling(difftime(data$event_time,data$first_day, unit="days")))
# one record with an actuation 11 years before study start - inhaler time error.  removed record
# 11 records of tone/medicine the day before the device was distributed - study team said this was likely a demo
data<-data[which(data$studyday>0),]
# 4 records removed after device was returned 
data<-data[which(as.Date(data$event_time)<=data$last_day),]

### creating a dose AM / PM indicator variable
data$dose_time<- ifelse(as.numeric(strftime(data$event_time, format="%H")) < 12, "AM", "PM")

# Add in the number of days in the study
data$retention <- as.integer(difftime(data$last_day,data$first_day, unit="days"))+1

# Create a shell of dose information, so that missing information is recorded
shell<-data %>%
  group_by(StudyID) %>%
  dplyr::select(StudyID,first_day,last_day,group,retention) %>%
  slice(1) 
shell<-setDT(shell)[ , list(StudyID = StudyID, retention = retention, group=group, first_day=first_day, last_day = last_day, 
                            date = seq(first_day, last_day, by = "days")), by = 1:nrow(shell)]
# double check that we have the correct number of rows per person
# check <- shell %>%
#   add_count(StudyID)
# table(check$retention==check$n)
# rm(check)
shell$studyday<-as.numeric(difftime(shell$date,shell$first_day, units = "days"))+1
shell$date<-shell$nrow<-NULL
shell<-rbind(shell,shell)[order(StudyID,studyday),]
shell$dose_time<-rep(c("AM","PM"),nrow(shell)/2)

### counting the number of doses at dose-time
temp <- data %>%
  filter(Event == "Medication") %>%
  group_by(StudyID,studyday,dose_time) %>%
  arrange(StudyID,studyday,dose_time,event_time) %>%
  mutate(Freq = n()) %>%
  slice(1) %>%
  dplyr::select(StudyID,studyday,dose_time,Freq,Event)
shell<-left_join(shell,temp)
rm(temp)
# take out the AM if it's missing on studyday 1 because they might not have got it until the afternoon
shell<-shell[-which(shell$studyday==1 & shell$dose_time=="AM" & is.na(shell$Freq)),]
# take out the PM if it's missing on the last studyday because they might have returned the device in the morning
shell<-shell[-which(shell$studyday==shell$retention & shell$dose_time=="PM" & is.na(shell$Freq)),]
# change missing events to 'medication missed' and frequencies to zero
shell$Event<-replace_na(shell$Event,"Medication Missed")
shell$Freq<-replace_na(shell$Freq,0)

### Time to first dose at dose-time (intervention arm only; in seconds)
# when did the tone play?
time_of_tone<- data %>%
  filter(group=='Intervention' & Event=="Tone")    %>%
  group_by(StudyID,studyday,dose_time) %>%
  slice(1) %>%
  rename(tone_time = event_time) %>%
  dplyr::select(StudyID,studyday,dose_time,tone_time)
# affix hour of tone to shell
time_of_tone$hour_of_tone<-as.numeric(strftime(time_of_tone$tone_time, format="%H"))
shell<-left_join(shell,time_of_tone)

# affix to data, to create time to dose
time_of_tone<-left_join(data[which(data$Event=="Medication"),],time_of_tone)
time_of_tone <- time_of_tone %>% rename(med_time = event_time)
time_of_tone$time_to_dose <- as.numeric(difftime(time_of_tone$med_time,time_of_tone$tone_time))
time_of_tone$time_to_dose[time_of_tone$time_to_dose<0] <- NA  
time_of_tone$tone_time<-time_of_tone$hour_of_tone<-NULL
time_of_tone <- time_of_tone %>% 
  group_by(StudyID,studyday,dose_time) %>% 
  mutate(time_to_dose = ifelse(sum(!is.na(time_to_dose))>0,min(time_to_dose, na.rm=T),NA)) %>% 
  slice(1)
# merge into the shell
data<-left_join(shell,dplyr::select(time_of_tone,-c("med_time","first_day","last_day")))

# surv_event - NA time_to_tone without tone_time means no tone
data$surv_event<-ifelse(data$group=="Intervention" & data$Event=="Medication" & !is.na(data$hour_of_tone) & !is.na(data$time_to_dose),1,
                        ifelse(data$group=="Intervention" & data$Event=="Medication Missed" & !is.na(data$hour_of_tone) & is.na(data$time_to_dose),0,NA))
data$time_to_dose_x<-ifelse(data$surv_event==0,
                            60*60*2,
                            ifelse(data$time_to_dose>=60*60*2,
                                   60*60*2,
                                   data$time_to_dose)) 
rm(time_of_tone,shell)

data_missing<-data[,c("StudyID","first_day","last_day","retention","group")] %>% group_by(StudyID) %>% slice(1)


########################################################################################
#      Clean-up Dataset of Missingness due to device fault
########################################################################################

missing <- read_excel("/Volumes/UCL Asthma Data/Missing data record_SAS format.xlsx")

names(missing)[1] <- "StudyID"
names(missing)[2] <- "period1"
names(missing)[3] <- "period2"
names(missing)[4] <- "period3"
names(missing)[5] <- "period4"
names(missing)[6] <- "period5"
names(missing)[7] <- "period6"
names(missing)[8] <- "period7"
names(missing)[9] <- "period8"
names(missing)[10] <- "period9"
names(missing)[11] <- "period10"

# drop header row
missing <- missing[which(missing$StudyID!="Participant ID" & !is.na(missing$StudyID)),]

# reshape data to long by period
missing <- melt(missing,id=c("StudyID"))

# drop if period is NA and then drop period
missing <- missing[which(!is.na(missing$value)),]
missing$variable<-NULL

# drop the people who were dropped from the study
missing<-inner_join(data_missing,missing)

# make the format consistent - always otherwise begins with a number
missing$value[missing$value=="Discontinued after 10/11"]<-"10/11 - "

# get rid of cross-over periods, which will lead to duplicates
missing$value[missing$value=="23/12 -10/3" & missing$StudyID=="027"]<-"24/12 -10/3"
missing$value[missing$value=="15/2 - 12/4 (missing device)" & missing$StudyID=="035"]<-"16/2 - 12/4"
missing$value[missing$value=="15/8-14/10 (missing w manufacturer)" & missing$StudyID=="076"]<-"16/8-14/10"

# split period into start and end
missing$start<-substr(missing$value,1,ifelse(regexpr(' ', missing$value)==-1 | regexpr(' ', missing$value)>8,regexpr('-', missing$value),regexpr(' ', missing$value))-1)
missing$end<-substr(missing$value,ifelse(regexpr(' ', missing$value)==-1 | regexpr(' ', missing$value)>8,regexpr('-', missing$value),regexpr(' ', missing$value))-1+1,100000)
# remove first '-' 
missing$end<-substr(missing$end,regexpr('-', missing$end)+1,100000)
# remove 'to'
missing$end<-substr(missing$end,regexpr('to', missing$end)+2,100000)
# remove the spaces from the start
missing$end<-substr(missing$end,ifelse(regexpr(' ', missing$end)==1,2,1),100000)
# remove times from the end
missing$end<-substr(missing$end,1,ifelse(regexpr(' ', missing$end)>0,regexpr(' ', missing$end),1000000))
# remove the spaces from the end
missing$end<-substr(missing$end,1,ifelse(regexpr(' ', missing$end)>0,regexpr(' ', missing$end)-1,100000))
# if all that's left is a time change it to blank
missing$end[regexpr(':', missing$end)!=-1]<-""
# if the month is missing from the start date set it to the month of the end date
missing$start<-ifelse(regexpr('/', missing$start)==-1,paste(missing$start,substr(missing$end,regexpr('/', missing$end),1000),sep=""),missing$start)
# make end equal to start if blank
missing$end<-ifelse(missing$end=="",missing$start,missing$end)

# convert start into an actual date
# was the year missing?
missing$startx<-as.Date.character(missing$start,format = "%d/%m/%y")
# if it was, was it the first study year?
missing$temp<-ifelse(as.Date.character(paste(missing$start,format(missing$first_day,"%Y"),sep="/"),format = "%d/%m/%Y")>=missing$first_day 
                     & as.Date.character(paste(missing$start,format(missing$first_day,"%Y"),sep="/"),format = "%d/%m/%Y")<=missing$last_day,
                     as.Date.character(paste(missing$start,format(missing$first_day,"%Y"),sep="/"),format = "%d/%m/%Y"),
                     NA)
missing$startx<-as.Date(ifelse(!is.na(missing$temp),
                               missing$temp,
                               missing$startx),origin = "1970-01-01")
missing$temp<-NULL
# or the second study year?
missing$temp<-ifelse(as.Date.character(paste(missing$start,as.numeric(format(missing$first_day,"%Y"))+1,sep="/"),format = "%d/%m/%Y")>=missing$first_day 
                     & as.Date.character(paste(missing$start,as.numeric(format(missing$first_day,"%Y"))+1,sep="/"),format = "%d/%m/%Y")<=missing$last_day,
                     as.Date.character(paste(missing$start,as.numeric(format(missing$first_day,"%Y"))+1,sep="/"),format = "%d/%m/%Y"),
                     NA)
missing$startx<-as.Date(ifelse(!is.na(missing$temp),
                               missing$temp,
                               missing$startx),origin = "1970-01-01")
missing$temp<-NULL

# convert end into an actual date
missing$endx<-as.Date.character(missing$end,format = "%d/%m/%y")
# if it was, was it the first study year?
missing$temp<-ifelse(as.Date.character(paste(missing$end,format(missing$first_day,"%Y"),sep="/"),format = "%d/%m/%Y")>=missing$first_day 
                     & as.Date.character(paste(missing$end,format(missing$first_day,"%Y"),sep="/"),format = "%d/%m/%Y")<=missing$last_day,
                     as.Date.character(paste(missing$end,format(missing$first_day,"%Y"),sep="/"),format = "%d/%m/%Y"),
                     NA)
missing$endx<-as.Date(ifelse(!is.na(missing$temp),
                             missing$temp,
                             missing$endx),origin = "1970-01-01")
missing$temp<-NULL
# or the second study year?
missing$temp<-ifelse(as.Date.character(paste(missing$end,as.numeric(format(missing$first_day,"%Y"))+1,sep="/"),format = "%d/%m/%Y")>=missing$first_day 
                     & as.Date.character(paste(missing$end,as.numeric(format(missing$first_day,"%Y"))+1,sep="/"),format = "%d/%m/%Y")<=missing$last_day,
                     as.Date.character(paste(missing$end,as.numeric(format(missing$first_day,"%Y"))+1,sep="/"),format = "%d/%m/%Y"),
                     NA)
missing$endx<-as.Date(ifelse(!is.na(missing$temp),
                             missing$temp,
                             missing$endx),origin = "1970-01-01")
missing$temp<-NULL

# one erroneous value to be hard coded
missing$endx[is.na(missing$endx)]<-missing$last_day[is.na(missing$endx)]
missing$startx[is.na(missing$startx)]<-missing$first_day[is.na(missing$startx)]

# assert no more duplicates
nrow(missing)==nrow(unique(missing))

# clean up dataset for merging
missing<-missing[,c("StudyID","startx","endx","first_day","last_day","retention","group")]
# modify to Long -  patients, day, time and event
missing<-setDT(missing)[ , list(StudyID = StudyID, retention=retention, first_day=first_day,last_day=last_day,group=group,
                                day = seq(startx, endx, by = "day")), 
                         by = 1:nrow(missing)]


missing<-as.data.frame(missing)
missing$day<-lubridate::as_datetime(as.character(missing$day),format = "%Y-%m-%d",tz="Pacific/Auckland")
missing$studyday<-round(as.numeric(difftime(missing$day,missing$first_day, units = "days"))+1)  # sometimes there is an extra hour due to DST
missing$nrow<-missing$day<-NULL
missing<-missing[rep(1:nrow(missing),each=2),] 
missing$dose_time<-rep(c("AM","PM"),nrow(missing)/2)
missing$Event<-"Data Missing"
rm(data_missing)

########################################################################################
#     Combining Study Data and Missingness Data
########################################################################################

# Observe when the inhalers weren't working
# then remove these observations from the study data
# rbind the data and the missing dates
data<-rbind.fill(data,missing)

# sort levels of event so that 'data missing' comes first, as this was sometimes included when data quality unreliable
data$Event<-factor(data$Event,levels=c("Data Missing","Medication","Medication Missed"))
data<-data[order(data$StudyID,data$studyday,data$dose_time,data$Event),]
data<- data %>% arrange(StudyID,studyday,dose_time,Event) %>% 
  group_by(StudyID,studyday,dose_time) %>% 
  slice(1) %>%
  group_by()
# check now unique by id, study day, dose_time
nrow(unique(data[,c("StudyID","studyday","dose_time")]))==nrow(data)
# drop the missing dates dataset
rm(missing)

# generate an indicator next date of missingness, for censoring
missing_period_starts <- data %>%
  filter(Event=="Data Missing" & dose_time=="AM") %>%
  group_by(StudyID) %>%
  arrange(StudyID,studyday) %>%
  dplyr::select(StudyID,studyday) %>%
  group_by(StudyID) %>%
  mutate(temp = (studyday-lag(studyday))!=1 | is.na(lag(studyday))) %>% # first one by ID, or non-consecutive
  filter(temp==TRUE) %>% # only need to keep the start day of each period of missing data
  rename(censor = studyday) %>%
  dplyr::select(-temp) %>%
  mutate(censor_start = paste("censor",row_number(),sep="_")) %>%  # index of period by person
  ungroup 

missing_period_starts<-spread(missing_period_starts,censor_start,censor)

data<-left_join(data,missing_period_starts)
rm(missing_period_starts)

data$censor_next<-ifelse(is.na(data$censor_1),NA,
                         ifelse(data$censor_1>data$studyday,data$censor_1,
                                ifelse(data$censor_2>data$studyday,data$censor_2,
                                       ifelse(data$censor_3>data$studyday,data$censor_3,
                                              ifelse(data$censor_4>data$studyday,data$censor_4,
                                                     ifelse(data$censor_5>data$studyday,data$censor_5,
                                                            ifelse(data$censor_6>data$studyday,data$censor_6,
                                                                   ifelse(data$censor_7>data$studyday,data$censor_7,
                                                                          ifelse(data$censor_8>data$studyday,data$censor_8,
                                                                                 NA)))))))))
data<-dplyr::select(data,-c("censor_1","censor_2","censor_3","censor_4","censor_5","censor_6","censor_7","censor_8"))

########################################################################################
#      Add in patient information
########################################################################################

#  adding in the demographic and outcome data
data_wu <- read.spss("/Volumes/UCL Asthma Data/HollyResults analysis 280515.sav", to.data.frame=TRUE)
data_wu <- data_wu[c("StudyID",
                     "Age",
                     "Gender",
                     "EthnicityDescript",
                     "DeprivationScale",
                     "Weight",
                     "HeightMeasure",
                     "AgeAtDiagnosis",
                     "FU1DateVisit",
                     "FU1NumberOfAttacks",
                     "FU1DateExacerbation1",
                     "FU1DateExacerbation2",
                     "FU1DateExacerbation3",
                     "FU2DateVisit",
                     "FU2NumberOfAttacks",
                     "FU2DateExacerbation1",
                     "FU2DateExacerbation2",
                     "FU2DateExacerbation3",
                     "FinalDateVisit",
                     "FinalNumberOfAttacks",
                     "FinalDateExacerbation1",
                     "FinalDateExacerbation2",
                     "FinalDateExacerbation3",
                     "BLHowManyAttacksLast12Months",
                     "BLTotalMS",
                     "BLACTTotal",
                     "BLPercentFEV1",
                     "BLFEV1FVCratio",
                     "FU1ACTTotal",
                     "FU1PercentFEV1",
                     "FU1FEV1FVCratio",
                     "FU2ACTTotal",
                     "FU2PercentFEV1",
                     "FU2FEV1FVCratio",
                     "FinalTotalMS",
                     "FinalACTTotal",
                     "FinalPercentFEV1",
                     "FinalFEV1FVCratio", 
                     "ProportionOfDaysNeedingReliever",
                     "ProportionDaysOffSchoolAsthma")]
data_wu$FU1DateVisit<-as.POSIXct(data_wu$FU1DateVisit,origin = "1582-10-14") # Beginning of the Julian Calendar. SPSS is weird.  
data_wu$FU1DateExacerbation1<-as.POSIXct(data_wu$FU1DateExacerbation1,origin = "1582-10-14") # Beginning of the Julian Calendar. SPSS is weird.  
data_wu$FU1DateExacerbation2<-as.POSIXct(data_wu$FU1DateExacerbation2,origin = "1582-10-14") # Beginning of the Julian Calendar. SPSS is weird.  
data_wu$FU1DateExacerbation3<-as.POSIXct(data_wu$FU1DateExacerbation3,origin = "1582-10-14") # Beginning of the Julian Calendar. SPSS is weird.  
data_wu$FU2DateVisit<-as.POSIXct(data_wu$FU2DateVisit,origin = "1582-10-14") # Beginning of the Julian Calendar. SPSS is weird.  
data_wu$FU2DateExacerbation1<-as.POSIXct(data_wu$FU2DateExacerbation1,origin = "1582-10-14") # Beginning of the Julian Calendar. SPSS is weird.  
data_wu$FU2DateExacerbation2<-as.POSIXct(data_wu$FU2DateExacerbation2,origin = "1582-10-14") # Beginning of the Julian Calendar. SPSS is weird.  
data_wu$FU2DateExacerbation3<-as.POSIXct(data_wu$FU2DateExacerbation3,origin = "1582-10-14") # Beginning of the Julian Calendar. SPSS is weird.

# categorise age 
data_wu$Age_Group<-NA
data_wu$Age_Group[data_wu$Age <9] = "6-8"
data_wu$Age_Group[data_wu$Age>8 & data_wu$Age<12] = "9-11"
data_wu$Age_Group[data_wu$Age>11] = "12-15"
data_wu$Age_Group = factor(data_wu$Age_Group, levels=c("6-8", "9-11", "12-15"))

# categorise deprivation
data_wu$Deprivation_Group<-NA
data_wu$Deprivation_Group[data_wu$DeprivationScale<4]="1-3"
data_wu$Deprivation_Group[data_wu$DeprivationScale<8 & data_wu$DeprivationScale>5]="4-7"
data_wu$Deprivation_Group[data_wu$DeprivationScale>7]="8-10"
data_wu$Deprivation_Group = factor(data_wu$Deprivation_Group, levels=c("1-3","4-7","8-10"))

# change missing ethnicity to 'other' to align with Amy's Lancet paper
data_wu$EthnicityDescript[data_wu$EthnicityDescript=="                    "]<-"Other               "

# Convert ProportionOfDaysNeedingReliever from factor to numeric
data_wu$ProportionOfDaysNeedingReliever<-unfactor(data_wu$ProportionOfDaysNeedingReliever)

#  replace NAs in FinalNumberOfAttacks
data_wu$FinalNumberOfAttacks<-replace_na(data_wu$FinalNumberOfAttacks,0)

# Keep only the people in both datasets in data_wu
data_wu<- data_wu[-which(data_wu$StudyID %in% c(Patients_withdrawn_from_trial)),]
rm(Patients_withdrawn_from_trial)

# merge demogrpahic information into the main dataset
data$StudyID<-as.numeric(data$StudyID)
data<-left_join(as.data.frame(data), data_wu)
data_wu<-left_join(data_wu,data[,c("StudyID","first_day","last_day","retention")]) %>% group_by(StudyID) %>% slice(1) %>% ungroup

########################################################################################
#      Initiation
########################################################################################

### It could be either their first medication, or first period of missingness - if that came first
initiation<- data %>%
  filter(Event %in% c("Medication","Data Missing")) %>%
  arrange(StudyID,studyday,dose_time) %>%
  group_by(StudyID) %>%
  slice(1) %>%
  mutate(first_med = ifelse(Event=="Data Missing",NA,studyday)) %>%
  dplyr::rename(initiation_event = Event) %>%
  dplyr::select(c("StudyID","first_med","initiation_event")) 

########################################################################################
#      Shell Setup
########################################################################################

### Get rid of all missing data records
data<-data[which(data$Event!="Data Missing"),]

# Work out how many days of non-missing data each person has (in total, and by month)
data<-data %>% 
  mutate(month = lubridate::month(first_day + days(studyday - 1))) %>% 
  group_by(StudyID,month) %>% 
  mutate(non_missing_days_month = max(row_number())/2) %>%
  group_by(StudyID) %>% 
  mutate(non_missing_days = max(row_number())/2) %>%
  ungroup()

# Create a shell to affix adherence measures to
Adherence <- data %>% 
  dplyr::select(StudyID,group,retention,non_missing_days) %>% 
  group_by(StudyID) %>% 
  slice(1)

# # Create a shell to affix adherence measures to for Ad hoc analysis
# Adherence_s <- data %>% 
#   dplyr::select(StudyID,group,retention,month,non_missing_days_month) %>% 
#   group_by(StudyID, month) %>% 
#   slice(1)

Adherence<-left_join(Adherence, initiation)
rm(initiation)

########################################################################################
#      Implementation
########################################################################################

### Time to dose
survival<-data[which(data$group=="Intervention"), ]

### Day-level (looking at dose times split by morning and evening)
# daily rate of taking morning, evening, both or neither doses ***across cohort***
summary_implementation <- data %>%
  dplyr::select(c("StudyID","studyday","dose_time","Freq","retention","non_missing_days")) %>%
  gather(variable, value, c(Freq)) %>%
  unite(temp, dose_time, variable) %>%
  spread(temp,value) %>%
  # drop the first day and the last day
  filter(studyday!=1 & studyday!=retention) %>%
  # add neither, both, Am or PM indicator
  mutate(day_bin = ifelse(AM_Freq==0 & PM_Freq==0,"neither",
                          ifelse(AM_Freq>=1 & PM_Freq==0, "AM only",
                                 ifelse(AM_Freq==0 & PM_Freq>=1,"PM only",
                                        "both")))) 

daily_implementation<-summary_implementation

### Day-level - want to know how behaviour changes across the whole population by time since initiation
summary_implementation<-summary_implementation %>%
  group_by(studyday) %>%
  dplyr::mutate(day_count=n()) %>%
  group_by(studyday,day_bin) %>%
  dplyr::mutate(category_day_count=n()) %>%
  arrange(studyday,day_bin) %>%
  slice(1) %>%
  dplyr::mutate(category_day_pct = 100*category_day_count/day_count) 
summary_implementation$day_bin<-as.factor(summary_implementation$day_bin)
summary_implementation$day_bin<-factor(summary_implementation$day_bin,
                                       levels=c("neither","AM only","PM only","both"))
summary_implementation<-dplyr::select(summary_implementation,-c("StudyID","AM_Freq","PM_Freq","non_missing_days"))


### Person-level
daily_implementation<- daily_implementation %>%
  mutate(day_freq = AM_Freq + PM_Freq) %>%
  group_by(StudyID) %>%
  #mutate(count_days_zero = sum(day_bin=="neither")) %>%
  mutate(perc_doses = (sum(AM_Freq>0)+sum(PM_Freq>0))*100/(2*non_missing_days)) %>%  # number of doses with SOME taken, over number of dose-times
  mutate(perc_days_zero = sum(day_bin=="neither")*100/non_missing_days) %>%
  mutate(perc_both = sum(day_bin=="both")*100/non_missing_days) %>%
  #mutate(index = ifelse(day_bin!="neither",day_bin,NA)) %>%
  #mutate(index_days = sum(!is.na(index))) %>%
  #mutate(perc_both_non_zero = sum(day_bin=="both",na.rm=T)*100/index_days) %>%
  slice(1) %>%
  dplyr::select(-c("studyday","AM_Freq","PM_Freq","day_freq","day_bin"))

### Little logic check we run, taking both daily doses is a more stringent criteria than % of doses taken
#table(daily_implementation$perc_both<=daily_implementation$perc_doses)
Adherence<-left_join(Adherence,daily_implementation)
rm(daily_implementation)


########################################################################################
#      Persistence
########################################################################################

persistence <- data %>%
  dplyr::select(StudyID,studyday,dose_time,Freq,Event,retention, non_missing_days) %>%
  group_by(StudyID) %>%
  # fifth_day_zero is true when the last 10 consecutive doses have been missed (5 days)
  #(fifth_day_zero = 10<=((Freq==0)*sequence(rle(as.character(Freq==0))$lengths))) %>%  
  # how many days (0.5 days = 1 dose) in a row have they not taken any medicine
  mutate(holiday_dur = (Freq==0)*sequence(rle(as.character(Freq==0))$lengths)/2) %>%  
  mutate(holiday_start_day = ifelse(holiday_dur>0 & dose_time=="AM",studyday-floor(holiday_dur),
                                    ifelse(holiday_dur>0 & dose_time=="PM",studyday-floor(holiday_dur-0.5),
                                           NA))) %>%
  #mutate(holiday_censored = ifelse(row_number()==max(row_number()) & Event=="Medication Missed",1,NA)) %>%
  filter(holiday_dur>=5) %>%  
  mutate(first_holiday_start = as.numeric(min(holiday_start_day))) %>%
  mutate(first_holiday_dur = as.numeric(max(holiday_dur*(holiday_start_day==first_holiday_start)))) %>%
  mutate(number_of_holidays = length(unique(holiday_start_day))) %>%
  mutate(number_of_holidays_over_nmdays = length(unique(holiday_start_day))*100/non_missing_days) %>%
  mutate(max_holiday_dur_over_nmdays = max(holiday_dur)*100/non_missing_days) %>% 
  group_by(StudyID,holiday_start_day) %>%
  slice(n())  %>%
  group_by(StudyID) %>%
  mutate(total_holiday_dur = sum(holiday_dur)) %>%
  mutate(total_holiday_dur_over_nmdays = sum(holiday_dur)*100/non_missing_days) %>% 
  dplyr::select(StudyID, number_of_holidays_over_nmdays, total_holiday_dur,
                first_holiday_start,first_holiday_dur,number_of_holidays,
                max_holiday_dur_over_nmdays,total_holiday_dur_over_nmdays) %>%
  slice(n()) %>% ungroup

Adherence<-left_join(Adherence,persistence) %>% 
  replace_na(list(number_of_holidays=0,
                  total_holiday_dur=0,
                  number_of_holidays_over_nmdays=0,
                  total_holiday_dur_over_nmdays=0,
                  max_holiday_dur_over_nmdays=0)) 
rm(persistence)

Adherence$number_of_holidaysx<-ifelse(Adherence$number_of_holidays==0,"a.0",
                                      ifelse(Adherence$number_of_holidays==1,"b.1",
                                             ifelse(Adherence$number_of_holidays<=5,"c.2-5",
                                                    ifelse(Adherence$number_of_holidays<=10,'d.6-10',
                                                           "e.>10"))))

# create a person-level dataset of the clinical outcomes for later
outcomes <- data %>%
  dplyr::select(StudyID,
                BLHowManyAttacksLast12Months,BLTotalMS,BLACTTotal,BLPercentFEV1,
                FinalNumberOfAttacks,FinalTotalMS,FinalACTTotal, FinalPercentFEV1) %>%
  group_by(StudyID) %>%
  slice(1) %>%
  mutate(BLHowManyAttacksLast12Months = (BLHowManyAttacksLast12Months %in% c("None","    ","1-3 "))*1)

########################################################################################
#      Analysis Setup
########################################################################################

measure_list<-c("perc_doses","perc_days_zero","perc_both",
                "total_holiday_dur_over_nmdays",
                "number_of_holidays_over_nmdays")

clusters <- Adherence[,measure_list]
clusters<-clusters %>% group_by()
rm(list=setdiff(ls(),c("clusters","measure_list")))

# Principal component analysis
clusters.pca <- prcomp(clusters, center = TRUE,scale. = TRUE)
summary(clusters.pca)
wss <- (nrow(clusters.pca$x[,1:2])-1)*sum(apply(clusters.pca$x[,1:2],2,var))
for (i in 2:10) wss[i] <- sum(kmeans(clusters.pca$x[,1:2], 
                                     centers=i)$withinss)


# Principal component analysis without scaling, for comparison
clusters.pca2 <- prcomp(clusters, center = TRUE,scale. = FALSE)
summary(clusters.pca2)
wss2 <- (nrow(clusters.pca2$x[,1:2])-1)*sum(apply(clusters.pca2$x[,1:2],2,var))
for (i in 2:10) wss2[i] <- sum(kmeans(clusters.pca2$x[,1:2], 
                                      centers=i)$withinss)


# MinMax scaling, for more comparison
norm <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}
clusters_min_max<-as.data.frame(lapply(clusters, norm))
clusters.pca3 <- prcomp(clusters_min_max, center = FALSE, scale = FALSE)
summary(clusters.pca3)
wss3 <- (nrow(clusters.pca3$x[,1:2])-1)*sum(apply(clusters.pca3$x[,1:2],2,var))
for (i in 2:10) wss3[i] <- sum(kmeans(clusters.pca3$x[,1:2], 
                                      centers=i)$withinss)


########################################################################################
#     Density Plots
########################################################################################

m<-0.1
pa1<-ggplot(as.data.frame(clusters.pca$x), aes(x=PC1)) +  geom_density(color="lightblue1", fill="lightblue1") + theme_bw() + xlab("") + ylab("Unit Variance Scaling")+ theme(plot.margin = unit(rep(m,4), "lines"))
pa2<-ggplot(as.data.frame(clusters.pca$x), aes(x=PC2)) +  geom_density(color="lightskyblue2", fill="lightskyblue2") + theme_bw() + xlab("") + ylab("")+ theme(plot.margin = unit(rep(m,4), "lines"))
pa3<-ggplot(as.data.frame(clusters.pca$x), aes(x=PC3)) +  geom_density(color="lightskyblue3", fill="lightskyblue3") + theme_bw() + xlab("") + ylab("")+ theme(plot.margin = unit(rep(m,4), "lines"))
pa4<-ggplot(as.data.frame(clusters.pca$x), aes(x=PC4)) +  geom_density(color="lightskyblue4", fill="lightskyblue4") + theme_bw() + xlab("") + ylab("")+ theme(plot.margin = unit(rep(m,4), "lines"))

pb1<-ggplot(as.data.frame(clusters.pca2$x), aes(x=PC1)) +  geom_density(color="darkseagreen1", fill="darkseagreen1") + theme_bw() + xlab("PC1") + ylab("No Scaling")+ theme(plot.margin = unit(rep(m,4), "lines"))
pb2<-ggplot(as.data.frame(clusters.pca2$x), aes(x=PC2)) +  geom_density(color="seagreen2", fill="seagreen2") + theme_bw() + xlab("PC2") + ylab("")+ theme(plot.margin = unit(rep(m,4), "lines"))
pb3<-ggplot(as.data.frame(clusters.pca2$x), aes(x=PC3)) +  geom_density(color="seagreen3", fill="seagreen3") + theme_bw() + xlab("PC3") + ylab("")+ theme(plot.margin = unit(rep(m,4), "lines"))
pb4<-ggplot(as.data.frame(clusters.pca2$x), aes(x=PC4)) +  geom_density(color="seagreen4", fill="seagreen4") + theme_bw() + xlab("PC4") + ylab("")+ theme(plot.margin = unit(rep(m,4), "lines"))

pc1<-ggplot(as.data.frame(clusters.pca3$x), aes(x=PC1)) +  geom_density(color="pink", fill="pink") + theme_bw() + xlab("") + ylab("MinMax Scaling")+ theme(plot.margin = unit(rep(m,4), "lines"))
pc2<-ggplot(as.data.frame(clusters.pca3$x), aes(x=PC2)) +  geom_density(color="palevioletred1", fill="palevioletred1") + theme_bw() + xlab("") + ylab("")+ theme(plot.margin = unit(rep(m,4), "lines"))
pc3<-ggplot(as.data.frame(clusters.pca3$x), aes(x=PC3)) +  geom_density(color="palevioletred3", fill="palevioletred3") + theme_bw() + xlab("") + ylab("")+ theme(plot.margin = unit(rep(m,4), "lines"))
pc4<-ggplot(as.data.frame(clusters.pca3$x), aes(x=PC4)) +  geom_density(color="palevioletred4", fill="palevioletred4") + theme_bw() + xlab("") + ylab("")+ theme(plot.margin = unit(rep(m,4), "lines"))

figure<-ggarrange(pa1, pa2, pa3,pa4,
                  pc1,pc2,pc3,pc4,
                  pb1,pb2,pb3,pb4,
                  ncol = 4,nrow=3,align = "v")
annotate_figure(figure,bottom=text_grob("Principal Components"),
                left =text_grob("Density", rot=90)) +
  theme(plot.margin=unit(c(1,1,1,1), "cm"))
rm(list=ls()[which(substr(ls(),1,1)=="p")])



########################################################################################
#      Scree Plots
########################################################################################

# confirm that 3 is the right number of clusters
xa1<-ggplot(data=as.data.frame(wss), aes(x=1:10, y=wss)) + geom_line() + geom_point() +
  scale_x_continuous(breaks=seq(1:10)) + ylab("Unit Variance Scaling") + xlab(" ") + theme_bw()
xa2<-ggplot(data=as.data.frame(wss2), aes(x=1:10, y=wss2)) + geom_line() + geom_point() +
  scale_x_continuous(breaks=seq(1:10)) + ylab("MinMax Scaling") + xlab(" ") + theme_bw()
xa3<-ggplot(data=as.data.frame(wss3), aes(x=1:10, y=wss3)) + geom_line() + geom_point() +
  scale_x_continuous(breaks=seq(1:10)) + ylab("No Scaling") + xlab(" ") + theme_bw()
figure<-ggarrange(xa1, xa2, xa3,nrow = 3, align="v")
annotate_figure(figure,bottom=text_grob("Number of Clusters"),
                left =text_grob("Within groups sum of squares", rot=90))
rm(list=ls()[which(substr(ls(),1,2)=="xa" | substr(ls(),1,3)=="wss")])

########################################################################################
#      Compare PCA variable scaling methods stability testing
########################################################################################

set.seed(12345)
cboot.hclust <- clusterboot(data.frame(clusters.pca$x[,1:2]),B=1000,clustermethod=hclustCBI,
                            method="ward.D", k=3, count = F, nstart=25)
cboot.hclust2 <- clusterboot(data.frame(clusters.pca2$x[,1:2]),B=1000,clustermethod=hclustCBI,
                             method="ward.D", k=3, count = F, nstart=25)
cboot.hclust3 <- clusterboot(data.frame(clusters.pca3$x[,1:2]),B=1000,clustermethod=hclustCBI,
                             method="ward.D", k=3, count = F, nstart=25)

# unit
round(cboot.hclust$bootmean,3)  
cboot.hclust$bootbrd

# minmax
round(cboot.hclust3$bootmean,3)  
cboot.hclust3$bootbrd

# none
round(cboot.hclust2$bootmean,3)    
cboot.hclust2$bootbrd

set.seed(12345)

# Get PC loadings
round(get_pca_var(clusters.pca)$coord,2)

clusters_results<-clusters
clusters_results$groups<-ifelse(cboot.hclust$result$partition==1,"C2",
                                ifelse(cboot.hclust$result$partition==2,"C3",
                                       "C1"))
tapply(clusters_results$perc_doses,clusters_results$groups,summary)
tapply(clusters_results$perc_days_zero,clusters_results$groups,summary)
tapply(clusters_results$perc_both,clusters_results$groups,summary)
tapply(clusters_results$number_of_holidays_over_nmdays,clusters_results$groups,summary)
tapply(clusters_results$total_holiday_dur_over_nmdays,clusters_results$groups,summary)

sum(clusters_results$groups=="C3" & clusters_results$number_of_holidays_over_nmdays>0)
sum(clusters_results$groups=="C3" & clusters_results$number_of_holidays_over_nmdays>0)*100/sum(clusters_results$groups=="C3")


########################################################################################
#      Identify best single feature
########################################################################################

# split into train, validate, and test
clusters_results<-clusters_results[sample(nrow(clusters_results)),] %>%
  mutate(set = ifelse(row_number()<0.70*nrow(clusters_results),
                      "train",
                      ifelse(row_number()<0.85*nrow(clusters_results),
                             "validate","test")))

for (var in measure_list) {
  # classifiers based on the clustering labels
  fit <- rpart(data=clusters_results[which(clusters_results$set=="train"),-ncol(clusters_results)], 
               as.formula(paste0("groups ~ ",var)),  
               method="class", maxdepth=2)
  # prune and plot tree with clustering labels
  pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
  
  assign(paste0("pred_",var), predict(pfit, clusters_results[which(clusters_results$set=="validate"),-ncol(clusters_results)],
                                      type='class'))
}
table(clusters_results$set)
for (var in measure_list) {
  print(var)
  print(table(unlist(clusters_results[which(clusters_results$set=="validate"),"groups"]),
              get(paste0("pred_",var))))
  print(accuracy(table(unlist(clusters_results[which(clusters_results$set=="validate"),"groups"]),
                       get(paste0("pred_",var)))))
}

########################################################################################
#      CART
########################################################################################

# classifiers based on the clustering labels
fit <- rpart(data=clusters_results[which(clusters_results$set=="train"),], 
             groups ~ perc_both,  method="class", maxdepth=2)
# prune and plot tree with clustering labels
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
plot(pfit, uniform=TRUE, main="Pruned Classification Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
clusters_results$treegroup <- ifelse(clusters_results$perc_both<82.5,
                                     ifelse(clusters_results$perc_both<20.4,
                                            "G1",
                                            "G2"),
                                     "G3")
clusters_results$treegroup<-as.factor(clusters_results$treegroup)

table(clusters_results$set)
table(clusters_results[which(clusters_results$set=="test"),c("treegroup","groups")])

# one long box plot for all variables, without axis labels (to be added as a caption) and with colour
par(mfrow=c(1,5),mar=rep(2,4))
boxplot(clusters_results$perc_doses ~ clusters_results$treegroup,
        xlab=' ', ylab='P ', ylim=c(0,100),
        main="(A)", col=brewer.pal(n = 4, name = "Pastel2"))
boxplot(clusters_results$perc_days_zero ~ clusters_results$treegroup,
        xlab=' ', ylab='P ', ylim=c(0,100),
        main="(B)", col=brewer.pal(n = 4, name = "Pastel2"))
boxplot(clusters_results$perc_both ~ clusters_results$treegroup,
        xlab=' ', ylab=' ', ylim=c(0,100),
        main="(C)", col=brewer.pal(n = 4, name = "Pastel2"))
boxplot(clusters_results$number_of_holidays_over_nmdays ~ clusters_results$treegroup,
        xlab=' ', ylab=' ',  ylim=c(0,7),
        main="(D)", col=brewer.pal(n = 4, name = "Pastel2"))
boxplot(clusters_results$total_holiday_dur_over_nmdays ~ clusters_results$treegroup,
        xlab=' ', ylab=' ', ylim=c(0,100),
        main="(E)", col=brewer.pal(n = 4, name = "Pastel2"))
par(mfrow = c(1,1))