########################################################################################
#      UCL Data Analysis
########################################################################################

rm(list=ls()) # remove all variables from workspace
par(mfrow = c(1,1))

#install_github("vqv/ggbiplot")

library(readxl)
library(plyr)
library(dplyr)
library(foreign)
library(ggplot2)
library(lme4)
library(survival)
library(cluster) 
library(survminer)
library(devtools)
library(ggbiplot)
library(RColorBrewer)
library(scales)
library(clue)
library(factoextra)
library(corrplot)
library(fpc)
library(rpart)
library(yardstick)
library(MASS)
library(randomForest)
library(wesanderson)

palette(alpha(brewer.pal(9,'Set1'), 0.5))

# This file was created in the R script for the primary paper, 
# also presented in this repository.
save.image("Conference paper file.RData")

########################################################################################
#      Data Cleaning Section
########################################################################################

### Missing Data
nrow(data_all[which(data_all$Event=="Data Missing"),])/2 # missing person-days
nrow(data_all)/2  # total person-days
100*sum(data_all$Event=="Data Missing" & data_all$dose_time=="AM") /  max(sum(data_all$dose_time=="AM"),sum(data_all$dose_time=="PM"))  # as a percentage
missing <- data_all %>%
  filter(Event=="Data Missing") %>%
  group_by(StudyID) %>%
  mutate(count=n()) %>%
  dplyr::slice(1)
length(unique(missing$StudyID))  # how many had missingness
100*length(unique(missing$StudyID))/length(unique(data_all$StudyID))  # percent with missing data
summary(missing$count/2)
summary(missing$retention)
rm(missing)
nrow(data_all[which(data_all$Event!="Data Missing"),])/2 # person days remaining
nrow(data_all[which(data_all$Event!="Data Missing"),]) # dose times remaining

########################################################################################
#      Demographics
########################################################################################

### Table 1
table(unique(data[,c("StudyID","group")])[,c("group")])
table(data_wu$Gender,data_wu$group)
table(data_wu$Age_Group,data_wu$group)
table(data_wu$EthnicityDescript,data_wu$group)
table(data_wu$Deprivation_Group,data_wu$group)

# extra comments for the text
table(data_wu$EthnicityDescript)
summary(data_wu$AgeAtDiagnosis)
sum(data_wu$AgeAtDiagnosis>6,na.rm=T)*100/nrow(data_wu)


########################################################################################
#      Initiation
########################################################################################

# on the day of the baseline visit, study day = 1
# unknown means first day may have been masked as they have records of missing data prior to first recorded dose
table(Adherence$first_med,useNA = "ifany")
table(Adherence$first_med,useNA = "ifany")*100/nrow(Adherence) # as a percentage
sum(Adherence$first_med>7,na.rm=T)# took over a week
sum(Adherence$first_med>7,na.rm=T)*100/nrow(Adherence) # %

# Was there a difference between control and intervention?
table(Adherence$first_med,Adherence$group,useNA = "ifany")
table(Adherence$first_med,Adherence$group,useNA = "ifany")[1,]*100/table(Adherence$group) # percentages first day
#summary(survfit(Surv(time = Adherence$first_med, event = !is.na(Adherence$first_med)) ~ group, data = Adherence))
ggsurvplot(survfit(Surv(time = Adherence$first_med, event = !is.na(Adherence$first_med)) ~ group, data = Adherence),
           data = Adherence, pval = TRUE, xlim=c(0,10))

j<-ggsurvplot(survfit(Surv(time = Adherence$first_med, event = !is.na(Adherence$first_med)) ~ group, data = Adherence),
              data = Adherence, pval = F, xlim=c(1,20), xlab="Study Day",  ylab = "Proportion initiated", fun="event",
              legend.title = "Study Group",legend.labs = c("Control", "Intervention"))
j$plot+scale_x_continuous(breaks=seq(1,20,1))

########################################################################################
#      Dose-time Observation
########################################################################################

table(data_all[which(data_all$Event!="Data Missing" & data_all$group=="Control"),c("Event")]) # medication taking in the control arm
prop.table(table(data_all[which(data_all$Event!="Data Missing" & data_all$group=="Control"),c("Event")]),)*100 # medication taking in the control arm - percentages

nrow(data_all[which(data_all$Event=="Medication" & data_all$group=="Intervention"),]) # how often medication taken in intervention arm
nrow(data_all[which(data_all$Event=="Medication" & data_all$group=="Intervention"),])*100/nrow(data_all[which(data_all$group=="Intervention" & data_all$Event!="Data Missing"),]) # as a percentage
nrow(data_all[which(data_all$Event=="Medication" & data_all$group=="Intervention" & is.na(data_all$time_to_dose)),]) # medication before tone
nrow(data_all[which(data_all$Event=="Medication" & data_all$group=="Intervention" & is.na(data_all$time_to_dose)),])*100/nrow(data_all[which(data_all$Event=="Medication" & data_all$group=="Intervention"),]) # %
nrow(data_all[which(data_all$Event=="Medication Missed" & data_all$group=="Intervention"),]) # how often medication missed in intervention arm
nrow(data_all[which(data_all$Event=="Medication Missed" & data_all$group=="Intervention"),])*100/nrow(data_all[which(data_all$group=="Intervention" & data_all$Event!="Data Missing"),]) # as a percentage

# these numbers are slightly different, because it weights equally per person, not per number of days of follow-up
#tapply(Adherence$perc_doses,Adherence$group,summary)

########################################################################################
#      Time to dose (Intervention arm only)
########################################################################################

### Time to dose (Intervention arm only)
# number of dose-times in the intervention arm
nrow(survival) 
# how often do we have no time to dose? and why?
sum(is.na(survival$time_to_dose))
sum(is.na(survival$time_to_dose))*100/nrow(survival) 
sum(survival$Event=="Medication Missed" & !is.na(survival$hour_of_tone)) # medication not taken after tone
sum(survival$Event=="Medication Missed" & !is.na(survival$hour_of_tone))*100/sum(is.na(survival$time_to_dose)) # reason percentage
sum(survival$Event=="Medication" & is.na(survival$time_to_dose)) # medication before tone
sum(survival$Event=="Medication" & is.na(survival$time_to_dose))*100/sum(is.na(survival$time_to_dose)) # reason percentage
sum(survival$Event=="Medication Missed" & is.na(survival$hour_of_tone)) # medication not taken after NO tone
sum(survival$Event=="Medication Missed" & is.na(survival$hour_of_tone))*100/sum(is.na(survival$time_to_dose)) # reason percentage
# we do have a time
sum(!is.na(survival$time_to_dose))
sum(!is.na(survival$time_to_dose))*100/nrow(survival) 

# time between tone and dose in intervention arm (seconds)
summary(survival$time_to_dose) # only in the intervention people that took it
summary(survival$time_to_dose_x) # all intervention people, with censoring at 2 hours

# where were the extreme values coming from?
sum(survival$time_to_dose>3*60*60,na.rm=T) # took longer than 3 hours
sum(survival$time_to_dose>3*60*60,na.rm=T)*100/sum(!is.na(survival$time_to_dose)) #%
tapply(survival$time_to_dose/(60*60),survival$hour_of_tone,summary)

### Time to dose over study day
addmargins(table(survival$surv_event))
summary(coxph(Surv(survival$time_to_dose_x, survival$surv_event)~ studyday, data =  survival))

########################################################################################
#      Implementation
########################################################################################

### Dose-time observation by days since baseline visit
## person-level trends
summary(Adherence$perc_both)
tapply(Adherence$perc_both,Adherence$group,summary)
# difference between arms
t.test(unlist(Adherence[which(Adherence$group=="Intervention"),c("perc_both")]),unlist(Adherence[which(Adherence$group=="Control"),c("perc_both")]))

tapply(Adherence$perc_days_zero, Adherence$group,summary)
summary(Adherence$perc_days_zero)

# daily rate of taking morning, evening, both or neither doses across cohort
# anyway, here's the  plot
ggplot(summary_implementation[which(summary_implementation$day_bin=="both"),], 
       aes(x=as.numeric(studyday), y=category_day_pct)) +
  geom_line(stat="identity") +
  geom_smooth() +
  scale_x_continuous(limits = c(0,200),expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,80),expand = c(0, 0)) +
  xlab("Days since baseline visit") +
  ylab("% days with both doses taken") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        axis.text.x = element_text(vjust = 1, hjust = 1))
summary_implementation[which(summary_implementation$studyday==5),]
summary_implementation[which(summary_implementation$studyday==150),]
daily_implementation<- summary_implementation[which(summary_implementation$day_bin=="both"),]
summary(lm(category_day_pct ~ studyday, data = daily_implementation))

########################################################################################
#      Persistence
########################################################################################

# how many people took at least one drug holiday of 5+ days.
sum(!is.na(Adherence$first_holiday_start))
sum(!is.na(Adherence$first_holiday_start))*100/nrow(Adherence)
# when did they start
summary(Adherence$first_holiday_start)
# how long did the first one last
summary(Adherence$first_holiday_dur)

# bar chart of number of holidays
x<-Adherence
x$x<-ifelse(Adherence$number_of_holidays==0,0,
            ifelse(Adherence$number_of_holidays<=3,1,
                   ifelse(Adherence$number_of_holidays<=7,2,
                          3)))
x<-as.data.frame(table(x$x))
x$percent<-x$Freq/sum(x$Freq)*100
x$colour<-ifelse(x$Var1=="0",0,1)
ggplot(x, aes(y=percent,x=Var1, fill=as.factor(colour))) +
  geom_bar(width=0.9,stat="identity") +
  theme_bw() +
  scale_y_continuous(limits = c(0,50),expand = c(0, 0)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title=element_text(size=14),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.text=element_text(size=10),
        axis.text=element_text(size=8)) +
  xlab("Number of treatment intermissions") +
  scale_fill_manual(values=c("black","dodgerblue2"),guide=FALSE) +
  ylab("Percentage of study participants") + 
  scale_x_discrete(labels=c("0" = "Zero", 
                            "1" = "1-3",
                            "2" = "4-6",
                            "3" = "7+"))
rm(x)

# by treatment arm
sum(!is.na(Adherence$first_holiday_start) & Adherence$group=="Intervention")
sum(!is.na(Adherence$first_holiday_start) & Adherence$group=="Intervention")*100/sum(Adherence$group=="Intervention")
sum(!is.na(Adherence$first_holiday_start) & Adherence$group=="Control")
sum(!is.na(Adherence$first_holiday_start) & Adherence$group=="Control")*100/sum(Adherence$group=="Control")

# duration spent on drug holidays
tapply(Adherence$total_holiday_dur,Adherence$group,summary)

# duration spent on drug holidays, percentage of non missing days
tapply(Adherence$total_holiday_dur_over_nmdays, Adherence$group,summary)


########################################################################################
#      Correlation between measures
########################################################################################

ggpairs(Adherence[,c("perc_doses","perc_days_zero","perc_both",
                     "number_of_holidays_over_nmdays",
                     "total_holiday_dur_over_nmdays")],
        axisLabels="none",
        columnLabels = c("A","B","C","D","E"),
        switch="y") +
  theme(panel.grid.major = element_blank())

ggplot(Adherence, aes(x=perc_doses, col=group)) + geom_density() +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0,100)) +
  scale_y_continuous(expand = expand_scale(mult = c(0,0.05))) +
  theme_bw() + xlab(" ") + labs(color = " ") + ggtitle("(A)") + 
  theme(legend.position="bottom")
ggplot(Adherence, aes(x=perc_days_zero, col=group)) + geom_density() +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0,100)) + ggtitle("(B)") +
  scale_y_continuous(expand = expand_scale(mult = c(0,0.05))) +
  theme_bw() + xlab(" ") + labs(color = " ") + 
  theme(legend.position="bottom")
ggplot(Adherence, aes(x=perc_both, col=group)) + geom_density() +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0,100)) + ggtitle("(C)") +
  scale_y_continuous(expand = expand_scale(mult = c(0,0.05))) +
  theme_bw() + xlab(" ") + labs(color = " ") + 
  theme(legend.position="bottom")

par(mar=c(2, 4, 1, 8), xpd=TRUE)
barplot(prop.table(table(Adherence$number_of_holidaysx,Adherence$group),2)*100,
        ylab="Percentage of participants",
        col=brewer.pal(5, "Purples"))
legend("topright", inset=c(-0.4,0), 
       legend=c("None","1","1-5","6-10",">10"), 
       col=brewer.pal(5, "Purples"),
       pch=16,
       title=NULL)



data<-prop.table(table(Adherence$number_of_holidaysx,Adherence$group), 2)
ggplot(data=data, aes(x=time, y=total_bill, fill=time)) +
  geom_bar(stat="identity")



ggplot(Adherence, aes(x=number_of_holidays_over_nmdays, col=group)) + geom_density() +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0,8)) +
  scale_y_continuous(expand = expand_scale(mult = c(0,0.05))) +
  theme_bw() + xlab("Number of treatment intermissions \n per 100 days of follow-up") + labs(color = " ") +
  theme(legend.position="bottom")
ggplot(Adherence, aes(x=total_holiday_dur_over_nmdays, col=group)) + geom_density() +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0,100)) +
  scale_y_continuous(expand = expand_scale(mult = c(0,0.05))) +
  theme_bw() + xlab("Duration spent on treatment intermission \n per 100 days of follow-up") + labs(color = " ") +
  theme(legend.position="bottom")


ggplot(summary_implementation[which(summary_implementation$day_bin=="both"),],
       aes(x=studyday, y=category_day_pct)) + geom_line() +
  geom_smooth() +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0,200)) + 
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0,80)) +
  theme_bw() + xlab("Days since baseline visit") + 
  ylab("% days taking both daily doses") 

ggplot(Adherence,aes(x=total_holiday_dur_over_nmdays, 
                     y=perc_days_zero)) + geom_point() +
  geom_smooth() + scale_x_continuous(expand = c(0.01,0.01), limits=c(0,100)) + 
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0,100)) +
  theme_bw() + xlab("Duration spent on treatment intermission \n per 100 days of follow-up") + 
  ylab("% days taking neither daily dose") 
cor(Adherence$total_holiday_dur_over_nmdays,Adherence$perc_days_zero, method="spearman")

ggplot(Adherence,aes(x=total_holiday_dur_over_nmdays, 
                     y=perc_both)) + geom_point() +
  geom_smooth() + scale_x_continuous(expand = c(0.01,0.01), limits=c(0,100)) + 
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0,100)) +
  theme_bw() + xlab("Duration spent on treatment intermission \n per 100 days of follow-up") + 
  ylab("% days taking both daily doses") 
cor(Adherence$total_holiday_dur_over_nmdays,Adherence$perc_both, method="spearman")

ggplot(Adherence,aes(x=number_of_holidays_over_nmdays, 
                     y=total_holiday_dur_over_nmdays, col=group)) + geom_point() +
  geom_smooth() + scale_x_continuous(expand = c(0.01,0.01), limits=c(0,6)) + 
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0,100)) +
  theme_bw() + xlab("Number of treatment intermissions \n per 100 days of follow-up") + 
  ylab("Duration spent on treatment \n intermission per 100 days of follow-up") +
  labs(color=" ") + theme(legend.position="bottom")
cor(Adherence$number_of_holidays_over_nmdays,Adherence$total_holiday_dur_over_nmdays, method="spearman")
