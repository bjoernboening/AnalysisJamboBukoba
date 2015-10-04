################################
#Code used for analyzing the Jambo Bukoba data
################################

###############################
# Set up R
###############################

### Load necessary libraries, if you miss a library type install.packes("nameofpackage")
library(plyr)
library(dplyr)
library(car)
library(doBy)
library(psych)

### Set working directory, put an # in front of the ones you don't need
#setwd("C:/Users/Christopher/Google Drive/Data Animals/Jambo Bukoba/Data/") #Christopher's directory
# XXX #Ben's directory 
#setwd("C:/Users/Benji/Desktop/Statistics/Project/JB/Change/AnalysisJamboBukoba")
# XXX #Dani's directory
setwd("C:/Users/Dani/Documents/Data_Animals/Jambo_Bukoba/FinalData")
# XXX #Bjoern's directory
# XXX #Laurence's directory


### Load CSV file
FinalData <- read.csv("FinalData.csv", stringsAsFactors = FALSE, header = TRUE, sep=";") # Import with no header
attach(FinalData)

###############################
# Necessary Recoding before analysis
###############################

# Replace NA with 0 if necessary. Why? Remember how for example merging the 
# FinalData with the data about bonanzas works. The workshop data is bound right
# to the FinalData data, that means for example that student with a bonanza now
# has a 1 in that variable. But students without a bonanza are SET TO NA, although
# 0 would be right. This is only one example, we have to figure out with which
# variables that makes sense. Christopher already prepared a function for recoding.
FinalData$Schoolproject_happened[is.na(FinalData$Schoolproject_happened)] <- 0
FinalData$SUM.WORKSHOPS[is.na(FinalData$SUM.WORKSHOPS)] <- 0



# Also of course male and female to 0 and 1, 0 being female, and label
FinalData$student_sex_binary <- recode(FinalData$student_sex, "'M'=1; 'F'=0;", as.factor.result=FALSE)

names(FinalData)
#############################
# Collapse Data by Identifier
#############################

SchoolLevel <- summaryBy(. ~ Identifier, FUN=c(mean), data=FinalData)
names(SchoolLevel)


#############################
# Create DistrictLevel
#############################
#DL12 <- summaryBy(. ~ X2012_DISTRICT_NAME, FUN=c(mean), data=FinalData)
#DL12 <- rename(DL12, c("X2012_DISTRICT_NAME" = "Districtname"))
#DL13 <- summaryBy(. ~ X2013_DISTRICT_NAME, FUN=c(mean), data=FinalData)
#DL13 <- rename(DL13, c("X2013_DISTRICT_NAME" = "Districtname"))
#DL14 <- summaryBy(. ~ X2014_DISTRICT_NAME, FUN=c(mean), data=FinalData)
#DL14 <- rename(DL14, c("X2014_DISTRICT_NAME" = "Districtname"))

#merge1 <- join(DL13, DL12, by= c("Districtname"))
#DistrictLevel <- join(DL14, DL13, by= c("Districtname"))
#table(DistrictLevel$Districtname)

#############################
# Descriptive statistics
#############################

mytable <- table(FinalData$Schoolproject_happened) # tab 
prop.table(mytable)
mytable2 <- table(FinalData$Schoolproject_happened, FinalData$year) #crosstab
prop.table(mytable2)
mean(FinalData$Schoolproject_happened) # Mean

mytable3 <- table(FinalData$student_sex_binary)
plot(mytable3)

mytable4 <- table(SUM.WORKSHOPS)
prop.table(mytable4)
margin.table(mytable4)
table(SUM.WORKSHOPS)

table(SchoolLevel$SUM.WORKSHOPS)

#Projects
table(FinalData$Schoolproject_happened)
project <- table(FinalData$Schoolproject_happened)
prop.table(project)
table(SchoolLevel$Schoolproject_happened)
project2 <- table(SchoolLevel$Schoolproject_happened)
prop.table(project2)

#Male/Female ratio
table(FinalData$student_sex)
MaleFemale <- table(FinalData$student_sex)
prop.table(MaleFemale)
#table(SchoolLevel$student_sex)
#MaleFemale2 <- table(SchoolLevel$student_sex)
#prop.table(MaleFemale2)
summary(SchoolLevel$student_sex)

#Schools: School mit mind. einem WS
#table(FinalData$SUM.WORKSHOPS)
#SchoolsWS <- table(FinalData$SUM.WORKSHOPS)
#prop.table(SchoolsWS)
table(SchoolLevel$SUM.WORKSHOPS)
SchoolsWS <- table(SchoolLevel$SUM.WORKSHOPS)
prop.table(SchoolsWS)


#Well this is more fancy stuff. It groups the FinalData thesis by year and summarises the number of workshops (hopefully, results not checked)
pie1 <- FinalData %>% group_by(year) %>% summarise(sum(SUM.WORKSHOPS))
pie(pie1$`sum(SUM.WORKSHOPS)`, labels=pie1$year)


#############################
# Inferential statistics
#############################

#creating new var: y2i,t --> Pass.Ratio
names(SchoolLevel)[31] <- 'PassRatio.2011.mean'
table(SchoolLevel$PassRatio.2011.mean)
summary(SchoolLevel$PassRatio.2011.mean)

PassRatio.2012.mean <- SchoolLevel$X2012_NUMBER_OF_STUDENTS_PASSED_.A.C..mean  / SchoolLevel$X2012_CLEAN_CANDIDATES_2012.mean
table(SchoolLevel$PassRatio.2012.mean)
summary(PassRatio.2012.mean)

PassRatio.2013.mean <- SchoolLevel$X2013_NUMBER_OF_STUDENTS_PASSED_.A.C..mean / SchoolLevel$X2013_CLEAN_CANDIDATES_2013.mean
table(SchoolLevel$PassRatio.2013.mean)
summary(PassRatio.2013.mean)

PassRatio.2014.mean <- SchoolLevel$X2014_NUMBER_OF_STUDENTS_PASSED_.A.C..mean / SchoolLevel$X2014_CLEAN_CANDIDATES_2014.mean
table(SchoolLevel$PassRatio.2014.mean)
summary(PassRatio.2014.mean)
#########################################
### Regression on PassRatio.YYYY.mean ###
#########################################
#PassRatio.2014.mean ~ WS
fit <- lm(PassRatio.2014.mean ~ WS.2010.mean + WS.2011.mean + WS.2012.mean + WS.2013.mean + WS.2014.mean, data=SchoolLevel)
summary(fit)
#PassRatio.2014.mean ~ WS and Bonanza
fit <- lm(PassRatio.2014.mean ~ WS.2010.mean + WS.2011.mean + WS.2012.mean + WS.2013.mean + WS.2014.mean + Bonanza_2012.mean + Bonanza_2013.mean + Bonanza_2014.mean, data=SchoolLevel)
summary(fit)
#PassRatio.2014.mean ~ WS and Bonanza and Interaction
fit <- lm(PassRatio.2014.mean ~ WS.2010.mean + WS.2011.mean + WS.2012.mean + WS.2013.mean + WS.2014.mean + Bonanza_2012.mean + Bonanza_2013.mean + Bonanza_2014.mean + Bonanza_2012.mean*WS.2012.mean + Bonanza_2013.mean*WS.2013.mean + Bonanza_2014*WS.2014.mean, data=SchoolLevel)
summary(fit)
#PassRatio.2014.mean ~ WS and Bonanza and Project
fit <- lm(PassRatio.2014.mean ~ WS.2010.mean + WS.2011.mean + WS.2012.mean + WS.2013.mean + WS.2014.mean + Bonanza_2012.mean + Bonanza_2013.mean + Bonanza_2014.mean + Project.2013.mean + Project.2014.mean, data=SchoolLevel)
summary(fit)
#PassRatio.2014.mean ~ WS and Bonanza and Project and Interaction
fit <- lm(PassRatio.2014.mean ~ WS.2010.mean + WS.2011.mean + WS.2012.mean + WS.2013.mean + WS.2014.mean + Bonanza_2012.mean + Bonanza_2013.mean + Bonanza_2014.mean + Project.2013.mean + Project.2014.mean + Bonanza_2012.mean*WS.2012.mean + Project.2013.mean*Bonanza_2013.mean*WS.2013.mean + Project.2014.mean*Bonanza_2014.mean*WS.2014.mean, data=SchoolLevel)
summary(fit)
#PassRatio.2013.mean ~ WS
fit <- lm(PassRatio.2013.mean ~ WS.2010.mean + WS.2011.mean + WS.2012.mean + WS.2013.mean, data=SchoolLevel)
summary(fit)
#PassRatio.2013.mean ~ WS and Bonanza
fit <- lm(PassRatio.2013.mean ~ WS.2010.mean + WS.2011.mean + WS.2012.mean + WS.2013.mean + Bonanza_2012.mean + Bonanza_2013.mean, data=SchoolLevel)
summary(fit)
#PassRatio.2013.mean ~ WS and Bonanza and Interaction
fit <- lm(PassRatio.2013.mean ~ WS.2010.mean + WS.2011.mean + WS.2012.mean + WS.2013.mean + Bonanza_2012.mean + Bonanza_2013.mean + Bonanza_2012.mean*WS.2012.mean + Bonanza_2013.mean*WS.2013.mean, data=SchoolLevel)
summary(fit)
#PassRatio.2013.mean ~ WS and Bonanza and Project
fit <- lm(PassRatio.2013.mean ~ WS.2010.mean + WS.2011.mean + WS.2012.mean + WS.2013.mean + Bonanza_2012.mean + Bonanza_2013.mean + Project.2013.mean, data=SchoolLevel)
summary(fit)
#PassRatio.2013.mean ~ WS and Bonanza and Project and Interaction
fit <- lm(PassRatio.2013.mean ~ WS.2010.mean + WS.2011.mean + WS.2012.mean + WS.2013.mean + WS.2014.mean + Bonanza_2012.mean + Bonanza_2013.mean + Bonanza_2014.mean + Project.2013.mean + Project.2014.mean + Bonanza_2012.mean*WS.2012.mean + Project.2013.mean*Bonanza_2013.mean*WS.2013.mean, data=SchoolLevel)
summary(fit)
#PassRatio.2012.mean ~ WS
fit <- lm(PassRatio.2012.mean ~ WS.2010.mean + WS.2011.mean + WS.2012.mean, data=SchoolLevel)
summary(fit)
#PassRatio.2012.mean ~ WS and Bonanza
fit <- lm(PassRatio.2012.mean ~ WS.2010.mean + WS.2011.mean + WS.2012.mean + Bonanza_2012.mean, data=SchoolLevel)
summary(fit)
#PassRatio.2012.mean ~ WS and Bonanza and Interaction
fit <- lm(PassRatio.2012.mean ~ WS.2010.mean + WS.2011.mean + WS.2012.mean + Bonanza_2012.mean + Bonanza_2012.mean*WS.2012.mean, data=SchoolLevel)
summary(fit)
#PassRatio.2011.mean ~ WS
fit <- lm(PassRatio.2011.mean ~ WS.2010.mean + WS.2011.mean, data=SchoolLevel)
summary(fit)
