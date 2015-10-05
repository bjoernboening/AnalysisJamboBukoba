################################
#Code used for analyzing the Jambo Bukoba data
################################

###############################
# Set up R
###############################

### Load necessary libraries, if you miss a library type install.packes("nameofpackage")
library(base)
library(plyr)
library(dplyr)
library(car)
library(doBy)
library(psych)

### Set working directory, put an # in front of the ones you don't need
setwd("C:/Users/Christopher/Google Drive/Data Animals/Jambo Bukoba/Data/Final data") #Christopher's directory
# XXX #Ben's directory 
# setwd("C:/Users/Benji/Desktop/Statistics/Project/JB/Change/AnalysisJamboBukoba")
# XXX #Dani's directory
# XXX #Bjoern's directory
# XXX #Laurence's directory


### Load CSV file
FinalData <- read.csv("Final Data.csv", stringsAsFactors = FALSE, header = TRUE, sep=";") # Import with no header
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
FinalData$Project_2013[is.na(FinalData$Project_2013)] <- 0
FinalData$Project_2014[is.na(FinalData$Project_2014)] <- 0
FinalData$Bonanza_sum[is.na(FinalData$Bonanza_sum)] <- 0
FinalData$Bonanza_2012[is.na(FinalData$Bonanza_2012)] <- 0
FinalData$Bonanza_2013[is.na(FinalData$Bonanza_2013)] <- 0
FinalData$Bonanza_2014[is.na(FinalData$Bonanza_2014)] <- 0
FinalData$SUM.WORKSHOPS[is.na(FinalData$SUM.WORKSHOPS)] <- 0

# Also of course male and female to 0 and 1, 0 being female, and label
FinalData$student_sex_binary <- recode(FinalData$student_sex, "'M'=1; 'F'=0;", as.factor.result=FALSE)

# Kagera and Geita should be in one variable
FinalData$Region <- replace(FinalData$regionKagera, FinalData$regionKagera=="Other", "Geita")
table(FinalData$Region)

#Drop regionKagera, regionGeita
FinalData$regionKagera <- NULL
FinalData$regionGeita <- NULL
FinalData$X2012_CENTRE_NAME <- NULL
FinalData$X2012CENTRE_CODE <- NULL
FinalData$X2012_REGION_NAME <- NULL
FinalData$X2012_DISTRICT_NAME <- NULL
FinalData$X2013_CENTRE_NAME <- NULL
FinalData$X2013CENTRE_CODE <- NULL
FinalData$X2013_REGION_NAME <- NULL
FinalData$X2013_DISTRICT_NAME <- NULL
FinalData$X2014_CENTRE_NAME <- NULL
FinalData$X2014CENTRE_CODE <- NULL
FinalData$X2014_REGION_NAME <- NULL
FinalData$X2014_DISTRICT_NAME <- NULL
FinalData$Schoolprojects_Schoolname <- NULL
FinalData$Schoolprojects_Districtname <- NULL
FinalData$Schoolprojects_School.Name. <- NULL
FinalData$Bonanzas_District <- NULL
FinalData$DISTRICTNAME <- NULL
FinalData$school <- NULL
FinalData$schoolname <- NULL

#Show all variables in dataframe
names(FinalData)


## Rename to make it easier
names(FinalData)[names(FinalData)=="Schoolname_cleaned"] <- "Schoolname"
names(FinalData)[names(FinalData)=="Schoolprojects_Description_.of_activities"] <- "Schoolproject_description"



#############################
# Collapse Data by Identifier
#############################

# First save the existing data (CC)
StudentLevel <- FinalData

#Ben's way of aggregating the data (CC)
SchoolLevel <- summaryBy(. ~ Identifier, FUN=c(mean), data=FinalData)
names(SchoolLevel)

#Actually I would prefer simply deleting duplicates as the data is already collapsed but that is up to you (CC)
#But this also means that all the calculation using student data has to be done before
SchoolLevel2 <- FinalData %>% distinct(year, Identifier)
SchoolLevel2$student_number <- NULL
SchoolLevel2$student_sex <- NULL
SchoolLevel2$student_name <- NULL
SchoolLevel2$string_grade <- NULL
SchoolLevel2$kiswahili_grade <- NULL
SchoolLevel2$english_grade <- NULL
SchoolLevel2$maarifa_grade <- NULL
SchoolLevel2$hisabati_grade <- NULL
SchoolLevel2$science_grade <- NULL
SchoolLevel2$average_grade <- NULL

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
pie(mytable)
mytable2 <- table(FinalData$Schoolproject_happened, FinalData$year) #crosstab
prop.table(mytable2)
mean(FinalData$Schoolproject_happened) # Mean

mytable3 <- table(FinalData$student_sex_binary)
pie(mytable3)

mytable4 <- table(SUM.WORKSHOPS)
prop.table(mytable4)
margin.table(mytable4)
table(SUM.WORKSHOPS)

#Projects
names(FinalData)
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

pie(MaleFemale)
pie(SchoolsWS)
pie(mytable)
pie(mytable2)
pie(mytable4)
pie(project)
pie(project2)


#Well this is more fancy stuff. It groups the FinalData thesis by year and summarises the number of workshops (hopefully, results not checked)
pie1 <- FinalData %>% group_by(year) %>% summarise(sum(SUM.WORKSHOPS))
pie(pie1$`sum(SUM.WORKSHOPS)`, labels=pie1$year)


#############################
# Inferential statistics
#############################

#############################
# Yi,t: Average.year.mean
#############################

#Workshop#
#myts <- ts(SchoolLevel$SUM.WORKSHOPS.mean, start=c(2010, 1), end=c(2015, 1), frequency=1)
attach(SchoolLevel)
names(SchoolLevel)
X2013_RANK_OF_SCHOOL_2013.mean  
summary(X2013_RANK_OF_SCHOOL_2013.mean )

# Workshops -> Result: not significant; other years have problem: e.g. WS.2013.mean = NA in regression for Av.TotalMarks 2013
fit1 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ WS.2014.mean)
summary(fit1)
fit1 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ WS.2014.mean + student_sex_binary.mean)
summary(fit1)
fit1 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ WS.2014.mean + WS.2013.mean + student_sex_binary.mean)
summary(fit1)
fit1 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ WS.2014.mean + WS.2013.mean + WS.2012.mean + student_sex_binary.mean)
summary(fit1)
fit1 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ WS.2014.mean + WS.2013.mean + WS.2012.mean + WS.2011.mean + student_sex_binary.mean)
summary(fit1)
fit1 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ WS.2012.mean + student_sex_binary.mean)
summary(fit1)
fit1 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ WS.2014.mean + WS.2013.mean + WS.2012.mean + WS.2011.mean + WS.2010.mean + student_sex_binary.mean)
summary(fit1)
summary(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean)

#fit <- lm(X2013_AVERAGE_TOTOL_MARKS_..250._2013.mean ~ WS.2013.mean)
#summary(fit)
#fit <- lm(X2013_AVERAGE_TOTOL_MARKS_..250._2013.mean ~ WS.2013.mean + student_sex_binary.mean)
#summary(fit)

# Bonanza -> Result: ??? -> problem with NAs in Bonanza2014
summary(Bonanza_2013.mean)
Bonanza_2013.mean[is.na(Bonanza_2013.mean)] <- 0
fit2 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ Bonanza_2013.mean + student_sex_binary.mean)
summary(fit2)
fit2 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ Bonanza_2014.mean + student_sex_binary.mean)
summary(fit2)
fit2 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ Bonanza_2014.mean + Bonanza_2013.mean + student_sex_binary.mean)
summary(fit2)
fit2 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ Bonanza_2014.mean + Bonanza_2013.mean + Bonanza_2012.mean + student_sex_binary.mean)
summary(fit2)

# School Projects
fit3 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ Schoolproject_happened.mean)
summary(fit3)
fit3 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ Schoolproject_happened.mean + student_sex_binary.mean)
summary(fit3)

# Interaction
fit4 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ WS.2014.mean*Bonanza_2014.mean*Schoolproject_happened.mean + student_sex_binary.mean)
summary(fit4)

#############################
# Y2i,t: PassRatio.year.mean
#############################  

#############################
# Y3i,t: Rank.year.mean
############################# 

X2013_RANK_OF_SCHOOL_2013.mean  

# Workshops 
fit5 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ WS.2014.mean)
summary(fit5)
fit5 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ WS.2014.mean + student_sex_binary.mean)
summary(fit5)
fit5 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ WS.2014.mean + WS.2013.mean + student_sex_binary.mean)
summary(fit5)
fit5 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ WS.2014.mean + WS.2013.mean + WS.2012.mean + student_sex_binary.mean)
summary(fit5)
fit5 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ WS.2014.mean + WS.2013.mean + WS.2012.mean + WS.2011.mean + student_sex_binary.mean)
summary(fit5)
fit5 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ WS.2014.mean + WS.2013.mean + WS.2012.mean + WS.2011.mean + WS.2010.mean + student_sex_binary.mean)
summary(fit5)

#fit <- lm(X2013_RANK_OF_SCHOOL_2013.mean ~ WS.2013.mean)
#summary(fit)
#fit <- lm(X2013_RANK_OF_SCHOOL_2013.mean ~ WS.2013.mean + student_sex_binary.mean)
#summary(fit)

# Bonanza -> Result: 
fit6 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ Bonanza_2014.mean)
summary(fit6)
fit6 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ Bonanza_2014.mean + student_sex_binary.mean)
summary(fit6)
fit6 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ Bonanza_2014.mean + Bonanza_2013.mean + student_sex_binary.mean)
summary(fit6)
fit6 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ Bonanza_2014.mean + Bonanza_2013.mean + Bonanza_2012.mean + student_sex_binary.mean)
summary(fit6)

# School Projects
fit7 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ Schoolproject_happened.mean)
summary(fit7)
fit7 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ Schoolproject_happened.mean + student_sex_binary.mean)
summary(fit7)

# Interaction
fit8 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ WS.2014.mean*Bonanza_2014.mean*Schoolproject_happened.mean + student_sex_binary.mean)
summary(fit8)
