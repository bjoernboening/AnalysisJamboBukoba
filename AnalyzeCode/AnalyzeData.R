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
library(magrittr)
library(graphics)
### Set working directory, put an # in front of the ones you don't need
#setwd("C:/Users/Christopher/Google Drive/Data Animals/Jambo Bukoba/Data/Final data") #Christopher's directory
# XXX #Ben's directory 
setwd("C:/Users/Benji/Desktop/Statistics/Project/JB/Change/AnalysisJamboBukoba")
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

# Also of course male and female to 0 and 1, 0 being female
FinalData$student_sex_binary <- recode(FinalData$student_sex, "'M'=1; 'F'=0;", as.factor.result=FALSE)
table(FinalData$student_sex_binary)

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

## Convert grades to numbers, but what does X mean?
FinalData$english_grade_numeric <- as.numeric(recode(FinalData$english_grade, "' A'=1; ' B'=2;' C'=3;' D'=4;' E'=5;' X'=0;", as.factor.result=FALSE))
FinalData$science_grade_numeric <- as.numeric(recode(FinalData$science_grade, "' A'=1; ' B'=2;' C'=3;' D'=4;' E'=5;' X'=0;", as.factor.result=FALSE))
FinalData$kiswahili_grade_numeric <- as.numeric(recode(FinalData$kiswahili_grade, "' A'=1; ' B'=2;' C'=3;' D'=4;' E'=5;' X'=0;", as.factor.result=FALSE))
FinalData$maarifa_grade_numeric <- as.numeric(recode(FinalData$maarifa_grade, "' A'=1; ' B'=2;' C'=3;' D'=4;' E'=5;' X'=0;", as.factor.result=FALSE))
FinalData$hisabati_grade_numeric <- as.numeric(recode(FinalData$hisabati_grade, "' A'=1; ' B'=2;' C'=3;' D'=4;' E'=5;' X'=0;", as.factor.result=FALSE))
FinalData$average_grade_numeric <- as.numeric(recode(FinalData$average_grade, "' A'=1; ' B'=2;' C'=3;' D'=4;' E'=5;' X'=0;", as.factor.result=FALSE))

class(FinalData$english_grade_numeric)
table(FinalData$english_grade_numeric, useNA = "always")
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
SchoolLevel2 <- FinalData %>% distinct(Identifier)
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

## Uses FinalData. Shows number of Schoolprojects that have occured. We have only 1.8 % of cases where this is the cases in the uncollapsed data.
mytable <- table(FinalData$Schoolproject_happened) # tab 
prop.table(mytable)
pie(mytable)
## Uses SchoolLevel. Shows the same but for collapsed data -> shows us that actually only 1 % of schools affected
table(SchoolLevel$Schoolproject_happened)
project2 <- table(SchoolLevel$Schoolproject_happened)
prop.table(project2)


## Uses FinalData. the same as before but now with cross tabulation. Shows us that they have all happened between 2013 and 2014
mytable2 <- table(FinalData$Schoolproject_happened, FinalData$year) #crosstab
prop.table(mytable2)

#include in presentation# Uses FinalData. shows the distrubtion of sex. We have 52.2 % females -> looks like a normal distribution. Good. 
table(FinalData$student_sex)
MaleFemale <- table(FinalData$student_sex)
prop.table(MaleFemale)
#include in presentation# Uses SchoolLevel. Shows ultimately that there are clases with up to 76 % female students and classes with 0 % female students.
MaleFemale2 <- table(SchoolLevel$student_sex)
prop.table(MaleFemale2)
summary(SchoolLevel$student_sex)

#include in presentation# shows the amount of workshops. There are a total of 134584 or 35.8 % students affected. Good. It also shows the distribution of 1, 2 and 3 workshops affecting students
mytable4 <- table(SUM.WORKSHOPS)
prop.table(mytable4)
table(SUM.WORKSHOPS)
margin.table(mytable4)
#include in presentation# uses SchoolLevel. Shows that there are 31.1 % schools affected.
table(SchoolLevel$SUM.WORKSHOPS)
SchoolsWS <- table(SchoolLevel$SUM.WORKSHOPS)
prop.table(SchoolsWS)
pie(SchoolsWS)

#############################
# Inferential statistics
#############################

#############################
# Yi,t: Average.year.mean
#############################

#Workshop#
#myts <- ts(SchoolLevel$SUM.WORKSHOPS.mean, start=c(2010, 1), end=c(2015, 1), frequency=1)
#attach(SchoolLevel)
#names(SchoolLevel)
#X2013_RANK_OF_SCHOOL_2013.mean  
#summary(X2013_RANK_OF_SCHOOL_2013.mean )

# Workshops -> Result: not significant; other years have problem: e.g. WS.2013.mean = NA in regression for Av.TotalMarks 2013
#fit1 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ WS.2014.mean)
#summary(fit1)
#fit1 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ WS.2014.mean + student_sex_binary.mean)
#summary(fit1)
#fit1 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ WS.2014.mean + WS.2013.mean + student_sex_binary.mean)
#summary(fit1)
#fit1 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ WS.2014.mean + WS.2013.mean + WS.2012.mean + student_sex_binary.mean)
#summary(fit1)
#fit1 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ WS.2014.mean + WS.2013.mean + WS.2012.mean + WS.2011.mean + student_sex_binary.mean)
#summary(fit1)
#fit1 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ WS.2012.mean + student_sex_binary.mean)
#summary(fit1)
#fit1 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ WS.2014.mean + WS.2013.mean + WS.2012.mean + WS.2011.mean + WS.2010.mean + student_sex_binary.mean)
#summary(fit1)
#summary(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean)

#fit <- lm(X2013_AVERAGE_TOTOL_MARKS_..250._2013.mean ~ WS.2013.mean)
#summary(fit)
#fit <- lm(X2013_AVERAGE_TOTOL_MARKS_..250._2013.mean ~ WS.2013.mean + student_sex_binary.mean)
#summary(fit)

# Bonanza -> Result: ??? -> problem with NAs in Bonanza2014
#summary(Bonanza_2013.mean)
#Bonanza_2013.mean[is.na(Bonanza_2013.mean)] <- 0
#fit2 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ Bonanza_2013.mean + student_sex_binary.mean)
#summary(fit2)
#fit2 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ Bonanza_2014.mean + student_sex_binary.mean)
#summary(fit2)
#fit2 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ Bonanza_2014.mean + Bonanza_2013.mean + student_sex_binary.mean)
#summary(fit2)
#fit2 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ Bonanza_2014.mean + Bonanza_2013.mean + Bonanza_2012.mean + student_sex_binary.mean)
#summary(fit2)

# School Projects
#fit3 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ Schoolproject_happened.mean)
#summary(fit3)
#fit3 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ Schoolproject_happened.mean + student_sex_binary.mean)
#summary(fit3)

# Interaction
#fit4 <- lm(X2014_AVERAGE_TOTOL_MARKS_..250._2014.mean ~ WS.2014.mean*Bonanza_2014.mean*Schoolproject_happened.mean + student_sex_binary.mean)
#summary(fit4)

#############################
# Y2i,t: PassRatio.year.mean
#############################  

#############################
# Y3i,t: Rank.year.mean
############################# 

#X2013_RANK_OF_SCHOOL_2013.mean  

# Workshops 
#fit5 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ WS.2014.mean)
#summary(fit5)
#fit5 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ WS.2014.mean + student_sex_binary.mean)
#summary(fit5)
#fit5 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ WS.2014.mean + WS.2013.mean + student_sex_binary.mean)
#summary(fit5)
#fit5 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ WS.2014.mean + WS.2013.mean + WS.2012.mean + student_sex_binary.mean)
#summary(fit5)
#fit5 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ WS.2014.mean + WS.2013.mean + WS.2012.mean + WS.2011.mean + student_sex_binary.mean)
#summary(fit5)
#fit5 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ WS.2014.mean + WS.2013.mean + WS.2012.mean + WS.2011.mean + WS.2010.mean + student_sex_binary.mean)
#summary(fit5)#

#fit <- lm(X2013_RANK_OF_SCHOOL_2013.mean ~ WS.2013.mean)
#summary(fit)
#fit <- lm(X2013_RANK_OF_SCHOOL_2013.mean ~ WS.2013.mean + student_sex_binary.mean)
#summary(fit)

# Bonanza -> Result: 
#fit6 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ Bonanza_2014.mean)
#summary(fit6)
#fit6 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ Bonanza_2014.mean + student_sex_binary.mean)
#summary(fit6)
#fit6 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ Bonanza_2014.mean + Bonanza_2013.mean + student_sex_binary.mean)
#summary(fit6)
#fit6 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ Bonanza_2014.mean + Bonanza_2013.mean + Bonanza_2012.mean + student_sex_binary.mean)
#summary(fit6)

# School Projects
#fit7 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ Schoolproject_happened.mean)
#summary(fit7)
#fit7 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ Schoolproject_happened.mean + student_sex_binary.mean)
#summary(fit7)

# Interaction
#fit8 <- lm(X2014_RANK_OF_SCHOOL_2014.mean ~ WS.2014.mean*Bonanza_2014.mean*Schoolproject_happened.mean + student_sex_binary.mean)
#summary(fit8)

##############################
# Descriptive Christoper (want to test something)
##############################

# Overall number of students in Kagera and Geita by year
table(FinalData$Region, FinalData$year, useNA = "always") 

# Add column and sum totals by saving results
tab = table(FinalData$Region, FinalData$year)
addmargins(tab)

# Percentage point for the same table
prop.table(tab)

# Now row percent
prop.table(tab, 1)

# Column percent 
prop.table(tab, 2)

# Gender of students only in 2014
tab <- FinalData %>% 
  filter(year == 2014)

table(tab$student_sex)

# Students in 2014 with compared to without teacher in workshop
table(tab$SUM.WORKSHOPS)

# Histogram of student grades englisch in 2014, is 4 actually a good or bad grade?
hist(tab$english_grade_numeric)

# Histogram only of students with workshop
tab <- FinalData%>%
  filter(SUM.WORKSHOPS!=0)

hist(tab$english_grade_numeric)

# T-Test: null hypothesis being that english grades are not different in 
# 2014, regardless of whether there was a workshop or not
t.test(english_grade_numeric ~ Schoolproject_happened, data = tab)

# T-Test boys and girls have same grades english 2014 -> null rejected
t.test(english_grade_numeric ~ student_sex, data = tab)

# Pie charts of gender with at least one workshop
tab <- FinalData%>%
  filter(SUM.WORKSHOPS!=0)
tab <- table(FinalData$student_sex_binary)
pie(tab)

####################################
# Restructure Data for Time Series Analysis
####################################

# Remove duplicates by year and Identifier
TimeSeries <- FinalData %>% distinct(Identifier, year)

# Check number of year
table(TimeSeries$year) # Looks plausible, what is the one in 2015?
table(TimeSeries$Identifier, TimeSeries$year)

# Set variables that are not used to NULL (drop)
TimeSeries$student_number <- NULL
TimeSeries$student_sex <- NULL
TimeSeries$student_name <- NULL
TimeSeries$string_grade <- NULL
TimeSeries$kiswahili_grade <- NULL
TimeSeries$english_grade <- NULL
TimeSeries$maarifa_grade <- NULL
TimeSeries$hisabati_grade <- NULL
TimeSeries$science_grade <- NULL
TimeSeries$average_grade <- NULL

# Generate candidates per year variable
TimeSeries$Candidates <- TimeSeries$X2012_CLEAN_CANDIDATES_2012
TimeSeries$X2012_CLEAN_CANDIDATES_2012 <- NULL

TimeSeries$Candidates[is.na(TimeSeries$Candidates)] <- TimeSeries$X2013_CLEAN_CANDIDATES_2013[is.na(TimeSeries$Candidates)]
TimeSeries$X2013_CLEAN_CANDIDATES_2013 <- NULL

TimeSeries$Candidates[is.na(TimeSeries$Candidates)] <- TimeSeries$X2014_CLEAN_CANDIDATES_2014[is.na(TimeSeries$Candidates)]
TimeSeries$X2014_CLEAN_CANDIDATES_2014 <- NULL


# Generate passed per year variable
TimeSeries$PassedCandidates <- TimeSeries$X2012_NUMBER_OF_STUDENTS_PASSED_.A.C.
TimeSeries$X2012_NUMBER_OF_STUDENTS_PASSED_.A.C. <- NULL

TimeSeries$PassedCandidates[is.na(TimeSeries$PassedCandidates)] <- TimeSeries$X2013_NUMBER_OF_STUDENTS_PASSED_.A.C.[is.na(TimeSeries$PassedCandidates)]
TimeSeries$X2013_NUMBER_OF_STUDENTS_PASSED_.A.C. <- NULL

TimeSeries$PassedCandidates[is.na(TimeSeries$PassedCandidates)] <- TimeSeries$X2014_NUMBER_OF_STUDENTS_PASSED_.A.C.[is.na(TimeSeries$PassedCandidates)]
TimeSeries$X2014_NUMBER_OF_STUDENTS_PASSED_.A.C. <- NULL

table(TimeSeries$PassedCandidates, useNA = "always")

# Generate average grade variables
TimeSeries$Average_grade <- TimeSeries$X2012_AVERAGE_TOTAL_MARKS_..250._2012
TimeSeries$X2012_AVERAGE_TOTAL_MARKS_..250._2012 <- NULL

TimeSeries$Average_grade[is.na(TimeSeries$Average_grade)] <- TimeSeries$X2013_AVERAGE_TOTOL_MARKS_..250._2013[is.na(TimeSeries$Average_grade)]
TimeSeries$X2013_AVERAGE_TOTOL_MARKS_..250._2013 <- NULL

TimeSeries$Average_grade[is.na(TimeSeries$Average_grade)] <- TimeSeries$X2014_AVERAGE_TOTOL_MARKS_..250._2014[is.na(TimeSeries$Average_grade)]
TimeSeries$X2014_AVERAGE_TOTOL_MARKS_..250._2014 <- NULL

table(TimeSeries$Average_grade, useNA = "always")
table(TimeSeries$Average_grade, TimeSeries$year, useNA = "always")

TimeSeries$X2013_AVERAGE_TOTOL_MARKS_..250._2012 <- NULL #Stuff not needed anymore
TimeSeries$X2013_CHANGE_ON_AVERAGE_TOTAL_MARKS_FROM_2012 <- NULL
TimeSeries$X2014_AVERAGE_TOTOL_MARKS_..250._2013 <- NULL
TimeSeries$X2014_CHANGE_ON_AVERAGE_TOTAL_MARKS_FROM_2013 <- NULL
TimeSeries$X2012_AVERAGE_TOTAL_MARKS_..250._2011 <- NULL
TimeSeries$X2012_CHANGE_ON_AVERAGE_TOTAL_MARKS_FROM_2011 <- NULL

# Generate ranking variable
TimeSeries$Ranking <- TimeSeries$X2012_RANK_OF_SCHOOL_2012
TimeSeries$X2012_RANK_OF_SCHOOL_2012 <- NULL

TimeSeries$Ranking[is.na(TimeSeries$Ranking)] <- TimeSeries$X2013_RANK_OF_SCHOOL_2013[is.na(TimeSeries$Ranking)]
TimeSeries$X2013_RANK_OF_SCHOOL_2013 <- NULL

TimeSeries$Ranking[is.na(TimeSeries$Ranking)] <- TimeSeries$X2014_RANK_OF_SCHOOL_2014[is.na(TimeSeries$Ranking)]
TimeSeries$X2014_RANK_OF_SCHOOL_2014 <- NULL

table(TimeSeries$Ranking, useNA = "always")
table(TimeSeries$Ranking, TimeSeries$year, useNA = "always")

TimeSeries$X2013_RANK_OF_SCHOOL_2012 <- NULL
TimeSeries$X2014_RANK_OF_SCHOOL_2013 <- NULL
TimeSeries$X2014_RANK_OF_SCHOOL_2013 <- NULL

# Generate Band of Schooles variable
TimeSeries$Band <- TimeSeries$X2012_BAND_OF_SCHOOL_2012
TimeSeries$X2012_BAND_OF_SCHOOL_2012 <- NULL

TimeSeries$Band[is.na(TimeSeries$Band)] <- TimeSeries$X2013_BAND_OF_SCHOOL_2013[is.na(TimeSeries$Band)]
TimeSeries$X2013_BAND_OF_SCHOOL_2013 <- NULL

TimeSeries$Band[is.na(TimeSeries$Band)] <- TimeSeries$X2014_BAND_OF_SCHOOL_2014[is.na(TimeSeries$Band)]
TimeSeries$X2014_BAND_OF_SCHOOL_2014 <- NULL

table(TimeSeries$Band, useNA = "always")
table(TimeSeries$Band, TimeSeries$year, useNA = "always")

TimeSeries$X2013_BAND_OF_SCHOOL_2012 <- NULL # Other stuff not needed
TimeSeries$X2013_BAND_OF_SCHOOL_2012 <- NULL
TimeSeries$X2014_BAND_OF_SCHOOL_2013 <- NULL

# With the schoolprojects it gets more complicated. The easist is to merge them new, before that drop old
TimeSeries$Schoolproject_description <- NULL
TimeSeries$Schoolprojects_Students_Total <- NULL
TimeSeries$Schoolprojects_Boys <- NULL
TimeSeries$Schoolprojects_Girls <- NULL
TimeSeries$Schoolproject_happened <- NULL

SchoolprojectsTimeSeries <- read.csv("C:/Users/Christopher/Google Drive/Data Animals/Jambo Bukoba/Data/Merge for Time Series/20151008_Schoolprojects_TimeSeries (CC).csv", stringsAsFactors = FALSE, header = TRUE, sep=";")
SchoolprojectsTimeSeries$Identifier <- toupper(SchoolprojectsTimeSeries$Identifier)
TimeSeries <- join(TimeSeries, SchoolprojectsTimeSeries, by= c("Identifier", "year"), type ="full")
TimeSeries$Schoolprojects_happened[is.na(TimeSeries$Schoolprojects_happened)] <- 0
table(TimeSeries$Schoolprojects_happened, useNA = "always")

# Same thing with the Bonanzas
TimeSeries$Bonanza_sum <- NULL
TimeSeries$Bonanza_2012 <- NULL
TimeSeries$Bonanza_2013 <- NULL
TimeSeries$Bonanza_2014 <- NULL

BonanzasTimeSeries <- read.csv("C:/Users/Christopher/Google Drive/Data Animals/Jambo Bukoba/Data/Merge for Time Series/20151008_Bonanzas_TimeSeries (CC).csv", stringsAsFactors = FALSE, header = TRUE, sep=";")
BonanzasTimeSeries$Identifier <- toupper(BonanzasTimeSeries$Identifier)
TimeSeries <- join(TimeSeries, BonanzasTimeSeries, by= c("Identifier", "year"), type ="full")
TimeSeries$Bonanza_happened[is.na(TimeSeries$Bonanza_happened)] <- 0
table(TimeSeries$Bonanza_happened, useNA = "always")

# And finally for workshop
TimeSeries$WS1 <- NULL
TimeSeries$WS2 <- NULL
TimeSeries$WS3 <- NULL
TimeSeries$WS4 <- NULL
TimeSeries$WS5 <- NULL
TimeSeries$WS6 <- NULL
TimeSeries$WS7 <- NULL
TimeSeries$WS8 <- NULL
TimeSeries$WS9 <- NULL
TimeSeries$WS10 <- NULL
TimeSeries$WS11 <- NULL
TimeSeries$WS12 <- NULL
TimeSeries$WS13 <- NULL
TimeSeries$WS14 <- NULL
TimeSeries$WS15 <- NULL
TimeSeries$WS16 <- NULL
TimeSeries$WS17 <- NULL
TimeSeries$WS18 <- NULL
TimeSeries$WS19 <- NULL
TimeSeries$WS20 <- NULL
TimeSeries$WS21 <- NULL
TimeSeries$WS22 <- NULL
TimeSeries$WS23 <- NULL
TimeSeries$WS24 <- NULL
TimeSeries$WS25 <- NULL
TimeSeries$WS26 <- NULL
TimeSeries$WS27 <- NULL
TimeSeries$WS28 <- NULL
TimeSeries$WS29 <- NULL
TimeSeries$WS30 <- NULL
TimeSeries$WS31 <- NULL
TimeSeries$WS32 <- NULL
TimeSeries$WS33 <- NULL
TimeSeries$WS.2010 <- NULL
TimeSeries$WS.2011 <- NULL
TimeSeries$WS.2012 <- NULL
TimeSeries$WS.2013 <- NULL
TimeSeries$WS.2014 <- NULL
TimeSeries$WS.2015 <- NULL


WorkshopTimeSeries <- read.csv("C:/Users/Christopher/Google Drive/Data Animals/Jambo Bukoba/Data/Merge for Time Series/20151008_Workshops_TimeSeries (CC).csv", stringsAsFactors = FALSE, header = TRUE, sep=";")
WorkshopTimeSeries$Identifier <- toupper(WorkshopTimeSeries$Identifier)
TimeSeries <- join(TimeSeries, WorkshopTimeSeries, by= c("Identifier", "year"), type ="full")
TimeSeries$Workshop_happened[is.na(TimeSeries$Workshop_happened)] <- 0
table(TimeSeries$Workshop_happened, useNA = "always")

# Time series done

############################################################
# Analysis time series data
############################################################

#More to come
