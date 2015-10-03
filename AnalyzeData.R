################################
#Code used for analyzing the Jambo Bukoba data
################################

###############################
# Set up R
###############################

### Load necessary libraries, if you miss a library type install.packes("nameofpackage")

library(doBy)
library(base)
library(plyr)
library(dplyr)
library(car)

### Set working directory, put an # in front of the ones you don't need
#setwd("C:/Users/Christopher/Google Drive/Data Animals/Jambo Bukoba/Data/") #Christopher's directory
# XXX #Ben's directory
setwd("C:/Users/Dani/Documents/Data_Animals/Jambo_Bukoba/")
# XXX #Bjoern's directory
# XXX #Laurence's directory

### Load CSV file
FinalData <- read.csv("FinalData/FinalData.csv", stringsAsFactors = FALSE, header = TRUE, sep=";") # Import with no header


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

#collapse School Level
SchoolLevel <- summaryBy(. ~ Identifier, FUN=c(mean), data=FinalData)

#############################
# Descriptive statistics
#############################

table(FinalData$Schoolproject_happened) # tab 
table(FinalData$Schoolproject_happened, FinalData$year) #crosstab
mean(FinalData$Schoolproject_happened) # Mean


#Well this is more fancy stuff. It groups the FinalData thesis by year and summarises the number of workshops (hopefully, results not checked)
pie1 <- FinalData %>% group_by(year) %>% summarise(sum(SUM.WORKSHOPS))
pie(pie1$`sum(SUM.WORKSHOPS)`, labels=pie1$year)

#Projects
table(FinalData$Schoolprojects_happened)
project <- table(FinalData$Schoolprojects_happened)
prop.table(project)
table(SchoolLevel$Schoolprojects_happened)
project2 <- table(SchoolLevel$Schoolprojects_happened)
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
