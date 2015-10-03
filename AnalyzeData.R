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
setwd("C:/Users/Benji/Desktop/Statistics/Project/JB/Change/AnalysisJamboBukoba")
# XXX #Dani's directory
# XXX #Bjoern's directory
# XXX #Laurence's directory


### Load CSV file
MASTER <- read.csv("Final Data.csv", stringsAsFactors = FALSE, header = TRUE, sep=";") # Import with no header
attach(MASTER)

###############################
# Necessary Recoding before analysis
###############################

# Replace NA with 0 if necessary. Why? Remember how for example merging the 
# Master with the data about bonanzas works. The workshop data is bound right
# to the Master data, that means for example that student with a bonanza now
# has a 1 in that variable. But students without a bonanza are SET TO NA, although
# 0 would be right. This is only one example, we have to figure out with which
# variables that makes sense. Christopher already prepared a function for recoding.
MASTER$Schoolproject_happened[is.na(MASTER$Schoolproject_happened)] <- 0
MASTER$SUM.WORKSHOPS[is.na(MASTER$SUM.WORKSHOPS)] <- 0



# Also of course male and female to 0 and 1, 0 being female, and label
MASTER$student_sex_binary <- recode(MASTER$student_sex, "'M'=1; 'F'=0;", as.factor.result=FALSE)

names(MASTER)
#############################
# Collapse Data by Identifier
#############################

SchoolLevel <- summaryBy(. ~ Identifier, FUN=c(mean), data=MASTER)
DistrictLevel <- summaryBy(. ~ DISTRICTNAME, FUN=c(mean), data=MASTER)

table(MASTER$DISTRICTNAME)

#############################
# Descriptive statistics
#############################

mytable <- table(MASTER$Schoolproject_happened) # tab 
prop.table(mytable)
mytable2 <- table(MASTER$Schoolproject_happened, MASTER$year) #crosstab
prop.table(mytable2)
mean(MASTER$Schoolproject_happened) # Mean

mytable3 <- table(MASTER$student_sex_binary)
plot(mytable3)

mytable4 <- table(SUM.WORKSHOPS)
prop.table(mytable4)
margin.table(mytable4)
table(SUM.WORKSHOPS)

table(collapse1$SUM.WORKSHOPS)


#Well this is more fancy stuff. It groups the master thesis by year and summarises the number of workshops (hopefully, results not checked)
pie1 <- MASTER %>% group_by(year) %>% summarise(sum(SUM.WORKSHOPS))
pie(pie1$`sum(SUM.WORKSHOPS)`, labels=pie1$year)


#############################
# Inferential statistics
#############################

