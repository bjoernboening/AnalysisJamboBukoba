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

### Set working directory, put an # in front of the ones you don't need
setwd("C:/Users/Christopher/Google Drive/Data Animals/Jambo Bukoba/Data/") #Christopher's directory
# XXX #Ben's directory
# XXX #Dani's directory
# XXX #Bjoern's directory
# XXX #Laurence's directory


### Load CSV file
MASTER <- read.csv("Final data/Final Data.csv", stringsAsFactors = FALSE, header = TRUE, sep=";") # Import with no header


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


#############################
# Descriptive statistics
#############################

table(MASTER$Schoolproject_happened) # tab 
table(MASTER$Schoolproject_happened, MASTER$year) #crosstab
mean(MASTER$Schoolproject_happened) # Mean


#Well this is more fancy stuff. It groups the master thesis by year and summarises the number of workshops (hopefully, results not checked)
pie1 <- MASTER %>% group_by(year) %>% summarise(sum(SUM.WORKSHOPS))
pie(pie1$`sum(SUM.WORKSHOPS)`, labels=pie1$year)


#############################
# Inferential statistics
#############################

