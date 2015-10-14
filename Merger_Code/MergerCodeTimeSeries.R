#############################################
# Create time series data
#############################################

library(stringr)
library(plyr)
library(dplyr)
library(zoo)

setwd("C:/Users/Christopher/Google Drive/Data Animals/Jambo Bukoba/Data/")
###############################
# Merge School data to Master -> we might not need this anymore as we probably won't use this data.
###############################

# Open both datasets
MASTER <- read.csv("Archiv/necta.org (primary school leaving examination results)/File2013_2014_Cleaned_KageraGeita 2015-09-18.csv", stringsAsFactors = FALSE, header = TRUE, sep=";") # Import with no header
School <- read.csv("Archiv/pesptz.org (books delivered)/Schooldata_final.csv", stringsAsFactors = FALSE, header = TRUE, sep=";") # Import with no header


# ########## Prepare school file
# # Write class 7 into Schoolsubset. I assume that because all the student grades are the school
# # leaving examination results, we only need 7th grade
# Schoolsubset <- subset(School, class=="VII")
# 
# # Remove duplicates, this is of course not optimal. 
# duplicates <- duplicated(Schoolsubset$schoolname)
# Schoolsubset = data.frame(Schoolsubset, duplicates)
# Schoolsubset <- subset(Schoolsubset , duplicates==FALSE)
# 
# # Get rid of symbols in schoolname
# schoolname2 <- Schoolsubset$schoolname
# Schoolsubset = data.frame(Schoolsubset, schoolname2)
# Schoolsubset$schoolname2 <- Schoolsubset$schoolname
# Schoolsubset$schoolname2 <- str_replace_all(Schoolsubset$schoolname2, '-', '')
# Schoolsubset$schoolname2 <- str_replace_all(Schoolsubset$schoolname2, " ", "")
# 
# # Make a letters capital
# Schoolsubset <- mutate_each(Schoolsubset, funs(toupper))
# 
# ######### Prepare MASTER file
# # Take of symbols in schoolname of MASTER File
# MASTER$schoolname2 <- str_replace_all(MASTER$schoolname, "[']", "")
# MASTER$schoolname2 <- str_replace_all(MASTER$schoolname2, "PRIMARY", "")
# MASTER$schoolname2 <- str_replace_all(MASTER$schoolname2, "SCHOOL", "")
# MASTER$schoolname2 <- str_replace_all(MASTER$schoolname2, " ", "")
# #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # Achtung, dadurch, dass hier keine Duplikate entfernt werden, ist noch ein Fehler drin. 
# # Dadurch werden Schuldaten zu Sch?lern gemerged, die dort nicht hingeh?ren.
# 
# 
# ####################### Merge school to master
# MASTER <- join(MASTER, Schoolsubset, by = "schoolname2") 


###################### 
# Add agregated school data
######################

# Import csv
PSLE2011_RANKING <- read.csv("Archiv/necta.org (aggregated)/PSLE2011_RANKING (by Christopher).csv", stringsAsFactors = FALSE, header = TRUE, sep = ";") # Import with no header
PSLE2012_RANKING <- read.csv("Archiv/necta.org (aggregated)/PSLE2012_RANKING.csv", stringsAsFactors = FALSE, header = TRUE, sep = ";") # Import with no header
PSLE2013_IMPROVEMENT <- read.csv("Archiv/necta.org (aggregated)/PSLE2013_IMPROVEMENT.csv", stringsAsFactors = FALSE, header = TRUE, sep = ";") # Import with no header
PSLE2013_RANKING <- read.csv("Archiv/necta.org (aggregated)/PSLE2013_RANKING.csv", stringsAsFactors = FALSE, header = TRUE, sep = ";") # Import with no header
PSLE2014_IMPROVEMENT <- read.csv("Archiv/necta.org (aggregated)/PSLE2014_IMPROVEMENT.csv", stringsAsFactors = FALSE, header = TRUE, sep = ";") # Import with no header
PSLE2014_RANKING <- read.csv("Archiv/necta.org (aggregated)/PSLE2014_RANKING.csv", stringsAsFactors = FALSE, header = TRUE, sep = ";") # Import with no header

########## Clean data
PSLE2011_RANKING$X2011_CENTRE_CODE <- str_replace_all(PSLE2011_RANKING$X2011_CENTRE_CODE, "-", "")
PSLE2011_RANKING$X2011_CENTRE_CODE <- str_replace_all(PSLE2011_RANKING$X2011_CENTRE_CODE, "S", "")
PSLE2012_RANKING$X2012_CENTRE_CODE <- str_replace_all(PSLE2012_RANKING$X2012_CENTRE_CODE, "-", "")
PSLE2012_RANKING$X2012_CENTRE_CODE <- str_replace_all(PSLE2012_RANKING$X2012_CENTRE_CODE, "S", "")
PSLE2013_RANKING$X2013_CENTRE_CODE <- str_replace_all(PSLE2013_RANKING$X2013_CENTRE_CODE, "-", "")
PSLE2013_RANKING$X2013_CENTRE_CODE <- str_replace_all(PSLE2013_RANKING$X2013_CENTRE_CODE, "S", "")
head(PSLE2013_RANKING$X2013_CENTRE_CODE)
PSLE2014_RANKING$X2014_CENTRE_CODE <- str_replace_all(PSLE2014_RANKING$X2014_CENTRE_CODE, "-", "")
PSLE2014_RANKING$X2014_CENTRE_CODE <- str_replace_all(PSLE2014_RANKING$X2014_CENTRE_CODE, "S", "")
PSLE2013_IMPROVEMENT$X2013_CENTRE_CODE <- str_replace_all(PSLE2013_IMPROVEMENT$X2013_CENTRE_CODE, "-", "")
PSLE2013_IMPROVEMENT$X2013_CENTRE_CODE <- str_replace_all(PSLE2013_IMPROVEMENT$X2013_CENTRE_CODE, "S", "")
PSLE2014_IMPROVEMENT$X2014_CENTRE_CODE <- str_replace_all(PSLE2014_IMPROVEMENT$X2014_CENTRE_CODE, "-", "")
PSLE2014_IMPROVEMENT$X2014_CENTRE_CODE <- str_replace_all(PSLE2014_IMPROVEMENT$X2014_CENTRE_CODE, "S", "")

# Add years
PSLE2011_RANKING$year <- 2011
PSLE2012_RANKING$year <- 2012
PSLE2013_RANKING$year <- 2013  
PSLE2014_RANKING$year <- 2014  
PSLE2013_IMPROVEMENT$year <- 2013  
PSLE2014_IMPROVEMENT$year <- 2014  

#Rename variables needed for merging
PSLE2011_RANKING <- rename(PSLE2011_RANKING, schoolcode = X2011_CENTRE_CODE)
PSLE2012_RANKING <- rename(PSLE2012_RANKING, schoolcode = X2012_CENTRE_CODE)
PSLE2013_RANKING <- rename(PSLE2013_RANKING, schoolcode = X2013_CENTRE_CODE)
PSLE2014_RANKING <- rename(PSLE2014_RANKING, schoolcode = X2014_CENTRE_CODE)

#Wollen wir Improvement drin haben? Könnten wir ja selbst berechnen. Wenn, dann müssen wir die Variablennamen in CSV noch ändern, die sind nämlich identisch mit denen aus Ranking
#PSLE2013_IMPROVEMENT <- rename(PSLE2013_IMPROVEMENT, schoolcode = X2013_CENTRE.CODE)
#PSLE2014_IMPROVEMENT <- rename(PSLE2014_IMPROVEMENT, schoolcode = PS1201.046)

###### Merging
MERGED <- full_join(MASTER, PSLE2013_RANKING, by= c("year", "schoolcode"))
MERGED <- full_join(MERGED, PSLE2014_RANKING, by= c("year", "schoolcode"))
#MERGED <- join(MERGED, PSLE2013_IMPROVEMENT, by= c("year", "schoolcode"))
#MERGED <- join(MERGED, PSLE2014_IMPROVEMENT, by= c("year", "schoolcode"))
MERGED <- full_join(MERGED, PSLE2012_RANKING, by= c("year", "schoolcode"))
MERGED <- full_join(MERGED, PSLE2011_RANKING, by= c("year", "schoolcode"))



###############################
# Create identifier in MASTER for merging with Jambo Bukoba as no schoolcode available
###############################

# That means put district and school together in one cell and get rid of things 
#like primary, englisch that does not necessarily belong to the school name
#Load again only if you do not want to run all the code above before
#MERGED <- read.csv("Merge/allmerged.csv", stringsAsFactors = FALSE, header = TRUE, sep=";") # Import with no header

#######################Clean school names

#Copy strings, for 2013 and 2014 and also 2012 and 2011 which is a different variable
MERGED$Schoolname_cleaned <- MERGED$X2013_CENTRE_NAME
MERGED$Schoolname_cleaned[is.na(MERGED$Schoolname_cleaned)] <- MERGED$X2014_CENTRE_NAME[is.na(MERGED$Schoolname_cleaned)]
MERGED$Schoolname_cleaned[is.na(MERGED$Schoolname_cleaned)] <- MERGED$X2012_CENTRE_NAME[is.na(MERGED$Schoolname_cleaned)]
MERGED$Schoolname_cleaned[is.na(MERGED$Schoolname_cleaned)] <- MERGED$X2011_CENTRE_NAME[is.na(MERGED$Schoolname_cleaned)]

MERGED$Schoolname_cleaned <- str_replace_all(MERGED$Schoolname_cleaned, 'PRIMARY', '')
MERGED$Schoolname_cleaned <- str_replace_all(MERGED$Schoolname_cleaned, 'SCHOOL', '')
MERGED$Schoolname_cleaned <- str_replace_all(MERGED$Schoolname_cleaned, 'ENGLISHM', '')
MERGED$Schoolname_cleaned <- str_replace_all(MERGED$Schoolname_cleaned, 'ENGLISH', '')
MERGED$Schoolname_cleaned <- str_replace_all(MERGED$Schoolname_cleaned, 'ENGLISCH', '')
MERGED$Schoolname_cleaned <- str_replace_all(MERGED$Schoolname_cleaned, 'ENGL', '')
MERGED$Schoolname_cleaned <- str_replace_all(MERGED$Schoolname_cleaned, '-', '')
MERGED$Schoolname_cleaned <- str_replace_all(MERGED$Schoolname_cleaned, '        N', "'N'")
MERGED$Schoolname_cleaned <- str_replace_all(MERGED$Schoolname_cleaned, ' A ', "'A'")
MERGED$Schoolname_cleaned <- str_replace_all(MERGED$Schoolname_cleaned, ' B ', "'B'")
MERGED$Schoolname_cleaned <- str_replace_all(MERGED$Schoolname_cleaned, 'MEDIUM', '')
MERGED$Schoolname_cleaned <- str_replace_all(MERGED$Schoolname_cleaned, 'ISLAMIC', '')
MERGED$Schoolname_cleaned <- str_replace_all(MERGED$Schoolname_cleaned, ' ISH ', '')
MERGED$Schoolname_cleaned <- str_replace_all(MERGED$Schoolname_cleaned, 'RHEC  M  ', 'RHEC')
MERGED$Schoolname_cleaned <- str_replace_all(MERGED$Schoolname_cleaned, 'PR.', '')
MERGED$Schoolname_cleaned <- str_replace_all(MERGED$Schoolname_cleaned, ' ', '')
table(MERGED$Schoolname_cleaned, MERGED$year)

#Make sure that district names are all in the same column
MERGED$DISTRICTNAME <- MERGED$X2012_DISTRICT_NAME
MERGED$DISTRICTNAME[is.na(MERGED$DISTRICTNAME)] <- MERGED$X2011_DISTRICT_NAME[is.na(MERGED$DISTRICTNAME)]
MERGED$DISTRICTNAME[is.na(MERGED$DISTRICTNAME)] <- MERGED$X2013_DISTRICT_NAME[is.na(MERGED$DISTRICTNAME)]
MERGED$DISTRICTNAME[is.na(MERGED$DISTRICTNAME)] <- MERGED$X2014_DISTRICT_NAME[is.na(MERGED$DISTRICTNAME)]
table(MERGED$DISTRICTNAME, MERGED$year)

# Merge with district names and get rid of spaces
MERGED$Identifier <- paste(MERGED$DISTRICTNAME, MERGED$Schoolname_cleaned)
MERGED$Identifier <- str_replace_all(MERGED$Identifier, ' ', '')
table(MERGED$Identifier)

###########################################
# Create time series data
###########################################

# Remove duplicates by year and Identifier
TimeSeries <- MERGED %>% distinct(Identifier, year)

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

TimeSeries$Candidates[is.na(TimeSeries$Candidates)] <- TimeSeries$X2011_CLEAN_CANDIDATES_2011[is.na(TimeSeries$Candidates)]
TimeSeries$X2011_CLEAN_CANDIDATES_2011 <- NULL

TimeSeries$Candidates[is.na(TimeSeries$Candidates)] <- TimeSeries$X2013_CLEAN_CANDIDATES_2013[is.na(TimeSeries$Candidates)]
TimeSeries$X2013_CLEAN_CANDIDATES_2013 <- NULL

TimeSeries$Candidates[is.na(TimeSeries$Candidates)] <- TimeSeries$X2014_CLEAN_CANDIDATES_2014[is.na(TimeSeries$Candidates)]
TimeSeries$X2014_CLEAN_CANDIDATES_2014 <- NULL


# Generate passed per year variable

TimeSeries$PassedCandidates <- TimeSeries$X2012_NUMBER_OF_STUDENTS_PASSED_.A.C.
TimeSeries$X2012_NUMBER_OF_STUDENTS_PASSED_.A.C. <- NULL

TimeSeries$PassedCandidates[is.na(TimeSeries$PassedCandidates)] <- TimeSeries$X2011_NUMBER_OF_STUDENTS_PASSED_.A.C.[is.na(TimeSeries$PassedCandidates)]
TimeSeries$X2011_NUMBER_OF_STUDENTS_PASSED_.A.C. <- NULL

TimeSeries$PassedCandidates[is.na(TimeSeries$PassedCandidates)] <- TimeSeries$X2013_NUMBER_OF_STUDENTS_PASSED_.A.C.[is.na(TimeSeries$PassedCandidates)]
TimeSeries$X2013_NUMBER_OF_STUDENTS_PASSED_.A.C. <- NULL

TimeSeries$PassedCandidates[is.na(TimeSeries$PassedCandidates)] <- TimeSeries$X2014_NUMBER_OF_STUDENTS_PASSED_.A.C.[is.na(TimeSeries$PassedCandidates)]
TimeSeries$X2014_NUMBER_OF_STUDENTS_PASSED_.A.C. <- NULL

table(TimeSeries$PassedCandidates, useNA = "always")

# Generate average grade variables
TimeSeries$Average_grade <- TimeSeries$X2012_AVERAGE_TOTAL_MARKS_..250._2012
TimeSeries$X2012_AVERAGE_TOTAL_MARKS_..250._2012 <- NULL

TimeSeries$Average_grade[is.na(TimeSeries$Average_grade)] <- TimeSeries$X2011_AVERAGE_TOTOL_MARKS_..250._2011[is.na(TimeSeries$Average_grade)]
TimeSeries$X2011_AVERAGE_TOTOL_MARKS_..250._2011 <- NULL

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

TimeSeries$Ranking[is.na(TimeSeries$Ranking)] <- TimeSeries$X2011_RANK_OF_SCHOOL_2011[is.na(TimeSeries$Ranking)]
TimeSeries$X2011_RANK_OF_SCHOOL_2011 <- NULL

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

TimeSeries$Band[is.na(TimeSeries$Band)] <- TimeSeries$X2011_BAND_OF_SCHOOL_2011[is.na(TimeSeries$Band)]
TimeSeries$X2011_BAND_OF_SCHOOL_2011 <- NULL

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

#Save as csv
write.csv(TimeSeries, "Final Data/Time Series.csv")
