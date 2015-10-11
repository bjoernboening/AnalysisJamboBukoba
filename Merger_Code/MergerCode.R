#######################
# MergerCode
# Written by Christopher
# Last edit 2015-8-12 by Christopher
#######################

#######################
# This code merges 
# 1.) the books delivery data cleaned by Clean school data R
# 2.) the student data cleand by Clean Necta 2013 2014 
# 3.) the aggregated school data that is cleaned anyway.
# Some minor cleaning and transforming is necessary. In a first step,
# MASTER and School are merged, in a second step the aggregated data is added.
#######################

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
PSLE2012_RANKING <- read.csv("Archiv/necta.org (aggregated)/PSLE2012_RANKING.csv", stringsAsFactors = FALSE, header = TRUE, sep = ";") # Import with no header
PSLE2013_IMPROVEMENT <- read.csv("Archiv/necta.org (aggregated)/PSLE2013_IMPROVEMENT.csv", stringsAsFactors = FALSE, header = TRUE, sep = ";") # Import with no header
PSLE2013_RANKING <- read.csv("Archiv/necta.org (aggregated)/PSLE2013_RANKING.csv", stringsAsFactors = FALSE, header = TRUE, sep = ";") # Import with no header
PSLE2014_IMPROVEMENT <- read.csv("Archiv/necta.org (aggregated)/PSLE2014_IMPROVEMENT.csv", stringsAsFactors = FALSE, header = TRUE, sep = ";") # Import with no header
PSLE2014_RANKING <- read.csv("Archiv/necta.org (aggregated)/PSLE2014_RANKING.csv", stringsAsFactors = FALSE, header = TRUE, sep = ";") # Import with no header

########## Clean data
PSLE2012_RANKING$X2012_CENTRE_CODE <- str_replace_all(PSLE2012_RANKING$X2012_CENTRE_CODE, "-", "")
PSLE2012_RANKING$X2012CENTRE_CODE <- str_replace_all(PSLE2012_RANKING$X2012_CENTRE_CODE, "S", "")
PSLE2013_RANKING$X2013_CENTRE_CODE <- str_replace_all(PSLE2013_RANKING$X2013_CENTRE_CODE, "-", "")
PSLE2013_RANKING$X2013_CENTRE_CODE <- str_replace_all(PSLE2013_RANKING$X2013_CENTRE_CODE, "S", "")
PSLE2014_RANKING$X2014_CENTRE_CODE <- str_replace_all(PSLE2014_RANKING$X2014_CENTRE_CODE, "-", "")
PSLE2014_RANKING$X2014_CENTRE_CODE <- str_replace_all(PSLE2014_RANKING$X2014_CENTRE_CODE, "S", "")
PSLE2013_IMPROVEMENT$X2013_CENTRE_CODE <- str_replace_all(PSLE2013_IMPROVEMENT$X2013_CENTRE_CODE, "-", "")
PSLE2013_IMPROVEMENT$X2013_CENTRE_CODE <- str_replace_all(PSLE2013_IMPROVEMENT$X2013_CENTRE_CODE, "S", "")
PSLE2014_IMPROVEMENT$X2014_CENTRE_CODE <- str_replace_all(PSLE2014_IMPROVEMENT$X2014_CENTRE_CODE, "-", "")
PSLE2014_IMPROVEMENT$X2014_CENTRE_CODE <- str_replace_all(PSLE2014_IMPROVEMENT$X2014_CENTRE_CODE, "S", "")

# Add years
PSLE2012_RANKING$year <- 2012
PSLE2013_RANKING$year <- 2013  
PSLE2014_RANKING$year <- 2014  
PSLE2013_IMPROVEMENT$year <- 2013  
PSLE2014_IMPROVEMENT$year <- 2014  

#Rename variables needed for merging
PSLE2012_RANKING <- rename(PSLE2012_RANKING, schoolcode = X2012_CENTRE_CODE)
PSLE2013_RANKING <- rename(PSLE2013_RANKING, schoolcode = X2013_CENTRE_CODE)
PSLE2014_RANKING <- rename(PSLE2014_RANKING, schoolcode = X2014_CENTRE_CODE)

#Wollen wir Improvement drin haben? Könnten wir ja selbst berechnen. Wenn, dann müssen wir die Variablennamen in CSV noch ändern, die sind nämlich identisch mit denen aus Ranking
#PSLE2013_IMPROVEMENT <- rename(PSLE2013_IMPROVEMENT, schoolcode = X2013_CENTRE.CODE)
#PSLE2014_IMPROVEMENT <- rename(PSLE2014_IMPROVEMENT, schoolcode = PS1201.046)

###### Merging
MERGED <- join(MASTER, PSLE2013_RANKING, by= c("year", "schoolcode"))
MERGED <- join(MERGED, PSLE2014_RANKING, by= c("year", "schoolcode"))
#MERGED <- join(MERGED, PSLE2013_IMPROVEMENT, by= c("year", "schoolcode"))
#MERGED <- join(MERGED, PSLE2014_IMPROVEMENT, by= c("year", "schoolcode"))
MERGED <- full_join(MERGED, PSLE2012_RANKING, by= c("year", "schoolcode"), match = "all")

###### Save file
#write.table(MERGED, file = 'Merge/allmerged.csv', sep=";", row.names=TRUE)

###############################
# Create identifier in MASTER for merging with Jambo Bukoba as no schoolcode available
###############################

# That means put district and school together in one cell and get rid of things 
#like primary, englisch that does not necessarily belong to the school name
#Load again only if you do not want to run all the code above before
#MERGED <- read.csv("Merge/allmerged.csv", stringsAsFactors = FALSE, header = TRUE, sep=";") # Import with no header

#######################Clean school names

#Copy strings
MERGED$Schoolname_cleaned <- MERGED$schoolname

head(MERGED$Schoolname_cleaned)

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

#Make sure that district names are all in the same column
MERGED$DISTRICTNAME <- MERGED$X2012_DISTRICT_NAME
MERGED$DISTRICTNAME[is.na(MERGED$DISTRICTNAME)] <- MERGED$X2013_DISTRICT_NAME[is.na(MERGED$DISTRICTNAME)]
MERGED$DISTRICTNAME[is.na(MERGED$DISTRICTNAME)] <- MERGED$X2014_DISTRICT_NAME[is.na(MERGED$DISTRICTNAME)]
table(MERGED$DISTRICTNAME)


# Merge with district names and get rid of spaces
MERGED$Identifier <- paste(MERGED$DISTRICTNAME, MERGED$Schoolname_cleaned)
MERGED$Identifier <- str_replace_all(MERGED$Identifier, ' ', '')

#List of identifierts from Mergeds if you want to as csv
#write.table(MERGED$Identifier, file = "Merge/Identifier merged all.csv", sep=";", row.names=TRUE)

###############################
# Merge Jambo Bukoba School projets to MASTER
###############################

#Load file containing the schoolprojects and make capital
Schoolprojects <- read.csv("MasterMerge/20150921_Schoolprojects only transformation 2.csv", stringsAsFactors = FALSE, header = TRUE, sep=";") # Import with no header
Schoolprojects$Identifier <- toupper(Schoolprojects$Identifier)

# Use full type so that those which do not match appear at the end
MERGED <- join(MERGED, Schoolprojects, by= c("Identifier"), type = "full")
# Alle erfolgreich gemergt

MERGED$Schoolproject_happened[is.na(MERGED$Schoolproject_happened)] <- 0
MERGED[] <- lapply(MERGED$Project_2013, Function_NA_0)

# Save if necessary
#write.table(MERGED, file = "Merge/allmerged.csv", sep=";", row.names=TRUE)

###############################
# Merge Jambo Bukoba Bonanzas to MASTER
###############################

Bonanzas <- read.csv("MasterMerge/20150921_Bonanzas only transformation 2(CC).csv", stringsAsFactors = FALSE, header = TRUE, sep=";") # Import with no header
Bonanzas$Identifier <- toupper(Bonanzas$Identifier)

# Use full type so that those which do not match appear at the end
MERGED <- join(MERGED, Bonanzas, by= c("Identifier"), type = "full")


#Safe if necessary
#write.table(MERGED, file = "Merge/allmerged.csv", sep=";", row.names=TRUE)

###############################
# Merge Jambo Bukoba Workshops to MASTER
###############################

Workshops <- read.csv("MasterMerge/20151002_Workshops only school level (CC).csv", stringsAsFactors = FALSE, header = TRUE, sep=";") # Import with no header
Workshops$Identifier <- toupper(Workshops$Identifier)

# Use full type so that those which do not match appear at the end
MERGED <- join(MERGED, Workshops, by= c("Identifier"), type ="full")

write.table(MERGED, file = "Merge/allmerged.csv", sep=";", row.names=TRUE)
