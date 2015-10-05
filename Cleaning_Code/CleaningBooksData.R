#########################
# Clean school data (books delivery)
# Written by Christopher
# Last edit 2015-08-12 by Christopher
########################

#######################
# Description: Use this code to clean the school data scrapped with
# Python code. The raw data is in two files, Data_raw contains the actual data and 
# Schooldata_raw cotains the name of schools. This code merges both, transforms and 
# cleans them so they can be used by the R code "MergerCodeR
#######################

########### Clean School Data
library(stringr)
library(plyr)
library(dplyr)
library(zoo)

School <- read.csv("C:/Users/Christopher/Google Drive/Data Animals/Jambo Bukoba/Data/pesptz.org (books delivered)/Data_raw.csv", stringsAsFactors = FALSE, header = FALSE, sep=";", quote = "") # Import with no header
# If this is not your path, you might wanna use this
#School <- read.csv(file.choose(), stringsAsFactors = FALSE, header = FALSE, sep=";", quote = "") # Import with no header
########### Git rid of symbols
list = c(">", "<", "td", "/", ",", "style=", "text-align:right", '"')
for (i in list) {
  School <- as.data.frame(sapply(School,gsub,pattern=i,replacement=""))
}

########### Get rid of last character in V4
remove_charachter <- function(x) {
  x <- str_sub(x, 0, -2)
}

School$V4 <- lapply(School$V4, remove_charachter) #Appply function

########### Get rid of row with 1
School <- subset(School, V1!="1")

########### Copy class to the left and one down
School$class <- ifelse(School$V1 == "I ", "I", NA ) #Look if V1 cotains <h3>, and if yes, copy the value into element school
School$class <- ifelse(School$V1 == "II ", "II", School$class) #Look if V1 cotains <h3>, and if yes, copy the value into element schoolSchool$class <- na.locf(School$class)
School$class <- ifelse(School$V1 == "III ", "III", School$class) #Look if V1 cotains <h3>, and if yes, copy the value into element school
School$class <- ifelse(School$V1 == "IV ", "IV", School$class) #Look if V1 cotains <h3>, and if yes, copy the value into element school
School$class <- ifelse(School$V1 == "V ", "V", School$class) #Look if V1 cotains <h3>, and if yes, copy the value into element school
School$class <- ifelse(School$V1 == "VI ", "VI", School$class) #Look if V1 cotains <h3>, and if yes, copy the value into element school
School$class <- ifelse(School$V1 == "VII ", "VII", School$class) #Look if V1 cotains <h3>, and if yes, copy the value into element schoolSchool$class <- ifelse(School$V1 == "I ", "I", NA ) #Look if V1 cotains <h3>, and if yes, copy the value into element school
School$class <- na.locf(School$class) # Functions that searches na and replaces it with the closest value
School <- subset(School, V2!="")

########### Indicate each school
School$school <- ifelse(School$class == "I", "newschool", NA ) #Look if V1 cotains <h3>, and if yes, copy the value into element school

########### Asssign number which will be used for merging later
rownumber = (1:6153)
School = data.frame(School, rownumber)

########### Create a subset, keeping only one observation of each school makes merging the schoolnames easier
Schoolsubset <- subset(School, school=="newschool")

############ Clean school names
Schoolnames <- read.csv("C:/Users/Christopher/Google Drive/Data Animals/Jambo Bukoba/Data/pesptz.org (books delivered)/Listofschools_raw.csv", stringsAsFactors = FALSE, header = FALSE, sep=";", quote = "") # Import with no header
Schoolnames <- subset(Schoolnames, V1!='"')
Schoolnames <- subset(Schoolnames, V1!='  Leave","')
Schoolnames <- subset(Schoolnames, V1!='(0)","')
Schoolnames$V1 <- str_replace_all(Schoolnames$V1, '","', "")

########### Get rid of rows
Schoolnames <- subset(Schoolnames , V1!='(0)"')
Schoolnames <- subset(Schoolnames , V1!='')

########### Append school data and schoolnames
Combined = data.frame(Schoolsubset, Schoolnames) # Combine


########## Merge
Complete <- merge(School, Combined, by = "rownumber" , all = TRUE)

######### Drop rows that are now double
# Unlist Complete$V4.x,as is list and we need numeric
Complete$V4.x <- as.numeric(unlist(Complete$V4.x))
# Then keep only those we want
Complete = data.frame(Complete$V1.x, Complete$V2.x, Complete$V3.x, Complete$V4.x, Complete$class.x, Complete$V1.1)

######### Fill missing values for schoolnames 
Complete$Complete.V1.1 <- na.locf(Complete$Complete.V1.1) # Functions that searches na and replaces it with the closest value

#### Rename and drop
Complete <- rename(Complete, 
  class = Complete.class.x,
  schoolname = Complete.V1.1,
  pupils = Complete.V1.x,
  books_delivered = Complete.V2.x,
  books_planned = Complete.V3.x,
  Percentage_of_delivery = Complete.V4.x
  )

write.table(Complete, file = "C:/Users/Christopher/Google Drive/Data Animals/Jambo Bukoba/Data/pesptz.org (books delivered)/Schooldata_final.csv", sep=";", row.names=FALSE)

