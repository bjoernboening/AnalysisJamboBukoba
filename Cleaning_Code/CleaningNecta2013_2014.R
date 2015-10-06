#######################
# Clean Necta 2013 2014
# Written by Christopher
# Last edit 2015-8-12 by Christopher
#######################

#######################
# This code cleanes the PSLE results for 2013 and 2014. The data is parsed
# and scrapped with Python before. 
#######################

#########################################################
# Seeting up R and loading file
#########################################################

library(stringr) # Needed for splitting
library(plyr) # Needed for renaming
library(zoo) #Needed for the schools

# Working directory
setwd("C:/Users/Christopher/Google Drive/Data Animals/Jambo Bukoba/Data/Archiv/necta.org (primary school leaving examination results)/")

# Import csv
File2013raw <- read.csv("File2013_raw.csv", stringsAsFactors = FALSE, header = FALSE) # Import with no header

# So that you do not have to import the data again if you destroy your dataframe, useful as data is large and processing the CSV takes time
File2013 <- File2013raw

# As the dataset is really big, create a small sample for testing or keep first 1000 if you want
#File2013 <- File2013[sample(nrow(File2013), 10000), ] #here keeping 1000 random
#File2013 <- File2013[1:1000,]

#####################################################
# Transform and clean
#####################################################

#Clean school name
school <- ifelse((grepl("<h3>", File2013$V1)), File2013$V1, NA) #Look if V1 cotains <h3>, and if yes, copy the value into element school
File2013 = data.frame(File2013, school) # Combine
File2013$school <- na.locf(File2013$school) # Functions that searches na and replaces it with the closest value

# Drop candidate number rows and school title
File2013 <- subset(File2013, V1!="b'CAND. NO'") # Get rid of candidate number rows by keeping only those who do not cotain Cand.No
schoolorstudent <- ifelse((grepl("<h3>", File2013$V1)), "School", "Student") # Mark wether school or student by looking for <h3>
File2013 = data.frame(File2013, schoolorstudent) # Combine
File2013 <- subset(File2013, schoolorstudent=="Student") # Get rid of schools by only keeping rows with Student
File2013 <- subset(File2013, select = -c(schoolorstudent) ) # Get rid of the schoolorstudent Variable
 
# Create a function that keeps everything between the 3rd and the last character and apply to V1 to V4
remove_charachter1 <- function(x) {
  x <- str_sub(x, 3, -2)
}

File2013[1:4] <- lapply(File2013[1:4], remove_charachter1) # Apply the function for the first four variables

# Split V4 in columns at "," and append to dataframe
split <- str_split_fixed(File2013$V4, ",",6)
File2013 <- data.frame(File2013, split)

#Rename column header
File2013 <- rename(File2013, c(
  "V1" = "student_number",
  "V2" = "student_sex",
  "V3" = "student_name",
  "V4" = "string_grade",
  "X1" = "kiswahili_grade", 
  "X2" = "english_grade", 
  "X3" = "maarifa_grade", 
  "X4" = "hisabati_grade", 
  "X5" = "science_grade",
  "X6" = "average_grade"))

#Funtion to Keep only the grade, so the last letter
remove_charachter2 <- function(x) {
  x <- str_sub(x, -2) # It counts backwards, so deletes everything until the second char counted backwards
}
File2013[6:11] <- lapply(File2013[6:11], remove_charachter2) #Appply function

#Create year (2013)
rownumber <- nrow(File2013)
year <- matrix(c(2013), nrow=rownumber, ncol=1, byrow = TRUE)
File2013 <- cbind(File2013, year)

#Clean school data by keeping the middle values
File2013$school <- str_sub(File2013$school, 21,-11)

#Split into school name and school code
File2013$schoolcode <- str_sub(File2013$school, -8)
File2013$schoolname <- str_sub(File2013$school, 1, -12)

#Mark region Kagera, student number starts with 05
File2013$regionKagera <- ifelse((grepl("PS05", File2013$student_number)), "Kagera", "Other") # Mark wether school or student by looking for <h3>
#Mark region Geita, student number start with 24
File2013$regionGeita <- ifelse((grepl("PS24", File2013$student_number)), "Geita", "Other") # Mark wether school or student by looking for <h3>
  

# Save data
#######################################################
# Same thing for 2014
######################################################

# Import csv
File2014raw <- read.csv("File2014_raw.csv", stringsAsFactors = FALSE, header = FALSE) # Import with no header

# So that you do not have to import the data again if you destroy your dataframe, useful as data is large and processing the CSV takes time
File2014 <- File2014raw

# As the dataset is really big, create a small sample for testing or keep first 1000 if you want
#File2014 <- File2014[sample(nrow(File2014), 10000), ] #here keeping 1000 random
#File2014 <- File2014[1:1000,]

#####################################################
# Transform and clean
#####################################################

#Clean school name
school <- ifelse((grepl("<h3>", File2014$V1)), File2014$V1, NA) #Look if V1 cotains <h3>, and if yes, copy the value into element school
File2014 = data.frame(File2014, school) # Combine
File2014$school <- na.locf(File2014$school) # Functions that searches na and replaces it with the closest value

# Drop candidate number rows and school title
File2014 <- subset(File2014, V1!="b'CAND. NO'") # Get rid of candidate number rows by keeping only those who do not cotain Cand.No
schoolorstudent <- ifelse((grepl("<h3>", File2014$V1)), "School", "Student") # Mark wether school or student by looking for <h3>
File2014 = data.frame(File2014, schoolorstudent) # Combine
File2014 <- subset(File2014, schoolorstudent=="Student") # Get rid of schools by only keeping rows with Student
File2014 <- subset(File2014, select = -c(schoolorstudent) ) # Get rid of the schoolorstudent Variable

# Create a function that keeps everything between the 3rd and the last character and apply to V1 to V4
remove_charachter1 <- function(x) {
  x <- str_sub(x, 3, -2)
}

File2014[1:4] <- lapply(File2014[1:4], remove_charachter1) # Apply the function for the first four variables

# Split V4 in columns at "," and append to dataframe
split <- str_split_fixed(File2014$V4, ",",6)
File2014 <- data.frame(File2014, split)

#Rename column header
File2014 <- rename(File2014, c(
  "V1" = "student_number",
  "V2" = "student_sex",
  "V3" = "student_name",
  "V4" = "string_grade",
  "X1" = "kiswahili_grade", 
  "X2" = "english_grade", 
  "X3" = "maarifa_grade", 
  "X4" = "hisabati_grade", 
  "X5" = "science_grade",
  "X6" = "average_grade"))

#Funtion to Keep only the grade, so the last letter
remove_charachter2 <- function(x) {
  x <- str_sub(x, -2) # It counts backwards, so deletes everything until the second char counted backwards
}
File2014[6:11] <- lapply(File2014[6:11], remove_charachter2) #Appply function

#Create year (2014)
rownumber <- nrow(File2014)
year <- matrix(c(2014), nrow=rownumber, ncol=1, byrow = TRUE)
File2014 <- cbind(File2014, year)

#Clean school data by keeping the middle values
File2014$school <- str_sub(File2014$school, 21,-11)

#Split into school name and school code
File2014$schoolcode <- str_sub(File2014$school, -8)
File2014$schoolname <- str_sub(File2014$school, 1, -12)


#Mark region Kagera, student number starts with 05
File2014$regionKagera <- ifelse((grepl("PS05", File2014$student_number)), "Kagera", "Other") # Mark wether school or student by looking for <h3>
#Mark region Geita, student number start with 24
File2014$regionGeita <- ifelse((grepl("PS24", File2014$student_number)), "Geita", "Other") # Mark wether school or student by looking for <h3>

######################################################
# Create one containing both
######################################################
File2013_2014 <- rbind(File2013, File2014)
write.table(File2013_2014, file = "C:/Users/Christopher/Google Drive/Data Animals/Jambo Bukoba/Data/Archiv/necta.org (primary school leaving examination results)/File2013_2014_Cleaned_all.csv", sep=";", row.names=FALSE)

#CSV only containing Kagera
File2013_2014_Kagera <- subset(File2013_2014, regionKagera == "Kagera") # Get rid of non-Kagera
write.table(File2013_2014_Kagera, file = "C:/Users/Christopher/Google Drive/Data Animals/Jambo Bukoba/Data/Archiv/necta.org (primary school leaving examination results)/File2013_2014_Cleaned_Kagera.csv", sep=";", row.names=FALSE)

File2013_2014_Geita <- subset(File2013_2014, regionGeita == "Geita") # Get rid if not Geita
library(dplyr)
File2013_2014_KageraGeita <- rbind_list(File2013_2014_Kagera, File2013_2014_Geita)
write.table(File2013_2014_KageraGeita, file = "C:/Users/Christopher/Google Drive/Data Animals/Jambo Bukoba/Data/Archiv/necta.org (primary school leaving examination results)/File2013_2014_Cleaned_KageraGeita.csv", sep=";", row.names=FALSE)


#That's it folks