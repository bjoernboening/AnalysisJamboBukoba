#####################################
# Give the data meaningful names after saved for participants
# Run after analysis code
#####################################

### Load necessary libraries, if you miss a library type install.packes("nameofpackage")
library(plyr)


# Set working directory
try(setwd("C:/Users/Christopher/Google Drive/Data Animals/Jambo Bukoba/Data/Final data"))

# Load data
SchoolLevel <- read.csv("School Level.csv")
StudentLevel  <- read.csv("Final Data cleaned.csv")
TimeSeries <- read.csv("Time Series.csv")

# Rename SchoolLevel data
SchoolLevel <- rename(SchoolLevel, c("X2013_CLEAN_CANDIDATES_2013"="Candidates_2013",	"X2013_NUMBER_OF_STUDENTS_PASSED_.A.C."="Passed_2013",	"X2013_AVERAGE_TOTOL_MARKS_..250._2013"="Average_Marks_2013",	"X2013_AVERAGE_TOTOL_MARKS_..250._2012"="DELETE",	"X2013_CHANGE_ON_AVERAGE_TOTAL_MARKS_FROM_2012"="Change_Average_Marks_2013",	"X2013_BAND_OF_SCHOOL_2013"="Band_2013",	"X2013_BAND_OF_SCHOOL_2012"="DELETE",	"X2013_RANK_OF_SCHOOL_2013"="Rank_2013",	"X2013_RANK_OF_SCHOOL_2012"="DELETE",	"year.y"="DELETE",	"X2014_CLEAN_CANDIDATES_2014"="Candidates_2014",	"X2014_NUMBER_OF_STUDENTS_PASSED_.A.C."="Passed_2014",	"X2014_AVERAGE_TOTOL_MARKS_..250._2014"="Average_Marks_2014",	"X2014_AVERAGE_TOTOL_MARKS_..250._2013"="DELETE",	"X2014_CHANGE_ON_AVERAGE_TOTAL_MARKS_FROM_2013"="Change_Average_Marks_2014",	"X2014_BAND_OF_SCHOOL_2014"="Band_2014",	"X2014_BAND_OF_SCHOOL_2013"="DELETE",	"X2014_RANK_OF_SCHOOL_2014"="Rank_2014",	"X2014_RANK_OF_SCHOOL_2013"="DELETE",	"X2012_CLEAN_CANDIDATES_2012"="Candidates_2012",	"X2012_NUMBER_OF_STUDENTS_PASSED_.A.C."="Passed_2012",	"X2012_AVERAGE_TOTAL_MARKS_..250._2012"="Average_Marks_2012",	"X2012_AVERAGE_TOTAL_MARKS_..250._2011"="Average_Marks_2011",	"X2012_CHANGE_ON_AVERAGE_TOTAL_MARKS_FROM_2011"="Change_Average_2012",	"X2012_BAND_OF_SCHOOL_2012"="",	"X2012_BAND_OF_SCHOOL_2011"="Band_2011",	"X2012_RANK_OF_SCHOOL_2012"="Rank_2012",	"X2012_RANK_OF_SCHOOL_2011"="Rank_2011",	"X2012_CleanLastYear"="Candidates_2011",	"X2012_PassedLastYear"="Passed_2011",	"X2012_PercentPassLastYear"="DELETE"))
SchoolLevel$DELETE <- NULL
SchoolLevel$DELETE <- NULL
SchoolLevel$DELETE <- NULL
SchoolLevel$DELETE <- NULL
SchoolLevel$DELETE <- NULL
SchoolLevel$DELETE <- NULL
SchoolLevel$DELETE <- NULL
SchoolLevel$DELETE <- NULL

# Rename StudentLevel data
StudentLevel <- rename(StudentLevel, c("X2013_CLEAN_CANDIDATES_2013"="Candidates_2013",	"X2013_NUMBER_OF_STUDENTS_PASSED_.A.C."="Passed_2013",	"X2013_AVERAGE_TOTOL_MARKS_..250._2013"="Average_Marks_2013",	"X2013_AVERAGE_TOTOL_MARKS_..250._2012"="DELETE",	"X2013_CHANGE_ON_AVERAGE_TOTAL_MARKS_FROM_2012"="Change_Average_Marks_2013",	"X2013_BAND_OF_SCHOOL_2013"="Band_2013",	"X2013_BAND_OF_SCHOOL_2012"="DELETE",	"X2013_RANK_OF_SCHOOL_2013"="Rank_2013",	"X2013_RANK_OF_SCHOOL_2012"="DELETE",	"year.y"="DELETE",	"X2014_CLEAN_CANDIDATES_2014"="Candidates_2014",	"X2014_NUMBER_OF_STUDENTS_PASSED_.A.C."="Passed_2014",	"X2014_AVERAGE_TOTOL_MARKS_..250._2014"="Average_Marks_2014",	"X2014_AVERAGE_TOTOL_MARKS_..250._2013"="DELETE",	"X2014_CHANGE_ON_AVERAGE_TOTAL_MARKS_FROM_2013"="Change_Average_Marks_2014",	"X2014_BAND_OF_SCHOOL_2014"="Band_2014",	"X2014_BAND_OF_SCHOOL_2013"="DELETE",	"X2014_RANK_OF_SCHOOL_2014"="Rank_2014",	"X2014_RANK_OF_SCHOOL_2013"="DELETE",	"X2012_CLEAN_CANDIDATES_2012"="Candidates_2012",	"X2012_NUMBER_OF_STUDENTS_PASSED_.A.C."="Passed_2012",	"X2012_AVERAGE_TOTAL_MARKS_..250._2012"="Average_Marks_2012",	"X2012_AVERAGE_TOTAL_MARKS_..250._2011"="Average_Marks_2011",	"X2012_CHANGE_ON_AVERAGE_TOTAL_MARKS_FROM_2011"="Change_Average_2012",	"X2012_BAND_OF_SCHOOL_2012"="",	"X2012_BAND_OF_SCHOOL_2011"="Band_2011",	"X2012_RANK_OF_SCHOOL_2012"="Rank_2012",	"X2012_RANK_OF_SCHOOL_2011"="Rank_2011",	"X2012_CleanLastYear"="Candidates_2011",	"X2012_PassedLastYear"="Passed_2011",	"X2012_PercentPassLastYear"="DELETE"))
StudentLevel$DELETE <- NULL
StudentLevel$DELETE <- NULL
StudentLevel$DELETE <- NULL
StudentLevel$DELETE <- NULL
StudentLevel$DELETE <- NULL
StudentLevel$DELETE <- NULL
StudentLevel$DELETE <- NULL
StudentLevel$DELETE <- NULL

# Rename TimeSeries data
TimeSeries <- rename(TimeSeries, c("school"="School",	"year"="Year",	"schoolcode"="Schoolcode",	"schoolname"="Schoolname",	"regionKagera"="Region_Kagera",	"regionGeita"="Region_Geita",	"X2013_CENTRE_NAME"="DELETE",	"X2013_DISTRICT_NAME"="DELETE",	"X2013_REGION_NAME"="DELETE",	"X2014_CENTRE_NAME"="DELETE",	"X2014_DISTRICT_NAME"="DELETE",	"X2014_REGION_NAME"="DELETE",	"X2012_CENTRE_NAME"="DELETE",	"X2012_DISTRICT_NAME"="DELETE",	"X2012_BAND_OF_SCHOOL_2011"="DELETE",	"X2012_RANK_OF_SCHOOL_2011"="DELETE",	"X2012_CleanLastYear"="DELETE",	"X2012_PassedLastYear"="DELETE",	"X2012_PercentPassLastYear"="DELETE",	"X2011_CENTRE_NAME"="DELETE",	"X2011_DISTRICT_NAME"="DELETE",	"X2011_PercentPassLastYear"="DELETE",	"Schoolname_cleaned"="Schoolname",	"DISTRICTNAME"="District",	"Identifier"="IDENTIFIER",	"student_sex_binary"="DELETE",	"student_sex_mean"="Student_sex_mean",	"Candidates"="Candidates",	"PassedCandidates"="Passed_Candidates",	"Average_grade"=""))
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL
TimeSeries$DELETE <- NULL

# Save all three
write.csv(SchoolLevel, "Data with good names/School Level.csv")
write.csv(StudentLevel, "Data with good names/Student Level.csv")
write.csv(TimeSeries, "Data with good names/Time Series.csv")
