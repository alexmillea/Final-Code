#set directory 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")

#load csv 
Age2016 <- read.csv("Q1_Age2016.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")
Q2Age2016 <- read.csv("Q2_Age2016.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")
Q3Age2016 <- read.csv("Q3_Age2016.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")
Q4Age2016 <- read.csv("Q4_Age2016.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")


#Q1 Age 2016 clean up
#Print
print(Age2016)

#Identify NA's
sapply(Age2016,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Age2016)

#Check total rows / remove row(s) not needed 
nrow(Age2016)
Age2016 <-Age2016[-6,]

#changing column names
names(Age2016)[names(Age2016) == "X"] <- "Age"

#check class of columns
class(Age2016$Age)
class(Age2016$Ballyfermot)
class(Age2016$North.Dublin)
class(Age2016$Lucan)
class(Age2016$Limerick)
class(Age2016$Tallaght)
class(Age2016$Roscrea)
class(Age2016$West)
class(Age2016$Tralee)

#Q2 Age 2016 clean up
#Print
print(Q2Age2016)

#Identify NA's
sapply(Q2Age2016,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q2Age2016)

#Check total rows / remove row(s) not needed 
nrow(Q2Age2016)
Q2Age2016 <-Q2Age2016[-6,]

#changing column names
names(Q2Age2016)[names(Q2Age2016) == "X"] <- "Age"

#check class of columns
class(Q2Age2016$Age)
class(Q2Age2016$Ballyfermot)
class(Q2Age2016$North.Dublin)
class(Q2Age2016$Lucan)
class(Q2Age2016$Limerick)
class(Q2Age2016$Tallaght)
class(Q2Age2016$Roscrea)
class(Q2Age2016$West)
class(Q2Age2016$Tralee)

#Q3 Age 2016 clean up
#Print
print(Q3Age2016)

#Identify NA's
sapply(Q3Age2016,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q3Age2016)

#Check total rows / remove row(s) not needed 
nrow(Q3Age2016)
Q3Age2016 <-Q3Age2016[-6,]

#changing column names
names(Q3Age2016)[names(Q3Age2016) == "X"] <- "Age"

class(Q3Age2016$Age)
class(Q3Age2016$Ballyfermot)
class(Q3Age2016$North.Dublin)
class(Q3Age2016$Lucan)
class(Q3Age2016$Limerick)
class(Q3Age2016$Tallaght)
class(Q3Age2016$Roscrea)
class(Q3Age2016$West)
class(Q3Age2016$Tralee)
class(Q3Age2016$Cork)
class(Q3Age2016$South.East)


#Q4 Age 2016 clean up
#Print
print(Q4Age2016)

#Identify NA's
sapply(Q4Age2016,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q4Age2016)
Q4Age2016 <-Q4Age2016[, -13]

#Check total rows / remove row(s) not needed 
nrow(Q4Age2016)
Q4Age2016 <-Q4Age2016[-6,]

#changing column names
names(Q4Age2016)[names(Q4Age2016) == "X"] <- "Age"

#check class of each column 
class(Q4Age2016$Age)
class(Q4Age2016$Ballyfermot)
class(Q4Age2016$North.Dublin)
class(Q4Age2016$Lucan)
class(Q4Age2016$Limerick)
class(Q4Age2016$Tallaght)
class(Q4Age2016$Roscrea)
class(Q4Age2016$West)
class(Q4Age2016$Tralee)
class(Q4Age2016$Cork)
class(Q4Age2016$South.East)
class(Q4Age2016$Collins.Ave)

#save to new csv
write.csv(Age2016, 'age2016Q1Cleaned.csv')
write.csv(Q2Age2016, 'age2016Q2Cleaned.csv')
write.csv(Q3Age2016, 'age2016Q3Cleaned.csv')
write.csv(Q4Age2016, 'age2016Q4Cleaned.csv')

#merging dataframes - Q1 + Q2
MergedAge <- merge(Age2016, Q2Age2016, 
                   by=c("Age", "Lucan", "Ballyfermot", "Tallaght", "North.Dublin", "Limerick", "Roscrea", "West", "Cork", "Tralee"),
                   all=TRUE)

#Merge data by rows and gets the sum of the values 
library(dplyr)
MergedAge <- MergedAge %>% 
  group_by(Age) %>% 
  summarise_all(funs(sum))

#Merge Q3 & Q4
MergedAge2 <- merge(Q3Age2016, Q4Age2016, 
                    by=c("Age", "Lucan", "Ballyfermot", "Tallaght", "North.Dublin", "Limerick", "Roscrea", "West", "Cork", "Tralee", "South.East"),
                    all=TRUE)

#change NA's to 0 - will merge the rows after this
MergedAge2[is.na(MergedAge2)] <- 0

library(dplyr)
MergedAge2 <- MergedAge2 %>% 
  group_by(Age) %>% 
  summarise_all(funs(sum))

#Merge Data into One DF
MergedAgeAll <- merge(MergedAge, MergedAge2, 
                      by=c("Age", "Lucan", "Ballyfermot", "Tallaght", "North.Dublin", "Limerick", "Roscrea", "West", "Cork", "Tralee"),
                      all=TRUE)

MergedAgeAll[is.na(MergedAgeAll)] <- 0

library(dplyr)
MergedAgeAll <- MergedAgeAll %>% 
  group_by(Age) %>% 
  summarise_all(funs(sum))

Age2016Final <- MergedAgeAll

#add new column date - needed when merge data
Age2016Final$Date <- c(2016, 2016, 2016, 2016, 2016)
write.csv(Age2016Final, 'Age2016FinalCleaned.csv', row.names = FALSE)


####################################
#Gender
#set directory 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")

#load csv 
Q1Gender2016 <- read.csv("Q1_Gender2016.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")
Q2Gender2016 <- read.csv("Q2_Gender2016.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")
Q3Gender2016 <- read.csv("Q3_Gender2016.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")
Q4Gender2016 <- read.csv("Q4_Gender2016.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")


#Q1 Gender 2016 clean up
#Print
print(Q1Gender2016)

#Identify NA's
sapply(Q1Gender2016,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q1Gender2016)

#Check total rows / remove row(s) not needed 
nrow(Q1Gender2016)
Q1Gender2016 <-Q1Gender2016[-4,]

#changing column names
names(Q1Gender2016)[names(Q1Gender2016) == "X"] <- "Gender"

#check the class of each column
class(Q1Gender2016$Gender)
class(Q1Gender2016$Ballyfermot)
class(Q1Gender2016$North.Dublin)
class(Q1Gender2016$Lucan)
class(Q1Gender2016$Limerick)
class(Q1Gender2016$Tallaght)
class(Q1Gender2016$Roscrea)
class(Q1Gender2016$West)
class(Q1Gender2016$Cork)
class(Q1Gender2016$Tralee)


#Q2 Gender 2016 clean up
#Print
print(Q2Gender2016)

#Identify NA's
sapply(Q2Gender2016,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q2Gender2016)

#Check total rows / remove row(s) not needed 
nrow(Q2Gender2016)
Q2Gender2016 <-Q2Gender2016[-4,]

#changing column names
names(Q2Gender2016)[names(Q2Gender2016) == "ï.."] <- "Gender"
#check class of columns
class(Q2Gender2016$Gender)
class(Q2Gender2016$Ballyfermot)
class(Q2Gender2016$North.Dublin)
class(Q2Gender2016$Lucan)
class(Q2Gender2016$Limerick)
class(Q2Gender2016$Tallaght)
class(Q2Gender2016$Roscrea)
class(Q2Gender2016$West)
class(Q2Gender2016$Cork)
class(Q2Gender2016$Tralee)

#Q3 Gender 2016 clean up
#Print
print(Q3Gender2016)

#Identify NA's
sapply(Q3Gender2016,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q3Gender2016)

#Check total rows / remove row(s) not needed 
nrow(Q3Gender2016)
Q3Gender2016 <-Q3Gender2016[-4,]

#changing column names
names(Q3Gender2016)[names(Q3Gender2016) == "ï.."] <- "Gender"

#check class of each column
class(Q3Gender2016$Gender)
class(Q3Gender2016$Ballyfermot)
class(Q3Gender2016$North.Dublin)
class(Q3Gender2016$Lucan)
class(Q3Gender2016$Limerick)
class(Q3Gender2016$Tallaght)
class(Q3Gender2016$Roscrea)
class(Q3Gender2016$West)
class(Q3Gender2016$Cork)
class(Q3Gender2016$Tralee)
class(Q3Gender2016$South.East)

#Q4 Gender 2016 clean up
#Print
print(Q4Gender2016)

#Identify NA's
sapply(Q4Gender2016,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q4Gender2016)
Q4Gender2016 <-Q4Gender2016[, -13]

#Check total rows / remove row(s) not needed 
nrow(Q4Gender2016)
Q4Gender2016 <-Q4Gender2016[-4,]

#changing column names
names(Q4Gender2016)[names(Q4Gender2016) == "ï.."] <- "Gender"

#check class of each column 
class(Q4Gender2016$Gender)
class(Q4Gender2016$Ballyfermot)
class(Q4Gender2016$North.Dublin)
class(Q4Gender2016$Lucan)
class(Q4Gender2016$Limerick)
class(Q4Gender2016$Tallaght)
class(Q4Gender2016$Roscrea)
class(Q4Gender2016$West)
class(Q4Gender2016$Cork)
class(Q4Gender2016$Tralee)
class(Q4Gender2016$South.East)
class(Q4Gender2016$Collins.Ave)

#save to new csv
write.csv(Q1Gender2016, 'Gender2016Q1Cleaned.csv')
write.csv(Q2Gender2016, 'Gender2016Q2Cleaned.csv')
write.csv(Q3Gender2016, 'Gender2016Q3Cleaned.csv')
write.csv(Q4Gender2016, 'Gender2016Q4Cleaned.csv')


#merging dataframes - Q1 + Q2
MergedGender <- merge(Q1Gender2016, Q2Gender2016, 
                      by=c("Gender", "Lucan", "Ballyfermot", "Tallaght", "North.Dublin", "Limerick", "Roscrea", "West", "Cork", "Tralee"),
                      all=TRUE)

#Merge data by rows and gets the sum of the values for each row
library(dplyr)
MergedGender <- MergedGender %>% 
  group_by(Gender) %>% 
  summarise_all(funs(sum))

#Merge Q3 & Q4
MergedGender2 <- merge(Q3Gender2016, Q4Gender2016, 
                       by=c("Gender", "Lucan", "Ballyfermot", "Tallaght", "North.Dublin", "Limerick", "Roscrea", "West", "Cork", "Tralee", "South.East"),
                       all=TRUE)

#change NA's to 0 - will merge the rows after this
MergedGender2[is.na(MergedGender2)] <- 0

library(dplyr)
MergedGender2 <- MergedGender2 %>% 
  group_by(Gender) %>% 
  summarise_all(funs(sum))


#Merge Data into One DF
MergedGenderAll <- merge(MergedGender, MergedGender2, 
                         by=c("Gender", "Lucan", "Ballyfermot", "Tallaght", "North.Dublin", "Limerick", "Roscrea", "West", "Cork", "Tralee"),
                         all=TRUE)

MergedGenderAll[is.na(MergedGenderAll)] <- 0

library(dplyr)
MergedGenderAll <- MergedGenderAll %>% 
  group_by(Gender) %>% 
  summarise_all(funs(sum))

#change collins.ave to numeric 
MergedGenderAll$South.East <- as.integer(MergedGenderAll$South.East)
#merging dataframes - Q1 + Q2
MergedGender <- merge(Q1Gender2016, Q2Gender2016, 
                      by=c("Gender", "Lucan", "Ballyfermot", "Tallaght", "North.Dublin", "Limerick", "Roscrea", "West", "Cork", "Tralee"),
                      all=TRUE)

#Merge data by rows and gets the sum of the values 
library(dplyr)
MergedGender <- MergedGender %>% 
  group_by(Gender) %>% 
  summarise_all(funs(sum))

#Merge Q3 & Q4
MergedGender2 <- merge(Q3Gender2016, Q4Gender2016, 
                       by=c("Gender", "Lucan", "Ballyfermot", "Tallaght", "North.Dublin", "Limerick", "Roscrea", "West", "Cork", "Tralee", "South.East"),
                       all=TRUE)

#change NA's to 0 - will merge the rows after this
MergedGender2[is.na(MergedGender2)] <- 0

library(dplyr)
MergedGender2 <- MergedGender2 %>% 
  group_by(Gender) %>% 
  summarise_all(funs(sum))

#change collins.ave to numeric 
MergedGender2$Collins.Ave <- as.integer(MergedGender2$Collins.Ave)

#Merge Data into One DF
MergedGenderAll <- merge(MergedGender, MergedGender2, 
                         by=c("Gender", "Lucan", "Ballyfermot", "Tallaght", "North.Dublin", "Limerick", "Roscrea", "West", "Cork", "Tralee"),
                         all=TRUE)

MergedGenderAll[is.na(MergedGenderAll)] <- 0

library(dplyr)
MergedGenderAll <- MergedGenderAll %>% 
  group_by(Gender) %>% 
  summarise_all(funs(sum))

#change collins.ave to numeric 
MergedGenderAll$Collins.Ave <- as.integer(MergedGenderAll$Collins.Ave)
MergedGenderAll$South.East <- as.integer(MergedGenderAll$South.East)

Gender2016Final <- MergedGenderAll

#new column - date
Gender2016Final$Date <- c(2016, 2016, 2016)
write.csv(Gender2016Final, 'Gender2016FinalCleaned.csv', row.names = FALSE)


#####################################
#Cause 
#set directory 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")

#load csv 
Q1Cause2016 <- read.csv("Q1_Cause2016.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")
Q2Cause2016 <- read.csv("Q2_Cause2016.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")
Q3Cause2016 <- read.csv("Q3_Cause2016.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")
Q4Cause2016 <- read.csv("Q4_Cause2016.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")


#Q1 Cause 2016 clean up
#Print
print(Q1Cause2016)

#Identify NA's
sapply(Q1Cause2016,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q1Cause2016)

#Check total rows / remove row(s) not needed 
nrow(Q1Cause2016)
Q3Cause2016 <-Q3Cause2016[-9,]

#changing column names
names(Q1Cause2016)[names(Q1Cause2016) == "X"] <- "Cause"
names(Q1Cause2016)[names(Q1Cause2016) == "Dublin.North"] <- "North.Dublin"

#Q2 Cause 2016 clean up
#Print
print(Q2Cause2016)

#Identify NA's
sapply(Q2Cause2016,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q2Cause2016)

#Check total rows / remove row(s) not needed 
nrow(Q2Cause2016)
Q2Cause2016 <-Q2Cause2016[-9,]

#changing column names
names(Q2Cause2016)[names(Q2Cause2016) == "ï.."] <- "Cause"
names(Q2Cause2016)[names(Q2Cause2016) == "Dublin.North"] <- "North.Dublin"

#Q3 Cause 2016 clean up
#Print
print(Q3Cause2016)

#Identify NA's
sapply(Q3Cause2016,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q3Cause2016)

#Check total rows / remove row(s) not needed 
nrow(Q3Cause2016)
Q3Cause2016 <-Q3Cause2016[-9,]

#changing column names
names(Q3Cause2016)[names(Q3Cause2016) == "ï.."] <- "Cause"
names(Q3Cause2016)[names(Q3Cause2016) == "Dublin.North"] <- "North.Dublin"

#Q4 Cause 2016 clean up
#Print
print(Q4Cause2016)

#Identify NA's
sapply(Q4Cause2016,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q4Cause2016)
Q4Cause2016 <-Q4Cause2016[, -13]

#Check total rows / remove row(s) not needed 
nrow(Q4Cause2016)
Q4Cause2016 <-Q4Cause2016[-9,]

#changing column names
names(Q4Cause2016)[names(Q4Cause2016) == "ï.."] <- "Cause"
names(Q4Cause2016)[names(Q4Cause2016) == "Dublin.North"] <- "North.Dublin"

#save to new csv
write.csv(Q1Cause2016, 'Cause2016Q1Cleaned.csv')
write.csv(Q2Cause2016, 'Cause2016Q2Cleaned.csv')
write.csv(Q3Cause2016, 'Cause2016Q3Cleaned.csv')
write.csv(Q4Cause2016, 'Cause2016Q4Cleaned.csv')


#merging dataframes - Q1 + Q2
MergedCause <- merge(Q1Cause2016, Q2Cause2016, 
                     by=c("Cause", "Lucan", "Ballyfermot", "Tallaght", "North.Dublin", "Limerick", "Roscrea", "West", "Cork", "Tralee"),
                     all=TRUE)

#Merge data by rows and gets the sum of the values 
library(dplyr)
MergedCause <- MergedCause %>% 
  group_by(Cause) %>% 
  summarise_all(funs(sum))

#Merge Q3 & Q4
MergedCause2 <- merge(Q3Cause2016, Q4Cause2016, 
                      by=c("Cause", "Lucan", "Ballyfermot", "Tallaght", "North.Dublin", "Limerick", "Roscrea", "West", "Cork", "Tralee", "South.East"),
                      all=TRUE)

#change NA's to 0 - will merge the rows after this
MergedCause2[is.na(MergedCause2)] <- 0

library(dplyr)
MergedCause2 <- MergedCause2 %>% 
  group_by(Cause) %>% 
  summarise_all(funs(sum))

#change collins.ave to numeric 
MergedCause2$Collins.Ave <- as.integer(MergedCause2$Collins.Ave)

#Merge Data into One DF
MergedCauseAll <- merge(MergedCause, MergedCause2, 
                        by=c("Cause", "Lucan", "Ballyfermot", "Tallaght", "North.Dublin", "Limerick", "Roscrea", "West", "Cork", "Tralee"),
                        all=TRUE)

MergedCauseAll[is.na(MergedCauseAll)] <- 0

library(dplyr)
MergedCauseAll <- MergedCauseAll %>% 
  group_by(Cause) %>% 
  summarise_all(funs(sum))

#change collins.ave to numeric 
MergedCauseAll$Collins.Ave <- as.integer(MergedCauseAll$Collins.Ave)
MergedCauseAll$South.East <- as.integer(MergedCauseAll$South.East)

Cause2016Final <- MergedCauseAll
Cause2016Final$Date <- c(2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016)
#Check total rows / remove row(s) not needed 
nrow(Cause2016Final)
Cause2016Final <-Cause2016Final[-9,]

write.csv(Cause2016Final, 'Cause2016FinalCleaned.csv', row.names = FALSE)

