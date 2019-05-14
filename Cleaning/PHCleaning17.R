#AgeMerge
#set directory 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")

#load csv 
Q1Age2017 <- read.csv("Q1Age2017.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")
Q2Age2017 <- read.csv("Q2Age2017.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")
Q3Age2017 <- read.csv("Q3Age2017.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")
Q4Age2017 <- read.csv("Q4Age2017.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")


#Q1 Age 2017 clean up
#Print
print(Q1Age2017)

#Identify NA's
sapply(Q1Age2017,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q1Age2017)
Q1Age2017 <-Q1Age2017[, -15]

#Check total rows / remove row(s) not needed 
nrow(Q1Age2017)
Q1Age2017 <-Q1Age2017[-6,]

#changing column names
names(Q1Age2017)[names(Q1Age2017) == "ï.."] <- "Age"
names(Q1Age2017)[names(Q1Age2017) == "Collins.Avenue"] <- "Collins.Ave"

#check class of each column 
class(Q1Age2017$Age)
class(Q1Age2017$Ballyfermot)
class(Q1Age2017$North.Dublin)
class(Q1Age2017$Pieta.House)
class(Q1Age2017$Limerick)
class(Q1Age2017$Tallaght)
class(Q1Age2017$Roscrea)
class(Q1Age2017$West)
class(Q1Age2017$Tralee)
class(Q1Age2017$Cork)
class(Q1Age2017$South.East)
class(Q1Age2017$Galway.City)
class(Q1Age2017$Wexford)
class(Q1Age2017$Collins.Ave)


#Q2 Age 2017 clean up
#Print
print(Q2Age2017)

#Identify NA's
sapply(Q2Age2017,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q2Age2017)
Q2Age2017 <-Q2Age2017[,-15]

#Check total rows / remove row(s) not needed 
nrow(Q2Age2017)
Q2Age2017 <-Q2Age2017[-6,]

#changing column names
names(Q2Age2017)[names(Q2Age2017) == "ï.."] <- "Age"
names(Q2Age2017)[names(Q2Age2017) == "Collins.Avenue"] <- "Collins.Ave"

#check class of each column 
class(Q2Age2017$Age)
class(Q2Age2017$Ballyfermot)
class(Q2Age2017$North.Dublin)
class(Q2Age2017$Pieta.House)
class(Q2Age2017$Limerick)
class(Q2Age2017$Tallaght)
class(Q2Age2017$Roscrea)
class(Q2Age2017$West)
class(Q2Age2017$Tralee)
class(Q2Age2017$Cork)
class(Q2Age2017$South.East)
class(Q2Age2017$Galway.City)
class(Q2Age2017$Wexford)
class(Q2Age2017$Collins.Ave)


#Q3 Age 2017 clean up
#Print
print(Q3Age2017)

#Identify NA's
sapply(Q3Age2017,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q3Age2017)
Q3Age2017 <-Q3Age2017[,-16]

#Check total rows / remove row(s) not needed 
nrow(Q3Age2017)
Q3Age2017 <-Q3Age2017[-6,]

#changing column names
names(Q3Age2017)[names(Q3Age2017) == "ï.."] <- "Age"
names(Q3Age2017)[names(Q3Age2017) == "Collins.Avenue"] <- "Collins.Ave"

#check class of each column 
class(Q3Age2017$Age)
class(Q3Age2017$Ballyfermot)
class(Q3Age2017$North.Dublin)
class(Q3Age2017$Pieta.House)
class(Q3Age2017$Limerick)
class(Q3Age2017$Tallaght)
class(Q3Age2017$Roscrea)
class(Q3Age2017$West)
class(Q3Age2017$Tralee)
class(Q3Age2017$Cork)
class(Q3Age2017$South.East)
class(Q3Age2017$Galway.City)
class(Q3Age2017$Wexford)
class(Q3Age2017$Collins.Ave)
class(Q3Age2017$North.West)
class(Q3Age2017$Midlands)


#Q4 Age 2017 clean up
#Print
print(Q4Age2017)

#Identify NA's
sapply(Q4Age2017,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q4Age2017)
Q4Age2017 <-Q4Age2017[, -16]

#Check total rows / remove row(s) not needed 
nrow(Q4Age2017)
Q4Age2017 <-Q4Age2017[-6,]

#changing column names
names(Q4Age2017)[names(Q4Age2017) == "ï.."] <- "Age"
names(Q4Age2017)[names(Q4Age2017) == "Collins.Avenue"] <- "Collins.Ave"

#check class of each column 
class(Q4Age2017$Age)
class(Q4Age2017$Ballyfermot)
class(Q4Age2017$North.Dublin)
class(Q4Age2017$Pieta.House)
class(Q4Age2017$Limerick)
class(Q4Age2017$Tallaght)
class(Q4Age2017$Roscrea)
class(Q4Age2017$West)
class(Q4Age2017$Tralee)
class(Q4Age2017$Cork)
class(Q4Age2017$South.East)
class(Q4Age2017$Galway.City)
class(Q4Age2017$Wexford)
class(Q4Age2017$Collins.Ave)
class(Q4Age2017$North.West)
class(Q4Age2017$Midlands)


#save to new csv
write.csv(Q1Age2017, 'Age2017Q1Cleaned.csv')
write.csv(Q2Age2017, 'Age2017Q2Cleaned.csv')
write.csv(Q3Age2017, 'Age2017Q3Cleaned.csv')
write.csv(Q4Age2017, 'Age2017Q4Cleaned.csv')

#merging dataframes - Q1 + Q2
MergedAge <- merge(Q1Age2017, Q2Age2017, 
                   by=c("Age", "Ballyfermot", "North.Dublin", "Pieta.House", "Limerick", "Tallaght", "Roscrea", "Cork", "West", "Tralee", "South.East", "Galway.City", "Collins.Ave", "Wexford"),
                   all=TRUE)

#Merge data by rows and gets the sum of the values 
library(dplyr)
MergedAge <- MergedAge %>% 
  group_by(Age) %>% 
  summarise_all(funs(sum))

#Merge Q3 & Q4
MergedAge2 <- merge(Q3Age2017, Q4Age2017, 
                    by=c("Age", "Ballyfermot", "North.Dublin", "Pieta.House", "Limerick", "Tallaght", "Roscrea", "Cork", "West", "Tralee", "South.East", "Collins.Ave", "Wexford", "North.West", "Midlands"),
                    all=TRUE)

#change NA's to 0 - will merge the rows after this
MergedAge2[is.na(MergedAge2)] <- 0

library(dplyr)
MergedAge2 <- MergedAge2 %>% 
  group_by(Age) %>% 
  summarise_all(funs(sum))


#Merge Data into One DF
MergedAgeAll <- merge(MergedAge, MergedAge2, 
                      by=c("Age", "Ballyfermot", "North.Dublin", "Pieta.House", "Limerick", "Tallaght", "Roscrea", "Cork", "West", "Tralee", "South.East", "Collins.Ave", "Wexford"),
                      all=TRUE)

MergedAgeAll[is.na(MergedAgeAll)] <- 0

library(dplyr)
MergedAgeAll <- MergedAgeAll %>% 
  group_by(Age) %>% 
  summarise_all(funs(sum))

Age2017Final <- MergedAgeAll

Age2017Final$Galway.City <- as.integer(Age2017Final$Galway.City)
Age2017Final$North.West <- as.integer(Age2017Final$North.West)
Age2017Final$Midlands <- as.integer(Age2017Final$Midlands)

Age2017Final$Date <- c(2017, 2017, 2017, 2017, 2017)

write.csv(Age2017Final, 'Age2017FinalCleaned.csv', row.names = FALSE)


# Plot the bar chart.
#plot(Age2015$North.Dublin, type = "o",col = "red", xlab = "Age Brackets", ylab = "Suicide Rate", 
# main = "North.Dublin Suicide Rate 2015 vs 2016")

#lines(Age2016Final$North.Dublin, type = "o", col = "blue")
#lines(Age2017Final$North.Dublin, type = "o", col = "green")

###########################
#Gender Merge
#set directory 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")

#load csv 
Q1Gender2017 <- read.csv("Q1Gender2017.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")
Q2Gender2017 <- read.csv("Q2Gender2017.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")
Q3Gender2017 <- read.csv("Q3Gender2017.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")
Q4Gender2017 <- read.csv("Q4Gender2017.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")


#Q1 Gender 2017 clean up
#Print
print(Q1Gender2017)

#Identify NA's
sapply(Q1Gender2017,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q1Gender2017)
Q1Gender2017 <-Q1Gender2017[, -15]

#Check total rows / remove row(s) not needed 
nrow(Q1Gender2017)
Q1Gender2017 <-Q1Gender2017[-4,]

#changing column names
names(Q1Gender2017)[names(Q1Gender2017) == "ï.."] <- "Gender"
names(Q1Gender2017)[names(Q1Gender2017) == "Collins.Avenue"] <- "Collins.Ave"

#check class of each column 
class(Q1Gender2017$Gender)
class(Q1Gender2017$Ballyfermot)
class(Q1Gender2017$North.Dublin)
class(Q1Gender2017$Pieta.House)
class(Q1Gender2017$Limerick)
class(Q1Gender2017$Tallaght)
class(Q1Gender2017$Roscrea)
class(Q1Gender2017$West)
class(Q1Gender2017$Tralee)
class(Q1Gender2017$Cork)
class(Q1Gender2017$South.East)
class(Q1Gender2017$Galway.City)
class(Q1Gender2017$Wexford)
class(Q1Gender2017$Collins.Ave)

#Q2 Gender 2017 clean up
#Print
print(Q2Gender2017)

#Identify NA's
sapply(Q2Gender2017,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q2Gender2017)
Q2Gender2017 <-Q2Gender2017[,-15]

#Check total rows / remove row(s) not needed 
nrow(Q2Gender2017)
Q2Gender2017 <-Q2Gender2017[-4,]

#changing column names
names(Q2Gender2017)[names(Q2Gender2017) == "ï.."] <- "Gender"
names(Q2Gender2017)[names(Q2Gender2017) == "Collins.Avenue"] <- "Collins.Ave"


#check class of each column 
class(Q2Gender2017$Gender)
class(Q2Gender2017$Ballyfermot)
class(Q2Gender2017$North.Dublin)
class(Q2Gender2017$Pieta.House)
class(Q2Gender2017$Limerick)
class(Q2Gender2017$Tallaght)
class(Q2Gender2017$Roscrea)
class(Q2Gender2017$West)
class(Q2Gender2017$Tralee)
class(Q2Gender2017$Cork)
class(Q2Gender2017$South.East)
class(Q2Gender2017$Galway.City)
class(Q2Gender2017$Wexford)
class(Q2Gender2017$Collins.Ave)

#Q3 Gender 2017 clean up
#Print
print(Q3Gender2017)

#Identify NA's
sapply(Q3Gender2017,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q3Gender2017)
Q3Gender2017 <-Q3Gender2017[,-16]

#Check total rows / remove row(s) not needed 
nrow(Q3Gender2017)
Q3Gender2017 <-Q3Gender2017[-4,]

#changing column names
names(Q3Gender2017)[names(Q3Gender2017) == "ï.."] <- "Gender"
names(Q3Gender2017)[names(Q3Gender2017) == "Collins.Avenue"] <- "Collins.Ave"

#check class of each column 
class(Q3Gender2017$Gender)
class(Q3Gender2017$Ballyfermot)
class(Q3Gender2017$North.Dublin)
class(Q3Gender2017$Pieta.House)
class(Q3Gender2017$Limerick)
class(Q3Gender2017$Tallaght)
class(Q3Gender2017$Roscrea)
class(Q3Gender2017$West)
class(Q3Gender2017$Tralee)
class(Q3Gender2017$Cork)
class(Q3Gender2017$South.East)
class(Q3Gender2017$Wexford)
class(Q3Gender2017$Collins.Ave)
class(Q3Gender2017$Midlands)

#Q4 Gender 2017 clean up
#Print
print(Q4Gender2017)

#Identify NA's
sapply(Q4Gender2017,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q4Gender2017)
Q4Gender2017 <-Q4Gender2017[, -16]

#Check total rows / remove row(s) not needed 
nrow(Q4Gender2017)
Q4Gender2017 <-Q4Gender2017[-4,]

#changing column names
names(Q4Gender2017)[names(Q4Gender2017) == "ï.."] <- "Gender"
names(Q4Gender2017)[names(Q4Gender2017) == "Collins.Avenue"] <- "Collins.Ave"


#check class of each column 
class(Q1Gender2017$Gender)
class(Q1Gender2017$Ballyfermot)
class(Q1Gender2017$North.Dublin)
class(Q1Gender2017$Pieta.House)
class(Q1Gender2017$Limerick)
class(Q1Gender2017$Tallaght)
class(Q1Gender2017$Roscrea)
class(Q1Gender2017$West)
class(Q1Gender2017$Tralee)
class(Q1Gender2017$Cork)
class(Q1Gender2017$South.East)
class(Q1Gender2017$Wexford)
class(Q1Gender2017$Collins.Ave)

#save to new csv
write.csv(Q1Gender2017, 'Gender2017Q1Cleaned.csv')
write.csv(Q2Gender2017, 'Gender2017Q2Cleaned.csv')
write.csv(Q3Gender2017, 'Gender2017Q3Cleaned.csv')
write.csv(Q4Gender2017, 'Gender2017Q4Cleaned.csv')

#merging dataframes - Q1 + Q2
MergedGender <- merge(Q1Gender2017, Q2Gender2017, 
                      by=c("Gender", "Ballyfermot", "North.Dublin", "Pieta.House", "Limerick", "Tallaght", "Roscrea", "Cork", "West", "Tralee", "South.East", "Galway.City", "Collins.Ave", "Wexford"),
                      all=TRUE)

#Merge data by rows and gets the sum of the values 
library(dplyr)
MergedGender <- MergedGender %>% 
  group_by(Gender) %>% 
  summarise_all(funs(sum))

#Merge Q3 & Q4
MergedGender2 <- merge(Q3Gender2017, Q4Gender2017, 
                       by=c("Gender", "Ballyfermot", "North.Dublin", "Pieta.House", "Limerick", "Tallaght", "Roscrea", "Cork", "West", "Tralee", "South.East", "Collins.Ave", "Wexford", "North.West", "Midlands"),
                       all=TRUE)

#change NA's to 0 - will merge the rows after this
MergedGender2[is.na(MergedGender2)] <- 0

library(dplyr)
MergedGender2 <- MergedGender2 %>% 
  group_by(Gender) %>% 
  summarise_all(funs(sum))


#Merge Data into One DF
MergedGenderAll <- merge(MergedGender, MergedGender2, 
                         by=c("Gender", "Ballyfermot", "North.Dublin", "Pieta.House", "Limerick", "Tallaght", "Roscrea", "Cork", "West", "Tralee", "South.East", "Collins.Ave", "Wexford"),
                         all=TRUE)

MergedGenderAll[is.na(MergedGenderAll)] <- 0

library(dplyr)
MergedGenderAll <- MergedGenderAll %>% 
  group_by(Gender) %>% 
  summarise_all(funs(sum))

Gender2017Final <- MergedGenderAll

Gender2017Final$Collins.Ave <- as.integer(Gender2017Final$Galway.City)
Gender2017Final$South.East <- as.integer(Gender2017Final$North.West)
Gender2017Final$South.East <- as.integer(Gender2017Final$Midlands)

#new column - date
Gender2017Final$Date <- c(2017, 2017, 2017)

write.csv(Gender2017Final, 'Gender2017FinalCleaned.csv', row.names = FALSE)

###########################
#Condition Merge (Cause = condition)
#set directory 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")

#load csv 
Q1Cause2017 <- read.csv("Q1Cause2017.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")
Q2Cause2017 <- read.csv("Q2Cause2017.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")
Q3Cause2017 <- read.csv("Q3Cause2017.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")
Q4Cause2017 <- read.csv("Q4Cause2017.csv", stringsAsFactors = FALSE, header=TRUE, sep = ",")


#Q1 Cause 2017 clean up
#Print
print(Q1Cause2017)

#Identify NA's
sapply(Q1Cause2017,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q1Cause2017)
Q1Cause2017 <-Q1Cause2017[, -15]

#Check total rows / remove row(s) not needed 
nrow(Q1Cause2017)
Q1Cause2017 <-Q1Cause2017[-9,]

#changing column names
names(Q1Cause2017)[names(Q1Cause2017) == "ï.."] <- "Cause"
names(Q1Cause2017)[names(Q1Cause2017) == "Collins.Avenue"] <- "Collins.Ave"

#check class of each column 
class(Q1Cause2017$Cause)
class(Q1Cause2017$Ballyfermot)
class(Q1Cause2017$North.Dublin)
class(Q1Cause2017$Pieta.House)
class(Q1Cause2017$Limerick)
class(Q1Cause2017$Tallaght)
class(Q1Cause2017$Roscrea)
class(Q1Cause2017$West)
class(Q1Cause2017$Tralee)
class(Q1Cause2017$Cork)
class(Q1Cause2017$South.East)
class(Q1Cause2017$Galway.City)
class(Q1Cause2017$Wexford)
class(Q1Cause2017$Collins.Ave)


#Q2 Cause 2017 clean up
#Print
print(Q2Cause2017)

#Identify NA's
sapply(Q2Cause2017,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q2Cause2017)
Q2Cause2017 <-Q2Cause2017[,-15]

#Check total rows / remove row(s) not needed 
nrow(Q2Cause2017)
Q2Cause2017 <-Q2Cause2017[-9,]

#changing column names
names(Q2Cause2017)[names(Q2Cause2017) == "ï.."] <- "Cause"
names(Q2Cause2017)[names(Q2Cause2017) == "Collins.Avenue"] <- "Collins.Ave"

#check class of each column 
class(Q2Cause2017$Cause)
class(Q2Cause2017$Ballyfermot)
class(Q2Cause2017$North.Dublin)
class(Q2Cause2017$Pieta.House)
class(Q2Cause2017$Limerick)
class(Q2Cause2017$Tallaght)
class(Q2Cause2017$Roscrea)
class(Q2Cause2017$West)
class(Q2Cause2017$Tralee)
class(Q2Cause2017$Cork)
class(Q2Cause2017$South.East)
class(Q2Cause2017$Galway.City)
class(Q2Cause2017$Wexford)
class(Q2Cause2017$Collins.Ave)


#Q3 Cause 2017 clean up
#Print
print(Q3Cause2017)

#Identify NA's
sapply(Q3Cause2017,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q3Cause2017)
Q3Cause2017 <-Q3Cause2017[,-16]

#Check total rows / remove row(s) not needed 
nrow(Q3Cause2017)
Q3Cause2017 <-Q3Cause2017[-9,]

#changing column names
names(Q3Cause2017)[names(Q3Cause2017) == "ï.."] <- "Cause"
names(Q3Cause2017)[names(Q3Cause2017) == "Collins.Avenue"] <- "Collins.Ave"

#check class of each column 
class(Q3Cause2017$Cause)
class(Q3Cause2017$Ballyfermot)
class(Q3Cause2017$North.Dublin)
class(Q3Cause2017$Pieta.House)
class(Q3Cause2017$Limerick)
class(Q3Cause2017$Tallaght)
class(Q3Cause2017$Roscrea)
class(Q3Cause2017$West)
class(Q3Cause2017$Tralee)
class(Q3Cause2017$Cork)
class(Q3Cause2017$South.East)
class(Q3Cause2017$Wexford)
class(Q3Cause2017$Collins.Ave)
class(Q3Cause2017$North.West)
class(Q3Cause2017$Midlands)


#Q4 Cause 2017 clean up
#Print
print(Q4Cause2017)

#Identify NA's
sapply(Q4Cause2017,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Q4Cause2017)
Q4Cause2017 <-Q4Cause2017[, -16]

#Check total rows / remove row(s) not needed 
nrow(Q4Cause2017)
Q4Cause2017 <-Q4Cause2017[-9,]

#changing column names
names(Q4Cause2017)[names(Q4Cause2017) == "ï.."] <- "Cause"
names(Q4Cause2017)[names(Q4Cause2017) == "Collins.Avenue"] <- "Collins.Ave"

#check class of each column 
class(Q4Cause2017$Cause)
class(Q4Cause2017$Ballyfermot)
class(Q4Cause2017$North.Dublin)
class(Q4Cause2017$Pieta.House)
class(Q4Cause2017$Limerick)
class(Q4Cause2017$Tallaght)
class(Q4Cause2017$Roscrea)
class(Q4Cause2017$West)
class(Q4Cause2017$Tralee)
class(Q4Cause2017$Cork)
class(Q4Cause2017$South.East)
class(Q4Cause2017$Galway.City)
class(Q4Cause2017$Wexford)
class(Q4Cause2017$Collins.Ave)
class(Q4Cause2017$North.West)
class(Q4Cause2017$Midlands)

#save to new csv
write.csv(Q1Cause2017, 'Cause2017Q1Cleaned.csv')
write.csv(Q2Cause2017, 'Cause2017Q2Cleaned.csv')
write.csv(Q3Cause2017, 'Cause2017Q3Cleaned.csv')
write.csv(Q4Cause2017, 'Cause2017Q4Cleaned.csv')

#merging dataframes - Q1 + Q2
MergedCause <- merge(Q1Cause2017, Q2Cause2017, 
                     by=c("Cause", "Ballyfermot", "North.Dublin", "Pieta.House", "Limerick", "Tallaght", "Roscrea", "Cork", "West", "Tralee", "South.East", "Galway.City", "Collins.Ave", "Wexford"),
                     all=TRUE)

#Merge data by rows and gets the sum of the values 
library(dplyr)
MergedCause <- MergedCause %>% 
  group_by(Cause) %>% 
  summarise_all(funs(sum))

#Merge Q3 & Q4
MergedCause2 <- merge(Q3Cause2017, Q4Cause2017, 
                      by=c("Cause", "Ballyfermot", "North.Dublin", "Pieta.House", "Limerick", "Tallaght", "Roscrea", "Cork", "West", "Tralee", "South.East", "Collins.Ave", "Wexford", "North.West", "Midlands"),
                      all=TRUE)

#change NA's to 0 - will merge the rows after this
MergedCause2[is.na(MergedCause2)] <- 0

library(dplyr)
MergedCause2 <- MergedCause2 %>% 
  group_by(Cause) %>% 
  summarise_all(funs(sum))


#Merge Data into One DF
MergedCauseAll <- merge(MergedCause, MergedCause2, 
                        by=c("Cause", "Ballyfermot", "North.Dublin", "Pieta.House", "Limerick", "Tallaght", "Roscrea", "Cork", "West", "Tralee", "South.East", "Collins.Ave", "Wexford"),
                        all=TRUE)

MergedCauseAll[is.na(MergedCauseAll)] <- 0

library(dplyr)
MergedCauseAll <- MergedCauseAll %>% 
  group_by(Cause) %>% 
  summarise_all(funs(sum))

Cause2017Final <- MergedCauseAll

Cause2017Final$Galway.City <- as.integer(Cause2017Final$Galway.City)
Cause2017Final$North.West <- as.integer(Cause2017Final$North.West)
Cause2017Final$Midlands <- as.integer(Cause2017Final$Midlands)

Cause2017Final$Date <- c(2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017)
write.csv(Cause2017Final, 'Cause2017FinalCleaned.csv', row.names = FALSE)

