#Pieta House Merge All
#Age
#set directory 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")
Age2013 <- read.csv("Age2013Cleaned.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", check.names = FALSE)
Age2014 <- read.csv("Age2014Cleaned.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", check.names = FALSE)
Age2015 <- read.csv("Age2015Cleaned.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", check.names = FALSE)
Age2016 <- read.csv("Age2016FinalCleaned.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", check.names = FALSE)
Age2017 <- read.csv("Age2017FinalCleaned.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", check.names = FALSE)

print(Age2013)
print(Age2014)
print(Age2015)
print(Age2016)
print(Age2017)

#merge 
#merging dataframes
AgeAll <- merge(Age2013, Age2014, 
                by=c("Age", "Lucan", "Ballyfermot", "Tallaght", "Finglas", "Limerick", "Date"),
                all=TRUE)

#change NA's to 0 - will merge the rows after this
AgeAll[is.na(AgeAll)] <- 0

#merge2
AgeAll <- merge(AgeAll, Age2015, 
                by=c("Age", "Lucan", "Ballyfermot", "Tallaght", "Limerick", "Roscrea", "West", "Cork", "Kerry", "Date"),
                all=TRUE)

AgeAll[is.na(AgeAll)] <- 0

AgeAll <- merge(AgeAll, Age2016, 
                by=c("Age", "Lucan", "Ballyfermot", "Tallaght", "North.Dublin", "Limerick", "Roscrea", "West", "Cork", "Tralee", "Date"),
                all=TRUE)

AgeAll[is.na(AgeAll)] <- 0

AgeAll <- merge(AgeAll, Age2017, 
                by=c("Age", "Ballyfermot", "Tallaght", "North.Dublin", "Limerick", "Roscrea", "West", "Cork", "Tralee", "South.East", "Collins.Ave", "Date"),
                all=TRUE)

AgeAll[is.na(AgeAll)] <- 0


AgeAll <- AgeAll[order(as.integer(AgeAll$Date),decreasing = FALSE), ]


#reorder columns 
library(dplyr)
AgeAll <- AgeAll %>% select(Age:Collins.Ave, Lucan:Midlands, Date)
write.csv(AgeAll, 'AgeAll.csv', row.names = FALSE)

########################################
#Pieta House Gender
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")
Gender2013 <- read.csv("Gender2013Cleaned.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", check.names = FALSE)
Gender2014 <- read.csv("Gender2014Cleaned.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", check.names = FALSE)
Gender2015 <- read.csv("Gender2015Cleaned.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", check.names = FALSE)
Gender2016 <- read.csv("Gender2016FinalCleaned.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", check.names = FALSE)
Gender2017 <- read.csv("Gender2017FinalCleaned.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", check.names = FALSE)

print(Gender2013)
print(Gender2014)
print(Gender2015)
print(Gender2016)
print(Gender2017)

#merge 
#merging dataframes
GenderAll <- merge(Gender2013, Gender2014, 
                   by=c("Gender", "Lucan", "Ballyfermot", "Tallaght", "Finglas", "Limerick", "Roscrea", "Date"),
                   all=TRUE)

#change NA's to 0 - will merge the rows after this
GenderAll[is.na(GenderAll)] <- 0

#merge2
GenderAll <- merge(GenderAll, Gender2015, 
                   by=c("Gender", "Lucan", "Ballyfermot", "Tallaght", "Limerick", "Roscrea", "West", "Cork", "Kerry", "Date"),
                   all=TRUE)

GenderAll[is.na(GenderAll)] <- 0

GenderAll <- merge(GenderAll, Gender2016, 
                   by=c("Gender", "Lucan", "Ballyfermot", "Tallaght", "North.Dublin", "Limerick", "Roscrea", "West", "Cork", "Tralee", "Date"),
                   all=TRUE)

GenderAll[is.na(GenderAll)] <- 0

GenderAll <- merge(GenderAll, Gender2017, 
                   by=c("Gender", "Ballyfermot", "Tallaght", "North.Dublin", "Limerick", "Roscrea", "West", "Cork", "Tralee", "South.East", "Collins.Ave", "Date"),
                   all=TRUE)

GenderAll[is.na(GenderAll)] <- 0


GenderAll <- GenderAll[order(as.integer(GenderAll$Date),decreasing = FALSE), ]

#reorder columns 
library(dplyr)
GenderAll <- GenderAll %>% select(Gender:Collins.Ave, Lucan:Midlands, Date)
write.csv(GenderAll, 'GenderAll.csv', row.names = FALSE)


#######################################
#Pieta House Condition
#merge all Cause data 

#set directory 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")
Cause2013 <- read.csv("Cause2013Cleaned.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", check.names = FALSE)
Cause2014 <- read.csv("Cause2014Cleaned.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", check.names = FALSE)
Cause2015 <- read.csv("Cause2015Cleaned.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", check.names = FALSE)
Cause2016 <- read.csv("Cause2016FinalCleaned.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", check.names = FALSE)
Cause2017 <- read.csv("Cause2017FinalCleaned.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", check.names = FALSE)

print(Cause2013)
print(Cause2014)
print(Cause2015)
print(Cause2016)
print(Cause2017)

#merge 
#merging dataframes
CauseAll <- merge(Cause2013, Cause2014, 
                  by=c("Cause", "Lucan", "Ballyfermot", "Tallaght", "Finglas", "Limerick", "Roscrea", "Date"),
                  all=TRUE)

#change NA's to 0 - will merge the rows after this
CauseAll[is.na(CauseAll)] <- 0

#merge2
CauseAll <- merge(CauseAll, Cause2015, 
                  by=c("Cause", "Lucan", "Ballyfermot", "Tallaght", "Limerick", "Roscrea", "West", "Cork", "Kerry", "Date"),
                  all=TRUE)

CauseAll[is.na(CauseAll)] <- 0

CauseAll <- merge(CauseAll, Cause2016, 
                  by=c("Cause", "Lucan", "Ballyfermot", "Tallaght", "North.Dublin", "Limerick", "Roscrea", "West", "Cork", "Tralee", "Date"),
                  all=TRUE)

CauseAll[is.na(CauseAll)] <- 0

CauseAll <- merge(CauseAll, Cause2017, 
                  by=c("Cause", "Ballyfermot", "Tallaght", "North.Dublin", "Limerick", "Roscrea", "West", "Cork", "Tralee", "South.East", "Collins.Ave", "Date"),
                  all=TRUE)

CauseAll[is.na(CauseAll)] <- 0


CauseAll <- CauseAll[order(as.integer(CauseAll$Date),decreasing = FALSE), ]

#reorder columns 
library(dplyr)
CauseAll <- CauseAll %>% select(Cause:Collins.Ave, Lucan:Midlands, Date)
write.csv(CauseAll, 'CauseAll.csv', row.names = FALSE)

