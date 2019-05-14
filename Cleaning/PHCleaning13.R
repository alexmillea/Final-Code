#### 2013 Cleaning Script
#gender 
#set directory 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")

#load csv 
Gender2013 <- read.csv("2013_Gender.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")

#Display Gender2013
print(Gender2013)

#Show NA's
sapply(Gender2013,function(x) sum(is.na(x)))

#remove NA's by column reference(not needed)
ncol(Gender2013)
Gender2013 <- Gender2013[, -8]

#check how many rows we have
nrow(Gender2013)
Gender2013 <- Gender2013[-3,]

#new column - date
Gender2013$Date <- c(2013, 2013)

#check class of columns
class(Gender2013$City)
class(Gender2013$Lucan)
class(Gender2013$B.Fermot)
class(Gender2013$Tallaght)
class(Gender2013$Finglas)
class(Gender2013$Limerick)
class(Gender2013$Roscrea)

#changing column names
names(Gender2013)[names(Gender2013) == "City"] <- "Gender"
names(Gender2013)[names(Gender2013) == "B.Fermot"] <- "Ballyfermot"


print(Gender2013)

#save file to a new csv
write.csv(Gender2013, 'Gender2013Cleaned.csv', row.names = FALSE)

########################
#condition
#set directory 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")

#load csv 
Cause2013 <- read.csv("2013_Cause.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")

#print dataframe
print(Cause2013)

#########################################################
# Table Legend for cause of suicide 
# 
# DSH =	Deliberate Self-Harm					
# SA/DSH =	Suicide Attempt and Deliberate Self-Harm					
# SA Only =	Suicide Attempt Only					
# SI/SA/DSH =	Suicidal Ideation, Suicidal Attmpt, and Deliberate Self-Harm					
# SI/DSH =	Suicidal Ideation and Deliberate Self-Harm					
# SI/SA =	Suicidal Ideation and and Suicide Attempt					
# SI Only =	Suicidal Ideation Only					
# DNAA =	Did Not Attend Assessment					
#
#########################################################


#Identify NA's
sapply(Cause2013,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Cause2013)
Cause2013 <- Cause2013[, -8]

#Check total rows / remove row(s) not needed 
nrow(Cause2013)
Cause2013 <-Cause2013[-9,]

#changing column names
names(Cause2013)[names(Cause2013) == "ï.."] <- "Cause"
names(Cause2013)[names(Cause2013) == "B.Fermot"] <- "Ballyfermot"

#new column - date
Cause2013$Date <- c(2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013)


#check class of columns
class(Cause2013$Cause)
class(Cause2013$Ballyfermot)
class(Cause2013$Lucan)
class(Cause2013$Limerick)
class(Cause2013$Tallaght)
class(Cause2013$Roscrea)
class(Cause2013$Finglas)

#display data
print(Cause2013)

#save to new csv
write.csv(Cause2013, 'Cause2013Cleaned.csv', row.names = FALSE)

############################
#Age
#set directory 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")

#load csv 
Age2013 <- read.csv("2013_Age.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")

#print dataframe
print(Age2013)

#Identify NA's
sapply(Age2013,function(x) sum(is.na(x)))

#Delete NA's/Columns or Rows not needed
ncol(Age2013)
Age2013 <- Age2013[, -7, -8]
Age2013 <- Age2013[, -7]

#Check total rows / remove row(s) not needed 
nrow(Age2013)
Age2013 <-Age2013[-6,]

#changing column names
names(Age2013)[names(Age2013) == "ï.."] <- "Age"
names(Age2013)[names(Age2013) == "B.Fermot"] <- "Ballyfermot"

#new column - date
Age2013$Date <- c(2013, 2013, 2013, 2013, 2013)

#check class of columns
class(Age2013$Age)
class(Age2013$Lucan)
class(Age2013$BallyFermot)
class(Age2013$Tallaght)
class(Age2013$Finglas)
class(Age2013$Limerick)
class(Age2013$Date)

#display data
print(Age2013)

#save to new csv
write.csv(Age2013, 'Age2013Cleaned.csv', row.names = FALSE)

