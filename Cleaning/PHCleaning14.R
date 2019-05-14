#set directory 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")

#load csv 
Age2014 <- read.csv("2014Age.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")

#print dataframe
print(Age2014)

#Identify NA's
sapply(Age2014,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Age2014)
Age2014 <- Age2014[, -11]

#Check total rows / remove row(s) not needed 
nrow(Age2014)
Age2014 <-Age2014[-6,]

#changing column names
names(Age2014)[names(Age2014) == "ï.."] <- "Age"
names(Age2014)[names(Age2014) == "B.Fermot"] <- "Ballyfermot"

#new column - date
Age2014$Date <- c(2014, 2014, 2014, 2014, 2014)

#check classes 
class(Age2014$Age)
class(Age2014$Lucan)
class(Age2014$BallyFermot)
class(Age2014$Tallaght)
class(Age2014$Finglas)
class(Age2014$Limerick)
class(Age2014$Roscrea)
class(Age2014$West)
class(Age2014$Cork)
class(Age2014$Kerry)
class(Age2014$Date)

#display data
print(Age2014)

#save to new csv
write.csv(Age2014, 'Age2014Cleaned.csv', row.names = FALSE)

######################
#condition 
#set directory 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")

#load csv 
Cause2014 <- read.csv("2014Cause.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")

#print dataframe
print(Cause2014)

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
sapply(Cause2014,function(x) sum(is.na(x)))

#Delete NA's/Columns or Rows not needed
ncol(Cause2014)
Cause2014 <- Cause2014[, -11]

#Check total rows / remove row(s) not needed 
nrow(Cause2014)
Cause2014 <-Cause2014[-9,]

#changing column names
names(Cause2014)[names(Cause2014) == "X"] <- "Cause"
names(Cause2014)[names(Cause2014) == "B.Fermot"] <- "Ballyfermot"

Cause2014$Date <- c(2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014)

#check class of columns
class(Cause2014$Cause)
class(Cause2014$Lucan)
class(Cause2014$BallyFermot)
class(Cause2014$Tallaght)
class(Cause2014$Finglas)
class(Cause2014$Limerick)
class(Cause2014$Roscrea)
class(Cause2014$West)
class(Cause2014$Cork)
class(Cause2014$Kerry)
class(Cause2014$Date)

#display data
print(Cause2014)

#save to new csv
write.csv(Cause2014, 'Cause2014Cleaned.csv', row.names = FALSE)


###########################
#Gender 
#set directory 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")

#load csv 
Gender2014 <- read.csv("2014Gender.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")

#print dataframe
print(Gender2014)

#Identify NA's
sapply(Gender2014,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Gender2014)
Gender2014 <- Gender2014[, -11]

#Check total rows / remove row(s) not needed 
nrow(Gender2014)
Gender2014 <-Gender2014[-4,]

#changing column names
names(Gender2014)[names(Gender2014) == "B.Fermot"] <- "Ballyfermot"

#new column - date
Gender2014$Date <- c(2014, 2014, 2014)

#check class of columns
class(Gender2014$Gender)
class(Gender2014$Lucan)
class(Gender2014$BallyFermot)
class(Gender2014$Tallaght)
class(Gender2014$Finglas)
class(Gender2014$Limerick)
class(Gender2014$Roscrea)
class(Gender2014$West)
class(Gender2014$Cork)
class(Gender2014$Kerry)

#display data
print(Gender2014)

#save to new csv
write.csv(Gender2014, 'Gender2014Cleaned.csv', row.names = FALSE)
