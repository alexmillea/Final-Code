#set directory 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")

#load csv 
Gender2015 <- read.csv("2015Gender.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")

#print dataframe
print(Gender2015)

#Identify NA's
sapply(Gender2015,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Gender2015)

#Check total rows / remove row(s) not needed 
nrow(Gender2015)
Gender2015 <-Gender2015[-4,]

#changing column names
names(Gender2015)[names(Gender2015) == "X"] <- "Gender"

#new column - date
Gender2015$Date <- c(2015, 2015, 2015)

#check class of columns
class(Gender2015$Gender)
class(Gender2015$Ballyfermot)
class(Gender2015$North.Dublin)
class(Gender2015$Lucan)
class(Gender2015$Limerick)
class(Gender2015$Tallaght)
class(Gender2015$Roscrea)
class(Gender2015$West)
class(Gender2015$Tralee)
class(Gender2015$Kerry)

#display data
print(Gender2015)

#save to new csv
write.csv(Gender2015, 'Gender2015Cleaned.csv', row.names = FALSE)

#######################
#Age
#set directory 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")

#load csv 
Age2015 <- read.csv("2015Age.csv", stringsAsFactors = FALSE, sep = ",")

#print dataframe
print(Age2015)

#Identify NA's
sapply(Age2015,function(x) sum(is.na(x)))

#Delete Columns or Rows not needed
ncol(Age2015)

#Check total rows / remove row(s) not needed 
nrow(Age2015)
Age2015 <-Age2015[-6,]

#changing column names
names(Age2015)[names(Age2015) == "X"] <- "Age"

#new column - date
Age2015$Date <- c(2015, 2015, 2015, 2015, 2015)

#check class of columns
class(Age2015$Age)
class(Age2015$Ballyfermot)
class(Age2015$North.Dublin)
class(Age2015$Lucan)
class(Age2015$Limerick)
class(Age2015$Tallaght)
class(Age2015$Roscrea)
class(Age2015$West)
class(Age2015$Tralee)
class(Age2015$Kerry)
class(Age2015$Date)

#display data
print(Age2015)

#save to new csv
write.csv(Age2015, 'Age2015Cleaned.csv', row.names = FALSE)

############################
#Cause 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")

#load csv 
Cause2015 <- read.csv("2015Cause.csv", stringsAsFactors = FALSE, sep = ",")

#print dataframe
print(Cause2015)

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
sapply(Cause2015,function(x) sum(is.na(x)))

#Delete NA's/Columns or Rows not needed
ncol(Cause2015)

#Check total rows / remove row(s) not needed 
nrow(Cause2015)
Cause2015 <-Cause2015[-9,]

#changing column names
names(Cause2015)[names(Cause2015) == "X"] <- "Cause"

Cause2015$Date <- c(2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015)

#check class of columns
class(Cause2015$Cause)
class(Cause2015$Ballyfermot)
class(Cause2015$North.Dublin)
class(Cause2015$Lucan)
class(Cause2015$Limerick)
class(Cause2015$Tallaght)
class(Cause2015$Roscrea)
class(Cause2015$West)
class(Cause2015$Tralee)
class(Cause2015$Kerry)
class(Cause2015$Date)

#display data
print(Cause2015)

#save to new csv
write.csv(Cause2015, 'Cause2015Cleaned.csv', row.names = FALSE)


