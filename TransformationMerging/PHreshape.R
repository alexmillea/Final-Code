#Pieta House Reshape
#re-structured data - Age
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")
AgeData <- read.csv("AgeAll.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", check.names = FALSE)

AgeData$Age <- as.factor(AgeData$Age)
str(AgeData)

library(reshape2)
library(reshapeGUI)
#reshape function
#varying - name we want to give the variable containing values in v.name
Age <- melt(data = AgeData, 
            id.vars=c('Age','Date'), 
            measure.vars=c('Ballyfermot','Tallaght','North.Dublin','Limerick','Roscrea',
                           'Cork','West','Collins.Ave','South.East','Lucan','Kerry','Finglas',
                           'Pieta.House','Wexford','North.West','Midlands','Tralee','Galway.City'))

str(Age)
#changing column names
names(Age)[names(Age) == "variable"] <- "Location"
names(Age)[names(Age) == "value"] <- "Suicides"

Age$Location <- as.factor(Age$Location)

write.csv(Age, 'AgeSuicides.csv', row.names = FALSE)

#Gender 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")
GenderData <- read.csv("GenderAll.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", check.names = FALSE)

GenderData$Gender <- as.factor(GenderData$Gender)
str(GenderData)

library(reshape2)
library(reshapeGUI)
Gender <- melt(data = GenderData, 
               id.vars=c('Gender','Date'), 
               measure.vars=c('Ballyfermot','Tallaght','North.Dublin','Limerick','Roscrea','West','Cork',
                              'Tralee','South.East','Collins.Ave','Lucan','Kerry','Finglas','Pieta.House',
                              'Wexford','Galway.City','North.West','Midlands'))

str(Gender)
#changing column names
names(Gender)[names(Gender) == "variable"] <- "Location"
names(Gender)[names(Gender) == "value"] <- "Suicides"

write.csv(Gender, 'GenderSuicides.csv', row.names = FALSE)

#Condition
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")
ConditionData <- read.csv("CauseAll.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", check.names = FALSE)

ConditionData$Cause <- as.factor(ConditionData$Cause)
str(ConditionData)

library(reshape2)
library(reshapeGUI)
#reshape function
#varying - name we want to give the variable containing values in v.name
Condition <- melt(data = ConditionData, 
                  id.vars=c('Cause','Date'), 
                  measure.vars=c('Ballyfermot','Tallaght','North.Dublin','Limerick','Roscrea',
                                 'West','Cork','Tralee','South.East','Collins.Ave','Lucan','Kerry',
                                 'Finglas','Pieta.House','Wexford','Galway.City','North.West','Midlands'))

str(Condition)
#changing column names
names(Condition)[names(Condition) == "variable"] <- "Location"
names(Condition)[names(Condition) == "value"] <- "Suicides"

write.csv(Condition, 'ConditionSuicides.csv', row.names = FALSE)
