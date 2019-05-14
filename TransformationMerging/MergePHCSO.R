#Merge Data - CSO & Pieta House
#pieta house - all
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")
gender <- read.csv("GenderSuicides.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
age <- read.csv("AgeSuicides.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
condition <- read.csv("ConditionSuicides.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)

#Pieta House datasets - prep for merging 
#tidyverse 
ageNew <- age

library(tidyr)
ageNew <- spread(ageNew, Age, Suicides)

gender$Gender <- gsub("gender", "", gender$Gender)
gender$Gender <- gsub("Trans", "_", gender$Gender)
gender$Gender <- gsub("_", "Transgender", gender$Gender)

genderNew <- gender
genderNew <- spread(genderNew, Gender, Suicides)
sapply(genderNew,function(x) sum(is.na(x)))
genderNew[is.na(genderNew)] <- 0

#condition
conditionNew <- condition

library(tidyr)
conditionNew <- spread(conditionNew, Cause, Suicides)
sapply(conditionNew,function(x) sum(is.na(x)))
conditionNew[is.na(conditionNew)] <- 0

# @reference https://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right
# Modified by : Alexander Millea
#
#merge data sets using join functions
library(tidyverse)
df <- inner_join(x = ageNew, y = genderNew)
df <- inner_join(x = df, y = conditionNew)
write.csv(df, 'FINALPieta1317.csv', row.names = FALSE)

#######################################
#Suicide CSO
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/CSO")
suicide08 <- read.csv("SexYearArea08-2017.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
suicide8013 <- read.csv("suicide8013.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
suicide50 <- read.csv("sexYearCleaned.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)

suicide08New <- suicide08
suicide08New <- suicide08New[c("Date", "Gender", "Location", "Count")]
suicide08New <- spread(suicide08New, Gender, Count)
suicide08New[is.na(suicide08New)] <- 0
write.csv(df, 'FINALsuicide0813.csv', row.names = FALSE)

#suicide '80- '13
suicide8013new <- suicide8013
suicide08New[is.na(suicide08New)] <- 0
write.csv(suicide8013new, 'FINALsuicide8013.csv', row.names = FALSE)

#suicide '50 - '17
suicide50 <- read.csv("sexYearCleaned.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
suicide50new <- suicide50
suicide50new <- suicide50new[, -3]
suicide50new$Location <- rep("Not Specified", nrow(suicide50new))
suicide50new$Gender <- rep("Not Specified", nrow(suicide50new))
colnames(suicide50new)[2] <- "Count"
suicide50new <- suicide50new[c("Date", "Location", "Gender", "Count")]
write.csv(suicide50new, 'FINALsuicide5017.csv', row.names = FALSE)

###################################################################################
#Social/Economic
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/CSO")
housing <- read.csv("housing.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
crime <- read.csv("crime.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
income <- read.csv("income.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
unemployment <- read.csv("unemployment.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
swelfare <- read.csv("swelfare.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)

housingNew <- housing
housingNew <- housingNew[c("Date", "Location", "Category", "Price")]
housingNew <- spread(housingNew, Category, Price)
write.csv(crimeNew, 'FINALhousing.csv', row.names = FALSE)

#income 
incomeNew <- income
incomeNew$Location <- rep("Not Specified", nrow(incomeNew))
incomeNew$Gender <- rep("Not Specified", nrow(incomeNew))
incomeNew <- spread(incomeNew, `Job Title`, `Weekly Income`)
write.csv(incomeNew, 'FINALincome.csv', row.names = FALSE)

#unmeployment 
unemploymentNew <- unemployment
unemploymentNew$Location <- rep("Not Specified", nrow(unemploymentNew))
unemploymentNew <- spread(unemploymentNew, `Unemployment Age`, `Unemployment Rate`)
colnames(unemploymentNew)[1] <- "Unemploy.Gender"
colnames(unemploymentNew)[2] <- "Date"
colnames(unemploymentNew)[5] <- "Unemploy.u25"
colnames(unemploymentNew)[7] <- "Unemploy.allages"
colnames(unemploymentNew)[6] <- "Unemploy.25+"
unemploymentNew[is.na(unemploymentNew)] <- 0
unemploymentNew <- unemploymentNew[c("Location", "Date", "Month", "Unemploy.Gender", "Unemploy.u25", "Unemploy.25+", "Unemploy.allages")]
write.csv(unemploymentNew, 'FINALunemploy.csv', row.names = FALSE)

#crime
crimeNew <- crime
crimeNew$Location <- rep("Not Specified", nrow(crimeNew))
crimeNew$Gender <- rep("Not Specified", nrow(crimeNew))
crimeNew <- spread(crimeNew, `Crime`, `Recorded Crimes`)
colnames(crimeNew)[,1] <- "Date"
write.csv(crimeNew, 'FINALcrime.csv', row.names = FALSE)

#social welfare
socialNew <- swelfare
socialNew$Location <- rep("Not Specified", nrow(socialNew))
socialNew <- socialNew[,-5]
socialNew$Category <- as.character(socialNew$Category)
socialNew <- spread(socialNew, `Age`, `Rate`)
colnames(socialNew)[6] <- "sw.u25"
colnames(socialNew)[7] <- "sw.25+"
colnames(socialNew)[8] <- "sw.allages"
socialNew$sw.u25 <- as.numeric(socialNew$sw.u25)
socialNew$`sw.25+` <- as.numeric(socialNew$`sw.25+`)
socialNew$sw.allages <- as.numeric(socialNew$sw.allages)
socialNew$Gender <- as.factor(socialNew$Gender)
socialNew$Category <- as.factor(socialNew$Category)
socialNew$Date <- as.numeric(socialNew$Date)
socialNew$Month <- as.numeric(socialNew$Month)
socialNew[is.na(socialNew)] <- 0

write.csv(socialNew, "FINALsw.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)

##########################################################
#Merging Suicide + Pieta House with Social/Economic
#suicide1317
#suicide + crime
#remove crime before 2013
crime = crime[crime$`Crime Date` > 2012,]
colnames(crime)[1] <- "Date"
dfc <- full_join(x = df, y = crime)

#Identify NA's
sapply(dfc,function(x) sum(is.na(x)))
dfc[is.na(dfc)] <- 0
write.csv(dfc, 'FINALsuicide1317c.csv', row.names = FALSE)

#suicide, crime, unemployment
unemploymentNew = unemploymentNew[unemploymentNew$Date > 2012,]
unemploymentNew = unemploymentNew[unemploymentNew$Date < 2018,]

dfcu <- full_join(x = dfc, y = unemploymentNew)
sapply(dfcu,function(x) sum(is.na(x)))
dfcu[is.na(dfcu)] <- 0
dfcu$Unemploy.Gender <- gsub(0, "Not Specified", dfcu$Unemploy.Gender)
dfcu$Unemploy.Gender[is.na(dfcu$Unemploy.Gender)] <- 0
write.csv(dfcu, 'FINALsuicide1317cu.csv', row.names = FALSE)

#suicide crime, unemployment, social welfare - does not correlate
#no location, age, gender match
swelfare = swelfare[swelfare$Date > 2012,]
swelfare = swelfare[swelfare$Date < 2018,]

dfcusw <- full_join(x = dfcu, y = swelfare)
sapply(dfcusw,function(x) sum(is.na(x)))
dfcusw[is.na(dfcusw)] <- 0
class(dfcusw$Category)
dfcusw$Category <- as.character(dfcusw$Category)
dfcusw$Category[is.na(dfcusw$Category)] <- "Not Specified"
dfcusw$Category <- as.factor(dfcusw$Category)
write.csv(dfcusw, 'FINALsuicide1317cusw.csv', row.names = FALSE)

#suicide c,u,sw,i,h
housingNew = housingNew[housingNew$Date > 2012,]
housingNew = housingNew[housingNew$Date < 2018,]
dfcuswh <- full_join(x = dfcusw, y = housingNew)
sapply(dfcuswh,function(x) sum(is.na(x)))

#used to replace factor NA's to Not Specified 
library(forcats)
fct_explicit_na(dfcuswh$Gender, "Not Specified")
dfcuswh[is.na(dfcuswh)] <- 0
dfcuswh$Gender <- gsub("0", "Not Specified", dfcuswh$Gender)
class(dfcuswh$Category)
dfcuswh$Category <- fct_explicit_na(dfcuswh$Category, "Not Specified")
write.csv(dfcuswh, 'FINALsuicide1317cuswh.csv', row.names = FALSE)

####################################################
#CSO DATA - suicide 08-17
#suicide + crime
dc <- full_join(x = suicide08New, y = crimeNew)

library(forcats)
fct_explicit_na(dc$Gender, "Not Specified")

#Identify NA's
sapply(dc,function(x) sum(is.na(x)))
dc[is.na(dc)] <- 0
write.csv(dc, 'FINALsuicide0817c.csv', row.names = FALSE)

#suicide, crime, unemployment
dcu <- full_join(x = dc, y = unemploymentNew)
sapply(dcu,function(x) sum(is.na(x)))
dcu[is.na(dcu)] <- 0
dcu$Gender <- gsub("0", "Not Specified", dcu$Gender)
write.csv(dcu, 'FINALsuicide0817cu.csv', row.names = FALSE)

#suicide crime, unemployment, social welfare
dcusw <- full_join(x = dcu, y = socialNew)
sapply(dcusw,function(x) sum(is.na(x)))
dcusw[is.na(dcusw)] <- 0
class(dcusw$Category)
dcusw$Category <- as.character(dcusw$Category)
dcusw$Category[is.na(dcusw$Category)] <- "Not Specified"
dcusw$Category <- as.factor(dcusw$Category)
write.csv(dcusw, 'FINALsuicide0817cusw.csv', row.names = FALSE)

#sucide c,u,sw, i 
dcuswi <- full_join(x = dcusw, y = incomeNew)
sapply(dcuswi,function(x) sum(is.na(x)))
dcuswi$Category <- dcuswi$Category[is.na(dcuswi$Category)] <- "Not Specified"
dcuswi[is.na(dcuswi)] <- 0
write.csv(dcuswi, 'FINALsuicide0817cuswi.csv', row.names = FALSE)

#suicide c,u,sw,i,h
dcuswih <- full_join(x = dcuswi, y = housingNew)
sapply(dcuswih,function(x) sum(is.na(x)))

#used to replace factor NA's to Not Specified 
library(forcats)
dcuswih$Gender <- fct_explicit_na(dcuswih$Gender, "Not Specified")
dcuswih$Category <- fct_explicit_na(dcuswih$Category, "Not Specified")
dcuswih[is.na(dcuswih)] <- 0
write.csv(dcuswih, 'FINALsuicide0817cuswih.csv', row.names = FALSE)

#######################################################
#suicide 80-13
#suicide + crime
xc <- full_join(x = suicide8013new, y = crimeNew)

library(forcats)
xc$Gender <- fct_explicit_na(xc$Gender, "Not Specified")

#Identify NA's
sapply(xc,function(x) sum(is.na(x)))
xc[is.na(xc)] <- 0
write.csv(xc, 'FINALsuicide8013c.csv', row.names = FALSE)

#suicide, crime, unemployment
xcu <- full_join(x = xc, y = unemploymentNew)
sapply(xcu,function(x) sum(is.na(x)))
xcu[is.na(xcu)] <- 0
xcu$Gender <- gsub("0", "Not Specified", xcu$Gender)
write.csv(xcu, 'FINALsuicide8013cu.csv', row.names = FALSE)

#suicide crime, unemployment, social welfare
xcusw <- full_join(x = xcu, y = socialNew)
sapply(xcusw,function(x) sum(is.na(x)))
xcusw[is.na(xcusw)] <- 0
class(xcusw$Category)
xcusw$Category <- as.character(xcusw$Category)
xcusw$Category[is.na(xcusw$Category)] <- "Not Specified"
xcusw$Category <- as.factor(xcusw$Category)
write.csv(xcusw, 'FINALsuicide8013cusw.csv', row.names = FALSE)

#sucide c,u,sw, i 
xcuswi <- full_join(x = xcusw, y = incomeNew)
sapply(xcuswi,function(x) sum(is.na(x)))
xcuswi$Category <- xcuswi$Category[is.na(xcuswi$Category)] <- "Not Specified"
xcuswi[is.na(xcuswi)] <- 0
write.csv(xcuswi, 'FINALsuicide8013cuswi.csv', row.names = FALSE)

#suicide c,u,sw,i,h
xcuswih <- full_join(x = xcuswi, y = housingNew)
sapply(xcuswih,function(x) sum(is.na(x)))

#used to replace factor NA's to Not Specified 
library(forcats)
xcuswih$Gender <- fct_explicit_na(xcuswih$Gender, "Not Specified")
xcuswih$Category <- fct_explicit_na(xcuswih$Category, "Not Specified")
xcuswih[is.na(xcuswih)] <- 0
write.csv(xcuswih, 'FINALsuicide8013cuswih.csv', row.names = FALSE)

#############################
#suicide 50-17
#suicide + crime
fc <- full_join(x = suicide50new, y = crimeNew)

library(forcats)
fc$Gender <- fct_explicit_na(fc$Gender, "Not Specified")

#Identify NA's
sapply(fc,function(x) sum(is.na(x)))
fc[is.na(fc)] <- 0
write.csv(fc, 'FINALsuicide5017c.csv', row.names = FALSE)

#suicide, crime, unemployment
fcu <- full_join(x = fc, y = unemploymentNew)
sapply(fcu,function(x) sum(is.na(x)))
fcu[is.na(fcu)] <- 0
fcu$Gender <- gsub("0", "Not Specified", fcu$Gender)
write.csv(fcu, 'FINALsuicide5017cu.csv', row.names = FALSE)

#suicide crime, unemployment, social welfare
fcusw <- full_join(x = fcu, y = socialNew)
sapply(fcusw,function(x) sum(is.na(x)))
fcusw[is.na(fcusw)] <- 0
class(fcusw$Category)
fcusw$Category <- as.character(fcusw$Category)
fcusw$Category[is.na(fcusw$Category)] <- "Not Specified"
fcusw$Category <- as.factor(fcusw$Category)
write.csv(fcusw, 'FINALsuicide5017cusw.csv', row.names = FALSE)

#sucide c,u,sw, i 
fcuswi <- full_join(x = fcusw, y = incomeNew)
sapply(fcuswi,function(x) sum(is.na(x)))
fcuswi$Category <- fcuswi$Category[is.na(fcuswi$Category)] <- "Not Specified"
fcuswi[is.na(fcuswi)] <- 0
write.csv(fcuswi, 'FINALsuicide5017cuswi.csv', row.names = FALSE)

#suicide c,u,sw,i,h
fcuswih <- full_join(x = fcuswi, y = housingNew)
sapply(fcuswih,function(x) sum(is.na(x)))

#used to replace factor NA's to Not Specified 
library(forcats)
fcuswih$Gender <- fct_explicit_na(fcuswih$Gender, "Not Specified")
fcuswih$Category <- fct_explicit_na(fcuswih$Category, "Not Specified")
fcuswih[is.na(fcuswih)] <- 0
write.csv(fcuswih, 'FINALsuicide5017cuswih.csv', row.names = FALSE)

#########################################################
# add fifth dataset creation if needed 
#80-13 + 08-17. Remove 08 - 13 and add 80-13 with dates onwards



