#CSO 1950 - 2017 
#set directory 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/CSO")

#load csv 
sexYear <- read.csv("sexYear.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")

#print dataframe
print(sexYear)

#Identify NA's
sapply(sexYear,function(x) sum(is.na(x)))

#Delete NA's/Columns or Rows not needed
ncol(sexYear)

#Check total rows / remove row(s) not needed 
nrow(sexYear)

#changing column names
names(sexYear)[names(sexYear) == "Year"] <- "Date"
names(sexYear)[names(sexYear) == "Suicides..Number."] <- "Suicides"
names(sexYear)[names(sexYear) == "Suicides..Rate.per.100.000.Pop.."] <- "Per_100,000"


#check class of columns
class(sexYear$Date)
class(sexYear$Suicides)
class(sexYear$`Per_100.000`)

#display data
print(sexYear)

#save to new csv
write.csv(sexYear, 'sexYearCleaned.csv', row.names = FALSE)


#####################################
#suicide 2008 - 2017 
#set directory 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/CSO")
SuicideSexYear <- read.csv("sex,area,year.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")

SuicideSexYear <- SuicideSexYear[-1, ]
colnames(SuicideSexYear) = SuicideSexYear[1, ]

colnames(SuicideSexYear)[1] <- "Gender"
colnames(SuicideSexYear)[2] <- "Location"
SuicideSexYear <- SuicideSexYear[-1, ]

SuicideSexYear$Gender[1:2] <- gsub(" ", "Male", SuicideSexYear$Gender[1:2])
SuicideSexYear <- SuicideSexYear[-1, ]

#@reference https://stackoverflow.com/questions/42769998/r-autofill-blanks-in-variable-until-next-value
library(zoo)
SuicideSexYear$Gender <- gsub('\\s+', '', SuicideSexYear$Gender)  # replace multiple spaces by blanks
SuicideSexYear$Gender[SuicideSexYear$Gender == ''] <- NA  # replace blanks by NAs
SuicideSexYear$Gender <- na.locf(SuicideSexYear$Gender)

#Identify NA's
sapply(SuicideSexYear,function(x) sum(is.na(x)))

#re-order rows
row.names(SuicideSexYear) <- 1:nrow(SuicideSexYear)

SuicideSexYear <- SuicideSexYear[-22, ]
SuicideSexYear <- SuicideSexYear[-47, ]
SuicideSexYear <- SuicideSexYear[-68, ]


#retructure data 
library(reshape2)
library(reshapeGUI)
reshapeGUI()
SuicideSexYear <- melt(data = SuicideSexYear, id.vars=c('Gender','Location'), 
                       measure.vars=c('2008','2009','2010','2011','2012','2013',
                                      '2014','2015','2016','2017'))
colnames(SuicideSexYear)[3] <- "Date"
colnames(SuicideSexYear)[4] <- "Count"
SuicideSexYear <- SuicideSexYear[c("Gender", "Location", "Count", "Date")]

#change specific values in row
library(dplyr)
library(car)
SuicideSexYear <- mutate(SuicideSexYear, Location= recode(Location, "'Ulster (part of)'='Ulster'"))

write.csv(SuicideSexYear, 'SexYearArea08-2017.csv', row.names = FALSE)

setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/esf")
SuicideSexYear <- read.csv("SexYearArea08-2017.csv", sep = ",", header = TRUE, check.names = FALSE)

SuicideSexYear <- spread(SuicideSexYear, Gender, Count)
sapply(SuicideSexYear,function(x) sum(is.na(x)))
SuicideSexYear[is.na(SuicideSexYear)] <- 0

SuicideSexYear <- SuicideSexYear[!grepl("2013", SuicideSexYear$Date),]
SuicideSexYear <- SuicideSexYear[!grepl("2014", SuicideSexYear$Date),]
SuicideSexYear <- SuicideSexYear[!grepl("2015", SuicideSexYear$Date),]
SuicideSexYear <- SuicideSexYear[!grepl("2016", SuicideSexYear$Date),]
SuicideSexYear <- SuicideSexYear[!grepl("2017", SuicideSexYear$Date),]

write.csv(SuicideSexYear, 'SexYearArea08-2017.csv', row.names = FALSE)

#################################
#Suicide 1980 - 2013
#set directory 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/CSO")
Suicide8013 <- read.csv("Suicide80-2013.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")

#remove column
Suicide8013 <- Suicide8013[, -1]

#make first row coulmn headers
colnames(Suicide8013) = Suicide8013[1, ]
colnames(Suicide8013)[1] <- "Gender"
colnames(Suicide8013)[2] <- "Location"
Suicide8013 <- Suicide8013[-1, ]

library(zoo)
Suicide8013$Gender <- gsub('\\s+', '', Suicide8013$Gender)  # replace multiple spaces by blanks
Suicide8013$Gender[Suicide8013$Gender == ''] <- NA  # replace blanks by NAs
Suicide8013$Gender <- na.locf(Suicide8013$Gender)
Suicide8013 <- Suicide8013[-1, ]

#change Bothsexes to Both
Suicide8013$Gender <- gsub("Bothsexes", "Both", Suicide8013$Gender)

#remove rows with NA values
library(tidyr)
Suicide8013 <- Suicide8013 %>% drop_na()

#naming conventions cleanup 
Suicide8013$Location <- gsub("Dublin City and County", "Dublin", Suicide8013$Location)
Suicide8013$Location <- gsub("North Tipperary", "Tipperary North", Suicide8013$Location)
Suicide8013$Location <- gsub("South Tipperary", "Tipperary South", Suicide8013$Location)
Suicide8013$Location <- gsub("All counties and regions", "All Regions", Suicide8013$Location)

str(Suicide8013)

#restructure data
library(reshape2)
library(reshapeGUI)

reshapeGUI()
Suicide8013 <- melt(data = Suicide8013, 
                    id.vars=c('Gender','Location'), 
                    measure.vars=c('1980','1981','1982','1983','1984','1985','1986','1987',
                                   '1988','1989','1990','1991','1992','1993','1994','1995',
                                   '1996','1997','1998','1999','2000','2001','2002','2003',
                                   '2004','2005','2006','2007','2008','2009','2010','2011',
                                   '2012','2013'))

colnames(Suicide8013)[3] <- "Date"
colnames(Suicide8013)[4] <- "Count"

Suicide8013 <- spread(Suicide8013, Gender, Count)

#change classes
Suicide8013$Location <- as.factor(Suicide8013$Location)
Suicide8013$Date <- as.numeric(as.character(Suicide8013$Date))

write.csv(Suicide8013, 'suicide8013.csv', row.names = FALSE)

##################################
#Suicide 2008 - 2017 
#sex area year 
#set directory 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/CSO")
SuicideSexYear <- read.csv("sex,area,year.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")

SuicideSexYear <- SuicideSexYear[-1, ]
colnames(SuicideSexYear) = SuicideSexYear[1, ]

colnames(SuicideSexYear)[1] <- "Gender"
colnames(SuicideSexYear)[2] <- "Location"
SuicideSexYear <- SuicideSexYear[-1, ]

SuicideSexYear$Gender[1:2] <- gsub(" ", "Male", SuicideSexYear$Gender[1:2])
SuicideSexYear <- SuicideSexYear[-1, ]

library(zoo)
SuicideSexYear$Gender <- gsub('\\s+', '', SuicideSexYear$Gender)  # replace multiple spaces by blanks
SuicideSexYear$Gender[SuicideSexYear$Gender == ''] <- NA  # replace blanks by NAs
SuicideSexYear$Gender <- na.locf(SuicideSexYear$Gender)

#Identify NA's
sapply(SuicideSexYear,function(x) sum(is.na(x)))

#re-order rows
row.names(SuicideSexYear) <- 1:nrow(SuicideSexYear)

SuicideSexYear <- SuicideSexYear[-22, ]
SuicideSexYear <- SuicideSexYear[-47, ]
SuicideSexYear <- SuicideSexYear[-68, ]


#retructure data 
library(reshape2)
library(reshapeGUI)
reshapeGUI()
SuicideSexYear <- melt(data = SuicideSexYear, id.vars=c('Gender','Location'), 
                       measure.vars=c('2008','2009','2010','2011','2012','2013',
                                      '2014','2015','2016','2017'))
colnames(SuicideSexYear)[3] <- "Date"
colnames(SuicideSexYear)[4] <- "Count"
SuicideSexYear <- SuicideSexYear[c("Gender", "Location", "Count", "Date")]

#change specific values in row
library(dplyr)
library(car)
SuicideSexYear <- mutate(SuicideSexYear, Location= recode(Location, "'Ulster (part of)'='Ulster'"))

write.csv(SuicideSexYear, 'SexYearArea08-2017.csv', row.names = FALSE)

#setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/CSO")
#SuicideSexYear <- read.csv("SexYearArea08-2017.csv", sep = ",", header = TRUE, check.names = FALSE)

#SuicideSexYear <- spread(SuicideSexYear, Gender, Count)
#sapply(SuicideSexYear,function(x) sum(is.na(x)))
#SuicideSexYear[is.na(SuicideSexYear)] <- 0

#SuicideSexYear <- SuicideSexYear[!grepl("2013", SuicideSexYear$Date),]
#SuicideSexYear <- SuicideSexYear[!grepl("2014", SuicideSexYear$Date),]
#SuicideSexYear <- SuicideSexYear[!grepl("2015", SuicideSexYear$Date),]
#SuicideSexYear <- SuicideSexYear[!grepl("2016", SuicideSexYear$Date),]
#SuicideSexYear <- SuicideSexYear[!grepl("2017", SuicideSexYear$Date),]
#write.csv(SuicideSexYear, 'SexYearArea08-2017.csv', row.names = FALSE)




