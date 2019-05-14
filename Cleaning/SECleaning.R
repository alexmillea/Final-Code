#Social Economic Factor Cleanign Script 
#recorded crimes 203 - 2017
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/CSO")

crimeRate <- read.csv("recordedcrime.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")

colnames(crimeRate)[1] <- "Crime"
colnames(crimeRate)[2] <- "2003"
colnames(crimeRate)[3] <- "2004"
colnames(crimeRate)[4] <- "2005"
colnames(crimeRate)[5] <- "2006"
colnames(crimeRate)[6] <- "2007"
colnames(crimeRate)[7] <- "2008"
colnames(crimeRate)[8] <- "2009"
colnames(crimeRate)[9] <- "2010"
colnames(crimeRate)[10] <- "2011"
colnames(crimeRate)[11] <- "2012"
colnames(crimeRate)[12] <- "2013"
colnames(crimeRate)[13] <- "2014"
colnames(crimeRate)[14] <- "2015"
colnames(crimeRate)[15] <- "2016"
colnames(crimeRate)[16] <- "2017"

#rename values
crimeRate$Crime<- gsub("Bothsexes", "Both", DeathRates$Gender)

crimeRate$Crime <- gsub('[[:digit:]]+', '', crimeRate$Crime)
crimeRate$Crime <- gsub(',', '', crimeRate$Crime)

#restructure data
library(reshape2)
library(reshapeGUI)
reshapeGUI()

crimeRate <- melt(data = crimeRate, id.vars=c('Crime'), 
                  measure.vars=c('2003','2004','2005','2006','2007','2008',
                                 '2009','2010','2011','2012','2013','2014',
                                 '2015','2016','2017'))

colnames(crimeRate)[2] <- "Crime Date"
colnames(crimeRate)[3] <- "Recorded Crimes"

crimeRate <- crimeRate[c("Crime Date", "Crime", "Recorded Crimes")]

write.csv(crimeRate, 'crime.csv', row.names = FALSE)

###########################
#housing cleaning 1975 - 2016
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/CSO")
housing <- read.csv("housingPrices.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")

housing <- housing[-1, ]
colnames(housing) = housing[1, ]
housing <- housing[-1, ]

colnames(housing)[1] <- "Location"
colnames(housing)[2] <- "Category"

#autofill fields
library(zoo)
housing$Location <- gsub('\\s+', '', housing$Location)  # replace multiple spaces by blanks
housing$Location[housing$Location == ''] <- NA  # replace blanks by NAs
housing$Location <- na.locf(housing$Location)

#remove rows with NA values
library(tidyr)
housing <- housing %>% drop_na()

#extract every second row
df.new = housing[seq(1, nrow(housing), 2), ]
df.new2 = housing[seq(2, nrow(housing), 2), ]

#restructure data
library(reshape2)
library(reshapeGUI)

reshapeGUI()
housing <- melt(data = housing, id.vars=c('Location','Category'), 
                measure.vars=c('1975','1976','1977','1978','1979','1980','1981','1982',
                               '1983','1984','1985','1986','1987','1988','1989','1990',
                               '1991','1992','1993','1994','1995','1996','1997','1998',
                               '1999','2000','2001','2002','2003','2004','2005','2006',
                               '2007','2008','2009','2010','2011','2012','2013','2014',
                               '2015','2016'))

colnames(housing)[3] <- "Date"
colnames(housing)[4] <- "Price"

class(housing$Location)
class(housing$Category)
class(housing$Date)
class(housing$Price)

housing$Location <- as.factor(housing$Location)
housing$Category <- as.factor(housing$Category)
housing$Price <- as.double(housing$Price)

housing$Price[is.na(housing$Price)] <- 0

#re-order rows
row.names(housing) <- 1:nrow(housing)

write.csv(housing, 'housing.csv', row.names = FALSE)

########################
#income 88 - 2008
#income
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/CSO")

income <- read.csv("income88-2008.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")

income <- income[-1, ]
income <- income[-1, ]
colnames(income) = income[1, ]
income <- income[-1, ]
colnames(income)[1] <- "Job Title"

str(income)
income$`Job Title` <- as.factor(income$`Job Title`)

#restructure data
library(reshape2)
library(reshapeGUI)

reshapeGUI()
income <- melt(data = income, id.vars=c('Job Title'), 
               measure.vars=c('1988','1989','1990','1991','1992','1993',
                              '1994','1995','1996','1997','1998','1999',
                              '2000','2001','2002','2003','2004','2005',
                              '2006','2007','2008'))

colnames(income)[2] <- "Date"
colnames(income)[3] <- "Weekly Income"

class(income$Date)
class(income$Income)

write.csv(income, 'income.csv', row.names = FALSE)

#############################
#social welfare 
#social
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/CSO")
swelfare <- read.csv("socialwelfare.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")

colnames(swelfare)[1] <- "Gender"
colnames(swelfare)[2] <- "Age"
colnames(swelfare)[3] <- "Category"

#autofill fields
library(zoo)
swelfare$Gender <- gsub('\\s+', '', swelfare$Gender)  # replace multiple spaces by blanks
swelfare$Gender[swelfare$Gender == ''] <- NA  # replace blanks by NAs
swelfare$Gender <- na.locf(swelfare$Gender)
swelfare <- swelfare[-1, ]

swelfare$Age <- gsub('\\s+', '', swelfare$Age)  # replace multiple spaces by blanks
swelfare$Age[swelfare$Age == ''] <- NA  # replace blanks by NAs
swelfare$Age <- na.locf(swelfare$Age)
swelfare <- swelfare[-1, ]

#remove rows with NA values
library(tidyr)
swelfare <- swelfare %>% drop_na()

swelfare$Gender <- gsub("Bothsexes", "Both", swelfare$Gender)


#restructure data 
library(reshape2)
library(reshapeGUI)
reshapeGUI()
swelfare.melt <- melt(data = swelfare, 
                      id.vars=c('Gender','Age','Category'), 
                      measure.vars=c('X1967M01','X1967M02','X1967M03','X1967M04','X1967M05','X1967M06','X1967M07','X1967M08','X1967M09','X1967M10','X1967M11','X1967M12','X1968M01','X1968M02','X1968M03','X1968M04','X1968M05','X1968M06','X1968M07','X1968M08','X1968M09','X1968M10','X1968M11','X1968M12','X1969M01','X1969M02','X1969M03','X1969M04','X1969M05','X1969M06','X1969M07','X1969M08','X1969M09','X1969M10','X1969M11','X1969M12','X1970M01','X1970M02','X1970M03','X1970M04','X1970M05','X1970M06','X1970M07','X1970M08','X1970M09','X1970M10','X1970M11','X1970M12','X1971M01','X1971M02','X1971M03','X1971M04','X1971M05','X1971M06','X1971M07','X1971M08','X1971M09','X1971M10','X1971M11','X1971M12','X1972M01','X1972M02','X1972M03','X1972M04','X1972M05','X1972M06','X1972M07','X1972M08','X1972M09','X1972M10','X1972M11','X1972M12','X1973M01','X1973M02','X1973M03','X1973M04','X1973M05','X1973M06','X1973M07','X1973M08','X1973M09','X1973M10','X1973M11','X1973M12','X1974M01','X1974M02','X1974M03','X1974M04','X1974M05','X1974M06','X1974M07','X1974M08','X1974M09','X1974M10','X1974M11','X1974M12','X1975M01','X1975M02','X1975M03','X1975M04','X1975M05','X1975M06','X1975M07','X1975M08','X1975M09','X1975M10','X1975M11','X1975M12','X1976M01','X1976M02','X1976M03','X1976M04','X1976M05','X1976M06','X1976M07','X1976M08','X1976M09','X1976M10','X1976M11','X1976M12','X1977M01','X1977M02','X1977M03','X1977M04','X1977M05','X1977M06','X1977M07','X1977M08','X1977M09','X1977M10','X1977M11','X1977M12','X1978M01','X1978M02','X1978M03','X1978M04','X1978M05','X1978M06','X1978M07','X1978M08','X1978M09','X1978M10','X1978M11','X1978M12','X1979M01','X1979M02','X1979M03','X1979M04','X1979M05','X1979M06','X1979M07','X1979M08','X1979M09','X1979M10','X1979M11','X1979M12','X1980M01','X1980M02','X1980M03','X1980M04','X1980M05','X1980M06','X1980M07','X1980M08','X1980M09','X1980M10','X1980M11','X1980M12','X1981M01','X1981M02','X1981M03','X1981M04','X1981M05','X1981M06','X1981M07','X1981M08','X1981M09','X1981M10','X1981M11','X1981M12','X1982M01','X1982M02','X1982M03','X1982M04','X1982M05','X1982M06','X1982M07','X1982M08','X1982M09','X1982M10','X1982M11','X1982M12','X1983M01','X1983M02','X1983M03','X1983M04','X1983M05','X1983M06','X1983M07','X1983M08','X1983M09','X1983M10','X1983M11','X1983M12','X1984M01','X1984M02','X1984M03','X1984M04','X1984M05','X1984M06','X1984M07','X1984M08','X1984M09','X1984M10','X1984M11','X1984M12','X1985M01','X1985M02','X1985M03','X1985M04','X1985M05','X1985M06','X1985M07','X1985M08','X1985M09','X1985M10','X1985M11','X1985M12','X1986M01','X1986M02','X1986M03','X1986M04','X1986M05','X1986M06','X1986M07','X1986M08','X1986M09','X1986M10','X1986M11','X1986M12','X1987M01','X1987M02','X1987M03','X1987M04','X1987M05','X1987M06','X1987M07','X1987M08','X1987M09','X1987M10','X1987M11','X1987M12','X1988M01','X1988M02','X1988M03','X1988M04','X1988M05','X1988M06','X1988M07','X1988M08','X1988M09','X1988M10','X1988M11','X1988M12','X1989M01','X1989M02','X1989M03','X1989M04','X1989M05','X1989M06','X1989M07','X1989M08','X1989M09','X1989M10','X1989M11','X1989M12','X1990M01','X1990M02','X1990M03','X1990M04','X1990M05','X1990M06','X1990M07','X1990M08','X1990M09','X1990M10','X1990M11','X1990M12','X1991M01','X1991M02','X1991M03','X1991M04','X1991M05','X1991M06','X1991M07','X1991M08','X1991M09','X1991M10','X1991M11','X1991M12','X1992M01','X1992M02','X1992M03','X1992M04','X1992M05','X1992M06','X1992M07','X1992M08','X1992M09','X1992M10','X1992M11','X1992M12','X1993M01','X1993M02','X1993M03','X1993M04','X1993M05','X1993M06','X1993M07','X1993M08','X1993M09','X1993M10','X1993M11','X1993M12','X1994M01','X1994M02','X1994M03','X1994M04','X1994M05','X1994M06','X1994M07','X1994M08','X1994M09','X1994M10','X1994M11','X1994M12','X1995M01','X1995M02','X1995M03','X1995M04','X1995M05','X1995M06','X1995M07','X1995M08','X1995M09','X1995M10','X1995M11','X1995M12','X1996M01','X1996M02','X1996M03','X1996M04','X1996M05','X1996M06','X1996M07','X1996M08','X1996M09','X1996M10','X1996M11','X1996M12','X1997M01','X1997M02','X1997M03','X1997M04','X1997M05','X1997M06','X1997M07','X1997M08','X1997M09','X1997M10','X1997M11','X1997M12','X1998M01','X1998M02','X1998M03','X1998M04','X1998M05','X1998M06','X1998M07','X1998M08','X1998M09','X1998M10','X1998M11','X1998M12','X1999M01','X1999M02','X1999M03','X1999M04','X1999M05','X1999M06','X1999M07','X1999M08','X1999M09','X1999M10','X1999M11','X1999M12','X2000M01','X2000M02','X2000M03','X2000M04','X2000M05','X2000M06','X2000M07','X2000M08','X2000M09','X2000M10','X2000M11','X2000M12','X2001M01','X2001M02','X2001M03','X2001M04','X2001M05','X2001M06','X2001M07','X2001M08','X2001M09','X2001M10','X2001M11','X2001M12','X2002M01','X2002M02','X2002M03','X2002M04','X2002M05','X2002M06','X2002M07','X2002M08','X2002M09','X2002M10','X2002M11','X2002M12','X2003M01','X2003M02','X2003M03','X2003M04','X2003M05','X2003M06','X2003M07','X2003M08','X2003M09','X2003M10','X2003M11','X2003M12','X2004M01','X2004M02','X2004M03','X2004M04','X2004M05','X2004M06','X2004M07','X2004M08','X2004M09','X2004M10','X2004M11','X2004M12','X2005M01','X2005M02','X2005M03','X2005M04','X2005M05','X2005M06','X2005M07','X2005M08','X2005M09','X2005M10','X2005M11','X2005M12','X2006M01','X2006M02','X2006M03','X2006M04','X2006M05','X2006M06','X2006M07','X2006M08','X2006M09','X2006M10','X2006M11','X2006M12','X2007M01','X2007M02','X2007M03','X2007M04','X2007M05','X2007M06','X2007M07','X2007M08','X2007M09','X2007M10','X2007M11','X2007M12','X2008M01','X2008M02','X2008M03','X2008M04','X2008M05','X2008M06','X2008M07','X2008M08','X2008M09','X2008M10','X2008M11','X2008M12','X2009M01','X2009M02','X2009M03','X2009M04','X2009M05','X2009M06','X2009M07','X2009M08','X2009M09','X2009M10','X2009M11','X2009M12','X2010M01','X2010M02','X2010M03','X2010M04','X2010M05','X2010M06','X2010M07','X2010M08','X2010M09','X2010M10','X2010M11','X2010M12','X2011M01','X2011M02','X2011M03','X2011M04','X2011M05','X2011M06','X2011M07','X2011M08','X2011M09','X2011M10','X2011M11','X2011M12','X2012M01','X2012M02','X2012M03','X2012M04','X2012M05','X2012M06','X2012M07','X2012M08','X2012M09','X2012M10','X2012M11','X2012M12','X2013M01','X2013M02','X2013M03','X2013M04','X2013M05','X2013M06','X2013M07','X2013M08','X2013M09','X2013M10','X2013M11','X2013M12','X2014M01','X2014M02','X2014M03','X2014M04','X2014M05','X2014M06','X2014M07','X2014M08','X2014M09','X2014M10','X2014M11','X2014M12','X2015M01','X2015M02','X2015M03','X2015M04','X2015M05','X2015M06','X2015M07','X2015M08','X2015M09','X2015M10','X2015M11','X2015M12','X2016M01','X2016M02','X2016M03','X2016M04','X2016M05','X2016M06','X2016M07','X2016M08','X2016M09','X2016M10','X2016M11','X2016M12','X2017M01','X2017M02','X2017M03','X2017M04','X2017M05','X2017M06','X2017M07','X2017M08','X2017M09','X2017M10','X2017M11','X2017M12','X2018M01','X2018M02','X2018M03','X2018M04','X2018M05','X2018M06','X2018M07','X2018M08','X2018M09','X2018M10','X2018M11','X2018M12','X2019M01'))
swelfare <- swelfare.melt

colnames(swelfare)[4] <- "Date"
colnames(swelfare)[5] <- "Rate"

swelfare$Date <- gsub("X", "", swelfare$Date)

#add new column for month
swelfare$Month <- swelfare$Date
swelfare$Date <- sub("^(\\d{4}).*$", "\\1", swelfare$Date)

library(stringr)
swelfare$Month <- stringr::str_extract(swelfare$Month, "\\d{2}")
swelfare$Month <- substring(swelfare$Month,6,7)

swelfare <- (swelfare[c("Gender", "Age", "Category", "Date", "Month", "Rate")])

write.csv(swelfare, 'swelfare.csv', row.names = FALSE)

#################################
#unemployment 98 - 2019
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/CSO")
unemployment <- read.csv("monthlyUnemployment.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")

colnames(unemployment)[1] <- "Gender"
colnames(unemployment)[2] <- "Age"

#autofill fields
library(zoo)
unemployment$Gender <- gsub('\\s+', '', unemployment$Gender)  # replace multiple spaces by blanks
unemployment$Gender[unemployment$Gender == ''] <- NA  # replace blanks by NAs
unemployment$Gender <- na.locf(unemployment$Gender)

#remove rows with NA values
library(tidyr)
unemployment <- unemployment %>% drop_na()

unemployment$Gender <- gsub("Bothsexes", "Both", unemployment$Gender)

unemployment$Gender <- as.factor(unemployment$Gender)
unemployment$Age <- as.factor(unemployment$Age)

#retructure data 
library(reshape2)
library(reshapeGUI)
reshapeGUI()
unemployment <- melt(data = unemployment, 
                     id.vars=c('Gender','Age'), 
                     measure.vars=c('X1998M01','X1998M02','X1998M03','X1998M04','X1998M05',
                                    'X1998M06','X1998M07','X1998M08','X1998M09','X1998M10',
                                    'X1998M11','X1998M12','X1999M01','X1999M02','X1999M03',
                                    'X1999M04','X1999M05','X1999M06','X1999M07','X1999M08',
                                    'X1999M09','X1999M10','X1999M11','X1999M12','X2000M01','X2000M02','X2000M03','X2000M04',
                                    'X2000M05','X2000M06','X2000M07','X2000M08','X2000M09','X2000M10','X2000M11','X2000M12',
                                    'X2001M01','X2001M02','X2001M03','X2001M04','X2001M05','X2001M06','X2001M07','X2001M08',
                                    'X2001M09','X2001M10','X2001M11','X2001M12','X2002M01','X2002M02','X2002M03','X2002M04',
                                    'X2002M05','X2002M06','X2002M07','X2002M08','X2002M09','X2002M10','X2002M11','X2002M12',
                                    'X2003M01','X2003M02','X2003M03','X2003M04','X2003M05','X2003M06','X2003M07','X2003M08',
                                    'X2003M09','X2003M10','X2003M11','X2003M12','X2004M01','X2004M02','X2004M03','X2004M04',
                                    'X2004M05','X2004M06','X2004M07','X2004M08','X2004M09','X2004M10','X2004M11','X2004M12',
                                    'X2005M01','X2005M02','X2005M03','X2005M04','X2005M05','X2005M06','X2005M07','X2005M08',
                                    'X2005M09','X2005M10','X2005M11','X2005M12','X2006M01','X2006M02','X2006M03','X2006M04',
                                    'X2006M05','X2006M06','X2006M07','X2006M08','X2006M09','X2006M10','X2006M11','X2006M12',
                                    'X2007M01','X2007M02','X2007M03','X2007M04','X2007M05','X2007M06','X2007M07','X2007M08',
                                    'X2007M09','X2007M10','X2007M11','X2007M12','X2008M01','X2008M02','X2008M03','X2008M04',
                                    'X2008M05','X2008M06','X2008M07','X2008M08','X2008M09','X2008M10','X2008M11','X2008M12',
                                    'X2009M01','X2009M02','X2009M03','X2009M04','X2009M05','X2009M06','X2009M07','X2009M08',
                                    'X2009M09','X2009M10','X2009M11','X2009M12','X2010M01','X2010M02','X2010M03','X2010M04',
                                    'X2010M05','X2010M06','X2010M07','X2010M08','X2010M09','X2010M10','X2010M11','X2010M12',
                                    'X2011M01','X2011M02','X2011M03','X2011M04','X2011M05','X2011M06','X2011M07','X2011M08',
                                    'X2011M09','X2011M10','X2011M11','X2011M12','X2012M01','X2012M02','X2012M03','X2012M04',
                                    'X2012M05','X2012M06','X2012M07','X2012M08','X2012M09','X2012M10','X2012M11','X2012M12',
                                    'X2013M01','X2013M02','X2013M03','X2013M04','X2013M05','X2013M06','X2013M07','X2013M08',
                                    'X2013M09','X2013M10','X2013M11','X2013M12','X2014M01','X2014M02','X2014M03','X2014M04',
                                    'X2014M05','X2014M06','X2014M07','X2014M08','X2014M09','X2014M10','X2014M11','X2014M12',
                                    'X2015M01','X2015M02','X2015M03','X2015M04','X2015M05','X2015M06','X2015M07','X2015M08',
                                    'X2015M09','X2015M10','X2015M11','X2015M12','X2016M01','X2016M02','X2016M03','X2016M04',
                                    'X2016M05','X2016M06','X2016M07','X2016M08','X2016M09','X2016M10','X2016M11','X2016M12',
                                    'X2017M01','X2017M02','X2017M03','X2017M04','X2017M05','X2017M06','X2017M07','X2017M08',
                                    'X2017M09','X2017M10','X2017M11','X2017M12','X2018M01','X2018M02','X2018M03','X2018M04',
                                    'X2018M05','X2018M06','X2018M07','X2018M08','X2018M09','X2018M10','X2018M11','X2018M12','X2019M01'))

colnames(unemployment)[2] <- "Unemployment Age"
colnames(unemployment)[3] <- "Unemployment Year"
colnames(unemployment)[4] <- "Unemployment Rate"

unemployment$`Unemployment Year` <- gsub("X", "", unemployment$`Unemployment Year`)

#add new column for month
unemployment$Month <- unemployment$`Unemployment Year`
unemployment$`Unemployment Year` <- sub("^(\\d{4}).*$", "\\1", unemployment$`Unemployment Year`)

library(stringr)
unemployment$Month <- substring(unemployment$Month,6,7)

unemployment <- (unemployment[c("Gender", "Unemployment Age", "Unemployment Year", "Month", "Unemployment Rate")])

write.csv(unemployment, 'unemployment.csv', row.names = FALSE)

