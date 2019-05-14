#SOM - Self Organising Maps
#
# @ reference: https://clarkdatalabs.github.io/soms/SOM_NBA
# Modified by: Alexander Millea
# Date: 25/03/2019
#
#Pieta House dataset 13-17
#
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")
ph1317 <- read.csv("FINALPieta1317.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
library(kohonen)
library(RColorBrewer)

#change location to numeric
som <- as.data.frame(ph1317)
som$Location <- unclass(som$Location)
som$Location <- as.numeric(som$Location)
colnames(som)

#create som measures based on variables
#want to find patterns in all genders based on location
som.measures1 <- c("Location", "Male", "U18", "18-24", "25-44", "45-64", "65+",
                   "DNAA", "DSH", "SA Only", "SA/DSH", "SI Only")
som1 <- som(scale(som[som.measures1]), grid = somgrid(6,4, "hexagonal"))
plot(som1, main = "Clusters for Males based on Location, Age, Condition")

som.measures2 <- c("Location", "Female", "U18", "18-24", "25-44", "45-64", "65+",
                   "DNAA", "DSH", "SA Only", "SA/DSH", "SI Only")
som2 <- som(scale(som[som.measures2]), grid = somgrid(6,4, "hexagonal"))
plot(som2, main = "Clusters for Females based on Location, Age, Condition")

som.measures3 <- c("Location", "Transgender", "U18", "18-24", "25-44", "45-64", "65+",
                   "DNAA", "DSH", "SA Only", "SA/DSH", "SI Only")
som3 <- som(scale(som[som.measures3]), grid = somgrid(6,4, "hexagonal"))
plot(som3, main = "Clusters for Transgender based on Location, Age, Condition")


#heatmap - red = higher
colors <- function(n, alpha = 1) {
  rev(heat.colors(n, alpha))
}
plot(som1, type = "count", palette.name = colors, heatkey = TRUE, main = "Heat Maps")
plot(som2, type = "count", palette.name = colors, heatkey = TRUE, main = "Heat Maps")
plot(som3, type = "count", palette.name = colors, heatkey = TRUE, main = "Heat Maps")

#####################################
#Pieta House dataset 13-17 with social & economic factors 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/CSO")
ph1317 <- read.csv("FINALsuicide1317cuswih.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
library(kohonen)
library(RColorBrewer)

#change location to numeric
som <- as.data.frame(ph1317)
som$Location <- unclass(som$Location)
som$Location <- as.numeric(som$Location)
colnames(som)

som <- som[, -100]
som <- som[, -20]

#create som measures based on variables
#want to find patterns in all genders based on location
som.measures1 <- c("Location", "Transgender", "U18", "18-24", "25-44", "45-64", "65+",
                   "DNAA", "DSH", "SA Only", "SA/DSH", "SI Only", "New House Prices (Euro)", 
                   "Second Hand House Prices (Euro)")
som1 <- som(scale(som[som.measures1]), grid = somgrid(6,4, "hexagonal"))
plot(som1, main = "Clusters for Transgender based on Location, Age, Condition and Housing")

som.measures2 <- c("Location", "Transgender", "U18", "18-24", "25-44", "45-64", "65+",
                   "DNAA", "DSH", "SA Only", "SA/DSH", "SI Only",
                   "Unemploy.u25", "Unemploy.25+")
som2 <- som(scale(som[som.measures2]), grid = somgrid(6,4, "hexagonal"))
plot(som2, main = "Clusters based on Gender, Location, Age, Condition and Unemployment")

som.measures3 <- c("Location", "U18", "18-24", "25-44", "45-64", "65+",
                   "DNAA", "DSH", "SA Only", "SA/DSH", "SI Only",
                   "Theft from shop", "Theft from person", "Theft and related offences")
som3 <- som(scale(som[som.measures3]), grid = somgrid(6,4, "hexagonal"))
plot(som3, main = "Clusters for Location, Age, Condition and Theft")

som.measures4 <- c("Location", "U18", "18-24", "25-44", "45-64", "65+",
                   "DNAA", "DSH", "SA Only", "SA/DSH", "SI Only","Possession of a firearm",
                   "Possession of drugs for personal use",
                   "Possession of drugs for sale or supply")
som4 <- som(scale(som[som.measures4]), grid = somgrid(6,4, "hexagonal"))
plot(som4, main = "Pieta House Compared With Drug Possession")

som.measures5 <- c("Location", "U18", "18-24", "25-44", "45-64", "65+",
                   "DNAA", "DSH", "SA Only", "SA/DSH", "SI Only",
                   "Burglary (not aggravated)",
                   "Burglary and related offences",
                   "Criminal damage (not arson)")
som5 <- som(scale(som[som.measures5]), grid = somgrid(6,4, "hexagonal"))
plot(som5, main = "Pieta House Compared With Burglary")

##################################
#suicide 1980 - 2013 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/CSO")
suicide8013 <- read.csv("FINALsuicide8013cuswih.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
library(kohonen)
library(RColorBrewer)

#change location to numeric
som <- as.data.frame(suicide8013)
som$Location <- unclass(som$Location)
som$Location <- as.numeric(som$Location)
colnames(som)

#remove unneeded columns + columns full of NA
som <- som[,-6]
som <- som[,-6]
som <- som[, -87]
som <- som[, colSums(is.na(som)) != nrow(som)]

#create som measures based on variables
#want to find patterns in all genders based on location
som.measures1 <- c("Both", "Date", "New House Prices (Euro)", "Second Hand House Prices (Euro)")
som1 <- som(scale(som[som.measures1]), grid = somgrid(6,4, "hexagonal"))
plot(som1, main = "Clusters Comparing Suicide, Location and Housing Prices")

som.measures2 <- c("Both","Unemploy.u25", "Unemploy.25+")
som2 <- som(scale(som[som.measures2]), grid = somgrid(6,4, "hexagonal"))
plot(som2, main = "Suicde (Male and Female) x Unemployment Rate")

som.measures3 <- c("Both", "Theft from shop",
                   "Theft from person", "Theft and related offences")
som3 <- som(scale(som[som.measures3]), grid = somgrid(6,4, "hexagonal"))

plot(som3, main = "Suicde Compared With Theft")

som.measures4 <- c("Both", "Possession of a firearm",
                   "Possession of drugs for personal use",
                   "Possession of drugs for sale or supply")
som4 <- som(scale(som[som.measures4]), grid = somgrid(6,4, "hexagonal"))
plot(som4, main = "Suicde Compared With Drug Possession")

som.measures5 <- c("Both", "Burglary (not aggravated)",
                   "Burglary and related offences",
                   "Criminal damage (not arson)")
som5 <- som(scale(som[som.measures5]), grid = somgrid(6,4, "hexagonal"))
plot(som5, main = "Suicide Compared With Burglary")

##########################################




##################################
#suicide 1950 - 2017 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/CSO")
suicide5017 <- read.csv("FINALsuicide5017cuswih.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
library(kohonen)
library(RColorBrewer)

#change location to numeric
som <- as.data.frame(suicide5017)
som$Location <- unclass(som$Location)
som$Location <- as.numeric(som$Location)
som$Gender <- unclass(som$Gender)
som$Gender <- as.numeric(som$Gender)
colnames(som)

#remove unneeded columns + columns full of NA
som <- som[,-84]
som <- som[, colSums(is.na(som)) != nrow(som)]

#create som measures based on variables
#want to find patterns in all genders based on location
som.measures1 <- c("Gender", "Date", "Count", "New House Prices (Euro)", "Second Hand House Prices (Euro)")
som1 <- som(scale(som[som.measures1]), grid = somgrid(6,4, "hexagonal"))
plot(som1, main = "Clusters Comparing Suicide, Location and Housing Prices")

som.measures2 <- c("Gender", "Date", "Count","Unemploy.u25", "Unemploy.25+")
som2 <- som(scale(som[som.measures2]), grid = somgrid(6,4, "hexagonal"))
plot(som2, main = "Suicde (Male and Female) x Unemployment Rate")

som.measures3 <- c("Gender", "Date", "Count", "Theft from shop",
                   "Theft from person", "Theft and related offences")
som3 <- som(scale(som[som.measures3]), grid = somgrid(6,4, "hexagonal"))
plot(som3, main = "Suicde Compared With Theft")

som.measures4 <- c("Gender", "Date", "Count", "Possession of a firearm",
                   "Possession of drugs for personal use",
                   "Possession of drugs for sale or supply")
som4 <- som(scale(som[som.measures4]), grid = somgrid(6,4, "hexagonal"))
plot(som4, main = "Suicde Compared With Drug Possession")

som.measures5 <- c("Gender", "Date", "Count", "Burglary (not aggravated)",
                   "Burglary and related offences",
                   "Criminal damage (not arson)")
som5 <- som(scale(som[som.measures5]), grid = somgrid(6,4, "hexagonal"))
plot(som5, main = "Suicide Compared With Burglary")


################################################
#suicide 08 - 13 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/CSO")
suicide0817 <- read.csv("FINALsuicide0817cuswih.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
library(kohonen)
library(RColorBrewer)

#change location to numeric
som <- as.data.frame(suicide0817)
som$Location <- unclass(som$Location)
som$Location <- as.numeric(som$Location)
som$Gender <- unclass(som$Gender)
som$Gender <- as.numeric(som$Gender)
colnames(som)

#remove unneeded columns + columns full of NA
som <- som[, -85]
som <- som[, colSums(is.na(som)) != nrow(som)]

#create som measures based on variables
#want to find patterns in all genders based on location
som.measures1 <- c("Male","Female", "Date", "New House Prices (Euro)", "Second Hand House Prices (Euro)")
som1 <- som(scale(som[som.measures1]), grid = somgrid(6,4, "hexagonal"))
plot(som1, main = "Clusters Comparing Suicide, Location and Housing Prices")

som.measures2 <- c("Male", "Female", "Date","Unemploy.u25", "Unemploy.25+")
som2 <- som(scale(som[som.measures2]), grid = somgrid(6,4, "hexagonal"))
plot(som2, main = "Suicde (Male and Female) x Unemployment Rate")

som.measures3 <- c("Male", "Female", "Date", "Theft from shop",
                   "Theft from person", "Theft and related offences")
som3 <- som(scale(som[som.measures3]), grid = somgrid(6,4, "hexagonal"))
plot(som3, main = "Suicde Compared With Theft")

som.measures4 <- c("Male", "Female", "Date", "Possession of a firearm",
                   "Possession of drugs for personal use",
                   "Possession of drugs for sale or supply")
som4 <- som(scale(som[som.measures4]), grid = somgrid(6,4, "hexagonal"))
plot(som4, main = "Suicde Compared With Drug Possession")

som.measures5 <- c("Male", "Female", "Date", "Burglary (not aggravated)",
                   "Burglary and related offences",
                   "Criminal damage (not arson)")
som5 <- som(scale(som[som.measures5]), grid = somgrid(6,4, "hexagonal"))
plot(som5, main = "Suicide Compared With Burglary")





