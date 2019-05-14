#PHdataive Statistics 
#Pieta House - Age 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/")
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/ShinyApp/irishsuicides")
phAge <- read.csv("phAge.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
suicide08 <- read.csv("suicide0817.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
suicide80 <- read.csv("suicide8013.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
suicide50 <- read.csv("suicide5017.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)


str(PHdata)
summary(PHdata)

#gives deeper insights into data
library(Hmisc)
describe(PHdata)

#4 histograms 2x2
par(mfrow=c(2,3))

#Age category comparison
hist(PHdata$U18, col = "yellow", main = "Under 18 years")
hist(PHdata$`18-24`, col = "blue", main = "Age 18 - 24 years")
hist(PHdata$`25-44`, col = "green", main = "Age 25 - 44 years")
hist(PHdata$`45-64`, col = "pink", main = "Age 25 - 44 years")
hist(PHdata$`65+`, col = "red", main = "Age 25 - 44 years")

#Gender category comparison
par(mfrow=c(1,3))
hist(PHdata$Male, col = "pink", main = "Num. of Male Clients")
hist(PHdata$Female, col = "blue", main = "Num.of Female Clients")
hist(PHdata$Transgender, col = "yellow", main = "Num. of Transgender Clients")

#sum DSH columns into 1
PHdata$DSHnew <- PHdata$DSH + PHdata$`DSH `
PHdata <- PHdata[, -12]
PHdata <- PHdata[, -12]

#condition category comparison
par(mfrow=c(2,4))
hist(PHdata$DNAA ,col = "yellow", main = "DNAA")
hist(PHdata$DSHnew, col = "blue", main = "DSH")
hist(PHdata$`SA Only`, col = "pink", main = "SA Only")
hist(PHdata$`SA/DSH`, col = "red", main = "SA / DSH")
hist(PHdata$`SI Only`, col = "green", main = "SI Only")
hist(PHdata$`SI/DSH`, col = "purple", main = "SI / DSH")
hist(PHdata$`SI/SA`, col = "orange", main = "SI / SA")
hist(PHdata$`SI/SA/DSH`, col = "beige", main = "SI / SA / DSH")


par(mfrow = c(1,3))
boxplot(PHdata$Male, col = "red") #outliers
boxplot(PHdata$Female, col = "green") #outliers
boxplot(PHdata$Transgender, col = "blue") #outliers

par(mfrow = c(1,1))
#boxplots comparing gender 
boxplot(PHdata$Male, PHdata$Female, PHdata$Transgender,
        names = c("Male", "Female", "Transgender"),
        main = "Gender Comparison on Seeking Sucidal Help",
        col = rainbow(3))

boxplot(PHdata$U18, PHdata$`18-24`, PHdata$`25-44`, PHdata$`45-64`, PHdata$`65+`,
        names = c("U18", "18-24", "25-44", "45-64", "65+"),
        main = "Age Comparison on Seeking Sucidal Help",
        col = rainbow(5))




