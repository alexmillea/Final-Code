#PCA 
#
#Pieta House - 1317
#
# @ reference: https://www.datacamp.com/community/tutorials/pca-analysis-r
# Modified by: Alexander Milela
# Date: 5/04/2019
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")
ph1317 <- read.csv("FINALPieta1317.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)

library(ggfortify)
df <- ph1317
df <- df[, -2]#remove categorical 
names(df)
df$DSHNEW <- df$DSH + df$`DSH `
df <- df[, -12]
df <- df[, -11]
colnames(df)[17] <- "DSH"

#scree plot to find optimal clusters
df.pca <- prcomp(df[, c(1:17)], center = TRUE, scale. = TRUE)
screeplot(df.pca, type = "lines")
#print matrix of clusters found
summary(df.pca)

#plot clusters
biplot(df.pca)

##################################
#Pieta House with Social/Economic Factors
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/CSO")
phse1317 <- read.csv("FINALsuicide1317cuswih.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)

library(ggfortify)
df <- phse1317

#removes columns that are factors
df <- df[,-which(sapply(df, class) == "factor")]
names(df)

#0 variance columns need to be removed 
df <- df[ , apply(df, 2, var) != 0]
df.pca <- prcomp(df[, c(1:101)], center = TRUE, scale. = TRUE)

#print matrix of clusters found
summary(df.pca)

#scree plot to find optimal clusters
screeplot(df.pca, type = "lines")

#rotation matrix - shows variables contributing the most
df.pca$rotation

#plot clusters
biplot(df.pca)

autoplot(prcomp(df), data = phse1317, colour = 'Date')
autoplot(prcomp(df), data = phse1317, colour = 'Location')
autoplot(prcomp(df), data = phse1317, colour = 'Date', 
         loadings = TRUE, loadings.colour = "blue",
         loadings.label = TRUE, loadings.label.size = 3)

##################################
#pca - suicide8013 with factors
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/CSO")
suicide8013 <- read.csv("FINALsuicide8013cuswih.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)

library(ggfortify)
df <- suicide8013
df <- df[,-which(sapply(df, class) == "factor")]
names(df)

df <- df[ , apply(df, 2, var) != 0]
df.pca <- prcomp(df[, c(1:107)], center = TRUE, scale. = TRUE)

#print matrix of clusters found
summary(df.pca)

#scree plot to find optimal clusters
screeplot(df.pca, type = "lines")

#rotation matrix - shows variables contributing the most
df.pca$rotation

#plot clusters
biplot(df.pca)

autoplot(prcomp(df), data = suicide8013, colour = 'Date')
autoplot(prcomp(df), data = suicide8013, colour = 'Location')
autoplot(prcomp(df), data = suicide8013, colour = 'Date', 
         loadings = TRUE, loadings.colour = "blue",
         loadings.label = TRUE, loadings.label.size = 3)

#######################
#pca - suicide0817
suicide0817 <- read.csv("FINALsuicide0817cuswih.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)

library(ggfortify)
df <- suicide0817

df <- df[,-which(sapply(df, class) == "factor")]
names(df)

df <- df[ , apply(df, 2, var) != 0]
df.pca <- prcomp(df[, c(1:106)], center = TRUE, scale. = TRUE)

#print matrix of clusters found
summary(df.pca)

#scree plot to find optimal clusters
screeplot(df.pca, type = "lines")

#rotation matrix - shows variables contributing the most
df.pca$rotation

#plot clusters
biplot(df.pca)

autoplot(prcomp(df), data = suicide0817, colour = 'Date')
autoplot(prcomp(df), data = suicide0817, colour = 'Location')
autoplot(prcomp(df), data = suicide0817, colour = 'Location', 
         loadings = TRUE, loadings.colour = "blue",
         loadings.label = TRUE, loadings.label.size = 3)

########
#PCA 50 - 17
suicide5017 <- read.csv("FINALsuicide5017cuswih.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)

library(ggfortify)
df <- suicide5017

df <- df[,-which(sapply(df, class) == "factor")]
names(df)

df <- df[ , apply(df, 2, var) != 0]
df.pca <- prcomp(df[, c(1:106)], center = TRUE, scale. = TRUE)

#print matrix of clusters found
summary(df.pca)

#scree plot to find optimal clusters
screeplot(df.pca, type = "lines")

#rotation matrix - shows variables contributing the most
df.pca$rotation

#plot clusters
biplot(df.pca)

autoplot(prcomp(df), data = suicide5017, colour = 'Date')
autoplot(prcomp(df), data = suicide5017, colour = 'Location')
autoplot(prcomp(df), data = suicide0817, colour = 'Location', 
         loadings = TRUE, loadings.colour = "blue",
         loadings.label = TRUE, loadings.label.size = 3)





