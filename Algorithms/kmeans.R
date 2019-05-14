#clustering - suicide datasets
#
# @reference: https://www.datacamp.com/community/tutorials/k-means-clustering-r
# Modified by: Alexander Millea
# Date: 12/03/2019
#
#

#Pieta House 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/PietaHouse")
ph1317 <- read.csv("FINALPieta1317.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)

#load libraries needed
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)

#K-Means
#Pieta House 13-17

#change factors to numerics 
df <- ph1317
df$Location <- factor(df$Location)
df$Location <- as.character(df$Location)
df$Location <- as.numeric(factor(df$Location))

df$Crime<- factor(df$Crime)
df$Crime <- as.character(df$Crime)
df$Crime <- as.numeric(factor(df$Crime))

#scale data
df <- scale(df)
head(df)

#compute distance (euclidean)
distance <- get_dist(df)

#visulise - can crash R
#fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#kmeans clustering k = 2, cluster sizes 26 & 64
k2 <- kmeans(df, center = 2, nstart = 25)
k2
fviz_cluster(k2, data = df)

#pairwise scatter plots - can slow down r 
df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(ph1317)) %>%
  ggplot(aes(Male, Female, color = factor(cluster), label = state)) +
  geom_text()

#trying different random values for K
k3 <- kmeans(df, center = 3, nstart = 25)
k3
k4 <- kmeans(df, center = 4, nstart = 25)
k4
k5 <- kmeans(df, center = 5, nstart = 25)
k5
k6 <- kmeans(df, center = 6, nstart = 25)
k6
k7 <- kmeans(df, center = 7, nstart = 25)
k7
k8 <- kmeans(df, center = 8, nstart = 25)
k8
k9 <- kmeans(df, center = 9, nstart = 25)
k9

#plots
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")
p5 <- fviz_cluster(k6, geom = "point",  data = df) + ggtitle("k = 6")
p6 <- fviz_cluster(k7, geom = "point",  data = df) + ggtitle("k = 7")
p7 <- fviz_cluster(k8, geom = "point",  data = df) + ggtitle("k = 8")
p8 <- fviz_cluster(k9, geom = "point",  data = df) + ggtitle("k = 9")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#elbow curve to get optimal k value 
set.seed(7)

#function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}
# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#nbclust to compute elbow curve using wss function
set.seed(7)
fviz_nbclust(df, kmeans, method = "wss")

#optimal number for k is 3 - taken from elbow curve diagram 
#kmeans with k = 3 - taken from elbow curve
set.seed(7)
final <- kmeans(df, 3, nstart = 25)
print(final)

#pairs diagram with clusters related to original dataset
plot(ph1317, col = final$cluster)

#kmeans results 
final$centers #centroids 
final$cluster #clusters
final$size #cluster size 

#table with clusters by date + location
t <- table(ph1317[,1], final$cluster)
t
t <- table(ph1317[,2], final$cluster)
t

#plot - final kmeans result
fviz_cluster(final, data = df, labelsize = 0)

#
# @reference: https://www.statmethods.net/advstats/cluster.html
# Modified by: Alexander Millea
# Date: 15/03/2019
#
#Hierarchial dendogram from results 
df <- scale(df)
#compute dissimilarity matrix
res.dist <- dist(df, method = "euclidean")

#compute hierarchical clustering
res.hc <- hclust(res.dist, method = "ward.D2")

#visual
plot(res.hc, cex = 0.5)

#dissimilarity matrix - euclidean dist already found (distance)
hc1 <- hclust(distance, method = "single") #single linkage
plot(hc1, cex = 0.6, hang = -1)

hc1a <- hclust(distance, method = "average") #avg linkeage
plot(hc1, cex = 0.6, hang = -1)

hc1c <- hclust(distance, method = "complete") #complete linkeage
plot(hc1, cex = 0.6, hang = -1)

plot(hc1c, cex = 0.6)
rect.hclust(hc1, k = 3, border = 2:5)

#agnes 
hc2 <- agnes(df, method = "complete")

#agglomerative coefficient - closer to one the better
#found strong clustering structure .911
hc2$ac

#ward method - shows strongest clustering structure of 4 methods
#methods to assess
m <- c("average", "single", "complete", "ward")
names(m) <- c("average", "single", "complete", "ward")

#function for coeeficients
ac <- function(x){
  agnes(df, method = x)$ac
}

map_dbl(m, ac)

hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "agnes")

#compare 
library(dendextend)
dend1 <- as.dendrogram (hc1c)
dend2 <- as.dendrogram (hc3)

tanglegram(dend1, dend2)

#############################################
#Pieta House 2013-2017with crime
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/CSO")
suicidec <- read.csv("FINALsuicide1317c.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)

df <- suicidec
df$Location <- factor(df$Location)
df$Location <- as.character(df$Location)
df$Location <- as.numeric(factor(df$Location))

df$Crime<- factor(df$Crime)
df$Crime <- as.character(df$Crime)
df$Crime <- as.numeric(factor(df$Crime))

#scale data
df <- scale(df)
head(df)

#compute distance (euclidean)
distance <- get_dist(df)

#visulise -
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#kmeans clustering k = 2, cluster sizes 26 & 64
k2 <- kmeans(df, center = 2, nstart = 25)
k2

#k = 2 plot 
fviz_cluster(k2, data = df)

#pairwise scatter plots 
df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(suicidec)) %>%
  ggplot(aes(Male, Female, color = factor(cluster), label = state)) +
  geom_text()

#trying different values for K
k3 <- kmeans(df, center = 3, nstart = 25)
k3
k4 <- kmeans(df, center = 4, nstart = 25)
k4
k5 <- kmeans(df, center = 5, nstart = 25)
k5
k6 <- kmeans(df, center = 6, nstart = 25)
k6
k7 <- kmeans(df, center = 7, nstart = 25)
k7
k8 <- kmeans(df, center = 8, nstart = 25)
k8
k9 <- kmeans(df, center = 9, nstart = 25)
k9

#plots
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")
p5 <- fviz_cluster(k6, geom = "point",  data = df) + ggtitle("k = 6")
p6 <- fviz_cluster(k7, geom = "point",  data = df) + ggtitle("k = 7")
p7 <- fviz_cluster(k8, geom = "point",  data = df) + ggtitle("k = 8")
p8 <- fviz_cluster(k9, geom = "point",  data = df) + ggtitle("k = 9")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#elbow curve to get optimal k value 
set.seed(7)
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#nbclust to compute elbow curve using wss function - slow
set.seed(7)
fviz_nbclust(df, kmeans, method = "wss")

#optimal number for k is 4 - taken from elbow curve diagram 
#kmeans with k = 4 - taken from elbow curve
set.seed(7)
final <- kmeans(df, 4, nstart = 25)
print(final)

#pairs diagram with clusters related to original dataset
plot(suicidec, col = final$cluster)

#kmeans results 
final$centers #centroids 
final$cluster #clusters
final$size #cluster size 

#table with clusters and locations
t <- table(suicidec[,1], final$cluster)
t
t <- table(suicidec[,2], final$cluster)
t

#plot - final kmeans result
fviz_cluster(final, data = df, labelsize = 0)

########################################
#Suicide 80-13 with additional factors 
suicide8013 <- read.csv("FINALsuicide8013cusw.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
df <- suicide8013

#change factors to numerics - will try kmeans without these columns later on
df$Location <- factor(df$Location)
df$Location <- as.character(df$Location)
df$Location <- as.numeric(factor(df$Location))

df$Gender <- factor(df$Gender)
df$Gender <- as.character(df$Gender)
df$Gender <- as.numeric(factor(df$Gender))

#remove unwanted columns
df <- df[, -89]
df <- df[, - 88]
df <- df[, -84]

#scale data
df <- scale(df)
print(df)

#compute distance (euclidean) - slows down r 
distance <- get_dist(df)

#visulise - crashes r / too slow
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#kmeans clustering k = 2
k2 <- kmeans(df, center = 2, nstart = 25)
k2
fviz_cluster(k2, data = df)

#pairwise scatter plots 
df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(df)) %>%
  ggplot(aes(Location, Female, color = factor(cluster), label = state)) +
  geom_text()

#trying different values for K
k3 <- kmeans(df, center = 3, nstart = 25)
k3
k4 <- kmeans(df, center = 4, nstart = 25)
k4
k5 <- kmeans(df, center = 5, nstart = 25)
k5
k6 <- kmeans(df, center = 6, nstart = 25)
k6

#plots
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#elbow curve to get optimal k value 
set.seed(7)
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#nbclust to compute k using different types of graphs
set.seed(7)
fviz_nbclust(df, kmeans, method = "silhouette") 
fviz_nbclust(df, kmeans, method = "b") 
fviz_nbclust(df, kmeans, method = "gap_stat") 

#optimal number for k is 3 - taken from elbow curve diagram 
#kmeans with k = 3 - taken from elbow curve & gap stat
final <- kmeans(df, 2, nstart = 25)
print(final)

#plots 
fviz_cluster(final, data = df)
final$centers #centroids 
final$cluster #clusters
final$size #cluster size 

#table with clusters and locations
t <- table(suicide8013[,2], final$cluster)
t
############################
#Suicide 80-17 with additional factors 
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Merged")
suicide8017 <- read.csv("FINALsuicide8017cuswih.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
df <- suicide8017

#change factors to numerics - will try kmeans without these columns later on
df$Location <- factor(df$Location)
df$Location <- as.character(df$Location)
df$Location <- as.numeric(factor(df$Location))

df$Gender <- factor(df$Gender)
df$Gender <- as.character(df$Gender)
df$Gender <- as.numeric(factor(df$Gender))

#remove unwanted columns
df <- df[, -101]

#scale data
df <- scale(df)
print(df)

#compute distance (euclidean) - slows down r 
distance <- get_dist(df)

#visulise -
#fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#kmeans clustering k = 2
k2 <- kmeans(df, center = 2, nstart = 25) #63.4%
k2
fviz_cluster(k2, data = df)

#pairwise scatter plots 
#df %>%
# as_tibble() %>%
#mutate(cluster = k2$cluster,
#      state = row.names(df)) %>%
#ggplot(aes(Location, Female, color = factor(cluster), label = state)) +
#geom_text()

#trying different values for K
k3 <- kmeans(df, center = 3, nstart = 25)#73.8%
k3
k4 <- kmeans(df, center = 4, nstart = 25) #76.9%
k4
k5 <- kmeans(df, center = 5, nstart = 25) #78.2
k5
k6 <- kmeans(df, center = 6, nstart = 25) #79.4
k6

#plots
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#elbow curve to get optimal k value 
set.seed(7)
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#nbclust to compute elbow curve using wss function
set.seed(7)
#fviz_nbclust(df, kmeans, method = "silhouette") #5
#fviz_nbclust(df, kmeans, method = "b") 
#fviz_nbclust(df, kmeans, method = "gap_stat") 

#optimal number for k is 3 - taken from elbow curve diagram 
#kmeans with k = 3 - taken from elbow curve & gap stat
final <- kmeans(df, 3, nstart = 25)#76.9
print(final)

#plots 
fviz_cluster(final, data = df)
fviz_cluster(final, data = df, geom = "point")

###########################


###########################
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/DATA - CSO&PIETAHOUSE/CSO")
suicide5017 <- read.csv("FINALsuicide5017cuswih.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
df <- suicide5017

#change factors to numerics - will try kmeans without these columns later on
df$Gender <- factor(df$Gender)
df$Gender <- as.character(df$Gender)
df$Gender <- as.numeric(factor(df$Gender))

#remove unwanted columns
df <- df[, -2]#location
df <- df[, -83]#category

#scale data
df <- scale(df)
print(df)

#compute distance (euclidean) - slows down r 
distance <- get_dist(df)

#visulise -
#fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#kmeans clustering k = 2
k2 <- kmeans(df, center = 2, nstart = 25) #63.7%
k2
fviz_cluster(k2, data = df)

#pairwise scatter plots 
df %>%
 as_tibble() %>%
mutate(cluster = k2$cluster,
      state = row.names(df)) %>%
ggplot(aes(Location, Female, color = factor(cluster), label = state)) +
geom_text()

set.seed(7)
#trying different values for K
k3 <- kmeans(df, center = 3, nstart = 25)
k3
k4 <- kmeans(df, center = 4, nstart = 25) 
k4
k5 <- kmeans(df, center = 5, nstart = 25)
k5
k6 <- kmeans(df, center = 6, nstart = 25)
k6

#plots
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#elbow curve to get optimal k value 
set.seed(7)
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#nbclust to compute elbow curve using wss function
set.seed(7)
fviz_nbclust(df, kmeans, method = "silhouette") #7
fviz_nbclust(df, kmeans, method = "b") 
fviz_nbclust(df, kmeans, method = "gap_stat") 

#optimal number for k is 5 - taken from elbow curve diagram 
#kmeans with k = 5 - taken from elbow curve & gap stat
final <- kmeans(df, 5, nstart = 25)#87.6
print(final)

#plots 
fviz_cluster(final, data = df)
#fviz_cluster(final, data = df, geom = "point")

t <- table(suicide5017[,1], final$cluster)#date
t
t <- table(suicide5017[,2], final$cluster)#location
t
t <- table(suicide5017[,3], final$cluster)#gender
t

df <- scale(df)
#compute dissimilarity matrix
res.dist <- dist(df, method = "euclidean")

#compute hierarchical clustering
res.hc <- hclust(res.dist, method = "ward.D2")

#visual
plot(res.hc, cex = 0.5)


#############################
#hierarchial clustering
#
# @ reference: https://www.r-bloggers.com/how-to-perform-hierarchical-clustering-using-r/ 
# Modified by: Alexander Millea
# Date: 15/03/2019
#

#dissimilarity matrix - euclidean dist already found (distance)
hc1 <- hclust(distance, method = "single") #single linkage
plot(hc1, cex = 0.6, hang = -1)

hc1a <- hclust(distance, method = "average") #avg linkeage
plot(hc1, cex = 0.6, hang = -1)

hc1c <- hclust(distance, method = "complete") #complete linkeage
plot(hc1, cex = 0.6, hang = -1)

plot(hc1c, cex = 0.6)
rect.hclust(hc1, k = 3, border = 2:5)

#agnes 
hc2 <- agnes(df, method = "complete")

#agglomerative coefficient - closer to one the better
#found strong clustering structure .911
hc2$ac

#ward method - shows strongest clustering structure of 4 methods
#methods to assess
m <- c("average", "single", "complete", "ward")
names(m) <- c("average", "single", "complete", "ward")

#function for coeeficients
ac <- function(x){
  agnes(df, method = x)$ac
}

map_dbl(m, ac)

hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "agnes")

#compare 
library(dendextend)
dend1 <- as.dendrogram (hc1c)
dend2 <- as.dendrogram (hc3)

tanglegram(dend1, dend2)

###########################################

#K-MEANS EXAMPLE 1 - UPDATE:
#index.DB is not compatiable with the latest RStudio update
#load libraries needed
#library(tidyverse)  # data manipulation
#library(cluster)    # clustering algorithms
#library(factoextra)

#remove location column - stored as X 
#df <- ph1317
#X <- df[,-2]

#pairs(X)

#kmeans with k = 3
#fitkm <- kmeans(X, centers = 3)
#fitkm

#k = 3 with 10 random starts 
#fitkm <- kmeans(X, centers = 3, nstart = 10)
#fitkm

#vector of numbers corresponding to a location & plotted - can be slow
#colvec <- as.numeric(df[,2])
#plot(X, col = colvec, pch = colvec)

#plot kmeans clusters - can be slow
#plot(X, col = fitkm$cluster, pch = fitkm$cluster)

#df2 <- df[, -2]
#dfScaled <- sapply(df2[, c(1:18)], FUN=function(x) { scale(x, scale = T, center=T)})

#testing multiple k values
#library(cluster)
#library(clusterSim)

#silhouette distance & kmeans 2-10
#results <- list()
#tot.withinss <- c()
#betweenss <- c()
#dbindex <- c()
#silhouettes <- c()
#for (k in 2:10) {
#  results[[k]] <- kmeans(dfScaled, k)
#  tot.withinss[k] <- results[[k]]$tot.withinss
#  betweenss[k] <- results[[k]]$betweenss
#  dbindex[k] <- index.DB(dfScaled, results[[k]]$cluster, centrotypes="centroids")$DB
#  s <- silhouette(results[[k]]$cluster, daisy(dfScaled))
#  silhouettes[k] <- mean(s[,3])
#}

#par(mfrow=c(2,2))
#plot(tot.withinss, xlab="k")
#plot(betweenss, xlab="k")
#plot(dbindex, xlab="k")
#plot(silhouettes, xlab="k")

#fitkm$centers #coords of centroids 
#centers <- fitkm$centers[fitkm$cluster, ] #vector of all centers for each point
#distances <- sqrt(rowSums((dfScaled - centers)^2)) #euclidean 
#summary(distances)
#sd(distances)

#outliers 
#outliers <- order(distances, decreasing = TRUE)[1:5]
#outliers <- (distances > (mean(distances) + (2 * sd(distances))) |
 #              distances < (mean(distances) - (2 * sd(distances))))

#print(dfScaled[outliers, ])

#plot clusters
#par(mfrow=c(1,1))
#plot(dfScaled[,c("Male", "U18")], pch="o", col=fitkm$cluster, cex=0.3)
#plot(dfScaled[,c("Male", "18-24")], pch="o", col=fitkm$cluster, cex=0.3)
#plot(dfScaled[,c("Male", "25-44")], pch="o", col=fitkm$cluster, cex=0.3)
#plot(dfScaled[,c("Male", "45-64")], pch="o", col=fitkm$cluster, cex=0.3)
#plot(dfScaled[,c("Male", "65+")], pch="o", col=fitkm$cluster, cex=0.3)

#plot cluster centers 
#points(fitkm$centers[,c("Male", "U18")], col=1:3, pch=8, cex=10)
#points(fitkm$centers[,c("Male", "18-24")], col=1:3, pch=8, cex=10)
#points(fitkm$centers[,c("Male", "25-44")], col=1:3, pch=8, cex=10)
#points(fitkm$centers[,c("Male", "45-64")], col=1:3, pch=8, cex=10)
#points(fitkm$centers[,c("Male", "65+")], col=1:3, pch=8, cex=10)


#plot outliers 
#points(dfScaled[outliers, c("Male", "U18")], pch="+", col=4, cex=1.5)
#points(dfScaled[outliers, c("Male", "18-24")], pch="+", col=4, cex=1.5)
#points(dfScaled[outliers, c("Male", "25-44")], pch="+", col=4, cex=1.5)
#points(dfScaled[outliers, c("Male", "45-64")], pch="+", col=4, cex=1.5)
#points(dfScaled[outliers, c("Male", "65+")], pch="+", col=4, cex=1.5)


