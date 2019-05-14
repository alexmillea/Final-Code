#statistical analysis
#KW test to compare the mean of male, female and transgender
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Merged")
suicide1317 <- read.csv("Suicide1317.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
df <- suicide1317[c("Male", "Female", "Transgender")]

#visualise 
#visualisation
hist(df$Male, col = "blue")
hist(df$Female, col = "red")
hist(df$Transgender, col = "yellow")

#3 seperate box plots by brand
boxplot(df$Male, df$Female, df$Transgender, 
        horizontal = FALSE,
        main = "Total Suicides Based On Gender",
        col = rainbow(3),
        names=c("Male","Female","Transgender"))

#test for normality
shapiro.test(df$Male) #W = 0.72264, p-value = 9.558e-12
shapiro.test(df$Female) #W = 0.73359, p-value = 1.751e-11
shapiro.test(df$Transgender) #W = 0.4093, p-value < 2.2e-16

#kruskal wallis test
kw.test =  kruskal.test(df)
kw.test
#Kruskal-Wallis chi-squared = 49.247, df = 2, p-value = 2.024e-11



##########################
#age categories
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Merged")
suicide1317 <- read.csv("Suicide1317.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
df <- suicide1317[c("U18", "18-24", "25-44", "45-64", "65+")]

#visualise 
#visualisation
hist(df$U18, col = "blue")
hist(df$`18-24`, col = "red")
hist(df$`25-44`, col = "yellow")
hist(df$`45-64`, col = "green")
hist(df$`65+`, col = "orange")

#3 seperate box plots by brand
boxplot(df$U18, df$`18-24`, df$`25-44`, df$`45-64`,df$`65+`, 
        horizontal = FALSE,
        main = "Total Suicides Based On Age",
        col = rainbow(5),
        names=c("u18","18-24","25-44", "45-64", "65+"))

#test for normality
shapiro.test(df$U18) #W = 0.68852, p-value = 1.602e-12
shapiro.test(df$`18-24`) #W = 0.72899, p-value = 1.355e-11
shapiro.test(df$`25-44`) #W = 0.74255, p-value = 2.91e-11
shapiro.test(df$`45-64`) #W = 0.76359, p-value = 1.007e-10
shapiro.test(df$`65+`) #W = 0.74114, p-value = 2.683e-11

#kruskal wallis test
kw.test =  kruskal.test(df)
kw.test
#Kruskal-Wallis chi-squared = 22.566, df = 4, p-value = 0.0001546

###################
#condition
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Merged")
suicide1317 <- read.csv("Suicide1317.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
df <- suicide1317[c("DNAA", "DSH", "SA Only", "SA/DSH", "SI Only", "SI/DSH", "SI/SA", "SI/SA/DSH")]

#visualise 
#visualisation
hist(df$DNAA, col = "blue")
hist(df$DSH, col = "red")
hist(df$`SA Only`, col = "yellow")
hist(df$`SA/DSH`, col = "green")
hist(df$`SI Only`, col = "orange")
hist(df$`SI/DSH`, col = "pink")
hist(df$`SI/SA`, col = "grey")
hist(df$`SI/SA/DSH`, col = "brown")

#3 seperate box plots by brand
boxplot(df$DNAA, df$DSH, df$`SA Only`, df$`SA/DSH`,df$`SI Only`, df$`SI/DSH`, df$`SI/SA`,df$`SI/SA/DSH`,
        horizontal = FALSE,
        main = "Total Suicides Based On Condition",
        col = rainbow(8),
        names=c("DNAA","DSH","SA Only", "SA/DSH", "SI Only", "SI/DSH", "SI/SA", "SI/SA/DSH"))

#test for normality
shapiro.test(df$DNAA) #W = 0.58941, p-value = 1.757e-14
shapiro.test(df$DSH) #W = 0.59386, p-value = 2.114e-14
shapiro.test(df$`SA Only`) #W = 0.73242, p-value = 1.639e-11
shapiro.test(df$`SA/DSH`) #W = 0.7137, p-value = 5.901e-12
shapiro.test(df$`SI Only`) #W = 0.67089, p-value = 6.708e-13
shapiro.test(df$`SI/DSH`) #W = 0.69831, p-value = 2.636e-12
shapiro.test(df$`SI/SA`) #W = 0.74661, p-value = 3.676e-11
shapiro.test(df$`SI/SA/DSH`) #W = 0.76988, p-value = 1.48e-10


#kruskal wallis test
kw.test =  kruskal.test(df)
kw.test





