#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#
# All graphs were sourced from plot.ly/r 
# @reference: https://plot.ly/feed/#/
# Modified by: Alexander Millea
# Date accessed: 22/04/2019 - 12/05/2019
#
#

library(shiny)
library(shinydashboard)
ph1317 <- read.csv("ph1317.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
phcrime <- read.csv("phcrime.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
suicide8013 <- read.csv("suicide8013.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
suicide50 <- read.csv("suicide5017.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
suicide08 <- read.csv("suicide0817.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
suicide08 <- read.csv("suicide0817.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
s50se <- read.csv("s50se.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
age <- read.csv("linegraph.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)

shinyServer(function(input, output){ 
  
  #valuebox
  output$count <- renderValueBox({
    valueBox(sum(ph1317$Male + ph1317$Female + ph1317$Transgender),
             "Total Num.of People Seeking Help 2013-2017", 
             color = "purple",
             icon = icon("plus"))
  })
  
  #valuebox
  output$age <- renderValueBox({
    valueBox("Male",
             "Gender Seeking Most Help 2013-17", 
             color = "orange",
             icon = icon("meeting"))
  })
  
  #valuebox
  output$location <- renderValueBox({
    valueBox("Lucan",
             "Location With Highest Rate 2013-17", 
             color = "green",
             icon = icon("map"))
  })
  
  #histograms 
  output$histogram <- renderPlot({
  hist(ph1317$Male, 
       col = "blue",
       breaks = input$bins)
  })
  
  output$histogram2 <- renderPlot({
    hist(ph1317$Female,
         col= "pink",
         breaks = input$bins)
  })
  
  output$histogram3 <- renderPlot({
    hist(ph1317$Transgender,
         col = "yellow",
         breaks = input$bins)
  })
  
  #kmeans graph
  selectedData <- reactive({
    ph1317[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  #cluster colour
  output$kmeans <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999", "#d142f4"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  #######end of kmeans
  
  #suicide location dropdown
  # Fill in the spot we created for a plot
  output$phonePlot <- renderPlot({
    
    # Render a barplot
    barplot(suicide8013[,input$Location]*27, 
            main=input$Location,
            ylab="Both",
            xlab="Location")
  })
  
  ### page two
  #valuebox
  output$counttwo <- renderValueBox({
    valueBox(sum(suicide08$Count),
             "Total Number of Suicides 2008 - 2017", 
             color = "purple",
             icon = icon("plus"))
  })
  
  #valuebox
  output$agetwo <- renderValueBox({
    valueBox("Male",
             "Gender With the Hightest Suicide Rates", 
             color = "orange",
             icon = icon("meeting"))
  })
  
  #valuebox
  output$locationtwo <- renderValueBox({
    valueBox("Leinster",
             "Province with Highest Suicide Rates", 
             color = "green",
             icon = icon("map"))
  })
  
  
  
})
  