#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(ggplot2)
library(RColorBrewer)
ph1317 <- read.csv("ph1317.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
phcrime <- read.csv("phcrime.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
suicide8013 <- read.csv("suicide8013.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
suicide50 <- read.csv("suicide5017.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
suicide08 <- read.csv("suicide0817.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
suicide08 <- read.csv("suicide0817.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
s50se <- read.csv("s50se.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
age <- read.csv("linegraph.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
#setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Final Code/ShinyApp/irishsuicides")

shinyUI <- dashboardPage(
  
  dashboardHeader(title = "Irish Suicide Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pieta House Statistics", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Suicide Statistics Summary", tabName = "datatwo", icon = icon("th")),
      menuItem("About", tabName = "about", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      #first tab
      tabItem(tabName = "dashboard",
              
              #value boxes
              fluidRow(
                valueBoxOutput("count"),
                valueBoxOutput("age"),
                valueBoxOutput("location")
              ),
              br (), 
              
              #line graph
              fluidRow(
                box(width = 12 ,
                    title = "Pieta House: Help Sought by Age and Location between 2013 - 2017",
                status = "primary",
                solidHeader = TRUE, 
                collapsible = TRUE,
                plot_ly(age, x = age$Location, y = age$U18, name = "Under 18", 
                        type = 'scatter', mode = 'lines', connectgaps = TRUE) %>%
                  add_trace(y = age$`18-24` - 5, name = "18-24", connectgaps = TRUE)%>%
                  add_trace(y = age$`25-44` - 5, name = "25-44", connectgaps = TRUE)%>%
                  add_trace(y = age$`45-64` - 5, name = "45-64", connectgaps = TRUE) %>%
                  add_trace(y = age$`65+` - 5, name = "65+", connectgaps = TRUE)
                )),
              
              br(),
              
              #Scatter graph - Suicide each year by location
              fluidRow(
                box(width = 6,
                        title = "Help Sought by Males Per Location",
                        status = "primary",
                        solidHeader = TRUE, 
                        collapsible = TRUE,
                        side = "left",
                plot_ly(ph1317, 
                        x = ph1317$Date, 
                        y = ph1317$Male,
                        mode = "markers", 
                        color = ph1317$Location,
                        size = ph1317$Date)
                ),
      
              #pie chart - transgender/location
              fluidRow(
                box(width = 6,
                    title = "Percentage of Help Sought by Transgender People Per Location",
                    status = "primary",
                    solidHeader = TRUE, 
                    collapsible = TRUE,
                plot_ly(data = ph1317, labels = ph1317$Location, values = ph1317$Transgender, type = 'pie') %>%
                  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
              ))),
              br(),
              
              #kmeans -ph1317 dataset
              fluidRow(
                box(
                  selectInput('xcol', 'X Variable', names(ph1317)),
                    selectInput('ycol', 'Y Variable', names(ph1317),
                                selected=names(ph1317)[[3]]),
                    numericInput('clusters', 'Cluster count (Select 1:10)', 3,
                                 min = 1, max = 10),
                   plotOutput('kmeans')
               )),
              
              fluidRow(
                box(
                  width = 12,
                  title = "Total Num. of Conditions Reported Per Location",
                  status = "primary",
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  plot_ly(ph1317, x = ph1317$Location, y = ph1317$DSH, type = 'bar', name = 'DSH') %>%
                    add_trace(y = ph1317$DNAA, name = 'DNAA') %>%
                    add_trace(y = ph1317$`SA Only`, name = 'SA Only') %>%
                    add_trace(y = ph1317$`SA/DSH`, name = 'SA/DSH') %>%
                    add_trace(y = ph1317$`SI Only`, name = 'SI Only') %>%
                    add_trace(y = ph1317$`SI/DSH`, name = 'SI/DSH') %>%
                    add_trace(y = ph1317$`SI/SA`, name = 'SI/SA') %>%
                    add_trace(y = ph1317$`SI/SA/DSH`, name = 'SI/SA/DSH') %>%
                    layout(yaxis = list(title = 'Total Num.of Conditions Recorded'), barmode = 'group')
                  
                )),
              #3d plot with crime 
              fluidRow(
                box(width = 12,
                    title = "Pieta House Male/Female Clients Compared With Crime",
                    status = "primary",
                    solidHeader = TRUE, 
                    collapsible = TRUE,
                    plot_ly(phcrime, x = phcrime$`Recorded Crimes`, y = phcrime$Female, z = phcrime$Male, type = 'scatter3d', mode = 'lines',
                            opacity = 1, line = list(width = 6, color = ~Date, reverscale = FALSE))
                    
                )),
              
              #3d scatter plot
              fluidRow(
                box(width = 12,
                    title = "Total Pieta House Help Figures by Gender and Year",
                    status = "primary",
                    solidHeader = TRUE, 
                    collapsible = TRUE,
                  plot_ly(ph1317, x = ~Male, y = ~Female, z = ~Transgender) %>%
                    add_markers(color = ~Date)
                )),
              
              br(),
              
              #historgram - male
              fluidRow(
                tabBox(
                tabPanel(title = "Male", 
                    status = "primary", 
                    solidHeader = TRUE,
                    plotOutput("histogram")),
                tabPanel(title = "Female", 
                    status = "primary", 
                    solidHeader = TRUE,
                    plotOutput("histogram2")),
                tabPanel(title = "Transgender", 
                    status = "primary", 
                    solidHeader = TRUE,
                    plotOutput("histogram3")
              )),
                box(title = "Histogram Slider",
                    status = "warning", 
                    solidHeader = TRUE,
                    sliderInput("bins", "Pieta House Frequency",1, 100, 50)
                )
              )
              
      ), 
      
      #second tab
      tabItem(
        tabName = "datatwo",
        #value boxes
        fluidRow(
          valueBoxOutput("counttwo"),
          valueBoxOutput("agetwo"),
          valueBoxOutput("locationtwo")
        ),
        br (),
        
        #line graph slider suicides 1950 - 2017
        fluidRow(
          box(width = 12,
              title = "Suicide Timeline Between 1950 - 2017",
              status = "primary",
              solidHeader = TRUE, 
              collapsible = TRUE,
              plot_ly(suicide50, x = suicide50$Date, name = "Date") %>%
                add_lines(y = suicide50$Suicides, name = "Suicides") %>%
                #add_lines(y = suicide80$Female, name = "sw", connectgaps = TRUE) %>%
                layout(
                  xaxis = list(
                    rangeselector = list(
                      buttons = list(
                        list(
                          count = 3,
                          label = "3 mo",
                          step = "month",
                          stepmode = "backward"),
                        list(
                          count = 6,
                          label = "6 mo",
                          step = "month",
                          stepmode = "backward"),
                        list(
                          count = 1,
                          label = "1 yr",
                          step = "year",
                          stepmode = "backward"),
                        list(
                          count = 1,
                          label = "YTD",
                          step = "year",
                          stepmode = "todate"),
                        list(step = "all"))),
                    
                    rangeslider = list(type = "date")),
                  
                  yaxis = list(title = "Suicide Count"))
              
          )
        ),
        
        #bar chart - gender suicides between 1980 - 2013
        fluidRow(
          box(
            width = 12,
            title = "Total Number of Suicides Between 1980 - 2013 Based on Gender",
            status = "primary",
            solidHeader = TRUE, 
            collapsible = TRUE,
            plot_ly(suicide8013, x = ~Date, y = ~Male, type = 'bar', name = 'Male', marker = list(color = 'rgb(255,248,17)')) %>%
              add_trace(y = ~Female, name = 'Female', marker = list(color = 'rgb(206,77,129)')) %>%
              add_trace(y = ~Both, name = 'Both', marker = list(color = 'rgb(64,176, 216)')) %>%
              layout(xaxis = list(title = "Date", tickangle = -45),
                     yaxis = list(title = "Total Number of Suicides"),
                     margin = list(b = 50),
                     barmode = 'group')
          )
        ),
        
        br(),
        
        fluidRow(
          box(
            width = 12,
            title = "Comparing Suicides With Social and Economic Factors",
            status = "primary",
            solidHeader = TRUE, 
            collapsible = TRUE,
            plot_ly(s50se, x = s50se$Location, y = s50se$Count, type = 'bar', name = 'Suicides') %>%
              add_trace(y = s50se$`New House Prices (Euro)`, name = 'Housing Prices') %>%
              add_trace(y = s50se$Unemploy.allages, name = 'Unemployment') %>%
              add_trace(y = s50se$`Theft and related offences`, name = 'Theft') %>%
              add_trace(y = s50se$`Burglary and related offences`, name = 'Burglary') %>%
              layout(yaxis = list(title = 'Total Num.of Conditions Recorded'), barmode = 'group')
            
          )),
        
        br(),
        #pie chart - gender suicides 2013 - 2017
        fluidRow(
          box(
            width = 6,
            title = "Suicide Rates By Gender 2008 - 2013",
            status = "primary",
            solidHeader = TRUE, 
            collapsible = TRUE,
            side = "left",
            plot_ly(data = suicide08, labels = suicide08$Gender, values = suicide08$Count, type = 'pie') %>%
              layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
        ),
      
      fluidRow(
        box(width = 6,
            title = "Total Suicides Per Location",
            status = "primary",
            solidHeader = TRUE, 
            collapsible = TRUE,
            plot_ly(suicide08, 
                    x = suicide08$Date, 
                    y = suicide08$Count,
                    mode = "markers", 
                    color = suicide08$Location,
                    size = suicide08$Date)
        ))
      )),
      
      #about tab
      tabItem(
        tabName = "about",
        
        br(),
        
        fluidRow(
          HTML('<center><img src="newlogo.png" width="350"></center>')
          
        ),
        
        br(),
        
        fluidRow(
          HTML('<center>
                  <p style = "font-size: 20px; margin-left: 10%; margin-right: 10%;" >
                    Irish Suicide Rates is an in-depth analysis to identify the
                    influence social and economic factors have on the overall suicide
                    statistic in Ireland. Using publicily available data from both the CSO
                    and Pieta House, the project was conducted using the programming language R
                    and the software tool RStudio. Adhering to the KDD methodology the process involved
                    sourcing data, cleaning and transformation, applying data mining tecniques, and visulisation.
                    R Shiny was chosen as the visualtion platform to display key summary statistics
                    of the data, as well as highlighting key discoveries. 
                  </p>
                  <br />
                  <p style = "font-size: 20px; margin-left: 10%; margin-right: 10%;">
                  The application was developed using R, RStudio and R Shiny. All visualisations were created using the Plotly library.
                  </p>

                  <p style = "font-size: 20px; margin-left: 10%; margin-right: 10%;"> <b> About the Developer:</b>
                  Alexander Millea is a 4th year BSc in Computing Student in the National College of Ireland.
                  Specalising in Data Analytics, this project showcases his knowledge and understanding of both data analytics and the programming language R.
                  </p>

               </center>')
          
        )
        
      )
      )
    )
  )