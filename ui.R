#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(maps)
library(mapproj)
library(corrplot)
library(stats) #to run quality of variance & t-test
library(psych) #to run descriptive analysis
library(dplyr)
library(ggplot2)
#mydata<-read.csv("data/data2")
mydata<-read.csv("data/UniWorldRank.csv")
#head(mydata)
listOfcountry<-unique(mydata$country)
listOfcountry<-append("All", listOfcountry)
names(listOfcountry)<-listOfcountry
mydata$research_output[mydata$research_output == 'Very high'] <- 'Very High'

top10university2017<- mydata[1:10,]
top10university2018<- mydata[924:933,]
top10university2019<- mydata[1895:1904,]
top10university2020<- mydata[2902:2911,]
top10university2021<- mydata[3964:3973,]
top10university2022<- mydata[5135:5144,]
df.top10 <- rbind(top10university2017,top10university2018,top10university2019,
                  top10university2020,top10university2021,top10university2022)
summary(df.top10)
sapply(df.top10,class)

top400university2017<- mydata[1:400,]
top400university2018<- mydata[924:1323,]
top400university2019<- mydata[1895:2295,]
top400university2020<- mydata[2902:3305,]
top400university2021<- mydata[3964:4363,]
top400university2022<- mydata[5135:5537,]
df.top400 <- rbind(top400university2017,top400university2018,top400university2019,top400university2020,
                   top400university2021,top400university2022)
summary(df.top400)

int.student <- df.top400$international_students
hist(df.top400$score)
boxplot(int.student)
boxplot(df.top400$score)
boxplot(as.numeric(df.top400$faculty_count))
hist(df.top400$student_faculty_ratio)

#compare top 10 and 11-20 from each year
top102017<-mydata[1:10,13]
top102017<-gsub(",","", top102017)
top102017<-as.numeric(top102017)
sumTop102017<-sum(top102017)

top202017<-mydata[11:20,13]
top202017<-gsub(",","", top202017)
top202017<-as.numeric(top202017)
sumTop202017<-sum(top202017)

top102018<-head(mydata[mydata$year==2018,13],10)
top102018<-gsub(",","", top102018)
top102018<-as.numeric(top102018)
sumTop102018<-sum(top102018)

top202018<-head(mydata[mydata$year==2018,13],20)
top202018<-gsub(",","", top202018)
top202018<-as.numeric(top202018)
sumTop202018<-sum(top202018)

finSumtop202018 <- sumTop202018 - sumTop102018

top102019<-head(mydata[mydata$year==2019,13],10)
top102019<-gsub(",","", top102019)
top102019<-as.numeric(top102019)
sumTop102019<-sum(top102019)

top202019<-head(mydata[mydata$year==2019,13],20)
top202019<-gsub(",","", top202019)
top202019<-as.numeric(top202019)
sumTop202019<-sum(top202019)

finSumtop202019 <- sumTop202019 - sumTop102019

top102020<-head(mydata[mydata$year==2020,13],10)
top102020<-gsub(",","", top102020)
top102020<-as.numeric(top102020)
sumTop102020<-sum(top102020)

top202020<-head(mydata[mydata$year==2020,13],20)
top202020<-gsub(",","", top202020)
top202020<-as.numeric(top202020)
sumTop202020<-sum(top202020)

finSumtop202020 <- sumTop202020 - sumTop102020

top102021<-head(mydata[mydata$year==2021,13],10)
top102021<-gsub(",","", top102021)
top102021<-as.numeric(top102021)
sumTop102021<-sum(top102021)

top202021<-head(mydata[mydata$year==2021,13],20)
top202021<-gsub(",","", top202021)
top202021<-as.numeric(top202021)
sumTop202021<-sum(top202021)

finSumtop202021 <- sumTop202021 - sumTop102021

top102022<-head(mydata[mydata$year==2022,13],10)
top102022<-gsub(",","", top102022)
top102022<-as.numeric(top102022)
sumTop102022<-sum(top102022)

top202022<-head(mydata[mydata$year==2022,13],20)
top202022<-gsub(",","", top202022)
top202022<-as.numeric(top202022)
sumTop202022<-sum(top202022)

finSumtop202022 <- sumTop202022 - sumTop102022

finTop102017 <- c(sumTop102017,sumTop202017)
finTop102018 <- c(sumTop102018,finSumtop202018)
finTop102019 <- c(sumTop102019,finSumtop202019)
finTop102020 <- c(sumTop102020,finSumtop202020)
finTop102021 <- c(sumTop102019,finSumtop202021)
finTop102022 <- c(sumTop102019,finSumtop202022)

intStudentTot<-data.frame(finTop102017,finTop102018,finTop102019,finTop102020,finTop102021,finTop102022)
names(intStudentTot)<-c("2017","2018","2019","2020","2021","2022")

mydata4 <- read.csv("data/data2.csv")
top400university2017<- mydata4[1:400,]
top400university2018<- mydata4[924:1323,]
top400university2019<- mydata4[1895:2295,]
top400university2020<- mydata4[2902:3305,]
top400university2021<- mydata4[3964:4363,]
top400university2022<- mydata4[5135:5537,]

df.top400 <- rbind(top400university2017,top400university2018,top400university2019,top400university2020,top400university2021,top400university2022)
write.csv(df.top400, file = "top400university.csv")

mydata3 <- read.csv("/Users/firdausapin/wqd7001_group_project/top400university.csv")

# Test for normality
describe(mydata3$research_output)
describe(mydata3$rank_display)
df <- mydata3
df <- df[, !(names(df) %in% c("No", "university", "year", "country", "city", "student_faculty_ratio", "region", "type", "size"))]
head(df)
cor.test(mydata3$research_output, mydata3$rank_display)
cr<-cor(df)

corrplot(cr, method="pie")
corrplot(cr, method="color")
corrplot(cr, method="number")
corrplot(cr, type="lower")
corrplot(cr, type="upper")



#mydata5<-mydata3[1:100,]
# Define UI for application that draws a histogram
ui <- fluidPage(
  img(src='aa2.png', width="100%"),
  hr(),
  br(),
  # Application title
  #titlePanel("World University Ranking"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("year",
                  label="University Ranking Based On Year",
                  choices=2017:2022,
                  selected = 2022),
      
      selectInput("country",
                  label="Select Country of University",
                  choices=listOfcountry,
                  selected="All")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Data Overview", dataTableOutput("rankByYear")),
                  tabPanel("Student Faculty Ratio", plotOutput("faculty_ratio")),
                  tabPanel("University Score", plotOutput("uni_score")),
                  tabPanel("Total of International Student", plotOutput("intStudentbyYear")),
                  tabPanel("Academic Research", plotOutput("academic_research")),
                  tabPanel("Country Preferences", plotOutput("country_preference")),
                  tabPanel("User Manual", textOutput("user_manual"))
      ),
    )
  ),
  
  hr(),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput("research_output",
                  label="Research Output",
                  choices=list("Very High","High","Medium","Low"),
                  selected ="Very High"),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Data Overview", dataTableOutput("researchOutput")),
      ),
    )
  ),
  
  hr(),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("ranking",
                  "World Rank:",
                  min = 1,
                  max = 700,
                  value = 100)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Data Overview", dataTableOutput("dataOverview"))
      ),
    )
  )
)