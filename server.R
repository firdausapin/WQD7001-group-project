#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  dat<-read.table(text="2017 2018 2019 2020 2021 2022
                75941 75941 75941 75941 75941 75941
                65787 57646 67052 66137 66137 65898", header = TRUE)
  
  dat2<-read.table(text="Rank Year outcome
                   Top_10 2017 75941
                   Top_11_20 2017 65787
                   Top_10 2018 75941
                   Top_11_20 2018 57646
                   Top_10 2019 75941
                   Top_11_20 2019 67052
                   Top_10 2020 75941
                   Top_11_20 2020 66137
                   Top_10 2021 75941
                   Top_11_20 2021 66137
                   Top_10 2022 75941
                   Top_11_20 2022 65898", header = TRUE)
  
  Rank <- c("Top 10","Top 11-20")
  Uni2017 <- c(75941,65787)
  Uni2018 <- c(75941,57646)
  Uni2019 <- c(75941,67052)
  Uni2020 <- c(75941,66137)
  Uni2021 <- c(75941,66137)
  Uni2022 <- c(75941,65898)
  
  tabRank <-data.frame(Rank,Uni2017,Uni2018,Uni2019,Uni2020,Uni2021,Uni2022)
  names(tabRank)<- c("Rank", "2017", "2018", "2019", "2020", "2021", "2022")
  
  #user manual
  output$user_manual<-renderText("Application Name: Best Universities around the Globe to Pursue Tertiary Education Based on Their Rankings from 2017 to 2022 

Group Leader: Nur Shafiqah Muhamad Baharum (S2151950)
Group Members: 
i.	Amalin Khairunnisa Abdullah (17125661)
ii.	Muhammad Asyraff bin Ponan (S2128251)
iii.	Firdaus (17220599)
iv.	Vickneswary Perumal (S2150313)

1.0 Brief Introduction
QS World University Rankings is an annual publication of global university rankings by Quacquarelli Symonds (QS). The QS ranking receives approval from the International Ranking Expert Group (IREG) and is viewed as one of the three most-widely read university rankings in the world. Hence, this project serves as a recommendation for students and parents to find the best university and information for universities to know their current ranking and to improve it in the future. 

2.0 Data Source
The data was obtained from Kaggle by Padhma Muniraj on QS World University Rankings from 2017 to 2022 (https://www.kaggle.com/code/padhmam/qs-world-university-rankings-eda-visualization/notebook). 
QS based its rankings to assess performance according to several key aspects of a university's mission: teaching, research, nurturing employability, and internationalisation. The methodological framework applied to assess the performance of education institutions are based on the following metrics:
i.	Academic Reputation (40%)
ii.	Employer Reputation (10%)
iii.	Faculty/Student Ratio (20%)
iv.	Citations per faculty (20%)
v.	International Faculty Ratio (5%)
vi.	International Student Ratio (5%)
Details on the methodology can be referred in here.

3.0 Dataset
The dataset is scrapped from the QR World Ranking website which consists of the following 15 attributes:
i.	university - name of the university
ii.	year - year of ranking
iii.	rank_display - rank given to the university
iv.	score - score of the university based on the six key metrics mentioned above
v.	link - link to the university profile page on QS website
vi.	country - country in which the university is located
vii.	city - city in which the university is located
viii.	region - continent in which the university is located
ix.	logo - link to the logo of the university
x.	type - type of university (public or private)
xi.	research_output - quality of research at the university
xii.	student_faculty_ratio - number of students assigned to per faculty
xiii.	international_students - number of international students enrolled at the university
xiv.	size - size of the university in terms of area
xv.	faculty_count - number of faculty or academic staff at the university

4.0 Data Transformation
Following are the steps for data transformation in QR World Ranking from 2017 to 2022 dataset:
1.	Loading the dataset
2.	Exploring the original dataset
3.	Dealing with missing data
4.	Data cleansing by removing unwanted attributes
5.	Converting character attributes as numerical
For more information on the data transformation, refer to our ui.R file.
Main Libraries used:
library(knitr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(kableExtra)
library(ggthemes)
library(plotly)
library(shiny)
library(stats)
library(psych)
library(corrplot)

5.0 Shiny Application Instruction
1.	The first sidebar consists of the following filters:
a.	University Ranking Based On Year
b.	Select Country of University 
2.	The tabs corresponding to the first side bar are:
a.	Data Overview:
Output: This displays the overview dataset for top 400 universities for each year.
b.	Student Faculty Ratio:
Output: This displays the histrogram of top 400 universities against its’ student faculty ratio.
c.	University Score:
Output: This displays the histrogram of top 400 universities against its’ score.
d.	Total of International Students:
Output: This displays the barchart of number of international students in Top 10 universities against Top 11 to 20 universities.
e.	Academic Research:
Output: This displays the correlation matrix of each attribute.
f.	Country Preferences:
Output: This displays the bargraph of the number of international students in each university.
g.	User Manual

3.	The second sidebar consists of the filter for Research Output.
4.	The third sidebar consists of a slider for World Rank ranging from 1 to 700.

For full codes please refer the GitHub repository. 

6.0 Deployment
The application is deployed on https://www.shinyapps.io/ web server. Web application can be accessed directly from the URL: https://firdaus17220599.shinyapps.io/wqd7001_group_project/

7.0 References
Padhma, M. (2022, February 8). 🎓QS World University Rankings EDA & Visualization. Kaggle. https://www.kaggle.com/code/padhmam/qs-world-university-rankings-eda-visualization/notebook


")
  
  # Create barplot object the plotOutput function is expecting
  output$country_preference <- renderPlot({
    options(repr.plot.width=5, repr.plot.height=5)
    theme_b<-theme(axis.text.x = element_blank(),legend.position = "none")
    
    mydata3 %>% select(year, country, international_students) %>%
      group_by(country, international_students) %>%
      filter(year == input$year) %>%
      ggplot(aes(x = country, y = international_students)) + geom_bar(stat = 'identity', fill = 'orchid4') +
      labs(title = "Country Preferences") +
      theme_b + geom_label(aes(label = country), size = 3)})
  
  output$rankByYear<-renderDataTable(if(input$country=='All' & input$year==2022){mydata[mydata$year==2022,]}
                                     else if(input$country=='All' & input$year!=2022){mydata[mydata$year == input$year,]}
                                     else{mydata[mydata$year == input$year & mydata$country == input$country,]
                                     }, options = list(pageLength = 5, autoWidth = TRUE))
  
  output$faculty_ratio<-renderPlot(hist(df.top400$student_faculty_ratio ,col = c("blue", "red", "gray", "green")))
  
  output$uni_score<-renderPlot(hist(df.top400$score ,col = c("blue", "red", "gray", "green")))
  
  output$dataOverview<-renderDataTable(mydata[1:input$ranking,])
  
  output$researchOutput<-renderDataTable(mydata[mydata$research_output == input$research_output,],options = list(pageLength = 5, autoWidth = TRUE))
  
  output$academic_research<-renderPlot(corrplot(cr, method="number"))
  
  #output$intStudentbyYear<-renderPlot(barplot(height=as.matrix(intStudentTot), main="Number of International Student in The Top 10 University vs 11-20 rank", xlab="Year", beside=TRUE, col=rainbow(2), ylim=c(0,100000) ))
  
  output$intStudentbyYear<-renderPlot(barplot(as.matrix(dat),main="Number of International Student in Top 10 University VS Number of International Student in Rank 11-20 University", xlab="Year", ylab="No. of Student", col.lab="blue", sub="Gold is Rank 1-10 and Red is Rank 11-20", col=c("gold3","red"), beside=TRUE))


})
