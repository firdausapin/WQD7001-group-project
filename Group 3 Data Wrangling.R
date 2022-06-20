
getwd()
# 1. Loading the dataset
dir <- getwd()
dir_data <- paste(dir,"/qs-world-university-rankings-2017-to-2022-V2.csv",sep="")
mydata <- read.csv(file = dir_data, head=TRUE, sep=",")
head(mydata)

# 2. Exploring the dataset
#To view the content, structure, summary, variable names and top 5 rows in the dataset
str(mydata) #There are 6842 observations and 15 variables. 
head(mydata) #To view the head of the variables with first 5 observations
tail(mydata) #To view the tail of the variables with last 5 observations.
View(mydata) #To view in table form.
ncol(mydata) #There are 15 columns
nrow(mydata) #There 6842 rows
dim(mydata) #There are 6842 observations and 15 variables.
summary(mydata) #To view the statistical summary of the dataset
names(mydata) #To view the name of each variables in the dataset
glimpse(mydata) #Optional


#3. Finding Missing Values
#To see the number of NAs in each variables using lapply() and is.na() function
lapply(mydata,function(x) { length(which(is.na(x)))})
#There are 75 NAs in student faculty ratio and 3662 NAs in score
#This does not truly represent the number of NAs in the dataset as there are other variables with NAs

#Since there are NAs not identified in R as compared to excel File, we rename all ' ' values as NA
library(dplyr) #Load dplyr library
mydata.NA <- mydata %>% 
  mutate_all(na_if, "")

#To re-observe the number of NAs in each variables after identifying each ' ' values as NA using colSums() and is.na function
colSums(is.na(mydata.NA))
#There are 75 NAs in student faculty ratio and 3662 NAs in score
#68 NAs in rank display, 178 NAs in city, 12 NAs in type, 2 NAs in research output
#75 NAs in student faculty ration, 164 NAs in international students, 2 NAs in size and 78 NAs in faculty count
(colMeans(is.na(mydata.NA)))*100 #To see the percentage of NAs in each variables


#4. Data Cleansing
#4.1 Remove non-useful or irrelevant columns 
mydata1 <- subset(mydata.NA,select=-c(logo,link)) #Remove logo and link column
str(mydata1) #There are 13 variables left with 6482 observations

#4.2 Remove rows with more than 3 NAs
total_row_na <- apply(mydata1, 1, function(x) sum(is.na(x)))
mydata2 <- mydata1[total_row_na < 4,]
str(mydata2) #408 observations with more than 3 NAs was removed
#There are 6434 observations left in the dataset

#4.3 Reassign NAs into 0 for Score variable
mydata2$score[is.na(mydata2$score)] = 0
colSums(is.na(mydata2)) #NAs are no longer flagged in the score variable
str(mydata2)

#4.4 Converting character columns into numerical

#4.4.1 Reassign NAs into 0 before converting to numeric variable
mydata2$international_students[is.na(mydata2$international_students)] = 0 #Reassign NAs into 0 before converting to numeric variable
mydata2$faculty_count[is.na(mydata2$faculty_count)] = 0 

#4.4.2 Remove commas in numbers
mydata2$international_students <- as.numeric(gsub(",","",mydata2$international_students))
mydata2$faculty_count <- as.numeric(gsub(",","",mydata2$faculty_count))

#4.4.3 Convert character variables to numerical
cols.num <- c("international_students","faculty_count")
mydata2[cols.num] <- sapply(mydata2[cols.num],as.numeric)
sapply(mydata2, class)
colSums(is.na(mydata2)) #When checking the NAs, NAs are no longer flagged in the faculty_count & international_students variables

#

write.csv(mydata2, file = "data2.csv") 


