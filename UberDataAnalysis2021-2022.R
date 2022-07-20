library(dplyr)
library(readr)
library(stats)
library(chron)
library(tidyverse)
library(data.table)
library(hrbrthemes)
library(tidyr)
library(viridis)
library(ggpubr)
library(psych)
library(lubridate)
library(ggrepel)
library(forcats)
library(scales)
library(MASS)

##importing the files Jan-July 2021
Jan2021 <- read.csv("Jan 2021.csv",skip = 5,header = TRUE)
Feb2021 <- read.csv("Feb 2021.csv",skip = 5,header = TRUE)
Mar2021 <- read.csv("Mar 2021.csv",skip = 5,header = TRUE)
Apr2021 <- read.csv("Apr 2021.csv",skip = 5,header = TRUE)
May2021 <- read.csv("May 2021.csv",skip = 5,header = TRUE)
June2021 <- read.csv("Jun 2021.csv",skip = 5,header = TRUE)
July2021 <- read.csv("Jul 2021.csv",skip = 5,header = TRUE)


##combining the data sets
MyUberData <-rbind(Feb2021,Mar2021,Apr2021,May2021,June2021,July2021)
  

##Importing August to April Data
AugustApril <- 
  read.csv("uber_AugApril22.csv",header = TRUE)%>%
   mutate("Request Time" = strptime(Transaction.timestamp..UTC., format = "%m/%d/%Y %H:%M"),
         "Date Requested" = as.IDate(`Request Time`),
         DayofWeek = weekdays(`Date Requested`),
         "Time Requested" = format(strptime(`Request.Time..Local.`,"%I:%M %p"), 
                                   format = "%H:%M"),
         Year = as.numeric(format(`Date Requested`, "%Y")),
         Months = factor(months(as.Date(`Date Requested`)),
                         levels = month.name),
         Employee.Name = paste(First.name,Surname))%>%
    dplyr::select(-c(1,3,4,7,8,9,10,11,16,20,21,24,25,26,29,30,31,32,33,34,35,36))%>%
  rename(Total.Amount = Transaction.amount.in.KES..incl..taxes.)
  


##Combining the two datasets
#UberData21_22 <- rbind(MyUberData[,-1],AugustApril[,-1])

#colnames(Jan_DataCleaning)%in%colnames(New.Data)

##Data cleaning
New.Data <- 
  MyUberData[-c(1,3,4,7,8,9,10,11,16,20,21,24,25,26,29,30,31,32,33,34,35,36)]%>%
  mutate("Request Time" = strptime(Transaction.timestamp..UTC., format = "%Y-%m-%d %H:%M"),
         "Date Requested" = as.IDate(`Request Time`),
        DayofWeek = weekdays(`Date Requested`),
        Year = as.numeric(format(`Date Requested`, "%Y")),
        Months = factor(months(as.Date(`Date Requested`)),
                        levels = month.name),
        Employee.Name = paste(First.name,Surname))%>%
  rename(Total.Amount = Transaction.amount.in.KES..incl..taxes.)
  



Jan_DataCleaning <- 
  (Jan2021[,-1])%>%#,AugustApril[,-1]
    dplyr::select(-c(2,3,6,7,8,9,10,15,19,20,23,24,25,28,29,30,31,32,33,34,35))%>%
    mutate("Request Time" = strptime(Transaction.timestamp..UTC., format = "%d/%m/%Y %H:%M"),
           "Date Requested" = as.IDate(`Request Time`),
           DayofWeek = weekdays(`Date Requested`),
           Year = as.numeric(format(`Date Requested`, "%Y")),
           Months = factor(months(as.Date(`Date Requested`)),
                           levels = month.name),
           Employee.Name = paste(First.name,Surname))%>%
    rename(Total.Amount = Transaction.amount.in.KES..incl..taxes.)
    

##combining the different dataframes into one
AnalysisData <- bind_rows(New.Data,AugustApril,Jan_DataCleaning)

##Analysis of Monthly payments , transactions and Adjustments made **

##Creating empty vectors for the columns we want to create for Payments and Transactions
Transaction = vector(length = 1341)
Payments = vector(length = 1341)

for (i in 1:NROW(AnalysisData$Total.Amount)) {
  if(AnalysisData$Total.Amount[i] <= 0){
    Payments[i] <- AnalysisData$Total.Amount[i]
  }else{(AnalysisData$Total.Amount[i] >= 0)
    Transaction[i] <-  AnalysisData$Total.Amount[i]
  }
}


AnalysisData$Transaction <- Transaction
AnalysisData$Payments <- Payments

##Analysis of Transactions and payments 
Analysis_Expense <- 
  AnalysisData%>%
    dplyr::select(Months,Year,Payments,Transaction)%>%
    distinct(Months,Year,Payments,Transaction)%>%
    group_by(Months,Year)%>%
    summarise(TotalPayments = sum(Payments),
              TotalTransaction = sum(Transaction))%>%
    mutate(Difference = (TotalPayments +TotalTransaction))
    #filter(Year == 2022)


##*find a way of doing it without using distinct

##Analysis of Monthly Expenditure 
MonthlyExpenses <- 
  AnalysisData%>%
    filter(Total.Amount >= 0)%>%
    dplyr::select(Months,Total.Amount,Year,Transaction.type)%>%
    distinct(Months,Total.Amount,Year,Transaction.type)%>%
    group_by(Months,Year)%>%
    summarise(MonthlyExpenditure = sum(Total.Amount))%>%
    dplyr::select(Year,MonthlyExpenditure)


##Visualization using a Histogram
ggplot(MonthlyExpenses,aes(fill = Months,x = Months,y = MonthlyExpenditure))+
  geom_histogram(position = "dodge",stat = "identity")+
  facet_grid(.~Year)+
  ggtitle("Monthly Expenditure")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



AnalysisData%>%
  filter(Total.Amount >= 0)%>%
  mutate(quarterz = quarters(`Date Requested`, frequency(4)))%>%
  dplyr::select(quarterz,Total.Amount,Year,Months)%>%
  group_by(quarterz,Year)%>%
  summarise(MonthlyExpenditure = sum(Total.Amount))%>%
 
#importing departmental data
Employee_Data <- read.csv("EmployeeNames.csv", header = TRUE)
AnalysisData$Employee.Name

codes = Employee_Data$Employee.Name
Department <- vector(mode="character", length=nrow(AnalysisData))

for (i in 1:nrow(AnalysisData)) {
  idx = match(AnalysisData$Employee.Name[i], codes)
  Department[i] <- Employee_Data$Department[idx]
}


##Departmental analysis
DeptAnalysis <- 
  cbind(AnalysisData,Department)%>%
    #filter(Total.Amount >= 0)%>%
    dplyr::select(Department,Transaction,Months,Year,Transaction.type)%>%
    group_by(Department,Months,Year,Transaction.type)%>%
    filter(Year == 2022)%>%
    summarise(DeptTotals = sum(Transaction))
  

DeptAnalysis$Department[which(is.na(DeptAnalysis$Department))] <- "Service&Technology"

##the NAs are service and technology fee
##Visualizing the Departments data
ggplot(DeptAnalysis,aes(fill = Department,x = Department,y = DeptTotals))+
  geom_histogram(position = "dodge",stat = "identity")+
  facet_grid(.~Months)+
  ggtitle("Monthly Expenditure")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


##Analysis of frequency of order
frequencyOfOrder <- 
  AnalysisData%>%
    filter(Total.Amount >= 0)%>%
    dplyr::select(Months,Year,Transaction.type, Employee.Name)%>%
    group_by(Months, Year)%>%
    count()%>%
    rename(freq = n)

#Visualizing the number of trips in each month
ggplot(frequencyOfOrder,aes(fill = Months,x = Months,y = freq))+
  geom_histogram(position = "dodge",stat = "identity")+
  facet_grid(.~Year)+
  ggtitle("Monthly Expenditure")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
##There seems to an increase in number of trips 
###Correlation between distance duration and amount paid
Correlation <-   
  AnalysisData%>%
    mutate(Distance = as.numeric(Distance..mi.),
           Duration = as.numeric(Duration..mins.))%>%
    filter(Year == "2022")%>%
    dplyr::select(Duration,Distance,Total.Amount)%>%
    na.omit()

    
cor(Correlation)

