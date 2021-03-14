
library(dplyr)
library(zoo)
set.seed(120)

library(readxl)
Online_Retail <- read.csv("A:/DATA_SETS/Online_Retail.csv")
View(Online_Retail) 

#Show the breakdown of the number of transactions by countries i.e. how many transactions 
#are in the dataset for?each country(consider all records including cancelled transactions).
#Show this in total number and also in percentage.
#Show only countries accounting for more than 1% of the total transactions.

Data1<- Online_Retail %>%
  group_by(Country)%>%
  summaris?(transactions = n())%>%
  mutate(percentage= (transactions/541909)*100)%>%
  arrange(desc(transactions))%>%
  filter(data <- percentage > 1)
Data1
#Create a new variable 'TransactionValue' that is the product of the exising 'Quantity' and 'UnitPrice' varia?les.
#Add this variable to the dataframe. 
 
Online_Retail<- mutate(Online_Retail, "TransactionValue"=TransactionValue<- Online_Retail$Quantity * Online_Retail$UnitPrice)
View(Online_Retail)        

#Using the newly created variable,TransactionValue, show?the breakdown of 
#transactionvaluesby countries i.e. how much money in total has been spent each country. 
#Show this in total sum of transaction values. 
#Show only countries with total transaction exceeding 130,000 British Pound.
#Online_Retail$Country ?- as.factor(Online_Retail$Country)

Online_Retail%>%
  group_by(Country)%>%
  summarise(total.sum.of.transaction.values = sum(TransactionValue))%>%
  arrange(desc(total.sum.of.transaction.values))%>%
  filter(total.sum.of.transaction.values>130000)

#Plot ?he histogramof transaction values from Germany.Usethe hist() function to plot.
#data1<- c(Online_Retail$Country,"TV"=Online_Retail$TransactionValue)
#length(Online_Retail$TransactionValue[Online_Retail$Country=="Germany"])

hist(x=log(Online_Retail$Transac?ionValue[Online_Retail$Country=="Germany"]),xlab = "TransactionValue",col = 'RED' ,main = 'Germany Transaction')

#Which customer had the highest number of transactions? 
#Which customer is most valuable (i.e. highest total sum of transactions)?

data<- On?ine_Retail %>%
  group_by(CustomerID)%>%
  summarise(CustomerTransaction = n())%>%
  filter(CustomerID != "NA")%>%
  filter(CustomerTransaction ==max(CustomerTransaction) )

#data<- Online_Retail[which.max(Online_Retail$TransactionValue),]

print(paste('cu?tomerID had the highest number of transactions is',data$CustomerID,'with max transaction of ',data$CustomerTransaction))

data2<- Online_Retail%>%
  group_by(CustomerID)%>%
  summarise(total.transaction.by.each.customer = sum(TransactionValue))%>%
  arrang?(desc(total.transaction.by.each.customer))%>%
  filter(CustomerID != "NA")%>%
  filter(total.transaction.by.each.customer ==max(total.transaction.by.each.customer) )
  
print(paste('Most valuable customerID is',data2$CustomerID,'with total transaction Amou?t $',data2$total.transaction.by.each.customer))


#Calculate  the  percentage  of  missing  values  for  each variable  in  the  dataset

NullValue<-colMeans(is.na(Online_Retail))
print(paste('Online customerID column has missing values in dataset and  i.e?',NullValue['CustomerID']*100,'% of whole data'))

#What are the number oftransactions withmissing CustomerID recordsby countries? 


Online_Retail%>%
  group_by(Country)%>%
  filter(is.na(CustomerID))%>%
  summarise(No.of.missing.CustomerID=n())



#In th? retail sector, it is very important to understand the return rate of the goods purchased by customers.  
#In  this  example,  we  can  define  this  quantity,  simply,as  the  ratio  of  the  numberof
#transactions cancelled (regardless of the transaction?value) over the total number of transactions.
#With this definition, what is the return rate for the French customers? (10marks). 
#Consider the cancelled transactions as those where the 'Quantity' variable hasa negative value.

return_val<-nrow(Online_Ret?il%>%
  group_by(CustomerID)%>%
  filter((Country=='France')&(TransactionValue<0)&(CustomerID != 'Na')))


total_french_customer<-nrow(Online_Retail%>%
  group_by(CustomerID)%>%
  filter((Country=='France')&(CustomerID != 'Na')))
  
  
print(paste('Return ?ate for french customer is given as',((return_val)/(total_french_customer))*100,'Percent'))



#What is the product that has generated the highest revenue for the retailer? 

Total_customer1<-Online_Retail%>%
  group_by(Description,StockCode)%>%
  summaris?(n=sum(TransactionValue))%>%
  arrange(desc(n))
a<- Total_customer1[Total_customer1['n']==max(Total_customer1['n']),]

print(paste('The product generated the highest revenue is', a$Description,'with stock code',a$StockCode))







#How many unique custome?s are represented in the dataset?


Total_customer<-Online_Retail%>%
  group_by(CustomerID)%>%
  summarise(n=n())%>%
  filter(! is.na(CustomerID))
  

print(paste('Total no. of customers with valid customer id are ',length(unique(Online_Retail$CustomerID))?1,'. This does not include null CustomerID'))



######################OPTIONAL QUESTIONS########################

#let's convert 'InvoiceDate' into a POSIXltobject:
Temp=strptime(Online_Retail$InvoiceDate,format='%m/%d/%Y %H:%M',tz='GMT')
View(Temp)
#Now,?let's separate date,  day  of  the  week  and  hour components dataframe with names as 
#New_Invoice_Date,Invoice_Day_Weekand New_Invoice_Hour:
Online_Retail$New_Invoice_Date<-as.Date(Temp)
#knowing two date values,the object allows you to know the differe?ce between the two dates in terms of the number days. 
Online_Retail$New_Invoice_Date[20000]-Online_Retail$New_Invoice_Date[10]
#Also we can convert dates to days of the week. Let's define a new variable for that
Online_Retail$Invoice_Day_Week=weekdays(Onl?ne_Retail$New_Invoice_Date)
#For the Hour, let's just take the hour (ignore the minute)  and  convert  into  a  normal  numerical value:
Online_Retail$New_Invoice_Hour =as.numeric(format(Temp,"%H"))
#Finally, lets define the month as a separate numeric var?able too:
Online_Retail$New_Invoice_Month = as.numeric(format(Temp, "%m"))
View(Online_Retail)

#Show the percentage of transactions (by numbers) by days of the week 
WeeklyNumber<- Online_Retail%>%
  group_by(Invoice_Day_Week)%>%
  summarise(Number.of.tra?saction=(n()))%>%
  mutate(Number.of.transaction,'percent'=(Number.of.transaction*100)/sum(Number.of.transaction))
WeeklyNumber
#Show  the  percentage  of  transactions  (by transaction  volume)  bydays  of  the  week 
WeeklyVolume<- Online_Retail%>%
  gro?p_by(Invoice_Day_Week)%>%
  summarise(Volume.of.transaction=(sum(TransactionValue)))%>%
  mutate(Volume.of.transaction,'percent'=(Volume.of.transaction*100)/sum(Volume.of.transaction))
WeeklyVolume

#Show the percentage of transactions (by transaction volu?e) by month of the year
MonthlyVolume<-Online_Retail%>%
  group_by(New_Invoice_Month)%>%
  summarise(Volume.By.Month=sum(TransactionValue))%>%
  mutate(Volume.By.Month,'Percent'=(Volume.By.Month*100)/sum(Volume.By.Month))
MonthlyVolume

#What was the date ?ith the highest number of transactions from Australia?

c<-Online_Retail%>%
  group_by(New_Invoice_Date,Country)%>%
  filter(Country=='Australia')%>%
  summarise(Number=sum(Quantity),amount=sum(TransactionValue))%>%
  arrange(desc(Number))

c<-c[c['Number'?==max(c['Number']),]  
c  
print(paste('The date with the highest number of transactions from Australia is', c['New_Invoice_Date'],'which is',c['amount'],'$'))

#The company needs to shut  down the  website  for twoconsecutivehours for maintenance. 
#What ?ould be the hour of the day to start this so that the distribution is at minimum for the customers? 
#The responsible IT team is available from 7:00 to 20:00 every day.

d=Online_Retail%>%
  group_by(New_Invoice_Hour)%>%
  summarise(Total.transaction= n())?d
e<-rollapply(d['Total.transaction'],2,sum)

print('As per the details in the morning between 7 to 9 is the best time for shut  down the  website  for twoconsecutivehours for maintenance')

#On average, how often the costumers comeback to the website for ?heir next shopping? 
#(i.e. what is  the average  number  of  days  between  consecutive  shopping)  (
#Optional/Golden  question:  18additional  marks!)
#Hint:  1.  A  close  approximation  is  also  acceptable  and  you  may  find diff() functionuseful.
?aa<-Online_Retail%>%
  group_by(CustomerID)%>%
  summarise(difference.in.consecutivedays= diff(New_Invoice_Date))%>%
  filter(difference.in.consecutivedays>0)

print(paste('the average  number  of  days  between  consecutive  shopping is',mean(aa$differenc?.in.consecutivedays)))
