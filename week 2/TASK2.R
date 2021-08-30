library(reshape2)
library(waffle)
library(ggthemes)
library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)
library(dplyr)
library(plotrix)

getwd()
setwd("C:/Users/prasa/OneDrive/Desktop/Data glacier/DataSets-main/DataSets-main")
Cab_Data <- read.csv("Cab_Data.csv")
City <- read.csv("City.csv")
Customer_ID <- read.csv("Customer_ID.csv")
Transaction_ID <- read.csv("Transaction_ID.csv")

#Hypothesis
#Is demand for Cab for Male is higher than Female?
#Is Avg profit earned by both Cab Companies the same?
#Does the demand for the cab increases with the increase in Age?
#Is there any Seasonality for the demand of cab rides
#People tend to pay more by card than in cash


#Creating a Master Data
#Joining Customer Id data with Transaction ID Data

Master_data = merge(x=Cab_Data,y=City,by="City")
Master_data1 <- merge(x=Transaction_ID,y=Customer_ID, all.x = TRUE)
Final_Master_Data <- merge(x= Master_data1, y=Master_data, all.x = TRUE)

str(Final_Master_Data)

summary(Final_Master_Data)
#Checking NA values in a Customer ID
colSums(is.na(Final_Master_Data))
Final_Master_Data <- na.omit(Final_Master_Data) 
colSums(is.na(Final_Master_Data))
View(Final_Master_Data)

#Checking for duplicate unique values
Final_Master_Data[duplicated(Final_Master_Data)]
Final_Master_Data <- Final_Master_Data %>% distinct()

# Changing the data type
Final_Master_Data$Population <- as.numeric(gsub(",", "", Final_Master_Data$Population)) 
Final_Master_Data$Users <- as.numeric(gsub(",", "", Final_Master_Data$Users)) 
#checking for outliers
boxplot(Final_Master_Data$Age)
boxplot(Final_Master_Data$Income..USD.Month.)
boxplot(Final_Master_Data$KM.Travelled)
boxplot(Final_Master_Data$Cost.of.Trip)
boxplot(Final_Master_Data$Population)
boxplot(Final_Master_Data$Users)
boxplot(Final_Master_Data$Price.Charged)


#assumption - cost of ride is directly proportional to amount of time taken and the timings (In night more price is charged)
#therefore we are not going to remove outliers

#changing the data type
Final_Master_Data$Company <- as.factor(Final_Master_Data$Company)
Final_Master_Data$City <- as.factor(Final_Master_Data$City)
Final_Master_Data$Payment_Mode <- as.factor(Final_Master_Data$Payment_Mode)
Final_Master_Data$Gender <- as.factor(Final_Master_Data$Gender)

#Data Transformation
#Introducing Profit,Profit/km,Cost/km.
Final_Master_Data <- Final_Master_Data %>% mutate(Profit = Final_Master_Data$Price.Charged-Final_Master_Data$Cost.of.Trip)
hist(Final_Data$Profit)

#There are some transactions which are not in profit.Cost of trip is greater than the price charged to customer, which is not likely to happen.
#Therefore the transactions where Cost of ride is greater than price charged will be considered as a Redundant data and it is removed
str(Final_Master_Data$Profit)

Final_Master_Data <- Final_Master_Data[Final_Master_Data$Profit > 0,]
hist(Final_Master_Data$Profit)
boxplot(Final_Master_Data$Profit)

#timing of the ride is not known ...as more rates can b charged in the night
#time duration is not known...as more amount of time can result into higher profit
#not removing outliers from Profit

Final_Master_Data <- Final_Master_Data %>% mutate(Profit_per_km = Final_Master_Data$Profit/Final_Master_Data$KM.Travelled)
Final_Master_Data$Profit_per_km <- round(Final_Master_Data$Profit_per_km, digits = 2)
hist(Final_Master_Data$Profit_per_km)

Final_Master_Data <- Final_Master_Data %>% mutate(Cost_per_km = Final_Master_Data$Cost.of.Trip/Final_Master_Data$KM.Travelled)
Final_Master_Data$Cost_per_km <- round(Final_Master_Data$Cost_per_km, digits = 2)
hist(Final_Master_Data$Cost_per_km)

#sorting date
Final_Master_Data <- Final_Master_Data %>% separate(Date.of.Travel, c('Day', 'Month','Year'))
Final_Master_Data$Day <- as.factor(Final_Master_Data$Day)
Final_Master_Data$Month <- as.factor(Final_Master_Data$Month)
Final_Master_Data$Year <- as.factor(Final_Master_Data$Year)


#Data exploration
#Exploring Age 
hist(Final_Master_Data$Age, main="Age of Cab drivers",
     xlab="Age in Years", labels = TRUE, ylim = c(30000,120000),
     col="orange", breaks = 5,
     freq=TRUE)

#Company
table(Final_Master_Data$Company)
Cab <- c(`Pink Cab (84693)`=84693, `Yellow Cab(268741)`=268741)
waffle(Cab/5000, rows=7, size=0.5, 
       colors=c("#ffc0cb", "#ffff00"), 
       title="Total Cab Rides", 
       xlab="1 square == 5000 cabs")

# Yellow Cab ride are used by cutomers almost 2.5 times more than Pink Cab

#Gender
table(Final_Master_Data$Gender)
Gender <-as.data.frame(table(Final_Master_Data$Gender))
colnames(Gender)[1] <- "Gender"
colnames(Gender)[2] <- "Count"

# Plot the chart.
piepercent<- round(100*Gender$Count/sum(Gender$Count), 1)
pie(Gender$Count, labels = piepercent, main = "City pie chart",col = rainbow(length(Gender$Count)))

legend("topright", c("Female","Male"), cex = 0.8,
       fill = rainbow(length(Gender$Count)))

# More number of Male customers than Female customers

#Gender Preference over the cab company
Data_Gender <-as.data.frame(table(Final_Master_Data$Gender,Final_Master_Data$Company))
colnames(Data_Gender) <- c("Gender", "Cab", "Count")

ggplot(Data_Gender, aes(x = Cab, y = Count, fill = Gender)) +
  geom_col()

#Both Male and Profit prefer Yellow cab than Pink Cab
#Demand for Male is higher than female

#Gender contribution to the profit of each cabs
Gender_Profit = setNames(aggregate(Final_Master_Data$Profit,by=list(Final_Master_Data$Company,Final_Master_Data$Gender),mean),c("Company","Gender","Profit"))
Gender_Profit <- Gender_Profit %>% spread(Gender,Profit)

#Payment MOde
table(Final_Master_Data$Payment_Mode)
#People pay more than card than Cash

#Age wise preference over cab company
#Exploring by age
Data_Age1 <-as.data.frame(table(Final_Master_Data$Age,Final_Master_Data$Company))
colnames(Data_Age1) <- c("Age1", "Cab", "Count")

ggplot(Data_Age1,aes(x = Age1,y = Count)) + 
  geom_bar(aes(fill = Cab),stat = "identity",position = "dodge") + 
  scale_y_log10()

#High demand for both of cabs before age of 40. Demand of cab decreases for the age 40 and above.
#age wise contribution to the profit

Age_Cab = setNames(aggregate(Final_Master_Data$Profit,by=list(Final_Master_Data$Company,Final_Master_Data$Age),mean),c("Company","Age","Profit"))

ggplot(Age_Cab,aes(x = Age,y = Profit)) + 
  geom_bar(aes(fill = Company),stat = "identity",position = "dodge") + 
  scale_y_log10()
#avg profit contribution for every age is more for yellow cab than pink cab

#Monthly & Yearly analysis for profit
#No seasonality trend as such but in winter holidays demand for cabs are more than in summer.
#Demand for yellow cab is more than pink 
Monthly_total = as.data.frame(table(Final_Master_Data$Month,Final_Master_Data$Company))
colnames(Monthly_total) <- c("Month","Cab","Total")

ggplot(Monthly_total,aes(x = Month,y = Total)) + 
  geom_bar(aes(fill = Cab),stat = "identity",position = "dodge") + 
  scale_y_log10()

#Monthly vs avg profit
Monthly = setNames(aggregate(Final_Master_Data$Profit,by=list(Final_Master_Data$Company,Final_Master_Data$Month),mean),c("Company","Month","Profit"))

ggplot(Monthly,aes(x = Month,y = Profit)) + 
  geom_bar(aes(fill = Company),stat = "identity",position = "dodge") + 
  scale_y_log10()

#Avg profit collected for every month is more for Yellow cab

#Avg profit Vs yearly
Yearly = setNames(aggregate(Final_Master_Data$Profit,by=list(Final_Master_Data$Company,Final_Master_Data$Year),mean),c("Company","Year","Profit"))

ggplot(Yearly,aes(x = Year,y = Profit)) + 
  geom_bar(aes(fill = Company),stat = "identity",position = "dodge") + 
  scale_y_log10()

#Avg profit per year is highest in 2017 for both the cabs
# Demand for cab rides per year
Yearly_total = as.data.frame(table(Final_Master_Data$Year,Final_Master_Data$Company))
colnames(Yearly_total) <- c("Year","Cab","Total")

Yearly_total <- Yearly_total %>% spread(Cab,Total)


#2017 has the highest demand for both of the cab companies

#Cost per km & Profit per km per cab
Cost_per_kilometer = setNames(aggregate(Final_Master_Data$Cost_per_km,by=list(Final_Master_Data$Company),mean),c("Company","Cost per km"))
Profit_per_kilometer = setNames(aggregate(Final_Master_Data$Profit_per_km,by=list(Final_Master_Data$Company),mean),c("Company","Profit per km"))

#Cost per km is more for yellow cab than pink cab
#profit per km is more than twice for yellow cab as compared to pink cab

#Total % of Pink and Yellow Cab Users out of total Users
Users <- as.data.frame(table(Final_Master_Data$City,Final_Master_Data$Company))
colnames(Users) <- c("City", "Cab", "Count")


Final_Users <- merge(x = Users, y = City, by= "City")
Final_Users <- Final_Users[,(-4)]
str(Final_Users)

Final_Users$Count <- as.numeric(Final_Users$Count)

Final_Users$Users <- as.numeric(gsub(",", "", Final_Users$Users)) 

Final_Users <- Final_Users %>% mutate(Percentage = (Count/Users)*100)

#dividing into 3 parts for proper visualization
Users_Final_1 <- Final_Users[(1:14),]
Users_Final_2 <- Final_Users[(15:26),]
Users_Final_3 <- Final_Users[(27:38),]

ggplot(Users_Final_1,aes(x = City,y = Percentage)) + 
  geom_bar(aes(fill = Cab),stat = "identity",position = "dodge") + 
  scale_y_log10()

ggplot(Users_Final_2,aes(x = City,y = Percentage)) + 
  geom_bar(aes(fill = Cab),stat = "identity",position = "dodge") + 
  scale_y_log10()

ggplot(Users_Final_3,aes(x = City,y = Percentage)) + 
  geom_bar(aes(fill = Cab),stat = "identity",position = "dodge") + 
  scale_y_log10()

#Out of total Cab users more number of users prefer yellow can than pink cab except for the cities San diego, Nashville, Sacramento.

#Loyal Customers
Loyal <- as.data.frame(table(Final_Master_Data$Customer.ID,Final_Master_Data$Company))
Loyal <- Loyal %>% spread(Var2,Freq)
colnames(Loyal)[1] <- "Customer ID"

#For loyal customers - customer should take atleast 10 rides

hist(Loyal$`Pink Cab`)
hist(Loyal$`Yellow Cab`)

Loyal_Yellow <- Loyal[Loyal$`Yellow Cab`>= 10,c(1,3)]
Loyal_Pink <- Loyal[Loyal$`Pink Cab`>= 10,c(1,2)]

#More number of customers are loyal to Yellow Cab than Pink Cab

#Recommendation and hypothesis result
#More number of customers are loyal to Yellow Cab than Pink Cab
#Out of total Cab users more number of users prefer yellow can than pink cab except for the cities San diego, Nashville, Sacramento.
#Cost per km is more gor yellow cab than pink cab
#profit per km is more than twice for yellow cab as compared to pink cab
#Not only Cab Demand but also avg profit per year for year 2017 is the highest
#No seasonality trend as such but in winter demand for cabs are more than in summer.
#Demand for yellow cab is more than pink 
#avg profit contribution for every age is more for yellow cab than pink cab
#High demand for both of cabs before age of 40. Demand of cab decreases for the age 40 and above.
#At every age, yellow cab more than pink cab
#Both Male and Profit prefer Yellow cab than Pink Cab
#Cab Demand for Male is higher than female
# Yellow Cab ride are used by customers almost 2.5 times more than Pink Cab

#There from above recommendations we can conclude that investment in Yellow Cab would be much better than pink cab