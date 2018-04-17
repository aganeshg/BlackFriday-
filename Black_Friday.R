library(ggplot2)
library(gridExtra)
##Set the working directory
setwd("C:/Users/Ganesh/Documents/Black_Friday")

##Importing the dataset into the datafram
BFTrain <- read.csv('train.csv',stringsAsFactors = F)
BFTest <- read.csv('test.csv',stringsAsFactors = F)

#Structure of dataset
str(BFTrain)

#Checking Missing data
sapply(BFTrain, function(x) sum(is.na(x)))
sapply(BFTest, function(x) sum(is.na(x)))

#Unique Data For Blackfriday & Data type modification
BFTrain$User_ID <- as.factor(BFTrain$User_ID)
BFTrain$Product_ID <- as.factor(BFTrain$Product_ID)
BFTrain$Marital_Status <- as.factor(ifelse(BFTrain$Marital_Status == 1, 'Married', 'Single'))
BFTrain$Age <- as.factor(BFTrain$Age)
BFTrain$Gender <- as.factor(ifelse(BFTrain$Gender=='M', 'Male', 'Female'))
BFTrain$Occupation <- as.factor(BFTrain$Occupation)
BFTrain$City_Category <- as.factor(BFTrain$City_Category)
BFTrain$Stay_In_Current_City_Years <- as.factor(BFTrain$Stay_In_Current_City_Years)

BFTest$User_ID <- as.factor(BFTest$User_ID)
BFTest$Product_ID <- as.factor(BFTest$Product_ID)
BFTest$Marital_Status <- as.factor(ifelse(BFTest$Marital_Status == 1, 'Married', 'Single'))
BFTest$Age <- as.factor(BFTest$Age)
BFTest$Gender <- as.factor(ifelse(BFTest$Gender=='M', 'Male', 'Female'))
BFTest$Occupation <- as.factor(BFTest$Occupation)
BFTest$City_Category <- as.factor(BFTest$City_Category)
BFTest$Stay_In_Current_City_Years <- as.factor(BFTest$Stay_In_Current_City_Years)

BF_dist <- distinct(BFTrain, User_ID, Age, Gender, Marital_Status, Occupation, City_Category, Stay_In_Current_City_Years)
head(BF_dist)

##creating a new data frame to stor the number of purchase made by each user
userIDCount <- as.data.frame(table(BFTrain$User_ID))
names(userIDCount) <- c("User_ID","User_Purchase_Count")
head(userIDCount)

##Joining i.e. storing the user purchase count in original data frame
BFTrain <- merge(x = BFTrain, y = userIDCount, by = "User_ID", all.x = T)
str(BFTrain)

# writing code such that if a new user comes for the first time his count is set to one in test dataset
BFTest <- merge(x = BFTest, y = userIDCount, by = "User_ID", all.x = TRUE)

#Removing the UserIDCount dataframe
rm(userIDCount)

#In Future, if we get new customer id then we need to assign the values as 1 in testdataset
BFTest[is.na(BFTest$User_Purchase_Count), "User_Purchase_Count"] <- 1
class(BFTest$User_Purchase_Count)

#User_purchase_count need to be change as Int
BFTest$User_Purchase_Count <- as.integer(BFTest$User_Purchase_Count)

#Updating the Blackfriday distinct variables
BF_dist <- distinct(BFTrain, User_ID, Age, Gender, Marital_Status, Occupation, City_Category, Stay_In_Current_City_Years, User_Purchase_Count)
head(BF_dist)

#Visulaizing the graph
d1 <- summary(BF_dist$User_Purchase_Count)

p1 <- ggplot(BF_dist, aes(x=User_Purchase_Count)) +geom_density(fill="red", col="black", alpha=0.80) + annotate(geom = "text", x = 6, y = 0.0125, label = "Min")  + annotate(geom = "text", x = 24, y = 0.013, label = "1st Qu.") + annotate(geom = "text", x = 50, y = 0.0125, label = "Median") + annotate(geom = "text", x = 90, y = 0.013, label = "Mean") + annotate(geom = "text", x = 112, y = 0.0125, label = "3rd Qu.") + annotate(geom = "text", x = 1015, y = 0.0125, label = "Max") + geom_vline(xintercept = c(6, 26, 54, 93.37, 117, 1026), size = 0.2, col = 'black') #+ lims(x = )

p2 <- ggplot(BF_dist, aes(x=User_Purchase_Count)) +geom_histogram(fill="red", col="black", alpha=0.80) 

p3 <- ggplot(BF_dist,aes(x= Age,y=User_Purchase_Count, fill=Age)) + geom_boxplot() + facet_grid(Gender~Marital_Status) + labs(x="Age",y="Customer Purchase Count")

p4 <- ggplot(BF_dist,aes(x= Occupation,y=User_Purchase_Count, fill=Occupation)) + geom_boxplot() + facet_grid(Gender~Marital_Status) + labs(x="Occupation",y="Customer Purchase Count")

p5 <- ggplot(BF_dist,aes(x=Age,y=User_Purchase_Count,fill=Stay_In_Current_City_Years))+geom_boxplot()+facet_grid(City_Category~ Stay_In_Current_City_Years) + labs(x="Age",y="Customer Purchase Count")

p5i <- ggplot(BF_dist,aes(x=Age,y=User_Purchase_Count,fill=Stay_In_Current_City_Years))+geom_boxplot()+facet_grid( Stay_In_Current_City_Years ~ City_Category) + labs(x="Age",y="Customer Purchase Count")

p6 <- ggplot(BF_dist,aes(x=Age,y=User_Purchase_Count,fill=Marital_Status))+geom_boxplot()+facet_grid(Gender~City_Category) + scale_fill_manual(values=c("tan4","limegreen"))  + labs(x="Age",y="Customer Purchase Count")

#grid.arrange(p1, p2, p3, p4 ,p5i ,p6, ncol = 1, nrow = 6); 
d1;p1;p2;p3;p4;p5;p5i;p6

#using product_Id doing the analysis
head(BFTrain$Product_ID)

#creating a new data frame to stor the number of purchase made by each user
ProductIDCount <- as.data.frame(table(BFTrain$Product_ID))
names(ProductIDCount) <- c("Product_ID","Product_Sold_Count")
head(ProductIDCount)

## joining i.e. storing the Product sold count in original data frame
BFTrain <- merge(x = BFTrain, y = ProductIDCount, by = "Product_ID", all.x = TRUE)

str(BFTrain)


## plot using train set since EDA set wont deal with product data
d2 <- summary(BFTrain$Product_Sold_Count)

p7 <- ggplot(BFTrain, aes(x=Product_Sold_Count)) +
  geom_density(fill="red", col="black", alpha=0.80) + 
  annotate(geom = "text", x = 1, y = 0.004, label = "Min")  + 
  annotate(geom = "text", x = 174, y = 0.00385, label = "1st Qu.") + 
  annotate(geom = "text", x = 357, y = 0.004, label = "Median") + 
  annotate(geom = "text", x = 450, y = 0.00385, label = "Mean") + 
  annotate(geom = "text", x = 620, y = 0.004, label = "3rd Qu.") + 
  annotate(geom = "text", x = 1880, y = 0.004, label = "Max") + 
  geom_vline(xintercept = c(1,174,357,450.5,620,1880), size = 0.2, col = 'black') 

d2;p7

# writing code such that if a new user comes for the first time his count is set to one in test dataset
BFTest <- merge(x = BFTest, y = ProductIDCount, by = "Product_ID", all.x = TRUE)
#Now we can remove the UserIDCount dataframe
rm(ProductIDCount)
BFTest[is.na(BFTest$User_Purchase_Count), "User_Purchase_Count"] <- 1
class(BFTest$User_Purchase_Count)
BFTest$User_Purchase_Count <- as.integer(BFTest$User_Purchase_Count)

#Gender male and female vs marital_status
d3 <- table(BF_dist$Gender, BF_dist$Marital_Status)

p8 <- ggplot(BF_dist, aes(x=Gender, fill= Marital_Status)) + 
  geom_bar(position = "dodge") + 
  ggtitle("") +  
  labs(x="Gender",y="No. of distinct Sales") + 
  annotate(geom = "text", x = 0.775, y = 619, label = "719")   + 
  annotate(geom = "text", x = 1.225, y = 847, label = "947") + 
  annotate(geom = "text", x = 1.775, y = 1655, label = "1755") + 
  annotate(geom = "text", x = 2.225, y = 2370, label = "2470") + 
  scale_fill_manual(values=c("red","limegreen")) 

d3; p8

##

unique(BFTrain$Product_Category_1)
