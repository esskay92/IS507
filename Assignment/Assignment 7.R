# LIBRARY IMPORT
library(plyr)
library(MASS)


# Reading the csv file
emp <- read.csv('Employee.csv')
head(emp)
View(emp)

# Converting 'Education' to numerical variable
emp$Education <- revalue(emp$Education, c("Bachelors"='1',"Masters"='2',"PHD"='3'))
emp$Education <- as.numeric(emp$Education)
View(head(emp))

# Converting 'City' to numerical variable
unique(emp$City)
emp$City <- revalue(emp$City, c("Bangalore"='1',"Pune"='2',"New Delhi"='3'))
emp$City <- as.numeric(emp$City)
View(head(emp))

# Converting 'Gender' to numerical variable
unique(emp$Gender)
emp$Gender <- revalue(emp$Gender, c("Male"='1',"Female"='2'))
emp$Gender <- as.numeric(emp$Gender)
View(head(emp))


# Converting 'EverBenched' to numerical variable
unique(emp$EverBenched)
emp$EverBenched <- revalue(emp$EverBenched, c("Yes"='1',"No"='0'))
emp$EverBenched <- as.numeric(emp$EverBenched)
View(head(emp))





# QUESTION 1(a) - PERFORMANCE OF CLASSIFIER USING CROSS VALIDATION

empLDA <- lda(emp$LeaveOrNot ~ .,data = emp, CV=TRUE)
empLDA

# Compare the results of the prediction (Confusion Matrix)
table1<-table(empLDA$class, emp$LeaveOrNot)
table1

# Calculating the accuracy of the model
sum(diag(table1)/sum(table1))





# QUESTION 1(b) - PERFORMANCE CLASSIFIER USING TRAIN TEST SPLIT

library(caTools)

# Set seed to ensure you always have same random numbers generated
set.seed(123)   

# Splits the data in the ratio mentioned in SplitRatio. 
# After splitting marks these rows as logical TRUE 
# The remaining are marked as logical FALSE
sample = sample.split(emp, SplitRatio = 0.70)

# Creates a training dataset named train with rows which are marked as TRUE
train = subset(emp,sample == TRUE) 

# Creates a testing dataset named test with rows which are marked as FALSE
test = subset(emp,sample == FALSE)

# The dependent variable must be categorical (Assuming No Cross-Validation)
empLDA2 = lda(LeaveOrNot ~ ., data = train)
empLDA2

# Predicting the target variable on test data
prd <- predict(empLDA2, test)$class
table2 <- table(prd, test$LeaveOrNot)
table2

# Accuracy of the model
sum(diag(table2)/sum(table2))

# Precision of the model
prec <- 185 / (115 + 185)
prec





# QUESTION 2

# Reading the dataset
df <- read.csv('data.csv')
View(head(df))

# Subsetting the dataset for getting relevant factors
soccer <- df[,c(4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,20,21,22,23,24)]
View(head(soccer))

# Set seed to ensure you always have same random numbers generated
set.seed(123)   

# Splits the data in the ratio mentioned in SplitRatio. 
# After splitting marks these rows as logical TRUE and 
# The remaining are marked as logical FALSE
sample = sample.split(soccer, SplitRatio = 0.70) 

# Creates a training dataset named train with rows which are marked as TRUE
train = subset(soccer,sample == TRUE) 

# Creates a testing dataset named test with rows which are marked as FALSE
test = subset(soccer,sample == FALSE)

# The dependent variable must be categorical (Assuming No Cross-Validation)
soccerLDA = lda(FTR ~ ., data = train)
soccerLDA

# Predicting the target variable on test data
prd <- predict(soccerLDA, test)$class
table3 <- table(prd, test$FTR)
table3

# Accuracy of the model
sum(diag(table3)/sum(table3))


